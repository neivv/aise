mod bw_render;
mod text;
mod ui;

use std::cell::RefCell;
use std::io;
use std::mem;
use std::path::{Path, PathBuf};
use std::ptr::null_mut;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::time::SystemTime;

use glium::backend::{Context, Facade};
use glium::implement_vertex;
use libc::c_void;
use opengl;
use winapi::um::libloaderapi::{FreeLibrary, GetProcAddress};
use winapi::um::wingdi::{
    wglCreateContext, wglDeleteContext, wglGetCurrentContext, wglGetProcAddress, wglMakeCurrent,
    ChoosePixelFormat, SetPixelFormat, SwapBuffers, PIXELFORMATDESCRIPTOR,
};
use winapi::um::wingdi::{
    PFD_DOUBLEBUFFER, PFD_DRAW_TO_WINDOW, PFD_MAIN_PLANE, PFD_SUPPORT_OPENGL, PFD_TYPE_RGBA,
};
use winapi::um::winuser::{
    CallWindowProcW, GetClientRect, GetDC, ReleaseDC, SetWindowLongPtrW, MSG,
};

use crate::bw;

whack_hooks!(stdcall, 0x00400000,
    0x004E05B0 => create_window();
    0x0041CA00 => redraw_screen();

    0x00410244 => SDrawUpdatePalette(u32, u32, *const u32, u32);
    !0 => TranslateAcceleratorA(*mut c_void, *mut c_void, *const MSG) -> u32;
);

whack_hooks!(stdcall, 0x15000000,
    0x15034CD0 => SDrawLockSurface(u32, *const bw::Rect32, *mut *mut u8, *mut u32, u32) -> u32;
    0x15034740 => SDrawUnlockSurface(u32, *mut u8, u32, u32) -> u32;
    0x15034C20 => SDrawRealizePalette();
);

whack_vars!(init_sc_vars, 0x00400000,
    0x0051BFB0 => bw_window: *mut c_void;
);

thread_local! {
    static CONTEXT: RefCell<Option<Rc<GlState>>> = RefCell::new(None);
}

struct GlState {
    context: Rc<Context>,
    state: RefCell<DrawState>,
}

struct DrawState {
    bw_render: bw_render::BwRender,
    ui: ui::Ui,
    draw_skips: u32,
}

impl DrawState {
    fn new(context: &Rc<Context>) -> DrawState {
        DrawState {
            bw_render: bw_render::BwRender::new(context),
            draw_skips: 7100, // Bw redraws screen a lot during loading, skip those
            ui: ui::Ui::new(context),
        }
    }
}

static OPENGL32_DLL: AtomicUsize = ATOMIC_USIZE_INIT;
static OLD_WINDOW_PROC: AtomicUsize = ATOMIC_USIZE_INIT;

pub unsafe fn init_hooks(patcher: &mut whack::ActivePatcher) {
    let mut exe = patcher.patch_exe(0x00400000);
    init_sc_vars(&mut exe);

    exe.hook_opt(create_window, create_window_hook);
    exe.hook_opt(redraw_screen, redraw_screen_hook);
}

fn redraw_screen_hook(orig: &Fn()) {
    orig();
    let state = match CONTEXT.with(|x| x.borrow().clone()) {
        Some(s) => s,
        None => return,
    };
    let context = &state.context;
    let mut state = state.state.borrow_mut();
    let state = &mut *state;
    if state.draw_skips != 0 {
        state.draw_skips -= 1;
        return;
    }
    let mut frame_buffer = glium::framebuffer::DefaultFramebuffer::back_left(context);
    state.bw_render.draw(context, &mut frame_buffer);
    state.ui.draw(context, &mut frame_buffer);
    context.swap_buffers().unwrap();
}

unsafe fn string_from_u8_ptr(ptr: *const u8) -> std::borrow::Cow<'static, str> {
    let length = (0..).find(|&x| *ptr.add(x) == 0).unwrap();
    String::from_utf8_lossy(std::slice::from_raw_parts(ptr, length))
}

fn create_window_hook(orig: &Fn()) {
    orig();
    unsafe {
        let context = match create_wgl_context(*bw_window) {
            Ok(o) => o,
            Err(e) => {
                error!("Couldn't create WGL context: {}", e);
                return;
            }
        };
        opengl::GetString::load_with(|s| wgl_get_proc_address(s));
        info!(
            "Initialized OpenGL {}, {} / {}",
            string_from_u8_ptr(opengl::GetString(opengl::VERSION)),
            string_from_u8_ptr(opengl::GetString(opengl::RENDERER)),
            string_from_u8_ptr(opengl::GetString(opengl::VENDOR)),
        );
        let debug_behavior = glium::debug::DebugCallbackBehavior::Custom {
            callback: Box::new(gl_debug_callback),
            synchronous: false,
        };
        let context = match glium::backend::Context::new(context, true, debug_behavior) {
            Ok(o) => o,
            Err(e) => {
                error!("Couldn't create GL context: {}", e);
                return;
            }
        };
        let state = RefCell::new(DrawState::new(&context));
        CONTEXT.with(|x| {
            *x.borrow_mut() = Some(Rc::new(GlState {
                context,
                state,
            }))
        });

        // Hook these here since wmode would otherwise overwrite this.
        let mut patcher = crate::PATCHER.lock().unwrap();
        {
            let mut exe = patcher.patch_exe(0x00400000);

            exe.hook(SDrawUpdatePalette, update_palette_hook);
        }
        {
            let lib = crate::windows::LoadLibrary("user32");
            let mut user32 = patcher.patch_library("user32", 0);
            let proc_address = crate::windows::GetProcAddress(lib, "TranslateAcceleratorA");
            let addr = proc_address as usize - lib as usize;
            user32.hook_closure_address(TranslateAcceleratorA, translate_accelerator_hook, addr);
            FreeLibrary(lib);
        }
        {
            // Hooking storm for aidebug compat
            let mut storm = patcher.patch_library("storm", 0);

            storm.hook_opt(SDrawLockSurface, lock_surface_hook);
            storm.hook_opt(SDrawUnlockSurface, unlock_surface_hook);
            storm.hook_closure(SDrawRealizePalette, |_: &Fn()| {});
        }
        hook_inputs(*bw_window);
    }
}

unsafe fn hook_inputs(window: *mut c_void) {
    let old_proc = SetWindowLongPtrW(window as *mut _, -4, window_proc as _);
    OLD_WINDOW_PROC.store(old_proc as usize, Ordering::Relaxed);
}

fn toggle_debugui() {
    CONTEXT.with(|x| {
        let s = x.borrow();
        if let Some(ref s) = *s {
            let mut state = s.state.borrow_mut();
            state.ui.toggle_shown();
        }
    });
}

fn handle_msg(msg: u32, wparam: usize, _lparam: isize) -> bool {
    use winapi::um::winuser::{VK_F1, WM_KEYDOWN};

    match msg {
        WM_KEYDOWN => {
            let key = wparam as i32;
            if key == VK_F1 {
                toggle_debugui();
                return true;
            }
        }
        _ => (),
    }
    false
}

fn translate_accelerator_hook(
    hwnd: *mut c_void,
    accel: *mut c_void,
    msg: *const MSG,
    orig: &Fn(*mut c_void, *mut c_void, *const MSG) -> u32,
) -> u32 {
    if unsafe { handle_msg((*msg).message, (*msg).wParam, (*msg).lParam) } == true {
        1
    } else {
        orig(hwnd, accel, msg)
    }
}

unsafe extern "system" fn window_proc(
    hwnd: *mut c_void,
    msg: u32,
    wparam: usize,
    lparam: isize,
) -> isize {
    let old_proc = OLD_WINDOW_PROC.load(Ordering::Relaxed);
    if handle_msg(msg, wparam, lparam) == true {
        return 0;
    }
    if old_proc != 0 {
        CallWindowProcW(
            mem::transmute(old_proc),
            hwnd as *mut _,
            msg,
            wparam,
            lparam,
        )
    } else {
        0
    }
}

fn update_palette_hook(start: u32, count: u32, colors: *const u32, _: u32) {
    CONTEXT.with(|x| {
        let s = x.borrow();
        if let Some(ref s) = *s {
            let mut state = s.state.borrow_mut();
            let slice = unsafe { std::slice::from_raw_parts(colors, count as usize) };
            state.bw_render.update_palette(&s.context, start, slice);
        }
    });
}

fn lock_surface_hook(
    id: u32,
    area: *const bw::Rect32,
    out: *mut *mut u8,
    width: *mut u32,
    unused: u32,
    orig: &Fn(u32, *const bw::Rect32, *mut *mut u8, *mut u32, u32) -> u32,
) -> u32 {
    unsafe {
        if id != 0 || out.is_null() || width.is_null() {
            return orig(id, area, out, width, unused);
        }
        let ptr = CONTEXT.with(|x| {
            let s = x.borrow();
            if let Some(ref s) = *s {
                let mut state = s.state.borrow_mut();
                // I hope the RefCell is enough to have this mutability be reasonable :)
                Some(state.bw_render.get_framebuf().as_mut_ptr())
            } else {
                None
            }
        });
        if let Some(ptr) = ptr {
            *out = ptr;
            *width = 640;
            1
        } else {
            return orig(id, area, out, width, unused);
        }
    }
}

fn unlock_surface_hook(
    id: u32,
    ptr: *mut u8,
    unused1: u32,
    unused2: u32,
    orig: &Fn(u32, *mut u8, u32, u32) -> u32,
) -> u32 {
    if id != 0 {
        return orig(id, ptr, unused1, unused2);
    }
    CONTEXT.with(|x| {
        let s = x.borrow();
        if let Some(ref s) = *s {
            let mut state = s.state.borrow_mut();
            state.bw_render.framebuf_updated();
        }
    });
    1
}

fn gl_debug_callback(
    source: glium::debug::Source,
    ty: glium::debug::MessageType,
    severity: glium::debug::Severity,
    id: u32,
    handled: bool,
    msg: &str,
) {
    use glium::debug::Severity;
    if handled {
        return;
    }
    match severity {
        Severity::Notification | Severity::Low => {
            debug!("GL debug: {:?} {:?} {}: {}", source, ty, id, msg);
        }
        Severity::Medium => {
            warn!("GL debug: {:?} {:?} {}: {}", source, ty, id, msg);
        }
        Severity::High => {
            error!("GL debug: {:?} {:?} {}: {}", source, ty, id, msg);
        }
    }
}

struct DcHandle {
    window: *mut c_void,
    dc: *mut c_void,
}

impl Drop for DcHandle {
    fn drop(&mut self) {
        unsafe {
            ReleaseDC(self.window as *mut _, self.dc as *mut _);
        }
    }
}

struct WglContext {
    dc: DcHandle,
    context: *mut c_void,
}

impl Drop for WglContext {
    fn drop(&mut self) {
        unsafe {
            wglMakeCurrent(null_mut(), null_mut());
            wglDeleteContext(self.context as *mut _);
        }
    }
}

unsafe fn wgl_get_proc_address(symbol: &str) -> *const std::ffi::c_void {
    let nullterm = format!("{}\0", symbol);
    let nullterm_ptr = nullterm.as_ptr() as *const i8;
    let mut result = wglGetProcAddress(nullterm_ptr) as *const std::ffi::c_void;
    if result.is_null() {
        let opengl32 = OPENGL32_DLL.load(Ordering::Relaxed);
        result = GetProcAddress(opengl32 as *mut _, nullterm_ptr) as *const std::ffi::c_void;
        if result.is_null() {
            //debug!("GL func {} not found", symbol);
        }
    }
    result
}

unsafe impl glium::backend::Backend for WglContext {
    fn swap_buffers(&self) -> Result<(), glium::SwapBuffersError> {
        unsafe {
            let ok = SwapBuffers(self.dc.dc as *mut _);
            if ok == 0 {
                panic!("SwapBuffers failed: {}", io::Error::last_os_error());
            }
        }
        Ok(())
    }

    unsafe fn get_proc_address(&self, symbol: &str) -> *const std::ffi::c_void {
        wgl_get_proc_address(symbol)
    }

    fn get_framebuffer_dimensions(&self) -> (u32, u32) {
        unsafe {
            let mut rect = mem::zeroed();
            GetClientRect(self.dc.window as *mut _, &mut rect);
            (
                (rect.right - rect.left) as u32,
                (rect.bottom - rect.top) as u32,
            )
        }
    }

    fn is_current(&self) -> bool {
        unsafe { wglGetCurrentContext() == self.context as *mut _ }
    }

    unsafe fn make_current(&self) {
        wglMakeCurrent(self.dc.dc as *mut _, self.context as *mut _);
    }
}

unsafe fn create_wgl_context(window: *mut c_void) -> Result<WglContext, io::Error> {
    let dc = GetDC(window as *mut _);
    if dc.is_null() {
        return Err(io::Error::last_os_error());
    }
    let dc_wrap = DcHandle {
        window,
        dc: dc as *mut c_void,
    };
    let desc = PIXELFORMATDESCRIPTOR {
        nSize: mem::size_of::<PIXELFORMATDESCRIPTOR>() as u16,
        nVersion: 1,
        dwFlags: PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        iPixelType: PFD_TYPE_RGBA,
        cColorBits: 32,
        cAlphaBits: 0,
        cDepthBits: 24,
        cStencilBits: 8,
        cAuxBuffers: 0,
        iLayerType: PFD_MAIN_PLANE,
        ..mem::zeroed()
    };
    let format = ChoosePixelFormat(dc, &desc);
    if format == 0 {
        return Err(io::Error::last_os_error());
    }
    let ok = SetPixelFormat(dc, format, &desc);
    if ok == 0 {
        return Err(io::Error::last_os_error());
    }
    let context = wglCreateContext(dc);
    if context.is_null() {
        return Err(io::Error::last_os_error());
    }
    wglMakeCurrent(dc, context);
    let handle = crate::windows::LoadLibrary("opengl32");
    OPENGL32_DLL.store(handle as usize, Ordering::Relaxed);
    Ok(WglContext {
        dc: dc_wrap,
        context: context as *mut _,
    })
}

#[derive(Copy, Clone)]
struct Vertex2d {
    position: [f32; 2],
}

implement_vertex!(Vertex2d, position);

fn vertex2d(x: f32, y: f32) -> Vertex2d {
    Vertex2d {
        position: [x, y],
    }
}

fn shader_path<P: AsRef<Path>>(filename: P) -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src/gl/shaders")
        .join(filename)
}

fn read_shader(path: &Path) -> (String, SystemTime) {
    loop {
        let text = std::fs::read_to_string(path).unwrap();
        if !text.is_empty() {
            return (text, modified_time(path));
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

fn modified_time(path: &Path) -> SystemTime {
    std::fs::metadata(path)
        .and_then(|x| x.modified())
        .unwrap_or_else(|_| SystemTime::UNIX_EPOCH)
}

fn compile_program<F: Facade>(
    facade: &F,
    vertex_filename: &str,
    fragment_filename: &str,
) -> Program {
    let vertex_path = shader_path(vertex_filename);
    let fragment_path = shader_path(fragment_filename);
    loop {
        let (vertex_shader, vertex_time) = read_shader(&vertex_path);
        let (fragment_shader, fragment_time) = read_shader(&fragment_path);
        let result = glium::Program::from_source(facade, &vertex_shader, &fragment_shader, None);
        match result {
            Ok(program) => {
                return Program {
                    vertex_filename: vertex_filename.into(),
                    vertex_path,
                    vertex_time,
                    fragment_filename: fragment_filename.into(),
                    fragment_path,
                    fragment_time,
                    program,
                };
            }
            Err(e) => {
                let msg = format!(
                    "Couldn't compile program {} / {}\n{}",
                    vertex_filename, fragment_filename, e,
                );
                crate::windows::message_box("Shader error", &msg);
            }
        }
    }
}

// Reloads shaders on change, on failure shows a message box and retries on box close
struct Program {
    vertex_filename: String,
    vertex_path: PathBuf,
    vertex_time: SystemTime,
    fragment_filename: String,
    fragment_path: PathBuf,
    fragment_time: SystemTime,
    program: glium::Program,
}

impl Program {
    pub fn glium_program<F: Facade>(&mut self, facade: &F) -> &glium::Program {
        let changed = self.vertex_time != modified_time(&self.vertex_path) ||
            self.fragment_time != modified_time(&self.fragment_path);
        if changed {
            info!(
                "Reloading shaders {} {}",
                self.vertex_filename, self.fragment_filename
            );
            *self = compile_program(facade, &self.vertex_filename, &self.fragment_filename);
        }
        &self.program
    }
}
