use std::ptr::{NonNull, null_mut};
use std::sync::atomic::{AtomicUsize, Ordering};
use once_cell::sync::OnceCell;

use libc::c_void;
use winapi::um::sysinfoapi::GetTickCount;

use crate::bw;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Control(NonNull<bw::Control>);

unsafe impl Send for Control {}
unsafe impl Sync for Control {}
unsafe impl Send for Dialog {}
unsafe impl Sync for Dialog {}

impl std::ops::Deref for Control {
    type Target = *mut bw::Control;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Control {
    pub unsafe fn new(pointer: *mut bw::Control) -> Control {
        Control(NonNull::new(pointer).unwrap())
    }

    pub fn string(&self) -> &str {
        unsafe {
            if crate::is_scr() {
                let this = (**self) as *mut bw::scr::Control;
                let ptr = (*this).string.data;
                std::ffi::CStr::from_ptr(ptr as *const i8).to_str()
                    .unwrap_or_else(|_| "")
            } else {
                #[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
                {
                    let string = (*self.0.as_ptr()).string;
                    if string.is_null() {
                        ""
                    } else {
                        std::ffi::CStr::from_ptr(string as *const i8).to_str()
                            .unwrap_or_else(|_| "")
                    }
                }
                #[cfg(any(feature = "scr-only", target_pointer_width = "64"))]
                {
                    ""
                }
            }
        }
    }

    pub fn set_string(self, value: &[u8]) {
        if !crate::is_scr() {
            return;
        }
        unsafe {
            let this = (*self) as *mut bw::scr::Control;
            assign_scr_string(&mut (*this).string, value);
        }
    }

    pub fn id(self) -> i16 {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).id,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).id,
            }
        }
    }

    pub fn control_type(self) -> u16 {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).ty,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).ty,
            }
        }
    }

    pub fn flags(self) -> u32 {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).flags,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).flags,
            }
        }
    }

    pub fn user_pointer(self) -> *mut c_void {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).user_ptr,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).user_ptr,
            }
        }
    }

    pub fn set_user_pointer(self, value: *mut c_void) {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).user_ptr = value,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).user_ptr = value,
            }
        }
    }

    pub fn set_short_user_value(self, value: u16) {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).misc_u16 = value,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).misc_u16 = value,
            }
        }
    }

    pub fn is_disabled(self) -> bool {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).flags & 0x2 != 0,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).flags2 & 0x1 != 0,
            }
        }
    }

    pub fn is_hidden(self) -> bool {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).flags & 0x8 == 0,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).flags & 0x2 == 0,
            }
        }
    }

    pub fn dialog_coords(self) -> bw::Rect {
        unsafe {
            (*self.0.as_ptr()).area
        }
    }

    pub fn screen_coords(self) -> bw::Rect {
        unsafe {
            let mut rect = (*self.0.as_ptr()).area;
            // Dialog coords are in screen coords, child controls
            // are relative to dialog
            if let Some(dialog) = self.parent() {
                let coords = (*dialog.as_control().0.as_ptr()).area;
                rect.left += coords.left;
                rect.right += coords.left;
                rect.top += coords.top;
                rect.bottom += coords.top;
            }
            rect
        }
    }

    /// Either parent, or self if this is a dialog.
    pub fn dialog(self) -> Dialog {
        self.parent().unwrap_or_else(|| unsafe { Dialog::new(*self as *mut bw::Dialog) })
    }

    pub fn parent(self) -> Option<Dialog> {
        unsafe {
            if self.control_type() == 0 {
                None
            } else {
                let parent = match crate::is_scr() {
                    false => (*self.0.as_ptr()).parent,
                    true => {
                        (*(self.0.as_ptr() as *mut bw::scr::Control)).parent as *mut bw::Dialog
                    }
                };
                Some(Dialog::new(parent))
            }
        }
    }

    pub fn set_event_handler(self, handler: InitedEventHandler) {
        unsafe {
            let scr = crate::is_scr();
            let old = match scr {
                false => {
                    (*self.0.as_ptr()).event_handler.map(|x| x as usize).unwrap_or(0)
                }
                true => {
                    (*(self.0.as_ptr() as *mut bw::scr::Control)).event_handler
                        .map(|x| x as usize).unwrap_or(0)
                }
            };
            handler.set_orig(old);
            let ptr = handler.func;
            match scr {
                false => {
                    (*self.0.as_ptr()).event_handler = Some(std::mem::transmute(ptr));
                }
                true => {
                    (*(self.0.as_ptr() as *mut bw::scr::Control)).event_handler =
                        Some(std::mem::transmute(ptr));
                }
            };
        }
    }

    pub fn contains_point(self, x: i16, y: i16) -> bool {
        unsafe {
            let area = (**self).area;
            area.contains_point(&bw::Point { x, y })
        }
    }

    pub fn send_ext_event(self, id: u32) -> u32 {
        let (x, y) = crate::bw_mouse();
        self.send_ext_event_mouse(id, x as i16, y as i16)
    }

    pub fn send_ext_event_mouse(self, id: u32, x: i16, y: i16) -> u32 {
        unsafe {
            if crate::is_scr() {
                let this = (*self) as *mut bw::scr::Control;
                let mut event = bw::scr::ControlEvent {
                    ext_type: id as usize,
                    ext_param: 0,
                    param: 0,
                    ty: 0xe,
                    x,
                    y,
                    time: GetTickCount(),
                };
                if let Some(e) = (*this).event_handler {
                    e(this, &mut event)
                } else {
                    0
                }
            } else {
                0
            }
        }
    }

    pub fn show(self) {
        if crate::is_scr() {
            if self.flags() & 0x2 == 0 {
                self.send_ext_event(0xd);
            }
        }
    }

    pub fn hide(self) {
        if crate::is_scr() {
            if self.flags() & 0x2 != 0 {
                if self.send_ext_event(0xe) != 0 {
                    self.send_ext_event(0x6);
                }
            }
        }
    }

    pub fn enable(self) {
        if self.is_disabled() {
            if crate::is_scr() {
                unsafe {
                    let this = (*self) as *mut bw::scr::Control;
                    (*this).flags2 &= !0x1;
                    self.dialog().update_control_under_mouse();
                }
            }
        }
    }

    pub fn disable(self) {
        if !self.is_disabled() {
            if crate::is_scr() {
                unsafe {
                    let this = (*self) as *mut bw::scr::Control;
                    (*this).flags2 &= !0x4;
                    (*this).flags2 |= 0x1;
                    self.send_ext_event(0x6);
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Dialog(NonNull<bw::Dialog>);

impl std::ops::Deref for Dialog {
    type Target = *mut bw::Dialog;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Dialog {
    pub unsafe fn new(pointer: *mut bw::Dialog) -> Dialog {
        Dialog(NonNull::new(pointer).unwrap())
    }

    pub fn as_control(self) -> Control {
        unsafe {
            Control::new(self.0.as_ptr() as *mut bw::Control)
        }
    }

    pub fn child_by_id(self, id: i16) -> Option<Control> {
        self.children().find(|x| x.id() == id)
    }

    pub fn children(self) -> impl Iterator<Item = Control> {
        unsafe {
            let child = match crate::is_scr() {
                false => (*self.0.as_ptr()).first_child,
                true => {
                    (*(self.0.as_ptr() as *mut bw::scr::Dialog)).first_child as *mut bw::Control
                }
            };
            IterChildren(child)
        }
    }

    fn update_control_under_mouse(self) {
        if crate::is_scr() {
            unsafe {
                let this = (*self) as *mut bw::scr::Dialog;
                let (x, y) = crate::bw_mouse();
                if let Some(ctrl) = self.find_ctrl_for_mouse(x as i16, y as i16) {
                    let ctrl_ptr = (*ctrl) as *mut bw::scr::Control;
                    if (*ctrl_ptr).flags2 & 0x4 == 0 {
                        self.set_active_control(ctrl);
                    }
                } else {
                    if let Some(ctrl) = self.active_control() {
                        let ctrl_ptr = (*ctrl) as *mut bw::scr::Control;
                        (*ctrl_ptr).flags2 &= !0x4;
                        (*this).active = null_mut();
                    }
                }
            }
        }
    }

    fn find_ctrl_for_mouse(self, x: i16, y: i16) -> Option<Control> {
        if !crate::is_scr() {
            return None;
        }
        unsafe {
            if !self.as_control().contains_point(x, y) {
                return None;
            }
            let x = x - (**self.as_control()).area.left;
            let y = y - (**self.as_control()).area.top;
            let mut result: Option<Control> = None;
            for child in self.children() {
                if child.contains_point(x, y) && child.send_ext_event_mouse(0x4, x, y) != 0 {
                    let better = if let Some(result) = result {
                        let ctrl_ptr = (*result) as *mut bw::scr::Control;
                        (*ctrl_ptr).flags2 & 0x20 == 0
                    } else {
                        true
                    };
                    if better {
                        result = Some(child);
                    }
                }
            }
            if result.is_none() && self.as_control().send_ext_event_mouse(0x4, x, y) != 0 {
                result = Some(self.as_control());
            }
            result
        }
    }

    fn set_active_control(self, ctrl: Control) {
        if crate::is_scr() {
            unsafe {
                let this = (*self) as *mut bw::scr::Dialog;
                if let Some(ctrl) = self.active_control() {
                    let ctrl_ptr = (*ctrl) as *mut bw::scr::Control;
                    (*ctrl_ptr).flags2 &= !0x4;
                    (*this).active = null_mut();
                }
                if ctrl != self.as_control() {
                    let ctrl_ptr = (*ctrl) as *mut bw::scr::Control;
                    (*ctrl_ptr).flags2 |= 0x4;
                    ctrl.send_ext_event(0x9);
                    (*this).active = ctrl_ptr;
                }
            }
        }
    }

    fn active_control(self) -> Option<Control> {
        unsafe {
            let ptr = match crate::is_scr() {
                false => (*self.0.as_ptr()).active,
                true => (*(self.0.as_ptr() as *mut bw::scr::Dialog)).active as *mut bw::Control,
            };
            if ptr.is_null() {
                None
            } else {
                Some(Control::new(ptr))
            }
        }
    }
}

struct IterChildren(*mut bw::Control);

impl Iterator for IterChildren {
    type Item = Control;
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.0.is_null() {
                None
            } else {
                let next = Control::new(self.0);
                self.0 = (**next).next;
                Some(next)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Event(NonNull<bw::ControlEvent>);

impl Event {
    pub unsafe fn new(pointer: *mut bw::ControlEvent) -> Event {
        Event(NonNull::new(pointer).unwrap())
    }

    pub fn mouse_pos(self) -> (i16, i16) {
        unsafe {
            if crate::is_scr() {
                let ptr = self.0.as_ptr() as *mut bw::scr::ControlEvent;
                ((*ptr).x, (*ptr).y)
            } else {
                let ptr = self.0.as_ptr();
                ((*ptr).x, (*ptr).y)
            }
        }
    }

    pub fn event_type(self) -> u16 {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).ty,
                true => (*(self.0.as_ptr() as *mut bw::scr::ControlEvent)).ty,
            }
        }
    }
}

// Event handlers:
// Dialog event handler calling convention is different between SCR (cdecl) and 1161 (fastcall),
// Allocate a wrapper functions for converting 1161 fastcalls to cdecl as well
// For SCR the wrapper just stores original event handler.

pub type EventHandlerFn = unsafe extern fn(
    *mut bw::Control,
    *mut bw::ControlEvent,
    unsafe extern fn(*mut bw::Control, *mut bw::ControlEvent) -> u32
) -> u32;

pub struct EventHandler {
    func: AtomicUsize,
}

#[derive(Copy, Clone)]
pub struct InitedEventHandler {
    func: *mut c_void,
    #[cfg(target_pointer_width = "32")]
    orig_offset: usize,
}

impl InitedEventHandler {
    #[cfg(target_pointer_width = "32")]
    pub fn set_orig(&self, orig: usize) {
        unsafe {
            ((self.func as *mut u8).add(self.orig_offset) as *mut usize).write_unaligned(orig);
        }
    }

    #[cfg(target_pointer_width = "64")]
    pub fn set_orig(&self, orig: usize) {
        unsafe {
            ((self.func as *mut u8).add(0x18) as *mut usize).write_unaligned(orig);
        }
    }

    pub fn func(&self) -> *mut c_void {
        self.func
    }
}

static EXEC_HEAP: OnceCell<usize> = OnceCell::new();

fn exec_alloc(size: usize) -> *mut u8 {
    use winapi::um::heapapi::HeapAlloc;
    unsafe {
        let heap = EXEC_HEAP.get_or_init(|| {
            use winapi::um::heapapi::HeapCreate;
            use winapi::um::winnt::HEAP_CREATE_ENABLE_EXECUTE;
            let heap = HeapCreate(HEAP_CREATE_ENABLE_EXECUTE, 0, 0);
            assert!(heap.is_null() == false);
            heap as usize
        });
        HeapAlloc(*heap as *mut _, 0, size) as *mut u8
    }
}

impl EventHandler {
    pub const fn new() -> EventHandler {
        EventHandler {
            func: AtomicUsize::new(0),
        }
    }

    #[cfg(target_pointer_width = "32")]
    pub fn init(&self, func: EventHandlerFn) -> InitedEventHandler {
        let func_ptr = self.func.load(Ordering::Relaxed);
        let (code, orig_offset) = if crate::is_scr() {
            (&[
                // c -> c
                0xb8, 0x00, 0x00, 0x00, 0x00, // mov eax, x (func)
                0x68, 0x00, 0x00, 0x00, 0x00, // push x (orig_wrap)
                0xff, 0x74, 0xe4, 0x0c, // push [esp + c]
                0xff, 0x74, 0xe4, 0x0c, // push [esp + c]
                0xff, 0xd0, // call eax
                0x83, 0xc4, 0x0c, // add esp, c
                0xc3, // ret
                // c -> c
                0xb8, 0x00, 0x00, 0x00, 0x00, // mov eax, x (real orig)
                0xff, 0xe0, // jmp eax
            ][..], 25)
        } else {
            (&[
                // fastcall -> c
                0xb8, 0x00, 0x00, 0x00, 0x00, // mov eax, x (func)
                0x68, 0x00, 0x00, 0x00, 0x00, // push x (orig_wrap)
                0x52, // push edx
                0x51, // push ecx
                0xff, 0xd0, // call eax
                0x83, 0xc4, 0x0c, // add esp, c
                0xc3, // ret
                // c -> fastcall
                0xb8, 0x00, 0x00, 0x00, 0x00, // mov eax, x (real orig)
                0x8b, 0x54, 0xe4, 0x08, // mov edx, [esp + 8]
                0x8b, 0x4c, 0xe4, 0x04, // mov ecx, [esp + 4]
                0xff, 0xd0, // call eax
                0xc3, // ret
            ][..], 19)
        };
        if func_ptr != 0 {
            return InitedEventHandler {
                func: func_ptr as *mut c_void,
                orig_offset
            }
        }
        let exec_code = exec_alloc(code.len());
        unsafe {
            std::ptr::copy_nonoverlapping(code.as_ptr(), exec_code, code.len());
            (exec_code.add(1) as *mut usize).write_unaligned(func as usize);
            (exec_code.add(6) as *mut usize).write_unaligned(exec_code as usize + orig_offset - 1);

            self.func.store(exec_code as usize, Ordering::Relaxed);
            InitedEventHandler {
                func: exec_code as *mut c_void,
                orig_offset,
            }
        }
    }

    #[cfg(target_pointer_width = "64")]
    pub fn init(&self, func: EventHandlerFn) -> InitedEventHandler {
        let func_ptr = self.func.load(Ordering::Relaxed);
        let code = &[
            0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // mov rax, x (func)
            0x49, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // mov r8, x (orig_wrap)
            0xff, 0xe0, // jmp rax
            0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // mov rax, x (real orig)
            0xff, 0xe0, // jmp rax
        ];
        if func_ptr != 0 {
            return InitedEventHandler {
                func: func_ptr as *mut c_void,
            }
        }
        let exec_code = exec_alloc(code.len());
        unsafe {
            std::ptr::copy_nonoverlapping(code.as_ptr(), exec_code, code.len());
            (exec_code.add(2) as *mut usize).write_unaligned(func as usize);
            (exec_code.add(0xc) as *mut usize).write_unaligned(exec_code as usize + 0x16);

            self.func.store(exec_code as usize, Ordering::Relaxed);
            InitedEventHandler {
                func: exec_code as *mut c_void,
            }
        }
    }
}

fn assign_scr_string(out: &mut bw::scr::BwString, value: &[u8]) {
    if out.capacity & !(isize::min_value() as usize) < value.len() {
        reserve_reset_scr_string(out, value.len());
    }
    unsafe {
        let data = out.data as *mut u8;
        std::ptr::copy_nonoverlapping(value.as_ptr(), data as *mut u8, value.len());
        *data.add(value.len()) = 0;
        out.length = value.len();
    }
}

fn reserve_reset_scr_string(out: &mut bw::scr::BwString, capacity: usize) {
    use crate::{bw_malloc, bw_free};
    unsafe {
        let new_buf = bw_malloc(capacity + 1);
        if out.capacity & (isize::min_value() as usize) == 0 {
            bw_free(out.data as *mut u8);
        }
        out.capacity = capacity;
        out.data = new_buf as *const u8;
    }
}
