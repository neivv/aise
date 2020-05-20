use std::ptr::{NonNull};
use std::sync::atomic::{AtomicUsize, Ordering};

use lazy_static::lazy_static;
use libc::c_void;

use crate::bw;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Control(NonNull<bw::Control>);

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
            let string = (*self.0.as_ptr()).string;
            if string.is_null() {
                ""
            } else {
                std::ffi::CStr::from_ptr(string as *const i8).to_str()
                    .unwrap_or_else(|_| "")
            }
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

    pub fn user_pointer(self) -> *mut c_void {
        unsafe {
            match crate::is_scr() {
                false => (*self.0.as_ptr()).user_ptr,
                true => (*(self.0.as_ptr() as *mut bw::scr::Control)).user_ptr,
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
        unsafe {
            let mut child = match crate::is_scr() {
                false => (*self.0.as_ptr()).first_child,
                true => {
                    (*(self.0.as_ptr() as *mut bw::scr::Dialog)).first_child as *mut bw::Control
                }
            };
            while !child.is_null() {
                let c = Control::new(child);
                if c.id() == id {
                    return Some(c);
                }
                child = (*child).next;
            }
            None
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
    orig_offset: usize,
}

impl InitedEventHandler {
    fn set_orig(&self, orig: usize) {
        unsafe {
            ((self.func as *mut u8).add(self.orig_offset) as *mut usize).write_unaligned(orig);
        }
    }
}

lazy_static! {
    static ref EXEC_HEAP: usize = unsafe {
        use winapi::um::heapapi::HeapCreate;
        use winapi::um::winnt::HEAP_CREATE_ENABLE_EXECUTE;
        let heap = HeapCreate(HEAP_CREATE_ENABLE_EXECUTE, 0, 0);
        assert!(heap.is_null() == false);
        heap as usize
    };
}

fn exec_alloc(size: usize) -> *mut u8 {
    use winapi::um::heapapi::HeapAlloc;
    unsafe {
        HeapAlloc(*EXEC_HEAP as *mut _, 0, size) as *mut u8
    }
}

impl EventHandler {
    pub const fn new() -> EventHandler {
        EventHandler {
            func: AtomicUsize::new(0),
        }
    }

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
}
