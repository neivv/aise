use std::ptr::{NonNull};

use crate::bw;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Control(NonNull<bw::Control>);

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
}

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
}
