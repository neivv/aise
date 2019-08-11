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
