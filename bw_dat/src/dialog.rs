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
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Dialog(NonNull<bw::Dialog>);

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
