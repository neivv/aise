use std::ptr::{NonNull};

use crate::bw;
use crate::{ImageId};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Image(NonNull<bw::Image>);

impl std::ops::Deref for Image {
    type Target = *mut bw::Image;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Image {
    pub unsafe fn from_ptr(ptr: *mut bw::Image) -> Option<Image> {
        NonNull::new(ptr).map(Image)
    }

    pub fn id(self) -> ImageId {
        unsafe { ImageId((**self).image_id) }
    }

    pub fn set_hidden(self, hidden: bool) {
        unsafe {
            if hidden {
                (**self).flags |= 0x40;
            } else {
                (**self).flags &= !0x40;
            }
        }
    }

    pub fn redraw(self) {
        unsafe {
            (**self).flags |= 0x1;
        }
    }
}
