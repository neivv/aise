use std::ptr::{NonNull};

use crate::bw;
use crate::{ImageId, Sprite};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Image(NonNull<bw::Image>);

unsafe impl Send for Image {}
unsafe impl Sync for Image {}

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

    pub fn set_offset(self, x: i8, y: i8) {
        unsafe {
            (**self).x_offset = x;
            (**self).y_offset = y;
        }
    }

    pub fn is_flipped(self) -> bool {
        unsafe {
            (**self).flags & 0x2 != 0
        }
    }

    pub fn flags(self) -> u16 {
        unsafe {
            (**self).flags
        }
    }

    pub fn parent(self) -> Sprite {
        unsafe {
            Sprite::from_ptr((**self).parent).expect("No image parent")
        }
    }

    pub fn animation(self) -> u8 {
        unsafe {
            (**self).iscript.animation
        }
    }
}
