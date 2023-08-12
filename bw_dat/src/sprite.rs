use std::ptr::{null_mut, NonNull};

use crate::bw;
use crate::{Image, SpriteId};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Sprite(NonNull<bw::Sprite>);

unsafe impl Send for Sprite {}
unsafe impl Sync for Sprite {}

impl std::ops::Deref for Sprite {
    type Target = *mut bw::Sprite;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Sprite {
    pub unsafe fn from_ptr(ptr: *mut bw::Sprite) -> Option<Sprite> {
        NonNull::new(ptr).map(Sprite)
    }

    pub fn visibility_mask(self) -> u8 {
        unsafe { (**self).visibility_mask }
    }

    pub fn flags(self) -> u8 {
        unsafe { (**self).flags }
    }

    pub fn player(self) -> u8 {
        unsafe { (**self).player }
    }

    pub fn set_player(self, val: u8) {
        unsafe { (**self).player = val; }
    }

    pub fn elevation_level(self) -> u8 {
        unsafe { (**self).elevation_level }
    }

    pub fn id(self) -> SpriteId {
        unsafe { SpriteId((**self).sprite_id) }
    }

    pub fn is_hidden(self) -> bool {
        self.flags() & 0x20 != 0
    }

    pub fn set_selection_flash_timer(self, value: u8) {
        unsafe { (**self).selection_flash_timer = value; }
    }

    pub fn main_image(self) -> Option<Image> {
        unsafe {
            let image = match crate::is_scr() {
                true => (**self).version_specific.scr.main_image,
                false => (**self).version_specific.legacy.main_image,
            };
            Image::from_ptr(image)
        }
    }

    pub fn images(self) -> impl Iterator<Item = Image> {
        unsafe {
            let image = match crate::is_scr() {
                true => (**self).version_specific.scr.first_image,
                false => (**self).version_specific.legacy.first_image,
            };
            ImageIterator(image)
        }
    }
}

pub struct ImageIterator(*mut bw::Image);

impl Iterator for ImageIterator {
    type Item = Image;
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let ret = Image::from_ptr(self.0);
            self.0 = ret.map(|x| (**x).next).unwrap_or(null_mut());
            ret
        }
    }
}
