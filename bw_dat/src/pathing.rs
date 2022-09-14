use std::ptr::{addr_of_mut, NonNull};

use crate::bw;

#[derive(Copy, Clone)]
pub struct Pathing(NonNull<bw::Pathing>);

impl std::ops::Deref for Pathing {
    type Target = *mut bw::Pathing;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Pathing {
    pub unsafe fn from_ptr(value: *mut bw::Pathing) -> Pathing {
        Pathing(NonNull::new(value).unwrap())
    }

    pub fn region(self, id: u16) -> Option<Region> {
        unsafe {
            let id = id as usize;
            if (**self).regions.len() < id {
                None
            } else {
                Some(Region::from_ptr(addr_of_mut!((**self).regions[id])))
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct Region(NonNull<bw::Region>);

impl std::ops::Deref for Region {
    type Target = *mut bw::Region;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Region {
    pub unsafe fn from_ptr(value: *mut bw::Region) -> Region {
        Region(NonNull::new(value).unwrap())
    }

    pub fn area(self) -> bw::Rect {
        unsafe { (**self).area }
    }

    pub fn neighbour_ids(self) -> impl Iterator<Item = u16> {
        unsafe {
            let amount = (**self).all_neighbours as usize;
            let arr = (**self).neighbour_ids;
            (0..amount).map(move |i| *arr.add(i))
        }
    }
}
