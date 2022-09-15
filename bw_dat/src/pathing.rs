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

    pub fn region_id_for_point(self, point: &bw::Point) -> Option<u16> {
        let x_tile = (point.x as usize) / 32;
        let y_tile = (point.y as usize) / 32;
        let index = y_tile.checked_mul(0x100)?.checked_add(x_tile)?;
        unsafe {
            let &value = (**self).map_tile_regions.get(index)?;
            if value < 0x2000 {
                Some(value)
            } else {
                let split_index = value as usize - 0x2000;
                let minitile_x = (point.x & 31) as u32 / 8;
                let minitile_y = (point.y & 31) as u32 / 8;
                let index = minitile_x + minitile_y * 4;
                let value = (**self).split_regions.get(split_index)?;
                if value.minitile_flags & (1u16 << index) == 0 {
                    Some(value.region_false)
                } else {
                    Some(value.region_true)
                }
            }
        }
    }

    pub fn region_for_point(self, point: &bw::Point) -> Option<Region> {
        self.region(self.region_id_for_point(point)?)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
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
