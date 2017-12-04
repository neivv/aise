use std::ptr::null_mut;

use bw;
use order::OrderId;

pub struct Unit(pub *mut bw::Unit);

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct UnitId(pub u16);

pub mod id {
    use super::UnitId;
    pub const NONE: UnitId = UnitId(0xe4);
    pub const ANY_UNIT: UnitId = UnitId(0xe5);
}

impl Unit {
    pub fn player(&self) -> u8 {
        unsafe { (*self.0).player }
    }

    pub fn id(&self) -> UnitId {
        UnitId(unsafe { (*self.0).unit_id })
    }

    pub fn position(&self) -> bw::Point {
        unsafe { (*self.0).position }
    }

    pub fn matches_id(&self, other: UnitId) -> bool {
        let id = self.id();
        if other == id::ANY_UNIT {
            true
        } else {
            id == other
        }
    }

    pub fn collision_rect(&self) -> bw::Rect {
        let collision_rect = bw::collision_rect(self.id());
        let position = self.position();
        bw::Rect {
            left: position.x - collision_rect.left,
            right: position.x + collision_rect.right + 1,
            top: position.y - collision_rect.top,
            bottom: position.y + collision_rect.bottom + 1,
        }
    }

    pub unsafe fn issue_secondary_order(&self, order: OrderId) {
        if (*self.0).secondary_order != order.0 {
            (*self.0).secondary_order = order.0;
            (*self.0).secondary_order_state = 0;
            // Uhh.. Is this sensible to allow to be done from AI scripts?
            (*self.0).currently_building = null_mut();
            (*self.0).unke8 = 0;
            (*self.0).unkea = 0;
        }
    }
}

pub fn find_units<F: FnMut(&Unit) -> bool>(area: &bw::Rect, mut filter: F) -> Vec<Unit> {
    unsafe {
        let mut unit = bw::first_active_unit();
        let mut result = Vec::new();
        while unit != null_mut() {
            let crect = Unit(unit).collision_rect();
            if rect_overlaps(&crect, area) {
                if filter(&Unit(unit)) {
                    result.push(Unit(unit));
                }
            }
            unit = (*unit).next;
        }
        result
    }
}

fn rect_overlaps(a: &bw::Rect, b: &bw::Rect) -> bool {
    a.left < b.right &&
        a.right > b.left &&
        a.top < b.bottom &&
        a.bottom > b.top
}
