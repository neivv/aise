use std::ptr::null_mut;

use bw;
use order::OrderId;

pub use bw_dat::UnitId;
pub use bw_dat::unit as id;

pub struct Unit(pub *mut bw::Unit);

impl Unit {
    pub fn from_ptr(ptr: *mut bw::Unit) -> Option<Unit> {
        if ptr == null_mut() {
            None
        } else {
            Some(Unit(ptr))
        }
    }

    pub fn sprite(&self) -> Option<*mut bw::Sprite> {
        unsafe {
            match (*self.0).sprite == null_mut() {
                true => None,
                false => Some((*self.0).sprite),
            }
        }
    }

    pub fn player(&self) -> u8 {
        unsafe { (*self.0).player }
    }

    pub fn id(&self) -> UnitId {
        UnitId(unsafe { (*self.0).unit_id })
    }

    pub fn position(&self) -> bw::Point {
        unsafe { (*self.0).position }
    }

    pub fn order(&self) -> OrderId {
        OrderId(unsafe { (*self.0).order })
    }

    pub fn energy(&self) -> u16 {
        unsafe { (*self.0).energy }
    }

    /// Is the unit cloaked or burrowed (So it requires detection)
    pub fn is_invisible(&self) -> bool {
        unsafe { (*self.0).flags & 0x300 != 0 }
    }

    /// Is the unit cloaked or burrowed (So it requires detection)
    pub fn is_invincible(&self) -> bool {
        unsafe {
            (*self.0).flags & 0x04000000 != 0
        }
    }

    pub fn orders(&self) -> Orders {
        unsafe {
            Orders {
                next: (*self.0).order_queue_begin,
                this: self,
                first: true,
            }
        }
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

pub struct Orders<'a> {
    next: *mut bw::Order,
    this: &'a Unit,
    first: bool,
}

pub struct Order {
    pub id: OrderId,
    pub position: bw::Point,
    pub target: Option<Unit>,
}

impl<'a> Iterator for Orders<'a> {
    type Item = Order;
    fn next(&mut self) -> Option<Order> {
        unsafe {
            if self.first {
                self.first = false;
                Some(Order {
                    id: self.this.order(),
                    position: (*self.this.0).order_target_pos,
                    target: Unit::from_ptr((*self.this.0).target),
                })
            } else if self.next != null_mut() {
                let order = self.next;
                self.next = (*order).next;
                Some(Order {
                    id: OrderId((*order).order_id),
                    position: (*order).position,
                    target: Unit::from_ptr((*order).target),
                })
            } else {
                None
            }
        }
    }
}

pub fn active_units() -> UnitListIter {
    UnitListIter(bw::first_active_unit())
}

pub struct UnitListIter(*mut bw::Unit);

impl Iterator for UnitListIter {
    type Item = Unit;
    fn next(&mut self) -> Option<Unit> {
        unsafe {
            if self.0 == null_mut() {
                None
            } else {
                let result = Some(Unit(self.0));
                self.0 = (*self.0).next;
                result
            }
        }
    }
}

pub fn find_units<F: FnMut(&Unit) -> bool>(area: &bw::Rect, mut filter: F) -> Vec<Unit> {
    let mut result = Vec::new();
    for unit in active_units() {
        let crect = unit.collision_rect();
        if rect_overlaps(&crect, area) {
            if filter(&unit) {
                result.push(unit);
            }
        }
    }
    result
}

// Also returns the distance
pub fn find_nearest<F>(point: bw::Point, mut filter: F) -> Option<(Unit, u32)>
where F: FnMut(&Unit) -> bool,
{
    let mut result = None;
    let mut result_dist = !0;
    for unit in active_units() {
        let distance = distance(unit.position(), point);
        if distance < result_dist {
            if filter(&unit) {
                result = Some(unit);
                result_dist = distance;
            }
        }
    }
    result.map(|x| (x, result_dist))
}

// BW algorithm
fn distance(a: bw::Point, b: bw::Point) -> u32 {
    let x = (a.x as i32).wrapping_sub(b.x as i32).abs() as u32;
    let y = (a.y as i32).wrapping_sub(b.y as i32).abs() as u32;
    let (greater, lesser) = if x > y {
        (x, y)
    } else {
        (y, x)
    };
    if greater / 4 > lesser {
        greater
    } else {
        greater * 59 / 64 + lesser * 99 / 256
    }
}

fn rect_overlaps(a: &bw::Rect, b: &bw::Rect) -> bool {
    a.left < b.right &&
        a.right > b.left &&
        a.top < b.bottom &&
        a.bottom > b.top
}
