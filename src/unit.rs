use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;

use serde::{Serializer, Serialize, Deserializer, Deserialize};

use bw;
use order::OrderId;

pub use bw_dat::UnitId;
pub use bw_dat::unit as id;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Unit(pub *mut bw::Unit);

unsafe impl Send for Unit {}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HashableUnit(pub Unit);

impl Hash for HashableUnit {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hasher.write_usize((self.0).0 as usize);
    }
}

impl Serialize for Unit {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;
        match save_mapping().borrow().get(&HashableUnit(*self)) {
            Some(id) => id.serialize(serializer),
            None => Err(S::Error::custom(format!("Couldn't get id for unit {:?}", self))),
        }
    }
}

impl<'de> Deserialize<'de> for Unit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match load_mapping().borrow().get(&id) {
            Some(&unit) => Ok(unit),
            None => Err(S::Error::custom(format!("Couldn't get unit for id {:?}", id))),
        }
    }
}

pub struct SaveIdMapping {
    next: Option<Unit>,
    list: SaveIdState,
    id: u32,
    in_subunit: bool,
}

enum SaveIdState {
    ActiveUnits,
    HiddenUnits,
}

fn save_id_mapping() -> SaveIdMapping {
    SaveIdMapping {
        next: Unit::from_ptr(bw::first_active_unit()),
        list: SaveIdState::ActiveUnits,
        id: 0,
        in_subunit: false,
    }
}

impl Iterator for SaveIdMapping {
    type Item = (Unit, u32);
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            while self.next.is_none() {
                match self.list {
                    SaveIdState::ActiveUnits => {
                        self.next = Unit::from_ptr(bw::first_hidden_unit());
                        self.list = SaveIdState::HiddenUnits;
                    }
                    SaveIdState::HiddenUnits => return None,
                }
            }
            self.id += 1;
            let result = (self.next.unwrap(), self.id);
            let unit = self.next.unwrap().0;
            if (*unit).subunit != null_mut() && !self.in_subunit {
                self.next = Unit::from_ptr((*unit).subunit);
                self.in_subunit = true;
            } else {
                if self.in_subunit {
                    self.in_subunit = false;
                    let parent = (*unit).subunit;
                    self.next = Unit::from_ptr((*parent).next);
                } else {
                    self.next = Unit::from_ptr((*unit).next);
                }
            }
            Some(result)
        }
    }
}

ome2_thread_local! {
    SAVE_ID_MAP: RefCell<HashMap<HashableUnit, u32>> = save_mapping(RefCell::new(HashMap::new()));
    LOAD_ID_MAP: RefCell<HashMap<u32, Unit>> = load_mapping(RefCell::new(HashMap::new()));
}

pub fn init_save_mapping() {
    *save_mapping().borrow_mut() = save_id_mapping().map(|(x, y)| (HashableUnit(x), y)).collect();
}

pub fn clear_save_mapping() {
    save_mapping().borrow_mut().clear();
}

pub fn init_load_mapping() {
    *load_mapping().borrow_mut() = save_id_mapping().map(|(x, y)| (y, x)).collect();
}

pub fn clear_load_mapping() {
    load_mapping().borrow_mut().clear();
}

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

    pub fn hitpoints(&self) -> i32 {
        unsafe { (*self.0).hitpoints }
    }

    pub fn shields(&self) -> i32 {
        if self.id().has_shields() {
            unsafe { (*self.0).shields }
        } else {
            0
        }
    }

    pub fn health(&self) -> i32 {
        self.hitpoints().saturating_add(self.shields())
    }

    pub fn energy(&self) -> u16 {
        unsafe { (*self.0).energy }
    }

    /// Is the unit cloaked or burrowed (So it requires detection)
    pub fn is_invisible(&self) -> bool {
        unsafe { (*self.0).flags & 0x300 != 0 }
    }

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

    pub fn target(&self) -> Option<Unit> {
        unsafe { Unit::from_ptr((*self.0).target) }
    }

    pub fn worker_ai(&self) -> Option<*mut bw::WorkerAi> {
        unsafe {
            let ai = (*self.0).ai as *mut bw::WorkerAi;
            if ai != null_mut() && (*ai).ai_type == 2 {
                Some(ai)
            } else {
                None
            }
        }
    }

    pub fn building_ai(&self) -> Option<*mut bw::BuildingAi> {
        unsafe {
            let ai = (*self.0).ai as *mut bw::BuildingAi;
            if ai != null_mut() && (*ai).ai_type == 3 {
                Some(ai)
            } else {
                None
            }
        }
    }

    pub fn matches_id(&self, other: UnitId) -> bool {
        let id = self.id();
        match other {
            id::ANY_UNIT => true,
            id::GROUP_MEN => id.group_flags() & 0x8 != 0,
            id::GROUP_BUILDINGS => id.group_flags() & 0x10 != 0,
            id::GROUP_FACTORIES => id.group_flags() & 0x20 != 0,
            other => id == other,
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
