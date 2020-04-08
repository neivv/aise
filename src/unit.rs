use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;

use fxhash::FxHashMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use bw_dat::Unit;

use crate::bw;
use crate::order::OrderId;

pub use bw_dat::unit as id;
pub use bw_dat::UnitId;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HashableUnit(pub Unit);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SerializableUnit(pub Unit);

impl Hash for HashableUnit {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        hasher.write_usize(*(self.0) as usize);
    }
}

impl Serialize for SerializableUnit {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;
        match save_mapping().borrow().get(&HashableUnit(self.0)) {
            Some(id) => id.serialize(serializer),
            None => Err(S::Error::custom(format!(
                "Couldn't get id for unit {:?}",
                self
            ))),
        }
    }
}

impl<'de> Deserialize<'de> for SerializableUnit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match load_mapping().borrow().get(&id) {
            Some(&unit) => Ok(SerializableUnit(unit)),
            None => Err(S::Error::custom(format!(
                "Couldn't get unit for id {:?}",
                id
            ))),
        }
    }
}

impl std::ops::Deref for SerializableUnit {
    type Target = Unit;
    fn deref(&self) -> &Self::Target {
        &self.0
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
        next: unsafe { Unit::from_ptr(bw::first_active_unit()) },
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
            let unit = *self.next.unwrap();
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
    SAVE_ID_MAP: RefCell<FxHashMap<HashableUnit, u32>> =
        save_mapping(RefCell::new(FxHashMap::default()));
    LOAD_ID_MAP: RefCell<FxHashMap<u32, Unit>> =
        load_mapping(RefCell::new(FxHashMap::default()));
}

pub fn init_save_mapping() {
    *save_mapping().borrow_mut() = save_id_mapping()
        .map(|(x, y)| (HashableUnit(x), y))
        .collect();
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

pub trait UnitExt {
    fn orders(&self) -> Orders<'_>;
    fn worker_ai(self) -> Option<*mut bw::WorkerAi>;
    fn building_ai(self) -> Option<*mut bw::BuildingAi>;
    fn guard_ai(self) -> Option<*mut bw::GuardAi>;
    fn military_ai(self) -> Option<*mut bw::MilitaryAi>;
    fn issue_secondary_order(self, order: OrderId);
    fn issue_order(self, order: OrderId, pos: bw::Point, unit: Option<Unit>);
    fn issue_order_ground(self, order: OrderId, target: bw::Point);
    fn issue_order_unit(self, order: OrderId, target: Unit);
    fn can_issue_order(self, order: OrderId) -> bool;
}

impl UnitExt for Unit {
    fn orders(&self) -> Orders<'_> {
        unsafe {
            Orders {
                next: (***self).order_queue_begin,
                this: self,
                first: true,
            }
        }
    }

    fn worker_ai(self) -> Option<*mut bw::WorkerAi> {
        unsafe {
            let ai = (**self).ai as *mut bw::WorkerAi;
            if ai != null_mut() && (*ai).ai_type == 2 {
                Some(ai)
            } else {
                None
            }
        }
    }

    fn building_ai(self) -> Option<*mut bw::BuildingAi> {
        unsafe {
            let ai = (**self).ai as *mut bw::BuildingAi;
            if ai != null_mut() && (*ai).ai_type == 3 {
                Some(ai)
            } else {
                None
            }
        }
    }

    fn guard_ai(self) -> Option<*mut bw::GuardAi> {
        unsafe {
            let ai = (**self).ai as *mut bw::GuardAi;
            if ai != null_mut() && (*ai).ai_type == 1 {
                Some(ai)
            } else {
                None
            }
        }
    }

    fn military_ai(self) -> Option<*mut bw::MilitaryAi> {
        unsafe {
            let ai = (**self).ai as *mut bw::MilitaryAi;
            if ai != null_mut() && (*ai).ai_type == 4 {
                Some(ai)
            } else {
                None
            }
        }
    }

    fn issue_secondary_order(self, order: OrderId) {
        unsafe {
            if (**self).secondary_order != order.0 {
                (**self).secondary_order = order.0;
                (**self).secondary_order_state = 0;
                // Uhh.. Is this sensible to allow to be done from AI scripts?
                (**self).currently_building = null_mut();
                (**self).unke8 = 0;
                (**self).unkea = 0;
            }
        }
    }

    fn issue_order(self, order: OrderId, pos: bw::Point, unit: Option<Unit>) {
        if self.can_issue_order(order) {
            let unit_ptr = unit.map(|x| *x).unwrap_or(null_mut());
            unsafe { bw::issue_order(*self, order, pos, unit_ptr, id::NONE) }
        }
    }

    fn issue_order_ground(self, order: OrderId, target: bw::Point) {
        self.issue_order(order, target, None)
    }

    fn issue_order_unit(self, order: OrderId, target: Unit) {
        self.issue_order(order, target.position(), Some(target));
    }

    fn can_issue_order(self, order: OrderId) -> bool {
        // This is checked by targeted command/rclick/etc command handlers, but bw accepts
        // it otherwise, but doesn't clear related unit, so things would end up buggy.
        if self.id() == id::SCV && self.order() == bw_dat::order::CONSTRUCTING_BUILDING {
            return order == bw_dat::order::STOP;
        }
        // Technically should also check datreqs, oh well
        self.is_completed() && !self.is_disabled()
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
                    position: (***self.this).order_target_pos,
                    target: Unit::from_ptr((***self.this).target),
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

pub fn hidden_units() -> UnitListIter {
    UnitListIter(bw::first_hidden_unit())
}

pub struct UnitListIter(*mut bw::Unit);

impl Iterator for UnitListIter {
    type Item = Unit;
    fn next(&mut self) -> Option<Unit> {
        unsafe {
            if self.0 == null_mut() {
                None
            } else {
                let result = Unit::from_ptr(self.0);
                self.0 = (*self.0).next;
                result
            }
        }
    }
}
