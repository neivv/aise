use std::hash::{Hash, Hasher};
use std::ptr::null_mut;

use once_cell::unsync::OnceCell;
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
        let unit_array = bw::unit_array();
        let index = unit_array.to_index(self.0);
        if index as usize >= unit_array.len() {
            Err(S::Error::custom(format!("Couldn't get id for unit {:?}", self)))
        } else {
            // Not doing index + 1 since these are not-Option units
            // Would have to specialize Option<Unit> ?? idk
            index.serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for SerializableUnit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let index = u32::deserialize(deserializer)?;
        let unit_array = bw::unit_array();
        if index as usize >= unit_array.len() {
            Err(S::Error::custom(format!("Couldn't get unit for id {:?}", index)))
        } else {
            unsafe {
                let unit = unit_array.ptr().add(index as usize);
                let unit = Unit::from_ptr(unit).unwrap();
                Ok(SerializableUnit(unit))
            }
        }
    }
}

impl std::ops::Deref for SerializableUnit {
    type Target = Unit;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait UnitExt {
    fn orders(&self) -> Orders<'_>;
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

    fn issue_secondary_order(self, order: OrderId) {
        unsafe {
            // Don't reset the order if already executing the same order, which
            // seems to be good rule almost always..
            let mut issue = (**self).secondary_order != order.0;
            // However, for training fighters the order will stay permamently active in
            // state 3 / 4 (3 makes carriers heal their interceptors), so it must be allowed
            // to be reissued unless it is currently in training state (2).
            if !issue &&
                order == bw_dat::order::TRAIN_FIGHTER &&
                (**self).secondary_order_state != 2
            {
                issue = true;
            }
            if issue {
                (**self).secondary_order = order.0;
                (**self).secondary_order_state = 0;
                // Uhh.. Is this sensible to allow to be done from AI scripts?
                (**self).secondary_order_target = bw::PointAndUnit {
                    pos: bw::Point { x: 0, y: 0 },
                    unit: null_mut(),
                };
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
                    target: self.this.target(),
                })
            } else if self.next != null_mut() {
                let order = self.next;
                self.next = (*order).next;
                Some(Order {
                    id: OrderId((*order).order_id),
                    target: Unit::from_ptr((*order).target.unit),
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
                self.0 = (*self.0).flingy.next as *mut bw::Unit;
                result
            }
        }
    }
}

pub struct UnitStrengths {
    air: *mut u32,
    ground: *mut u32,
    limit: u32,
}

impl UnitStrengths {
    pub fn ground(&self, id: UnitId) -> u32 {
        if id.0 as u32 >= self.limit {
            0
        } else {
            unsafe { *self.ground.add(id.0 as usize) }
        }
    }

    pub fn air(&self, id: UnitId) -> u32 {
        if id.0 as u32 >= self.limit {
            0
        } else {
            unsafe { *self.air.add(id.0 as usize) }
        }
    }
}

pub struct LazyUnitStrengths(OnceCell<UnitStrengths>);

impl LazyUnitStrengths {
    pub fn new() -> LazyUnitStrengths {
        LazyUnitStrengths(OnceCell::new())
    }

    pub fn get(&self) -> &UnitStrengths {
        self.0.get_or_init(|| {
            let (air, ground) = crate::samase::unit_base_strength();
            UnitStrengths {
                air,
                ground,
                limit: bw_dat::UnitId::entry_amount(),
            }
        })
    }
}
