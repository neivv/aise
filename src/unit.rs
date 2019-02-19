use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;

use byteorder::{ReadBytesExt, LE};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use bw;
use bw_dat::{tech, upgrade};
use game::Game;
use order::OrderId;

pub use bw_dat::unit as id;
pub use bw_dat::UnitId;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Unit(pub *mut bw::Unit);

unsafe impl Send for Unit {}
unsafe impl Sync for Unit {}

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
            None => Err(S::Error::custom(format!(
                "Couldn't get id for unit {:?}",
                self
            ))),
        }
    }
}

impl<'de> Deserialize<'de> for Unit {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match load_mapping().borrow().get(&id) {
            Some(&unit) => Ok(unit),
            None => Err(S::Error::custom(format!(
                "Couldn't get unit for id {:?}",
                id
            ))),
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

    pub fn secondary_order(&self) -> OrderId {
        OrderId(unsafe { (*self.0).secondary_order })
    }

    pub fn hitpoints(&self) -> i32 {
        unsafe { (*self.0).hitpoints }
    }

    pub fn hp_percent(&self) -> i32 {
        self.hitpoints().saturating_mul(100) / (self.id().hitpoints())
    }

    pub fn spider_mines(&self, game: Game) -> u8 {
        if game.tech_researched(self.player(), tech::SPIDER_MINES) || self.id().is_hero() {
            unsafe { (*self.0).unit_specific[0] }
        } else {
            0
        }
    }

    pub fn hangar_count(&self) -> u8 {
        // Count fighters outside hangar if carrier
        unsafe {
            match self.id() {
                id::CARRIER | id::GANTRITHOR => {
                    (*self.0).unit_specific[8] + (*self.0).unit_specific[9]
                }
                _ => (*self.0).unit_specific[8],
            }
        }
    }

    pub fn hangar_cap(&self, game: Game) -> u8 {
        match self.id() {
            id::CARRIER | id::GANTRITHOR => {
                let upgrade = upgrade::CARRIER_CAPACITY;
                if self.id().is_hero() || game.upgrade_level(self.player(), upgrade) > 0 {
                    8
                } else {
                    4
                }
            }
            id::REAVER | id::WARBRINGER => {
                let upgrade = upgrade::REAVER_CAPACITY;
                if self.id().is_hero() || game.upgrade_level(self.player(), upgrade) > 0 {
                    10
                } else {
                    5
                }
            }
            _ => 0,
        }
    }

    pub fn is_transport(&self, game: Game) -> bool {
        let upgrade = upgrade::VENTRAL_SACS;
        if self.id() == id::OVERLORD && game.upgrade_level(self.player(), upgrade) == 0 {
            false
        } else {
            self.id().cargo_space_provided() > 0
        }
    }

    pub fn has_nuke(&self) -> bool {
        unsafe {
            let nuke = Unit::from_ptr(
                (&(*self.0).unit_specific2[..]).read_u32::<LE>().unwrap() as *mut bw::Unit
            );
            match nuke {
                Some(n) => n.is_completed(),
                None => false,
            }
        }
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

    pub fn is_air(&self) -> bool {
        unsafe { (*self.0).flags & 0x4 != 0 }
    }

    /// Is the unit cloaked or burrowed (So it requires detection)
    pub fn is_invisible(&self) -> bool {
        unsafe { (*self.0).flags & 0x300 != 0 }
    }

    pub fn is_invincible(&self) -> bool {
        unsafe { (*self.0).flags & 0x04000000 != 0 }
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

    pub fn guard_ai(&self) -> Option<*mut bw::GuardAi> {
        unsafe {
            let ai = (*self.0).ai as *mut bw::GuardAi;
            if ai != null_mut() && (*ai).ai_type == 1 {
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
        let collision_rect = self.id().dimensions();
        let position = self.position();
        bw::Rect {
            left: (position.x - collision_rect.left).max(0),
            right: position.x + collision_rect.right + 1,
            top: (position.y - collision_rect.top).max(0),
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

    pub fn is_completed(&self) -> bool {
        unsafe { (*self.0).flags & 0x1 != 0 }
    }

    pub fn is_landed_building(&self) -> bool {
        unsafe { (*self.0).flags & 0x2 != 0 }
    }

    pub fn is_burrowed(&self) -> bool {
        unsafe { (*self.0).flags & 0x10 != 0 }
    }

    pub fn is_hallucination(&self) -> bool {
        unsafe { (*self.0).flags & 0x4000_0000 != 0 }
    }

    pub fn subunit_linked(&self) -> Option<Unit> {
        unsafe { Unit::from_ptr((*self.0).subunit) }
    }

    pub fn addon(&self) -> Option<Unit> {
        unsafe {
            if self.id().is_building() {
                let ptr = (&(*self.0).unit_specific[..]).read_u32::<LE>().unwrap() as *mut bw::Unit;
                Unit::from_ptr(ptr)
            } else {
                None
            }
        }
    }

    pub fn tech_in_progress(self) -> bw_dat::TechId {
        unsafe {
            if self.id().is_building() {
                bw_dat::TechId((*self.0).unit_specific[0x8].into())
            } else {
                tech::NONE
            }
        }
    }

    pub fn upgrade_in_progress(self) -> bw_dat::UpgradeId {
        unsafe {
            if self.id().is_building() {
                bw_dat::UpgradeId((*self.0).unit_specific[0x9].into())
            } else {
                upgrade::NONE
            }
        }
    }

    pub fn cargo_count(&self) -> u8 {
        unsafe { (*self.0).loaded_units.iter().filter(|&&x| x != 0).count() as u8 }
    }

    pub fn issue_order(&self, order: OrderId, pos: bw::Point, unit: Option<Unit>) {
        if self.can_issue_order(order) {
            let unit_ptr = unit.map(|x| x.0).unwrap_or(null_mut());
            unsafe { bw::issue_order(self.0, order, pos, unit_ptr, id::NONE) }
        }
    }

    pub fn issue_order_ground(&self, order: OrderId, target: bw::Point) {
        self.issue_order(order, target, None)
    }

    pub fn issue_order_unit(&self, order: OrderId, target: Unit) {
        self.issue_order(order, target.position(), Some(target));
    }

    pub fn is_disabled(&self) -> bool {
        unsafe {
            (*self.0).lockdown_timer != 0 ||
                (*self.0).stasis_timer != 0 ||
                (*self.0).maelstrom_timer != 0 ||
                (*self.0).flags & 0x400 != 0
        }
    }

    pub fn can_issue_order(&self, order: OrderId) -> bool {
        // This is checked by targeted command/rclick/etc command handlers, but bw accepts
        // it otherwise, but doesn't clear related unit, so things would end up buggy.
        if self.id() == id::SCV && self.order() == bw_dat::order::CONSTRUCTING_BUILDING {
            return order == bw_dat::order::STOP;
        }
        // Technically should also check datreqs, oh well
        self.is_completed() && !self.is_disabled()
    }

    pub fn is_hidden(&self) -> bool {
        unsafe { (*(*self.0).sprite).flags & 0x20 != 0 }
    }

    pub fn empty_build_slot(self) -> Option<u8> {
        unsafe {
            let mut pos = (*self.0).current_build_slot as usize;
            for _ in 0..5 {
                if pos == 5 {
                    pos = 0;
                }
                if (*self.0).build_queue[pos] == id::NONE.0 {
                    return Some(pos as u8);
                }
                pos += 1;
            }
            None
        }
    }

    pub fn is_building_addon(self) -> bool {
        if let Some(addon) = self.addon() {
            self.order() == bw_dat::order::BUILD_ADDON &&
                self.is_landed_building() &&
                !addon.is_completed()
        } else {
            false
        }
    }

    pub fn is_constructing_building(self) -> bool {
        let current_build_unit =
            unsafe { UnitId((*self.0).build_queue[(*self.0).current_build_slot as usize]) };
        current_build_unit != id::NONE && current_build_unit.is_building()
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
                let result = Some(Unit(self.0));
                self.0 = (*self.0).next;
                result
            }
        }
    }
}
