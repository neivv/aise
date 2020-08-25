pub mod dialog;
pub mod game;
pub mod expr;
pub mod image;
pub mod unit;
pub mod sprite;

mod bw;
mod parse_expr;

pub use crate::game::Game;
pub use crate::image::Image;
pub use crate::unit::{Unit, UnitArray};
pub use crate::sprite::Sprite;

pub mod structs {
    pub use crate::bw::structs::*;
}

use std::mem;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use bitflags::bitflags;
use serde_derive::{Serialize, Deserialize};

pub use bw::DatTable;

static IS_SCR: AtomicBool = AtomicBool::new(false);
static BW_MALLOC: AtomicUsize = AtomicUsize::new(0);
static BW_FREE: AtomicUsize = AtomicUsize::new(0);
static BW_MOUSE: AtomicUsize = AtomicUsize::new(0);
static EXTENDED_ARRAYS: AtomicUsize = AtomicUsize::new(0);
static EXTENDED_ARRAYS_LEN: AtomicUsize = AtomicUsize::new(0);

#[repr(C)]
pub struct ExtendedArray {
    pub pointer: *mut u8,
    pub end: *mut u8,
    pub unused: usize,
    pub unused2: usize,
}

impl ExtendedArray {
    fn len(&self) -> usize {
        (self.end as usize).wrapping_sub(self.pointer as usize)
    }

    pub fn read_u8(&self, offset: usize) -> u8 {
        if offset < self.len() {
            unsafe { *self.pointer.add(offset) }
        } else {
            0
        }
    }

    pub fn write_u8(&self, offset: usize, value: u8) {
        if offset < self.len() {
            unsafe { *self.pointer.add(offset) = value; }
        }
    }

    pub fn read_u32(&self, offset: usize) -> u32 {
        let offset = offset << 2;
        if offset < self.len() {
            unsafe { *(self.pointer.add(offset) as *const u32) }
        } else {
            0
        }
    }

    pub fn write_u32(&self, offset: usize, value: u32) {
        let offset = offset << 2;
        if offset < self.len() {
            unsafe { *(self.pointer.add(offset) as *mut u32) = value; }
        }
    }
}

pub fn set_is_scr(value: bool) {
    IS_SCR.store(value, Ordering::Relaxed);
}

fn is_scr() -> bool {
    IS_SCR.load(Ordering::Relaxed) == true
}

pub unsafe fn set_bw_malloc(
    malloc: unsafe extern fn(usize) -> *mut u8,
    free: unsafe extern fn(*mut u8),
) {
    BW_MALLOC.store(malloc as usize, Ordering::Relaxed);
    BW_FREE.store(free as usize, Ordering::Relaxed);
}

pub unsafe fn set_bw_mouse_func(
    mouse: unsafe extern fn(*mut i32, *mut i32),
) {
    BW_MOUSE.store(mouse as usize, Ordering::Relaxed);
}

pub unsafe fn set_extended_arrays(arrays: *const ExtendedArray, len: usize) {
    EXTENDED_ARRAYS.store(arrays as usize, Ordering::Relaxed);
    EXTENDED_ARRAYS_LEN.store(len, Ordering::Relaxed);
}

unsafe fn bw_malloc(val: usize) -> *mut u8 {
    let func: unsafe extern fn(usize) -> *mut u8 =
        mem::transmute(BW_MALLOC.load(Ordering::Relaxed));
    func(val)
}

unsafe fn bw_free(val: *mut u8) {
    let func: unsafe extern fn(*mut u8) =
        mem::transmute(BW_FREE.load(Ordering::Relaxed));
    func(val)
}

pub fn extended_array(index: u32) -> Option<&'static ExtendedArray> {
    let index = index as usize;
    if index < EXTENDED_ARRAYS_LEN.load(Ordering::Relaxed) {
        unsafe {
            Some(&*(EXTENDED_ARRAYS.load(Ordering::Relaxed) as *const ExtendedArray).add(index))
        }
    } else {
        None
    }
}

fn bw_mouse() -> (i32, i32) {
    unsafe {
        let mut x = 0;
        let mut y = 0;
        let func: Option<unsafe extern fn(*mut i32, *mut i32)> =
            mem::transmute(BW_MOUSE.load(Ordering::Relaxed));
        if let Some(func) = func {
            func(&mut x, &mut y);
            (x, y)
        } else {
            (0, 0)
        }
    }
}

macro_rules! init_fns {
    ($($fn_name:ident, $global:ident,)*) => {
        $(
            static $global: [AtomicUsize; 2] = [AtomicUsize::new(0), AtomicUsize::new(0)];
        )*
        $(
            pub unsafe fn $fn_name(dat: *const DatTable, fields: usize) {
                $global[0].store(dat as usize, Ordering::Relaxed);
                $global[1].store(fields, Ordering::Relaxed);
            }
        )*
    }
}

init_fns! {
    init_units, UNITS_DAT,
    init_weapons, WEAPONS_DAT,
    init_flingy, FLINGY_DAT,
    init_sprites, SPRITES_DAT,
    init_images, IMAGES_DAT,
    init_orders, ORDERS_DAT,
    init_upgrades, UPGRADES_DAT,
    init_techdata, TECHDATA_DAT,
    init_sfxdata, SFXDATA_DAT,
    init_portdata, PORTDATA_DAT,
}

bitflags! {
    pub struct RaceFlags: u8 {
        const ZERG = 0x1;
        const TERRAN = 0x2;
        const PROTOSS = 0x4;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub enum Race {
    Zerg,
    Terran,
    Protoss,
}

impl Race {
    pub fn id(self) -> u8 {
        match self {
            Race::Zerg => 0,
            Race::Terran => 1,
            Race::Protoss => 2,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct OrderId(pub u8);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct UnitId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct WeaponId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct FlingyId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct UpgradeId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct TechId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct SpriteId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Hash)]
pub struct ImageId(pub u16);

unsafe fn dat_read(dat: &'static [AtomicUsize; 2], id: u32, field: u32) -> u32 {
    dat_read_opt(dat, id, field).unwrap_or_else(|| panic!("Missing field {:x}", field))
}

unsafe fn dat_read_opt(dat: &'static [AtomicUsize; 2], id: u32, field: u32) -> Option<u32> {
    if dat[1].load(Ordering::Relaxed) as u32 <= field {
        return None;
    }
    let dat = dat[0].load(Ordering::Relaxed) as *const bw::DatTable;
    let dat = &*dat.add(field as usize);
    assert!(dat.entries > id);
    Some(match dat.entry_size {
        1 => *(dat.data as *const u8).offset(id as isize) as u32,
        2 => *(dat.data as *const u16).offset(id as isize) as u32,
        4 => *(dat.data as *const u32).offset(id as isize),
        x => panic!("Invalid dat entry size: {}", x),
    })
}

pub mod weapon {
    use super::WeaponId;
    pub const HALO_ROCKETS: WeaponId = WeaponId(0x6a);
    pub const NONE: WeaponId = WeaponId(0x82);
}

pub mod upgrade {
    use super::UpgradeId;
    pub const PLASMA_SHIELDS: UpgradeId = UpgradeId(0xf);
    pub const U_268_SHELLS: UpgradeId = UpgradeId(0x16);
    pub const VENTRAL_SACS: UpgradeId = UpgradeId(0x18);
    pub const GROOVED_SPINES: UpgradeId = UpgradeId(0x1e);
    pub const SINGULARITY_CHARGE: UpgradeId = UpgradeId(0x21);
    pub const REAVER_CAPACITY: UpgradeId = UpgradeId(0x24);
    pub const CARRIER_CAPACITY: UpgradeId = UpgradeId(0x2b);
    pub const CHITINOUS_PLATING: UpgradeId = UpgradeId(0x34);
    pub const CHARON_BOOSTER: UpgradeId = UpgradeId(0x36);
    pub const NONE: UpgradeId = UpgradeId(0x3d);
}

pub mod tech {
    use super::TechId;
    pub const SPIDER_MINES: TechId = TechId(0x3);
    pub const CLOAKING_FIELD: TechId = TechId(0x9);
    pub const PERSONNEL_CLOAKING: TechId = TechId(0xa);
    pub const BURROWING: TechId = TechId(0xb);
    pub const NONE: TechId = TechId(0x2c);
}

pub mod order {
    use super::OrderId;
    pub const DIE: OrderId = OrderId(0x0);
    pub const STOP: OrderId = OrderId(0x1);
    pub const BUNKER_GUARD: OrderId = OrderId(0x5);
    pub const MOVE: OrderId = OrderId(0x6);
    pub const ATTACK: OrderId = OrderId(0x8);
    pub const ATTACK_OBSCURED: OrderId = OrderId(0x9);
    pub const ATTACK_UNIT: OrderId = OrderId(0xa);
    pub const ATTACK_FIXED_RANGE: OrderId = OrderId(0xb);
    pub const ATTACK_MOVE: OrderId = OrderId(0xe);
    pub const TOWER_ATTACK: OrderId = OrderId(0x13);
    pub const SUBUNIT_ATTACK: OrderId = OrderId(0x16);
    pub const NOTHING: OrderId = OrderId(0x17);
    pub const DRONE_BUILD: OrderId = OrderId(0x19);
    pub const DRONE_BUILD2: OrderId = OrderId(0x1a);
    pub const INFEST: OrderId = OrderId(0x1b);
    pub const SCV_BUILD: OrderId = OrderId(0x1e);
    pub const PROBE_BUILD: OrderId = OrderId(0x1f);
    pub const CONSTRUCTING_BUILDING: OrderId = OrderId(0x21);
    pub const REPAIR: OrderId = OrderId(0x22);
    pub const PLACE_ADDON: OrderId = OrderId(0x24);
    pub const BUILD_ADDON: OrderId = OrderId(0x25);
    pub const TRAIN: OrderId = OrderId(0x26);
    pub const RALLY_UNIT: OrderId = OrderId(0x27);
    pub const RALLY_POS: OrderId = OrderId(0x28);
    pub const ZERG_BIRTH: OrderId = OrderId(0x29);
    pub const UNIT_MORPH: OrderId = OrderId(0x2a);
    pub const BUILDING_MORPH: OrderId = OrderId(0x2b);
    pub const BUILD_NYDUS_EXIT: OrderId = OrderId(0x2e);
    pub const FOLLOW: OrderId = OrderId(0x31);
    pub const CARRIER_ATTACK: OrderId = OrderId(0x35);
    pub const CARRIER_ATTACK_OBSCURED: OrderId = OrderId(0x36);
    pub const CARRIER_ATTACK_UNIT: OrderId = OrderId(0x38);
    pub const REAVER_ATTACK: OrderId = OrderId(0x3b);
    pub const REAVER_ATTACK_OBSCURED: OrderId = OrderId(0x3c);
    pub const REAVER_ATTACK_UNIT: OrderId = OrderId(0x3d);
    pub const TRAIN_FIGHTER: OrderId = OrderId(0x3f);
    pub const INTERCEPTOR_ATTACK: OrderId = OrderId(0x40);
    pub const SCARAB_ATTACK: OrderId = OrderId(0x41);
    pub const SHIELD_BATTERY: OrderId = OrderId(0x44);
    pub const BUILDING_LAND: OrderId = OrderId(0x47);
    pub const LIFTOFF: OrderId = OrderId(0x48);
    pub const UPGRADE: OrderId = OrderId(0x4c);
    pub const SPAWNING_LARVA: OrderId = OrderId(0x4e);
    pub const HARVEST_GAS_MOVE: OrderId = OrderId(0x51);
    pub const HARVEST_GAS: OrderId = OrderId(0x53);
    pub const RETURN_GAS: OrderId = OrderId(0x54);
    pub const HARVEST_MINERALS_MOVE: OrderId = OrderId(0x55);
    pub const HARVEST_MINERALS: OrderId = OrderId(0x57);
    pub const RETURN_MINERALS: OrderId = OrderId(0x5a);
    pub const ENTER_TRANSPORT: OrderId = OrderId(0x5c);
    pub const LOAD_UNIT_TRANSPORT: OrderId = OrderId(0x5e);
    pub const SPREAD_CREEP: OrderId = OrderId(0x66);
    pub const ARCHON_WARP: OrderId = OrderId(0x69);
    pub const ARCHON_SUMMON: OrderId = OrderId(0x6a);
    pub const HOLD_POSITION: OrderId = OrderId(0x6b);
    pub const CLOAK: OrderId = OrderId(0x6d);
    pub const DECLOAK: OrderId = OrderId(0x6e);
    pub const UNLOAD: OrderId = OrderId(0x6f);
    pub const MOVE_UNLOAD: OrderId = OrderId(0x70);
    pub const UNBURROW: OrderId = OrderId(0x76);
    pub const CLOAKING_NEARBY_UNITS: OrderId = OrderId(0x83);
    pub const SAP_UNIT: OrderId = OrderId(0x86);
    pub const SAP_LOCATION: OrderId = OrderId(0x87);
    pub const HALLUCINATED: OrderId = OrderId(0x95);
    pub const RESET_COLLISION_HARVESTER: OrderId = OrderId(0x97);
    pub const COMPUTER_AI: OrderId = OrderId(0x9c);
    pub const AI_ATTACK_MOVE: OrderId = OrderId(0x9d);
    pub const REVEAL_TRAP: OrderId = OrderId(0xab);
    pub const MEDIC_MOVE: OrderId = OrderId(0xb1);
    pub const DARK_ARCHON_MELD: OrderId = OrderId(0xb7);
}

#[derive(Eq, PartialEq, Copy, Clone)]
#[repr(C)]
pub struct PlacementBox {
    pub width: u16,
    pub height: u16,
}

impl UnitId {
    pub fn optional(id: u32) -> Option<UnitId> {
        let limit = UnitId::entry_amount();
        if id > u16::max_value() as u32 || id >= limit ||
            (id >= unit::NONE.0 as u32 && id <= 260)
        {
            None
        } else {
            Some(UnitId(id as u16))
        }
    }

    pub fn entry_amount() -> u32 {
        unsafe {
            let dat = UNITS_DAT[0].load(Ordering::Relaxed) as *const DatTable;
            if dat.is_null() {
                u32::max_value()
            } else {
                (*dat).entries as u32
            }
        }
    }

    pub fn get(self, id: u32) -> u32 {
        unsafe { crate::dat_read(&UNITS_DAT, self.0 as u32, id) }
    }

    pub fn get_opt(self, id: u32) -> Option<u32> {
        unsafe { crate::dat_read_opt(&UNITS_DAT, self.0 as u32, id) }
    }

    pub fn hitpoints(self) -> i32 {
        self.get(8) as i32
    }

    pub fn shields(self) -> i32 {
        if self.has_shields() {
            // Yeah, it is stored as displayed
            self.get(7) as i32 * 256
        } else {
            0
        }
    }

    pub fn has_shields(self) -> bool {
        self.get(6) != 0
    }

    pub fn subunit(self) -> Option<UnitId> {
        UnitId::optional(self.get(1))
    }

    pub fn ground_weapon(self) -> Option<WeaponId> {
        WeaponId::optional(self.get(17))
    }

    pub fn air_weapon(self) -> Option<WeaponId> {
        WeaponId::optional(self.get(19))
    }

    pub fn flags(self) -> u32 {
        self.get(22)
    }

    pub fn ai_flags(self) -> u8 {
        self.get(21) as u8
    }

    pub fn return_to_idle_order(self) -> OrderId {
        OrderId(self.get(14) as u8)
    }

    pub fn attack_unit_order(self) -> OrderId {
        OrderId(self.get(15) as u8)
    }

    pub fn is_building(self) -> bool {
        self.flags() & 0x1 != 0
    }

    pub fn is_creature(self) -> bool {
        !self.is_building() &&
            !self.is_powerup() &&
            !self.is_doodad_unit() &&
            self != unit::DARK_SWARM
    }

    pub fn is_worker(self) -> bool {
        self.flags() & 0x8 != 0
    }

    pub fn is_subunit(self) -> bool {
        self.flags() & 0x10 != 0
    }

    pub fn is_hero(self) -> bool {
        self.flags() & 0x40 != 0
    }

    pub fn is_powerup(self) -> bool {
        self.flags() & 0x800 != 0
    }

    pub fn is_organic(self) -> bool {
        self.flags() & 0x10000 != 0
    }

    pub fn require_psi(self) -> bool {
        self.flags() & 0x80000 != 0
    }

    pub fn require_creep(self) -> bool {
        self.flags() & 0x20000 != 0
    }

    pub fn is_town_hall(self) -> bool {
        self.flags() & 0x1000 != 0
    }

    pub fn is_resource_container(self) -> bool {
        self.flags() & 0x2000 != 0
    }

    pub fn is_mechanical(self) -> bool {
        self.flags() & 0x4000_0000 != 0
    }

    pub fn group_flags(self) -> u32 {
        self.get(44)
    }

    pub fn races(self) -> RaceFlags {
        RaceFlags::from_bits_truncate((self.group_flags() as u8) & 0x7)
    }

    pub fn primary_race(self) -> Option<Race> {
        let races = self.races();
        if races.intersects(RaceFlags::ZERG) {
            Some(Race::Zerg)
        } else if races.intersects(RaceFlags::PROTOSS) {
            Some(Race::Protoss)
        } else if races.intersects(RaceFlags::TERRAN) {
            Some(Race::Terran)
        } else {
            None
        }

    }

    pub fn rank(self) -> u8 {
        self.get(11) as u8
    }

    pub fn armor(self) -> u32 {
        self.get(27)
    }

    pub fn armor_upgrade(self) -> Option<UpgradeId> {
        UpgradeId::optional(self.get(25))
    }

    pub fn mineral_cost(self) -> u32 {
        self.get(40)
    }

    pub fn gas_cost(self) -> u32 {
        self.get(41)
    }

    pub fn build_time(self) -> u32 {
        self.get(42)
    }

    pub fn supply_provided(self) -> u32 {
        self.get(45)
    }

    pub fn supply_cost(self) -> u32 {
        self.get(46)
    }

    pub fn placement(self) -> PlacementBox {
        unsafe {
            let dat = UNITS_DAT[0].load(Ordering::Relaxed) as *const DatTable;
            let dat = &*dat.add(36);
            assert!(dat.entries > u32::from(self.0));
            *(dat.data as *const PlacementBox).offset(self.0 as isize)
        }
    }

    pub fn cargo_space_used(self) -> u32 {
        self.get(47)
    }

    pub fn cargo_space_provided(self) -> u32 {
        self.get(48)
    }

    pub fn dimensions(self) -> Rect {
        unsafe {
            let dat = UNITS_DAT[0].load(Ordering::Relaxed) as *const DatTable;
            let dat = &*dat.add(38);
            assert!(dat.entries > u32::from(self.0));
            *(dat.data as *const Rect).offset(self.0 as isize)
        }
    }

    pub fn armor_type(self) -> u8 {
        self.get(26) as u8
    }

    pub fn rclick_action(self) -> u8 {
        self.get(28) as u8
    }

    pub fn sight_range(self) -> u8 {
        self.get(24) as u8
    }

    pub fn target_acquisition_range(self) -> u8 {
        self.get(23) as u8
    }

    pub fn is_beacon(self) -> bool {
        match self {
            unit::ZERG_BEACON | unit::TERRAN_BEACON | unit::PROTOSS_BEACON |
                unit::ZERG_FLAG_BEACON | unit::TERRAN_FLAG_BEACON |
                unit::PROTOSS_FLAG_BEACON => true,
            _ => false,
        }
    }

    /// If the unit is a gas building (not geyser)
    pub fn is_gas_building(self) -> bool {
        match self {
            unit::REFINERY | unit::EXTRACTOR | unit::ASSIMILATOR => true,
            _ => false,
        }
    }

    pub fn is_doodad_unit(self) -> bool {
        self.0 >= 203 && self.0 <= 213
    }

    pub fn map_specific_name(self) -> Option<u32> {
        Some(self.get(51)).filter(|&x| x != 0)
    }

    pub fn fighter_id(self) -> Option<UnitId> {
        match self {
            unit::REAVER | unit::WARBRINGER => Some(unit::SCARAB),
            unit::CARRIER | unit::GANTRITHOR => Some(unit::INTERCEPTOR),
            _ => None,
        }
    }

    pub fn icon(&self) -> u32 {
        self.get_opt(0x43)
            .unwrap_or_else(|| self.0 as u32)
    }
}

impl FlingyId {
    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&FLINGY_DAT, self.0 as u32, id) }
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
#[repr(C)]
pub struct Rect {
    pub left: i16,
    pub top: i16,
    pub right: i16,
    pub bottom: i16,
}

impl WeaponId {
    pub fn optional(id: u32) -> Option<WeaponId> {
        if id > u16::max_value() as u32 || id == weapon::NONE.0 as u32 {
            None
        } else {
            Some(WeaponId(id as u16))
        }
    }

    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&WEAPONS_DAT, self.0 as u32, id) }
    }

    pub fn damage(&self) -> u32 {
        self.get(14)
    }

    pub fn upgrade(&self) -> Option<UpgradeId> {
        UpgradeId::optional(self.get(6))
    }

    pub fn bonus(&self) -> u32 {
        self.get(15)
    }

    pub fn factor(&self) -> u32 {
        self.get(17)
    }

    pub fn label(&self) -> u32 {
        self.get(0)
    }

    pub fn max_range(&self) -> u32 {
        self.get(5)
    }

    pub fn min_range(&self) -> Option<NonZeroU32> {
        NonZeroU32::new(self.get(4))
    }

    pub fn damage_type(&self) -> u8 {
        self.get(7) as u8
    }

    pub fn attack_angle(&self) -> u8 {
        self.get(18) as u8
    }

    pub fn cooldown(&self) -> u32 {
        self.get(16)
    }

    pub fn icon(&self) -> u32 {
        self.get(23)
    }
}

impl UpgradeId {
    pub fn optional(id: u32) -> Option<UpgradeId> {
        if id > u16::max_value() as u32 || id == upgrade::NONE.0 as u32 {
            None
        } else {
            Some(UpgradeId(id as u16))
        }
    }

    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&UPGRADES_DAT, self.0 as u32, id) }
    }

    pub fn label(&self) -> u32 {
        self.get(8)
    }

    pub fn mineral_cost(&self) -> u32 {
        self.get(0)
    }

    pub fn gas_cost(&self) -> u32 {
        self.get(2)
    }

    pub fn time(&self) -> u32 {
        self.get(4)
    }

    pub fn mineral_factor(&self) -> u32 {
        self.get(1)
    }

    pub fn gas_factor(&self) -> u32 {
        self.get(3)
    }

    pub fn time_factor(&self) -> u32 {
        self.get(5)
    }

    pub fn icon(&self) -> u32 {
        self.get(7)
    }

    pub fn repeat_count(&self) -> u32 {
        self.get(10)
    }
}

impl TechId {
    pub fn optional(id: u32) -> Option<TechId> {
        if id > u16::max_value() as u32 || id == tech::NONE.0 as u32 {
            None
        } else {
            Some(TechId(id as u16))
        }
    }

    fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&TECHDATA_DAT, self.0 as u32, id) }
    }

    pub fn mineral_cost(&self) -> u32 {
        self.get(0)
    }

    pub fn gas_cost(&self) -> u32 {
        self.get(1)
    }

    pub fn time(&self) -> u32 {
        self.get(2)
    }

    pub fn energy_cost(&self) -> u32 {
        self.get(3)
    }

    pub fn icon(&self) -> u32 {
        self.get(6)
    }

    pub fn label(&self) -> u32 {
        self.get(7)
    }
}

impl OrderId {
    pub fn is_secondary(&self) -> bool {
        use order::*;
        match *self {
            TRAIN |
            CLOAKING_NEARBY_UNITS |
            CLOAK |
            DECLOAK |
            BUILD_ADDON |
            TRAIN_FIGHTER |
            SHIELD_BATTERY |
            SPAWNING_LARVA |
            SPREAD_CREEP |
            HALLUCINATED => true,
            _ => false,
        }
    }

    pub fn is_attack_order(&self) -> bool {
        use order::*;
        match *self {
            ATTACK |
            ATTACK_OBSCURED |
            ATTACK_UNIT |
            ATTACK_FIXED_RANGE |
            ATTACK_MOVE |
            TOWER_ATTACK |
            SUBUNIT_ATTACK |
            CARRIER_ATTACK |
            CARRIER_ATTACK_OBSCURED |
            CARRIER_ATTACK_UNIT |
            REAVER_ATTACK |
            REAVER_ATTACK_OBSCURED |
            REAVER_ATTACK_UNIT |
            INTERCEPTOR_ATTACK |
            SCARAB_ATTACK |
            SAP_UNIT |
            SAP_LOCATION |
            AI_ATTACK_MOVE |
            REVEAL_TRAP => true,
            _ => false,
        }
    }

    fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&ORDERS_DAT, self.0 as u32, id) }
    }

    pub fn tech(&self) -> Option<TechId> {
        TechId::optional(self.get(14))
    }

    pub fn weapon(&self) -> Option<WeaponId> {
        WeaponId::optional(self.get(13))
    }
}
