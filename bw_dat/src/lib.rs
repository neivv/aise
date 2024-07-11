pub mod dialog;
pub mod game;
pub mod expr;
pub mod image;
mod pathing;
pub mod unit;
pub mod sprite;

mod bw;
mod parse_expr;

pub use crate::game::Game;
pub use crate::image::Image;
pub use crate::pathing::{Pathing, Region};
pub use crate::unit::{Unit, UnitArray};
pub use crate::sprite::Sprite;

pub mod structs {
    pub use crate::bw::structs::*;
}

use std::mem;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicUsize, AtomicPtr, Ordering};
#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
use std::sync::atomic::AtomicBool;

use bitflags::bitflags;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub use bw::DatTable;

#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
static IS_SCR: AtomicBool = AtomicBool::new(false);
static BW_MALLOC: AtomicUsize = AtomicUsize::new(0);
static BW_FREE: AtomicUsize = AtomicUsize::new(0);
static BW_MOUSE: AtomicUsize = AtomicUsize::new(0);
static EXTENDED_ARRAYS: AtomicPtr<ExtendedArray> = AtomicPtr::new(std::ptr::null_mut());
static EXTENDED_ARRAYS_LEN: AtomicUsize = AtomicUsize::new(0);
static DAT_GLOBALS: [DatGlobal; DAT_TYPE_COUNT] = [const { DatGlobal::new() }; DAT_TYPE_COUNT];

const DAT_UNITS: u8 = DatType::Units as u8;
const DAT_UPGRADES: u8 = DatType::Upgrades as u8;
static FAST_UNIT_HAS_SHIELDS: FastDatField<DAT_UNITS, 6, 1> = FastDatField::new();
static FAST_UNIT_SHIELDS: FastDatField<DAT_UNITS, 7, 2> = FastDatField::new();
static FAST_UNIT_HP: FastDatField<DAT_UNITS, 8, 4> = FastDatField::new();
static FAST_UNIT_FLAGS: FastDatField<DAT_UNITS, 22, 4> = FastDatField::new();
static FAST_UNIT_MINERAL_COST: FastDatField<DAT_UNITS, 40, 2> = FastDatField::new();
static FAST_UNIT_GAS_COST: FastDatField<DAT_UNITS, 41, 2> = FastDatField::new();
static FAST_UNIT_BUILD_TIME: FastDatField<DAT_UNITS, 42, 2> = FastDatField::new();
static FAST_UPGRADES_MINERAL_COST: FastDatField<DAT_UPGRADES, 0, 2> = FastDatField::new();
static FAST_UPGRADES_GAS_COST: FastDatField<DAT_UPGRADES, 2, 2> = FastDatField::new();
static FAST_UPGRADES_TIME: FastDatField<DAT_UPGRADES, 4, 2> = FastDatField::new();

enum DatType {
    Units = 0,
    Weapons,
    Flingy,
    Sprites,
    Images,
    Orders,
    Upgrades,
    TechData,
    SfxData,
    PortData,
    Buttons,
}

const DAT_TYPE_COUNT: usize = DatType::Buttons as usize + 1;

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

#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
pub fn set_is_scr(value: bool) {
    IS_SCR.store(value, Ordering::Relaxed);
}

#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
fn is_scr() -> bool {
    IS_SCR.load(Ordering::Relaxed) == true
}

#[cfg(any(feature = "scr-only", target_pointer_width = "64"))]
pub fn set_is_scr(_: bool) {}

#[cfg(any(feature = "scr-only", target_pointer_width = "64"))]
fn is_scr() -> bool {
    true
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
    EXTENDED_ARRAYS.store(arrays.cast_mut(), Ordering::Relaxed);
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
            Some(&*EXTENDED_ARRAYS.load(Ordering::Relaxed).add(index))
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

struct DatGlobal {
    arrays: AtomicPtr<DatTable>,
    length: AtomicUsize,
}

struct FastDatField<const DAT: u8, const FIELD: u16, const SIZE: u32>(AtomicPtr<DatTable>);

impl<const DAT: u8, const FIELD: u16, const SIZE: u32> FastDatField<DAT, FIELD, SIZE> {
    const fn new() -> FastDatField<DAT, FIELD, SIZE> {
        FastDatField(AtomicPtr::new(std::ptr::null_mut()))
    }

    fn get(&self, id: u32) -> u32 {
        let ptr = self.0.load(Ordering::Relaxed);
        if ptr.is_null() == false {
            fast_dat_field_fast::<SIZE>(ptr, id)
        } else {
            fast_dat_field_slow(id, ((DAT as u32) << 16) | (FIELD as u32))
        }
    }

    #[cold]
    unsafe fn init(&self, dat: *const DatTable, fields: usize) {
        fast_dat_flags_init(&self.0, FIELD, SIZE, dat, fields)
    }
}

unsafe fn fast_dat_flags_init(
    result: &AtomicPtr<DatTable>,
    field: u16,
    size: u32,
    dat: *const DatTable,
    field_count: usize,
) {
    if field_count > field as usize {
        let ptr = dat.add(field as usize);
        if (*ptr).entry_size == size {
            result.store(ptr.cast_mut(), Ordering::Relaxed);
            return;
        }
    }
    result.store(std::ptr::null_mut(), Ordering::Relaxed);
}

impl DatGlobal {
    const fn new() -> DatGlobal {
        DatGlobal {
            arrays: AtomicPtr::new(std::ptr::null_mut()),
            length: AtomicUsize::new(0),
        }
    }
}

macro_rules! init_fns {
    ($($fn_name:ident, $ty:expr, $post_load:ident,)*) => {
        $(
            pub unsafe fn $fn_name(dat: *const DatTable, fields: usize) {
                let global = &DAT_GLOBALS[$ty as usize];
                global.arrays.store(dat.cast_mut(), Ordering::Relaxed);
                global.length.store(fields, Ordering::Relaxed);
                $post_load(dat, fields);
            }
        )*
    }
}

unsafe fn init_units_post(dat: *const DatTable, fields: usize) {
    FAST_UNIT_HAS_SHIELDS.init(dat, fields);
    FAST_UNIT_SHIELDS.init(dat, fields);
    FAST_UNIT_HP.init(dat, fields);
    FAST_UNIT_FLAGS.init(dat, fields);
    FAST_UNIT_MINERAL_COST.init(dat, fields);
    FAST_UNIT_GAS_COST.init(dat, fields);
    FAST_UNIT_BUILD_TIME.init(dat, fields);
}

unsafe fn init_upgrades_post(dat: *const DatTable, fields: usize) {
    FAST_UPGRADES_MINERAL_COST.init(dat, fields);
    FAST_UPGRADES_GAS_COST.init(dat, fields);
    FAST_UPGRADES_TIME.init(dat, fields);
}

#[inline(always)]
unsafe fn init_post_nop(_: *const DatTable, _: usize) {
}

init_fns! {
    init_units, DatType::Units, init_units_post,
    init_weapons, DatType::Weapons, init_post_nop,
    init_flingy, DatType::Flingy, init_post_nop,
    init_sprites, DatType::Sprites, init_post_nop,
    init_images, DatType::Images, init_post_nop,
    init_orders, DatType::Orders, init_post_nop,
    init_upgrades, DatType::Upgrades, init_upgrades_post,
    init_techdata, DatType::TechData, init_post_nop,
    init_sfxdata, DatType::SfxData, init_post_nop,
    init_portdata, DatType::PortData, init_post_nop,
    init_buttons, DatType::Buttons, init_post_nop,
}

bitflags! {
    #[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
    pub struct RaceFlags: u8 {
        const ZERG = 0x1;
        const TERRAN = 0x2;
        const PROTOSS = 0x4;
    }
}

#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
pub enum Race {
    Zerg = 0,
    Terran = 1,
    Protoss = 2,
}

impl Race {
    pub fn id(self) -> u8 {
        self as u8
    }

    pub fn from_id(val: u8) -> Option<Race> {
        if val < 3 {
            Some(unsafe { mem::transmute(val) })
        } else {
            None
        }
    }

    pub fn as_flags(self) -> RaceFlags {
        RaceFlags::from_bits_truncate(1 << (self as u8))
    }
}

#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct OrderId(pub u8);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnitId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct WeaponId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FlingyId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UpgradeId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TechId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct SpriteId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ImageId(pub u16);
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ButtonSetId(pub u16);

unsafe fn dat_read(dat: &DatGlobal, id: u32, field: u32) -> u32 {
    dat_read_opt(dat, id, field).unwrap_or(0)
}

unsafe fn dat_read_opt(dat: &DatGlobal, id: u32, field: u32) -> Option<u32> {
    if dat.length.load(Ordering::Relaxed) <= field as usize {
        return None;
    }
    let dat = dat.arrays.load(Ordering::Relaxed);
    let dat = &*dat.add(field as usize);
    if dat.entries <= id {
        return None;
    }
    let data = dat.data;
    Some(match dat.entry_size {
        1 => *(data as *const u8).add(id as usize) as u32,
        2 => *(data as *const u16).add(id as usize) as u32,
        4 => *(data as *const u32).add(id as usize),
        _ => return None,
    })
}

/// Skips some checks that were verified before in init_dat
fn fast_dat_field_fast<const SIZE: u32>(dat: *mut DatTable, id: u32) -> u32 {
    unsafe {
        if (*dat).entries <= id {
            return 0;
        }
        let data = (*dat).data;
        match SIZE {
            1 => *(data as *const u8).add(id as usize) as u32,
            2 => *(data as *const u16).add(id as usize) as u32,
            4 => *(data as *const u32).add(id as usize),
            _ => unreachable!(),
        }
    }
}

#[inline(never)]
fn fast_dat_field_slow(id: u32, dat_and_field: u32) -> u32 {
    let dat = dat_and_field >> 16;
    let field = dat_and_field & 0xffff;
    let dat = &DAT_GLOBALS[dat as usize];
    unsafe { crate::dat_read(dat, id, field) }
}

pub mod weapon {
    use super::WeaponId;
    pub const HALO_ROCKETS: WeaponId = WeaponId(0x6a);
    pub const NONE: WeaponId = WeaponId(0x82);
}

pub mod upgrade {
    use super::UpgradeId;
    pub const INFANTRY_ARMOR: UpgradeId = UpgradeId(0x0);
    pub const VEHICLE_PLATING: UpgradeId = UpgradeId(0x1);
    pub const SHIP_PLATING: UpgradeId = UpgradeId(0x2);
    pub const CARAPACE: UpgradeId = UpgradeId(0x3);
    pub const FLYER_CARAPACE: UpgradeId = UpgradeId(0x4);
    pub const PROTOSS_ARMOR: UpgradeId = UpgradeId(0x5);
    pub const PROTOSS_PLATING: UpgradeId = UpgradeId(0x6);
    pub const INFANTRY_WEAPONS: UpgradeId = UpgradeId(0x7);
    pub const VEHICLE_WEAPONS: UpgradeId = UpgradeId(0x8);
    pub const SHIP_WEAPONS: UpgradeId = UpgradeId(0x9);
    pub const ZERG_MELEE_ATTACKS: UpgradeId = UpgradeId(0xa);
    pub const ZERG_MISSILE_ATTACKS: UpgradeId = UpgradeId(0xb);
    pub const ZERG_FLYER_ATTACKS: UpgradeId = UpgradeId(0xc);
    pub const PROTOSS_GROUND_WEAPONS: UpgradeId = UpgradeId(0xd);
    pub const PROTOSS_AIR_WEAPONS: UpgradeId = UpgradeId(0xe);
    pub const PLASMA_SHIELDS: UpgradeId = UpgradeId(0xf);
    pub const U_268_SHELLS: UpgradeId = UpgradeId(0x10);
    pub const ION_THRUSTERS: UpgradeId = UpgradeId(0x11);
    pub const BURST_LASERS: UpgradeId = UpgradeId(0x12);
    pub const TITAN_REACTOR: UpgradeId = UpgradeId(0x13);
    pub const OCULAR_IMPLANTS: UpgradeId = UpgradeId(0x14);
    pub const MOEBIUS_REACTOR: UpgradeId = UpgradeId(0x15);
    pub const APOLLO_REACTOR: UpgradeId = UpgradeId(0x16);
    pub const COLOSSUS_REACTOR: UpgradeId = UpgradeId(0x17);
    pub const VENTRAL_SACS: UpgradeId = UpgradeId(0x18);
    pub const ANTENNAE: UpgradeId = UpgradeId(0x19);
    pub const PNEUMATIZED_CARAPACE: UpgradeId = UpgradeId(0x1a);
    pub const METABOLIC_BOOST: UpgradeId = UpgradeId(0x1b);
    pub const ADRENAL_GLANDS: UpgradeId = UpgradeId(0x1c);
    pub const MUSCULAR_AUGMENTS: UpgradeId = UpgradeId(0x1d);
    pub const GROOVED_SPINES: UpgradeId = UpgradeId(0x1e);
    pub const GAMETE_MEIOSIS: UpgradeId = UpgradeId(0x1f);
    pub const METASYNAPTIC_NODE: UpgradeId = UpgradeId(0x20);
    pub const SINGULARITY_CHARGE: UpgradeId = UpgradeId(0x21);
    pub const LEG_ENHANCEMENTS: UpgradeId = UpgradeId(0x22);
    pub const SCARAB_DAMAGE: UpgradeId = UpgradeId(0x23);
    pub const REAVER_CAPACITY: UpgradeId = UpgradeId(0x24);
    pub const GRAVITIC_DRIVE: UpgradeId = UpgradeId(0x25);
    pub const SENSOR_ARRAY: UpgradeId = UpgradeId(0x26);
    pub const GRAVITIC_BOOSTERS: UpgradeId = UpgradeId(0x27);
    pub const KHAYDARIN_AMULET: UpgradeId = UpgradeId(0x28);
    pub const APIAL_SENSORS: UpgradeId = UpgradeId(0x29);
    pub const GRAVITIC_THRUSTERS: UpgradeId = UpgradeId(0x2a);
    pub const CARRIER_CAPACITY: UpgradeId = UpgradeId(0x2b);
    pub const KHAYDARIN_CORE: UpgradeId = UpgradeId(0x2c);
    pub const UPGRADE_45: UpgradeId = UpgradeId(0x2d);
    pub const UPGRADE_46: UpgradeId = UpgradeId(0x2e);
    pub const ARGUS_JEWEL: UpgradeId = UpgradeId(0x2f);
    pub const UPGRADE_48: UpgradeId = UpgradeId(0x30);
    pub const ARGUS_TALISMAN: UpgradeId = UpgradeId(0x31);
    pub const UPGRADE_50: UpgradeId = UpgradeId(0x32);
    pub const CADUCEUS_REACTOR: UpgradeId = UpgradeId(0x33);
    pub const CHITINOUS_PLATING: UpgradeId = UpgradeId(0x34);
    pub const ANABOLIC_SYNTHESIS: UpgradeId = UpgradeId(0x35);
    pub const CHARON_BOOSTER: UpgradeId = UpgradeId(0x36);
    pub const NONE: UpgradeId = UpgradeId(0x3d);
}

pub mod tech {
    use super::TechId;
    pub const STIM_PACKS: TechId = TechId(0x0);
    pub const LOCKDOWN: TechId = TechId(0x1);
    pub const EMP_SHOCKWAVE: TechId = TechId(0x2);
    pub const SPIDER_MINES: TechId = TechId(0x3);
    pub const SCANNER_SWEEP: TechId = TechId(0x4);
    pub const SIEGE_MODE: TechId = TechId(0x5);
    pub const DEFENSIVE_MATRIX: TechId = TechId(0x6);
    pub const IRRADIATE: TechId = TechId(0x7);
    pub const YAMATO_GUN: TechId = TechId(0x8);
    pub const CLOAKING_FIELD: TechId = TechId(0x9);
    pub const PERSONNEL_CLOAKING: TechId = TechId(0xa);
    pub const BURROWING: TechId = TechId(0xb);
    pub const INFESTATION: TechId = TechId(0xc);
    pub const SPAWN_BROODLINGS: TechId = TechId(0xd);
    pub const DARK_SWARM: TechId = TechId(0xe);
    pub const PLAGUE: TechId = TechId(0xf);
    pub const CONSUME: TechId = TechId(0x10);
    pub const ENSNARE: TechId = TechId(0x11);
    pub const PARASITE: TechId = TechId(0x12);
    pub const PSIONIC_STORM: TechId = TechId(0x13);
    pub const HALLUCINATION: TechId = TechId(0x14);
    pub const RECALL: TechId = TechId(0x15);
    pub const STASIS_FIELD: TechId = TechId(0x16);
    pub const ARCHON_WARP: TechId = TechId(0x17);
    pub const RESTORATION: TechId = TechId(0x18);
    pub const DISRUPTION_WEB: TechId = TechId(0x19);
    pub const TECH_26: TechId = TechId(0x1a);
    pub const MIND_CONTROL: TechId = TechId(0x1b);
    pub const DARK_ARCHON_MELD: TechId = TechId(0x1c);
    pub const FEEDBACK: TechId = TechId(0x1d);
    pub const OPTICAL_FLARE: TechId = TechId(0x1e);
    pub const MAELSTROM: TechId = TechId(0x1f);
    pub const LURKER_ASPECT: TechId = TechId(0x20);
    pub const TECH_33: TechId = TechId(0x21);
    pub const HEALING: TechId = TechId(0x22);
    pub const NONE: TechId = TechId(0x2c);
}

pub mod order {
    use super::OrderId;
    pub const DIE: OrderId = OrderId(0x0);
    pub const STOP: OrderId = OrderId(0x1);
    pub const GUARD: OrderId = OrderId(0x2);
    pub const PLAYER_GUARD: OrderId = OrderId(0x3);
    pub const TURRET_GUARD: OrderId = OrderId(0x4);
    pub const BUNKER_GUARD: OrderId = OrderId(0x5);
    pub const MOVE: OrderId = OrderId(0x6);
    pub const REAVER_STOP: OrderId = OrderId(0x7);
    pub const ATTACK: OrderId = OrderId(0x8);
    pub const ATTACK_OBSCURED: OrderId = OrderId(0x9);
    pub const ATTACK_UNIT: OrderId = OrderId(0xa);
    pub const ATTACK_FIXED_RANGE: OrderId = OrderId(0xb);
    pub const ATTACK_GROUND: OrderId = OrderId(0xc);
    pub const HOVER: OrderId = OrderId(0xd);
    pub const ATTACK_MOVE: OrderId = OrderId(0xe);
    pub const INFEST_MINE: OrderId = OrderId(0xf);
    pub const NOTHING_UNUSED: OrderId = OrderId(0x10);
    pub const POWER_UP_UNKNOWN: OrderId = OrderId(0x11);
    pub const TOWER_GUARD: OrderId = OrderId(0x12);
    pub const TOWER_ATTACK: OrderId = OrderId(0x13);
    pub const SPIDER_MINE: OrderId = OrderId(0x14);
    pub const STAY_IN_RANGE: OrderId = OrderId(0x15);
    pub const SUBUNIT_ATTACK: OrderId = OrderId(0x16);
    pub const NOTHING: OrderId = OrderId(0x17);
    pub const TURRET_ATTACK: OrderId = OrderId(0x18);
    pub const DRONE_BUILD: OrderId = OrderId(0x19);
    pub const DRONE_BUILD2: OrderId = OrderId(0x1a);
    pub const INFEST: OrderId = OrderId(0x1b);
    pub const INFEST2: OrderId = OrderId(0x1c);
    pub const INFEST3: OrderId = OrderId(0x1d);
    pub const SCV_BUILD: OrderId = OrderId(0x1e);
    pub const PROBE_BUILD: OrderId = OrderId(0x1f);
    pub const PROBE_CREATE_BUILDING: OrderId = OrderId(0x20);
    pub const CONSTRUCTING_BUILDING: OrderId = OrderId(0x21);
    pub const REPAIR: OrderId = OrderId(0x22);
    pub const REPAIR_OBSCURED: OrderId = OrderId(0x23);
    pub const PLACE_ADDON: OrderId = OrderId(0x24);
    pub const BUILD_ADDON: OrderId = OrderId(0x25);
    pub const TRAIN: OrderId = OrderId(0x26);
    pub const RALLY_UNIT: OrderId = OrderId(0x27);
    pub const RALLY_POS: OrderId = OrderId(0x28);
    pub const ZERG_BIRTH: OrderId = OrderId(0x29);
    pub const UNIT_MORPH: OrderId = OrderId(0x2a);
    pub const BUILDING_MORPH: OrderId = OrderId(0x2b);
    pub const TERRAN_BUILD_SELF: OrderId = OrderId(0x2c);
    pub const ZERG_BUILD_SELF: OrderId = OrderId(0x2d);
    pub const BUILD_NYDUS_EXIT: OrderId = OrderId(0x2e);
    pub const ENTER_NYDUS: OrderId = OrderId(0x2f);
    pub const PROTOSS_BUILD_SELF: OrderId = OrderId(0x30);
    pub const FOLLOW: OrderId = OrderId(0x31);
    pub const CARRIER_IDLE: OrderId = OrderId(0x32);
    pub const CARRIER_REAVER_MOVE: OrderId = OrderId(0x33);
    pub const CARRIER_STOP: OrderId = OrderId(0x34);
    pub const CARRIER_ATTACK: OrderId = OrderId(0x35);
    pub const CARRIER_ATTACK_OBSCURED: OrderId = OrderId(0x36);
    pub const CARRIER_MOVE: OrderId = OrderId(0x37);
    pub const CARRIER_ATTACK_UNIT: OrderId = OrderId(0x38);
    pub const CARRIER_HOLD_POSITION: OrderId = OrderId(0x39);
    pub const REAVER_IDLE: OrderId = OrderId(0x3a);
    pub const REAVER_ATTACK: OrderId = OrderId(0x3b);
    pub const REAVER_ATTACK_OBSCURED: OrderId = OrderId(0x3c);
    pub const REAVER_ATTACK_UNIT: OrderId = OrderId(0x3d);
    pub const REAVER_HOLD_POSITION: OrderId = OrderId(0x3e);
    pub const TRAIN_FIGHTER: OrderId = OrderId(0x3f);
    pub const INTERCEPTOR_ATTACK: OrderId = OrderId(0x40);
    pub const SCARAB_ATTACK: OrderId = OrderId(0x41);
    pub const RECHARGE_SHIELDS_UNIT: OrderId = OrderId(0x42);
    pub const RECHARGE_SHIELDS_AREA: OrderId = OrderId(0x43);
    pub const SHIELD_BATTERY: OrderId = OrderId(0x44);
    pub const INTERCEPTOR_RETURN: OrderId = OrderId(0x45);
    pub const DRONE_LAND: OrderId = OrderId(0x46);
    pub const BUILDING_LAND: OrderId = OrderId(0x47);
    pub const LIFTOFF: OrderId = OrderId(0x48);
    pub const DRONE_LIFTOFF: OrderId = OrderId(0x49);
    pub const LIFTING_OFF: OrderId = OrderId(0x4a);
    pub const RESEARCH_TECH: OrderId = OrderId(0x4b);
    pub const UPGRADE: OrderId = OrderId(0x4c);
    pub const LARVA: OrderId = OrderId(0x4d);
    pub const SPAWNING_LARVA: OrderId = OrderId(0x4e);
    pub const HARVEST: OrderId = OrderId(0x4f);
    pub const HARVEST_OBSCURED: OrderId = OrderId(0x50);
    pub const HARVEST_GAS_MOVE: OrderId = OrderId(0x51);
    pub const HARVEST_GAS_START: OrderId = OrderId(0x54);
    pub const HARVEST_GAS: OrderId = OrderId(0x53);
    pub const RETURN_GAS: OrderId = OrderId(0x54);
    pub const HARVEST_MINERALS_MOVE: OrderId = OrderId(0x55);
    pub const HARVEST_MINERALS_START: OrderId = OrderId(0x56);
    pub const HARVEST_MINERALS: OrderId = OrderId(0x57);
    pub const HARVEST_MINERALS_INTERRUPTED: OrderId = OrderId(0x58);
    pub const HARVEST_MINERALS_UNKNOWN: OrderId = OrderId(0x59);
    pub const RETURN_MINERALS: OrderId = OrderId(0x5a);
    pub const HARVEST_INTERRUPTED_SHIELD_RECHARGE: OrderId = OrderId(0x5b);
    pub const ENTER_TRANSPORT: OrderId = OrderId(0x5c);
    pub const TRANSPORT_IDLE: OrderId = OrderId(0x5d);
    pub const LOAD_UNIT_TRANSPORT: OrderId = OrderId(0x5e);
    pub const LOAD_UNIT_BUNKER: OrderId = OrderId(0x5f);
    pub const LOAD_UNIT_UNKNOWN: OrderId = OrderId(0x60);
    pub const POWERUP: OrderId = OrderId(0x61);
    pub const SIEGE_MODE: OrderId = OrderId(0x62);
    pub const TANK_MODE: OrderId = OrderId(0x63);
    pub const WATCH_TARGET: OrderId = OrderId(0x64);
    pub const SPREAD_CREEP_INIT: OrderId = OrderId(0x65);
    pub const SPREAD_CREEP: OrderId = OrderId(0x66);
    pub const SPREAD_CREEP_STOP: OrderId = OrderId(0x67);
    pub const GUARDIAN_ASPECT: OrderId = OrderId(0x68);
    pub const ARCHON_WARP: OrderId = OrderId(0x69);
    pub const ARCHON_SUMMON: OrderId = OrderId(0x6a);
    pub const HOLD_POSITION: OrderId = OrderId(0x6b);
    pub const QUEEN_HOLD_POSITION: OrderId = OrderId(0x6c);
    pub const CLOAK: OrderId = OrderId(0x6d);
    pub const DECLOAK: OrderId = OrderId(0x6e);
    pub const UNLOAD: OrderId = OrderId(0x6f);
    pub const MOVE_UNLOAD: OrderId = OrderId(0x70);
    pub const YAMATO_GUN: OrderId = OrderId(0x71);
    pub const YAMATO_GUN_OBSCURED: OrderId = OrderId(0x72);
    pub const LOCKDOWN: OrderId = OrderId(0x73);
    pub const BURROW: OrderId = OrderId(0x74);
    pub const BURROWED: OrderId = OrderId(0x75);
    pub const UNBURROW: OrderId = OrderId(0x76);
    pub const DARK_SWARM: OrderId = OrderId(0x77);
    pub const PARASITE: OrderId = OrderId(0x78);
    pub const SPAWN_BROODLING: OrderId = OrderId(0x79);
    pub const EMP_SHOCKWAVE: OrderId = OrderId(0x7a);
    pub const NUKE_WAIT: OrderId = OrderId(0x7b);
    pub const NUKE_TRAIN: OrderId = OrderId(0x7c);
    pub const NUKE_LAUNCH: OrderId = OrderId(0x7d);
    pub const NUKE_PAINT: OrderId = OrderId(0x7e);
    pub const NUKE_UNIT: OrderId = OrderId(0x7f);
    pub const NUKE_GROUND: OrderId = OrderId(0x80);
    pub const NUKE_TRACK: OrderId = OrderId(0x81);
    pub const ARBITER_INIT: OrderId = OrderId(0x82);
    pub const CLOAKING_NEARBY_UNITS: OrderId = OrderId(0x83);
    pub const PLACE_MINE: OrderId = OrderId(0x84);
    pub const RIGHT_CLICK: OrderId = OrderId(0x85);
    pub const SAP_UNIT: OrderId = OrderId(0x86);
    pub const SAP_LOCATION: OrderId = OrderId(0x87);
    pub const SUICIDE_HOLD_POSITION: OrderId = OrderId(0x88);
    pub const RECALL: OrderId = OrderId(0x89);
    pub const RECALLED: OrderId = OrderId(0x8a);
    pub const SCANNER_SWEEP: OrderId = OrderId(0x8b);
    pub const SCANNER_SWEEP_IDLE: OrderId = OrderId(0x8c);
    pub const DEFENSIVE_MATRIX: OrderId = OrderId(0x8d);
    pub const PSI_STORM: OrderId = OrderId(0x8e);
    pub const IRRADIATE: OrderId = OrderId(0x8f);
    pub const PLAGUE: OrderId = OrderId(0x90);
    pub const CONSUME: OrderId = OrderId(0x91);
    pub const ENSNARE: OrderId = OrderId(0x92);
    pub const STASIS_FIELD: OrderId = OrderId(0x93);
    pub const HALLUCINATION: OrderId = OrderId(0x94);
    pub const HALLUCINATED: OrderId = OrderId(0x95);
    pub const RESET_COLLISION: OrderId = OrderId(0x96);
    pub const RESET_COLLISION_HARVESTER: OrderId = OrderId(0x97);
    pub const PATROL: OrderId = OrderId(0x98);
    pub const CTF_COP_INIT: OrderId = OrderId(0x99);
    pub const CTF_COP_IDLE: OrderId = OrderId(0x9a);
    pub const CTF_COP_IDLE2: OrderId = OrderId(0x9b);
    pub const COMPUTER_AI: OrderId = OrderId(0x9c);
    pub const AI_ATTACK_MOVE: OrderId = OrderId(0x9d);
    pub const AI_HARASS_MOVE: OrderId = OrderId(0x9e);
    pub const AI_PATROL: OrderId = OrderId(0x9f);
    pub const AI_GUARD: OrderId = OrderId(0xa0);
    pub const RESCUABLE_IDLE: OrderId = OrderId(0xa1);
    pub const NEUTRAL_IDLE: OrderId = OrderId(0xa2);
    pub const AI_RETURN: OrderId = OrderId(0xa3);
    pub const INIT_PYLON_POWER: OrderId = OrderId(0xa4);
    pub const SCARAB_SELF_DESTRUCT: OrderId = OrderId(0xa5);
    pub const CRITTER: OrderId = OrderId(0xa6);
    pub const TRAP: OrderId = OrderId(0xa7);
    pub const OPEN_DOOR: OrderId = OrderId(0xa8);
    pub const CLOSE_DOOR: OrderId = OrderId(0xa9);
    pub const HIDE_TRAP: OrderId = OrderId(0xaa);
    pub const REVEAL_TRAP: OrderId = OrderId(0xab);
    pub const ENABLE_DOODAD: OrderId = OrderId(0xac);
    pub const DISABLE_DOODAD: OrderId = OrderId(0xad);
    pub const WARP_IN: OrderId = OrderId(0xae);
    pub const MEDIC: OrderId = OrderId(0xaf);
    pub const MEDIC_HEAL: OrderId = OrderId(0xb0);
    pub const MEDIC_MOVE: OrderId = OrderId(0xb1);
    pub const MEDIC_HOLD_POSITION: OrderId = OrderId(0xb2);
    pub const MEDIC_RETURN: OrderId = OrderId(0xb3);
    pub const RESTORATION: OrderId = OrderId(0xb4);
    pub const DISRUPTION_WEB: OrderId = OrderId(0xb5);
    pub const MIND_CONTROL: OrderId = OrderId(0xb6);
    pub const DARK_ARCHON_MELD: OrderId = OrderId(0xb7);
    pub const FEEDBACK: OrderId = OrderId(0xb8);
    pub const OPTICAL_FLARE: OrderId = OrderId(0xb9);
    pub const MAELSTROM: OrderId = OrderId(0xba);
    pub const JUNK_YARD_DOG: OrderId = OrderId(0xbb);
    pub const FATAL: OrderId = OrderId(0xbc);
    pub const NONE: OrderId = OrderId(0xbd);
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
        if id > u16::MAX as u32 || id >= limit ||
            (id >= unit::NONE.0 as u32 && id <= 260)
        {
            None
        } else {
            Some(UnitId(id as u16))
        }
    }

    fn global() -> &'static DatGlobal {
        &DAT_GLOBALS[DatType::Units as usize]
    }

    pub fn entry_amount() -> u32 {
        unsafe {
            let dat = Self::global().arrays.load(Ordering::Relaxed);
            if dat.is_null() {
                u32::MAX
            } else {
                (*dat).entries as u32
            }
        }
    }

    pub fn get(self, id: u32) -> u32 {
        unsafe { crate::dat_read(Self::global(), self.0 as u32, id) }
    }

    pub fn get_opt(self, id: u32) -> Option<u32> {
        unsafe { crate::dat_read_opt(Self::global(), self.0 as u32, id) }
    }

    pub fn hitpoints(self) -> i32 {
        FAST_UNIT_HP.get(self.0 as u32) as i32
    }

    pub fn shields(self) -> i32 {
        if self.has_shields() {
            let shields = FAST_UNIT_SHIELDS.get(self.0 as u32) as i32;
            shields.saturating_mul(256)
        } else {
            0
        }
    }

    pub fn has_shields(self) -> bool {
        FAST_UNIT_HAS_SHIELDS.get(self.0 as u32) != 0
    }

    pub fn flingy(self) -> FlingyId {
        FlingyId(self.get(0) as u16)
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
        FAST_UNIT_FLAGS.get(self.0 as u32)
    }

    pub fn ai_flags(self) -> u8 {
        self.get(21) as u8
    }

    pub fn ai_init_order(self) -> OrderId {
        OrderId(self.get(12) as u8)
    }

    pub fn human_init_order(self) -> OrderId {
        OrderId(self.get(13) as u8)
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

    pub fn is_addon(self) -> bool {
        self.flags() & 0x2 != 0
    }

    pub fn is_air(self) -> bool {
        self.flags() & 0x4 != 0
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

    pub fn overlay_size(self) -> u8 {
        let size = ((self.flags() & 0x0600_0000) >> 0x19) as u8;
        // Size 3 => size 1
        // (Flag 0200_0000 takes priority over 0400_0000)
        // Clears higher bit if lower bit is set, keeps lower bit unconditionally
        size & ((!(size << 1)) | 1)
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
        FAST_UNIT_MINERAL_COST.get(self.0 as u32)
    }

    pub fn gas_cost(self) -> u32 {
        FAST_UNIT_GAS_COST.get(self.0 as u32)
    }

    pub fn build_time(self) -> u32 {
        FAST_UNIT_BUILD_TIME.get(self.0 as u32)
    }

    pub fn supply_provided(self) -> u32 {
        self.get(45)
    }

    pub fn supply_cost(self) -> u32 {
        self.get(46)
    }

    pub fn placement(self) -> PlacementBox {
        unsafe {
            let dat = Self::global().arrays.load(Ordering::Relaxed);
            let dat = &*dat.add(36);
            assert!(dat.entries > u32::from(self.0));
            *(dat.data as *const PlacementBox).add(self.0 as usize)
        }
    }

    pub fn addon_position(self) -> PlacementBox {
        unsafe {
            let dat = Self::global().arrays.load(Ordering::Relaxed);
            let dat = &*dat.add(37);
            let index = if (*dat).entries == 0xc0 {
                // Default BW
                self.0 - 106
            } else {
                // Extended dat, assume starting from 0
                self.0
            };
            assert!(dat.entries > u32::from(index));
            *(dat.data as *const PlacementBox).add(index as usize)
        }
    }

    pub fn cargo_space_used(self) -> u32 {
        self.get(47)
    }

    pub fn cargo_space_provided(self) -> u32 {
        self.get(48)
    }

    pub fn dimensions(self) -> DimensionRect {
        unsafe {
            let dat = Self::global().arrays.load(Ordering::Relaxed);
            let dat = &*dat.add(38);
            assert!(dat.entries > u32::from(self.0));
            *(dat.data as *const DimensionRect).offset(self.0 as isize)
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

    pub fn misc_flags(self) -> u32 {
        self.get(53)
    }

    pub fn is_fighter(self) -> bool {
        match self {
            unit::SCARAB | unit::INTERCEPTOR => true,
            _ => false,
        }
    }

    pub fn fighter_id(self) -> Option<UnitId> {
        match self {
            unit::REAVER | unit::WARBRINGER => Some(unit::SCARAB),
            unit::CARRIER | unit::GANTRITHOR => Some(unit::INTERCEPTOR),
            _ => None,
        }
    }

    pub fn icon(self) -> u32 {
        self.get_opt(0x43)
            .unwrap_or_else(|| self.0 as u32)
    }

    pub fn death_timer(self) -> u32 {
        self.get_opt(0x4a)
            .unwrap_or_else(|| {
                match self {
                    unit::BROODLING => 1800,
                    _ => 0,
                }
            })
    }

    pub fn alternate_rank_string(self) -> u32 {
        self.get_opt(0x4b)
            .unwrap_or_else(|| {
                match self {
                    unit::GANTRITHOR => 0x23c,
                    unit::NORAD_II | unit::HYPERION => 0x23b,
                    _ => 0,
                }
            })
    }

    pub fn is_higher_tier_morphed_building(self) -> bool {
        use crate::unit::*;
        match self {
            LAIR | HIVE | GREATER_SPIRE | SUNKEN_COLONY | SPORE_COLONY => true,
            _ => false,
        }
    }

    pub fn matches_id(self, other: UnitId) -> bool {
        self == other || match other {
            unit::GROUP_MEN | unit::GROUP_BUILDINGS | unit::GROUP_FACTORIES => {
                let flags = self.group_flags();
                let mask = match other {
                    unit::GROUP_MEN => 0x8,
                    unit::GROUP_BUILDINGS => 0x10,
                    unit::GROUP_FACTORIES | _ => 0x20,
                };
                flags & mask == mask
            }
            x => x == unit::ANY_UNIT,
        }
    }
}

impl FlingyId {
    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&DAT_GLOBALS[DatType::Flingy as usize], self.0 as u32, id) }
    }

    pub fn sprite(self) -> SpriteId {
        SpriteId(self.get(0x00) as u16)
    }

    pub fn top_speed(self) -> u32 {
        self.get(0x01)
    }

    pub fn acceleration(self) -> u32 {
        self.get(0x02)
    }

    pub fn turn_speed(self) -> u32 {
        self.get(0x04)
    }

    pub fn movement_type(self) -> u32 {
        self.get(0x06)
    }
}

impl SpriteId {
    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&DAT_GLOBALS[DatType::Sprites as usize], self.0 as u32, id) }
    }

    pub fn image(self) -> ImageId {
        ImageId(self.get(0x00) as u16)
    }

    pub fn health_bar_width(self) -> u32 {
        self.get(0x01)
    }

    pub fn start_as_visible(self) -> bool {
        self.get(0x03) != 0
    }

    pub fn selection_circle(self) -> u8 {
        self.get(0x04) as u8
    }

    pub fn selection_y(self) -> i8 {
        self.get(0x05) as i8
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
#[repr(C)]
pub struct DimensionRect {
    pub left: u16,
    pub top: u16,
    pub right: u16,
    pub bottom: u16,
}

impl DimensionRect {
    pub fn width(&self) -> u16 {
        u16::try_from((self.left as u32) + (self.right as u32) + 1)
            .unwrap_or(u16::MAX)
    }

    pub fn height(&self) -> u16 {
        u16::try_from((self.top as u32) + (self.bottom as u32) + 1)
            .unwrap_or(u16::MAX)
    }
}

impl WeaponId {
    pub fn optional(id: u32) -> Option<WeaponId> {
        if id > u16::MAX as u32 || id == weapon::NONE.0 as u32 {
            None
        } else {
            Some(WeaponId(id as u16))
        }
    }

    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(&DAT_GLOBALS[DatType::Weapons as usize], self.0 as u32, id) }
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

    pub fn error_msg(&self) -> u32 {
        self.get(22)
    }

    pub fn flingy(self) -> FlingyId {
        FlingyId(self.get(1) as u16)
    }

    pub fn flags(&self) -> u32 {
        self.get(3)
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

    pub fn behaviour(&self) -> u8 {
        self.get(8) as u8
    }

    pub fn death_time(&self) -> u32 {
        self.get(9)
    }

    pub fn effect(&self) -> u8 {
        self.get(10) as u8
    }

    pub fn inner_splash(&self) -> u32 {
        self.get(11)
    }

    pub fn middle_splash(&self) -> u32 {
        self.get(12)
    }

    pub fn outer_splash(&self) -> u32 {
        self.get(13)
    }

    pub fn attack_angle(&self) -> u8 {
        self.get(18) as u8
    }

    pub fn launch_spin(&self) -> u8 {
        self.get(19) as u8
    }

    pub fn x_offset(&self) -> u8 {
        self.get(20) as u8
    }

    pub fn y_offset(&self) -> u8 {
        self.get(21) as u8
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
        if id > u16::MAX as u32 || id == upgrade::NONE.0 as u32 {
            None
        } else {
            Some(UpgradeId(id as u16))
        }
    }

    fn global() -> &'static DatGlobal {
        &DAT_GLOBALS[DatType::Upgrades as usize]
    }

    pub fn entry_amount() -> u32 {
        unsafe {
            let dat = Self::global().arrays.load(Ordering::Relaxed);
            if dat.is_null() {
                u32::MAX
            } else {
                (*dat).entries as u32
            }
        }
    }

    pub fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(Self::global(), self.0 as u32, id) }
    }

    pub fn label(&self) -> u32 {
        self.get(8)
    }

    pub fn mineral_cost(&self) -> u32 {
        FAST_UPGRADES_MINERAL_COST.get(self.0 as u32)
    }

    pub fn gas_cost(&self) -> u32 {
        FAST_UPGRADES_GAS_COST.get(self.0 as u32)
    }

    pub fn time(&self) -> u32 {
        FAST_UPGRADES_TIME.get(self.0 as u32)
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
        if id > u16::MAX as u32 || id == tech::NONE.0 as u32 {
            None
        } else {
            Some(TechId(id as u16))
        }
    }

    fn global() -> &'static DatGlobal {
        &DAT_GLOBALS[DatType::TechData as usize]
    }

    pub fn entry_amount() -> u32 {
        unsafe {
            let dat = Self::global().arrays.load(Ordering::Relaxed);
            if dat.is_null() {
                u32::MAX
            } else {
                (*dat).entries as u32
            }
        }
    }

    fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(Self::global(), self.0 as u32, id) }
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

    /// Displayed value, not multiplied by 256
    /// Maybe should change this..
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

    fn global() -> &'static DatGlobal {
        &DAT_GLOBALS[DatType::Orders as usize]
    }

    fn get(&self, id: u32) -> u32 {
        unsafe { crate::dat_read(Self::global(), self.0 as u32, id) }
    }

    pub fn tech(&self) -> Option<TechId> {
        TechId::optional(self.get(14))
    }

    pub fn weapon(&self) -> Option<WeaponId> {
        WeaponId::optional(self.get(13))
    }

    pub fn label(&self) -> u32 {
        self.get(0)
    }

    pub fn use_weapon_targeting(&self) -> bool {
        self.get(1) != 0
    }

    pub fn subunit_inheritance(&self) -> bool {
        self.get(4) != 0
    }

    pub fn interruptable(&self) -> bool {
        self.get(6) != 0
    }

    pub fn deaccelerate_to_waypoint(&self) -> bool {
        self.get(7) != 0
    }

    pub fn queuable(&self) -> bool {
        self.get(8) != 0
    }

    pub fn keep_target_when_disabled(&self) -> bool {
        self.get(9) != 0
    }

    pub fn terrain_clip(&self) -> bool {
        self.get(10) != 0
    }

    pub fn fleeable(&self) -> bool {
        self.get(11) != 0
    }

    pub fn animation(&self) -> u8 {
        self.get(15) as u8
    }

    pub fn icon(&self) -> u32 {
        self.get(16)
    }

    pub fn obscured(&self) -> OrderId {
        OrderId(self.get(18) as u8)
    }
}

impl ButtonSetId {
    fn global() -> &'static DatGlobal {
        &DAT_GLOBALS[DatType::Buttons as usize]
    }

    pub fn get(self, id: u32) -> u32 {
        unsafe { crate::dat_read(Self::global(), self.0 as u32, id) }
    }

    pub fn get_opt(self, id: u32) -> Option<u32> {
        unsafe { crate::dat_read_opt(Self::global(), self.0 as u32, id) }
    }
}
