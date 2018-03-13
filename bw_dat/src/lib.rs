extern crate libc;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate whack;

mod bw;

pub use bw::DatTable;

pub unsafe fn init_1161(patcher: &mut whack::ModulePatcher) {
    bw::init_vars(patcher);
}

// Split like this since patcheer is lazy and won't apply the var inits immediatly..
pub unsafe fn init_1161_post() {
    init_units(&bw::units_dat[0]);
    init_weapons(&bw::weapons_dat[0]);
    init_flingy(&bw::flingy_dat[0]);
    init_upgrades(&bw::upgrades_dat[0]);
    init_techdata(&bw::techdata_dat[0]);
    init_orders(&bw::orders_dat[0]);
    init_sprites(&bw::sprites_dat[0]);
    init_images(&bw::images_dat[0]);
    init_sfxdata(&bw::sfxdata_dat[0]);
    init_portdata(&bw::portdata_dat[0]);
}

macro_rules! init_fns {
    ($($fn_name:ident, $global:ident,)*) => {
        $(
            static mut $global: *const bw::DatTable = 0 as *const bw::DatTable;
        )*
        $(
            pub unsafe fn $fn_name(dat: *const DatTable) {
                $global = dat;
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct OrderId(pub u8);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct UnitId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct WeaponId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct UpgradeId(pub u16);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct TechId(pub u16);

unsafe fn get(dat: *const bw::DatTable, id: u32, field: u32) -> u32 {
    let dat = &*dat.offset(field as isize);
    assert!(dat.entries > id);
    match dat.entry_size {
        1 => *(dat.data as *const u8).offset(id as isize) as u32,
        2 => *(dat.data as *const u16).offset(id as isize) as u32,
        4 => *(dat.data as *const u32).offset(id as isize),
        x => panic!("Invalid dat entry size: {}", x),
    }
}

pub mod unit {
    use super::UnitId;
    pub const COCOON: UnitId = UnitId(0x3b);
    pub const LURKER_EGG: UnitId = UnitId(0x61);
    pub const COMMAND_CENTER: UnitId = UnitId(0x6a);
    pub const NUCLEAR_SILO: UnitId = UnitId(0x6c);
    pub const PYLON: UnitId = UnitId(0x9c);
    pub const NONE: UnitId = UnitId(0xe4);
    pub const ANY_UNIT: UnitId = UnitId(0xe5);
    pub const GROUP_MEN: UnitId = UnitId(0xe6);
    pub const GROUP_BUILDINGS: UnitId = UnitId(0xe7);
    pub const GROUP_FACTORIES: UnitId = UnitId(0xe8);
}

pub mod weapon {
    use super::WeaponId;
    pub const NONE: WeaponId = WeaponId(0x82);
}

pub mod upgrade {
    use super::UpgradeId;
    pub const NONE: UpgradeId = UpgradeId(0x3d);
}

pub mod tech {
    use super::TechId;
    pub const NONE: TechId = TechId(0x2c);
}

pub mod order {
    use super::OrderId;
    pub const DIE: OrderId = OrderId(0x0);
    pub const MOVE: OrderId = OrderId(0x6);
    pub const ATTACK: OrderId = OrderId(0x8);
    pub const ATTACK_OBSCURED: OrderId = OrderId(0x9);
    pub const ATTACK_UNIT: OrderId = OrderId(0xa);
    pub const ATTACK_FIXED_RANGE: OrderId = OrderId(0xb);
    pub const ATTACK_MOVE: OrderId = OrderId(0xe);
    pub const TOWER_ATTACK: OrderId = OrderId(0x13);
    pub const SUBUNIT_ATTACK: OrderId = OrderId(0x16);
    pub const DRONE_BUILD: OrderId = OrderId(0x19);
    pub const SCV_BUILD: OrderId = OrderId(0x1e);
    pub const PROBE_BUILD: OrderId = OrderId(0x1f);
    pub const PLACE_ADDON: OrderId = OrderId(0x24);
    pub const BUILD_ADDON: OrderId = OrderId(0x25);
    pub const TRAIN: OrderId = OrderId(0x26);
    pub const RALLY_UNIT: OrderId = OrderId(0x27);
    pub const RALLY_POS: OrderId = OrderId(0x28);
    pub const ZERG_BIRTH: OrderId = OrderId(0x29);
    pub const BUILD_NYDUS_EXIT: OrderId = OrderId(0x2e);
    pub const UNIT_MORPH: OrderId = OrderId(0x2a);
    pub const BUILDING_MORPH: OrderId = OrderId(0x2b);
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
    pub const SPAWNING_LARVA: OrderId = OrderId(0x4e);
    pub const HARVEST_GAS: OrderId = OrderId(0x53);
    pub const RETURN_GAS: OrderId = OrderId(0x54);
    pub const RETURN_MINERALS: OrderId = OrderId(0x5a);
    pub const SPREAD_CREEP: OrderId = OrderId(0x66);
    pub const CLOAK: OrderId = OrderId(0x6d);
    pub const DECLOAK: OrderId = OrderId(0x6e);
    pub const CLOAKING_NEARBY_UNITS: OrderId = OrderId(0x83);
    pub const SAP_UNIT: OrderId = OrderId(0x86);
    pub const SAP_LOCATION: OrderId = OrderId(0x87);
    pub const HALLUCINATED: OrderId = OrderId(0x95);
    pub const RESET_COLLISION_HARVESTER: OrderId = OrderId(0x97);
    pub const AI_ATTACK_MOVE: OrderId = OrderId(0x9d);
    pub const REVEAL_TRAP: OrderId = OrderId(0xab);
}

impl UnitId {
    pub fn get(&self, id: u32) -> u32 {
        unsafe {
            ::get(UNITS_DAT, self.0 as u32, id)
        }
    }

    pub fn hitpoints(&self) -> i32 {
        self.get(8) as i32
    }

    pub fn shields(&self) -> i32 {
        // Yeah, it is stored as displayed
        self.get(7) as i32 * 256
    }

    pub fn has_shields(&self) -> bool {
        self.get(6) != 0
    }

    pub fn ground_weapon(&self) -> Option<WeaponId> {
        ::WeaponId::optional(self.get(17))
    }

    pub fn air_weapon(&self) -> Option<WeaponId> {
        ::WeaponId::optional(self.get(19))
    }

    pub fn flags(&self) -> u32 {
        self.get(22)
    }

    pub fn group_flags(&self) -> u32 {
        self.get(44)
    }

    pub fn armor(&self) -> u32 {
        self.get(27)
    }

    pub fn armor_upgrade(&self) -> Option<UpgradeId> {
        ::UpgradeId::optional(self.get(25))
    }

    pub fn mineral_cost(&self) -> u32 {
        self.get(40)
    }

    pub fn gas_cost(&self) -> u32 {
        self.get(41)
    }

    pub fn build_time(&self) -> u32 {
        self.get(42)
    }

    pub fn supply_cost(&self) -> u32 {
        self.get(46)
    }
}

impl WeaponId {
    fn optional(id: u32) -> Option<WeaponId> {
        if id > u16::max_value() as u32 || id == weapon::NONE.0 as u32 {
            None
        } else {
            Some(WeaponId(id as u16))
        }
    }

    pub fn get(&self, id: u32) -> u32 {
        unsafe {
            ::get(WEAPONS_DAT, self.0 as u32, id)
        }
    }

    pub fn damage(&self) -> u32 {
        self.get(14)
    }

    pub fn upgrade(&self) -> Option<UpgradeId> {
        ::UpgradeId::optional(self.get(6))
    }

    pub fn bonus(&self) -> u32 {
        self.get(15)
    }

    pub fn factor(&self) -> u32{
        self.get(17)
    }

    pub fn label(&self) -> u32 {
        self.get(0)
    }

}

impl UpgradeId {
    fn optional(id: u32) -> Option<UpgradeId> {
        if id > u16::max_value() as u32 || id == upgrade::NONE.0 as u32 {
            None
        } else {
            Some(UpgradeId(id as u16))
        }
    }

    pub fn get(&self, id: u32) -> u32 {
        unsafe {
            ::get(UPGRADES_DAT, self.0 as u32, id)
        }
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
    fn optional(id: u32) -> Option<TechId> {
        if id > u16::max_value() as u32 || id == tech::NONE.0 as u32 {
            None
        } else {
            Some(TechId(id as u16))
        }
    }

    fn get(&self, id: u32) -> u32 {
        unsafe {
            ::get(TECHDATA_DAT, self.0 as u32, id)
        }
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
}

impl OrderId {
    pub fn is_secondary(&self) -> bool {
        use order::*;
        match *self {
            TRAIN | CLOAKING_NEARBY_UNITS | CLOAK | DECLOAK | BUILD_ADDON | TRAIN_FIGHTER |
                SHIELD_BATTERY | SPAWNING_LARVA | SPREAD_CREEP | HALLUCINATED => true,
            _ => false,
        }
    }

    pub fn is_attack_order(&self) -> bool {
        use order::*;
        match *self {
            ATTACK | ATTACK_OBSCURED | ATTACK_UNIT | ATTACK_FIXED_RANGE | ATTACK_MOVE |
                TOWER_ATTACK | SUBUNIT_ATTACK | CARRIER_ATTACK | CARRIER_ATTACK_OBSCURED |
                CARRIER_ATTACK_UNIT | REAVER_ATTACK | REAVER_ATTACK_OBSCURED |
                REAVER_ATTACK_UNIT | INTERCEPTOR_ATTACK | SCARAB_ATTACK | SAP_UNIT |
                SAP_LOCATION | AI_ATTACK_MOVE | REVEAL_TRAP => true,
            _ => false,
        }
    }

    fn get(&self, id: u32) -> u32 {
        unsafe {
            ::get(ORDERS_DAT, self.0 as u32, id)
        }
    }

    pub fn tech(&self) -> Option<TechId> {
        ::TechId::optional(self.get(14))
    }
}
