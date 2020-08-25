use std::mem;
use std::ptr::{NonNull};

use byteorder::{ReadBytesExt, LE};

use crate::bw;
use crate::game::Game;
use crate::tech;
use crate::upgrade;
use crate::sprite::Sprite;
use crate::{UnitId, TechId, OrderId, UpgradeId, RaceFlags};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Unit(NonNull<bw::Unit>);

#[derive(Copy, Clone, Debug)]
pub struct UnitArray {
    start: *mut bw::Unit,
    length: usize,
}

impl std::ops::Deref for Unit {
    type Target = *mut bw::Unit;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Unit {
    pub unsafe fn from_ptr(ptr: *mut bw::Unit) -> Option<Unit> {
        NonNull::new(ptr).map(Unit)
    }

    pub fn sprite(self) -> Option<Sprite> {
        unsafe { Sprite::from_ptr((**self).sprite) }
    }

    pub fn player(self) -> u8 {
        unsafe { (**self).player }
    }

    pub fn id(self) -> UnitId {
        UnitId(unsafe { (**self).unit_id })
    }

    /// Morphing buildings show the name/graphics of incomplete building
    pub fn building_morph_displayed_id(self) -> UnitId {
        if !self.is_completed() {
            if let Some(dest_id) = self.first_queued_unit() {
                return match dest_id {
                    LAIR | HIVE | GREATER_SPIRE | SUNKEN_COLONY | SPORE_COLONY => dest_id,
                    _ => self.id(),
                }
            }
        }
        self.id()
    }

    pub fn position(self) -> bw::Point {
        unsafe { (**self).position }
    }

    pub fn order(self) -> OrderId {
        OrderId(unsafe { (**self).order })
    }

    pub fn order_state(self) -> u8 {
        unsafe { (**self).order_state }
    }

    pub fn secondary_order(self) -> OrderId {
        OrderId(unsafe { (**self).secondary_order })
    }

    pub fn hitpoints(self) -> i32 {
        unsafe { (**self).hitpoints }
    }

    pub fn hp_percent(self) -> i32 {
        self.hitpoints().saturating_mul(100)
            .checked_div(self.id().hitpoints())
            .unwrap_or(100)
    }

    pub fn armor(self, game: Game) -> u32 {
        let id = self.id();
        let mut val = id.armor();
        if let Some(upgrade) = id.armor_upgrade() {
            val = val.saturating_add(game.upgrade_level(self.player(), upgrade) as u32);
        }
        if id == TORRASQUE || (id == ULTRALISK &&
            game.upgrade_level(self.player(), upgrade::CHITINOUS_PLATING) > 0
        ) {
            val = val.saturating_add(2);
        }
        val
    }

    pub fn is_transport(self, game: Game) -> bool {
        let upgrade = upgrade::VENTRAL_SACS;
        if self.id() == OVERLORD && game.upgrade_level(self.player(), upgrade) == 0 {
            false
        } else {
            self.id().cargo_space_provided() > 0 && !self.is_hallucination()
        }
    }

    pub fn can_load_unit(self, game: Game, units: &UnitArray, target: Unit) -> bool {
        if !self.is_transport(game) || self.is_disabled() {
            return false;
        }
        let space_needed = target.id().cargo_space_used();
        if self.id().is_building() {
            if !target.id().races().intersects(RaceFlags::TERRAN) || space_needed != 1 {
                return false;
            }
        }
        let used_space = self.loaded_units(units)
            .map(|unit| unit.id().cargo_space_used() as u32)
            .sum::<u32>();
        used_space + space_needed as u32 <= self.id().cargo_space_provided()
    }

    pub fn loaded_units(self, units: &UnitArray) -> impl Iterator<Item = Unit> {
        let high_bits = match units.len() > 1700 {
            true => unsafe { (**self).scr_carried_unit_high_bits },
            false => 0,
        };
        let units = units.clone();
        (0usize..8).filter_map(move |i| {
            let id = unsafe {
                let low = (**self).loaded_units[i] as u32;
                let high = (high_bits >> (i * 2)) as u32 & 0x3;
                low | (high << 16)
            };
            units.get_by_unique_id(id)
        })
    }

    pub fn has_nuke(self) -> bool {
        unsafe {
            let nuke = Unit::from_ptr(
                (&(**self).unit_specific2[..]).read_u32::<LE>().unwrap() as *mut bw::Unit
            );
            match nuke {
                Some(n) => n.is_completed(),
                None => false,
            }
        }
    }

    pub fn shields(self) -> i32 {
        if self.id().has_shields() {
            unsafe { (**self).shields }
        } else {
            0
        }
    }

    pub fn health(self) -> i32 {
        self.hitpoints().saturating_add(self.shields())
    }

    pub fn energy(self) -> u16 {
        unsafe { (**self).energy }
    }

    pub fn flags(self) -> u32 {
        unsafe { (**self).flags  }
    }

    pub fn is_air(self) -> bool {
        unsafe { (**self).flags & 0x4 != 0 }
    }

    pub fn in_bunker(self) -> bool {
        unsafe { (**self).flags & 0x20 != 0 }
    }

    /// Is the unit cloaked or burrowed (So it requires detection)
    pub fn is_invisible(self) -> bool {
        unsafe { (**self).flags & 0x300 != 0 }
    }

    pub fn is_invincible(self) -> bool {
        unsafe { (**self).flags & 0x04000000 != 0 }
    }

    pub fn target(self) -> Option<Unit> {
        unsafe { Unit::from_ptr((**self).target) }
    }

    pub fn matches_id(self, other: UnitId) -> bool {
        let id = self.id();
        match other {
            ANY_UNIT => true,
            GROUP_MEN => id.group_flags() & 0x8 != 0,
            GROUP_BUILDINGS => id.group_flags() & 0x10 != 0,
            GROUP_FACTORIES => id.group_flags() & 0x20 != 0,
            other => id == other,
        }
    }

    pub fn collision_rect(self) -> bw::Rect {
        let collision_rect = self.id().dimensions();
        let position = self.position();
        bw::Rect {
            left: (position.x - collision_rect.left).max(0),
            right: position.x + collision_rect.right + 1,
            top: (position.y - collision_rect.top).max(0),
            bottom: position.y + collision_rect.bottom + 1,
        }
    }

    pub fn is_completed(self) -> bool {
        unsafe { (**self).flags & 0x1 != 0 }
    }

    pub fn is_landed_building(self) -> bool {
        unsafe { (**self).flags & 0x2 != 0 }
    }

    pub fn is_burrowed(self) -> bool {
        unsafe { (**self).flags & 0x10 != 0 }
    }

    pub fn is_under_dweb(self) -> bool {
        unsafe { (**self).flags & 0x8000 != 0 }
    }

    pub fn has_free_cloak(self) -> bool {
        unsafe { (**self).flags & 0x800 != 0 }
    }

    pub fn is_hallucination(self) -> bool {
        unsafe { (**self).flags & 0x4000_0000 != 0 }
    }

    pub fn subunit_linked(self) -> Option<Unit> {
        unsafe { Unit::from_ptr((**self).subunit) }
    }

    /// Returns the turret unit if unit has it, otherwise itself
    pub fn subunit_turret(self) -> Unit {
        if let Some(subunit) = self.subunit_linked() {
            if subunit.id().is_subunit() {
                return subunit;
            }
        }
        self
    }

    /// Returns the turret parent if unit is a turret, otherwise itself
    pub fn subunit_parent(self) -> Unit {
        if self.id().is_subunit() {
            self.subunit_linked().unwrap()
        } else {
            self
        }
    }

    pub fn addon(self) -> Option<Unit> {
        unsafe {
            if self.id().is_building() {
                let ptr = (&(**self).unit_specific[..]).read_u32::<LE>().unwrap() as *mut bw::Unit;
                Unit::from_ptr(ptr)
            } else {
                None
            }
        }
    }

    pub fn tech_in_progress(self) -> Option<TechId> {
        unsafe {
            if self.id().is_building() {
                TechId::optional((**self).unit_specific[0x8].into())
            } else {
                None
            }
        }
    }

    pub fn upgrade_in_progress(self) -> Option<UpgradeId> {
        unsafe {
            if self.id().is_building() {
                UpgradeId::optional((**self).unit_specific[0x9].into())
            } else {
                None
            }
        }
    }

    pub fn cargo_count(self) -> u8 {
        unsafe { (**self).loaded_units.iter().filter(|&&x| x != 0).count() as u8 }
    }

    pub fn is_disabled(self) -> bool {
        unsafe {
            (**self).lockdown_timer != 0 ||
                (**self).stasis_timer != 0 ||
                (**self).maelstrom_timer != 0 ||
                (**self).flags & 0x400 != 0
        }
    }

    pub fn is_blind(self) -> bool {
        unsafe { (**self).is_blind != 0 }
    }

    pub fn is_parasited(self) -> bool {
        unsafe { (**self).parasited_by_players != 0 }
    }

    pub fn acid_spore_count(self) -> u8 {
        unsafe { (**self).acid_spore_count }
    }

    pub fn death_timer(self) -> u16 {
        unsafe { (**self).death_timer }
    }

    pub fn can_detect(self) -> bool {
        self.id().flags() & 0x8000 != 0 &&
            self.is_completed() &&
            !self.is_blind() &&
            !self.is_disabled()
    }

    pub fn powerup(self) -> Option<Unit> {
        if self.id().is_worker() {
            unsafe {
                let powerup =
                    (&(**self).unit_specific[..]).read_u32::<LE>().unwrap() as *mut bw::Unit;
                Unit::from_ptr(powerup)
            }
        } else {
            None
        }
    }

    pub fn mine_amount(self, game: Game) -> u8 {
        let id = self.id();
        if id == VULTURE || id == JIM_RAYNOR_VULTURE {
            if id.is_hero() || game.tech_researched(self.player(), tech::SPIDER_MINES) {
                unsafe { (**self).unit_specific[0] }
            } else {
                0
            }
        } else {
            0
        }
    }

    pub fn uses_fighters(self) -> bool {
        match self.id() {
            CARRIER | GANTRITHOR | REAVER | WARBRINGER => true,
            _ => false,
        }
    }

    pub fn fighter_amount(self) -> u32 {
        if self.uses_fighters() {
            // Count fighters outside hangar if carrier
            unsafe {
                match self.id() {
                    CARRIER | GANTRITHOR => {
                        ((**self).unit_specific[8] as u32)
                            .saturating_add((**self).unit_specific[9] as u32)
                    }
                    _ => (**self).unit_specific[8] as u32,
                }
            }
        } else {
            0
        }
    }

    pub fn hangar_cap(self, game: Game) -> u32 {
        match self.id() {
            CARRIER | GANTRITHOR => {
                let upgrade = upgrade::CARRIER_CAPACITY;
                if self.id().is_hero() || game.upgrade_level(self.player(), upgrade) > 0 {
                    8
                } else {
                    4
                }
            }
            REAVER | WARBRINGER => {
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

    pub fn is_hidden(self) -> bool {
        self.sprite().map(|s| s.is_hidden()).unwrap_or(true)
    }

    pub fn empty_build_slot(self) -> Option<u8> {
        unsafe {
            let mut pos = (**self).current_build_slot as usize;
            for _ in 0..5 {
                if pos == 5 {
                    pos = 0;
                }
                if (**self).build_queue[pos] == NONE.0 {
                    return Some(pos as u8);
                }
                pos += 1;
            }
            None
        }
    }

    pub fn currently_building(self) -> Option<Unit> {
        unsafe { Unit::from_ptr((**self).currently_building) }
    }

    pub fn is_building_addon(self) -> bool {
        if let Some(currently_building) = self.currently_building() {
            self.secondary_order() == crate::order::BUILD_ADDON &&
                self.is_landed_building() &&
                !currently_building.is_completed()
        } else {
            false
        }
    }

    pub fn first_queued_unit(self) -> Option<UnitId> {
        let index = unsafe { (**self).current_build_slot as usize };
        let current_build_unit = unsafe { UnitId((**self).build_queue[index]) };
        if current_build_unit == NONE {
            None
        } else {
            Some(current_build_unit)
        }
    }

    pub fn nth_queued_unit(self, slot: u8) -> Option<UnitId> {
        let index = unsafe { ((**self).current_build_slot as usize + slot as usize) % 5 };
        let current_build_unit = unsafe { UnitId((**self).build_queue[index]) };
        if current_build_unit == NONE {
            None
        } else {
            Some(current_build_unit)
        }
    }

    pub fn is_constructing_building(self) -> bool {
        let current_build_unit =
            unsafe { UnitId((**self).build_queue[(**self).current_build_slot as usize]) };
        current_build_unit != NONE && current_build_unit.is_building()
    }

    pub fn resource_amount(self) -> u16 {
        unsafe { (&(**self).unit_specific2[0..]).read_u16::<LE>().unwrap() }
    }

    pub fn rank(self) -> u8 {
        unsafe { self.id().rank().saturating_add((**self).rank) }
    }

    pub fn kills(self) -> u32 {
        unsafe { (**self).kills as u32 }
    }

    pub fn has_ai(self) -> bool {
        unsafe { !(**self).ai.is_null() }
    }

    pub fn is_enemy(self, game: Game, target: Unit) -> bool {
        let target_player = if target.player() == 11 {
            match target.sprite() {
                Some(s) => s.player(),
                _ => return false,
            }
        } else {
            target.player()
        };
        !game.allied(self.player(), target_player)
    }

    pub fn related(self) -> Option<Unit> {
        unsafe { Unit::from_ptr((**self).related) }
    }

    pub fn is_invisible_hidden_to(self, player: u8) -> bool {
        let mask = 1 << player;
        self.is_invisible() && unsafe { (**self).detection_status & mask == 0 }
    }

    /// Checks sprite visibility, not cloak/burrow
    pub fn is_visible_to(self, player: u8) -> bool {
        if player < 8 {
            let mask = 1 << player;
            if let Some(sprite) = self.sprite() {
                sprite.visibility_mask() & mask != 0
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn halt_distance(self) -> u32 {
        unsafe {
            let speed = ((**self).next_speed).max(0) as u32;
            if speed == 0 || (**self).flingy_movement_type != 0 {
                0
            } else {
                speed.saturating_mul(speed)
                    .checked_div((**self).acceleration as u32 * 2)
                    .unwrap_or(0)
                    .max(0)
            }
        }
    }

    pub fn is_carrying_powerup(self) -> bool {
        unsafe {
            (**self).carried_powerup_flags != 0 && (**self).carried_powerup_flags & 3 == 0
        }
    }

    pub fn is_carrying_minerals(self) -> bool {
        unsafe {
            (**self).carried_powerup_flags & 0x2 != 0
        }
    }

    pub fn is_carrying_gas(self) -> bool {
        unsafe {
            (**self).carried_powerup_flags & 0x1 != 0
        }
    }
}

impl UnitArray {
    pub unsafe fn new(start: *mut bw::Unit, length: usize) -> UnitArray {
        UnitArray {
            start,
            length,
        }
    }

    pub fn ptr(&self) -> *mut bw::Unit {
        self.start
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn get_by_unique_id(&self, id: u32) -> Option<Unit> {
        let long_id = self.length > 1700;
        let (index, minor) = if long_id {
            ((id & 0x1fff) as usize, (id >> 0xd) as u8)
        } else {
            ((id & 0x7ff) as usize, (id >> 0xb) as u8)
        };
        if index > self.length || index == 0 {
            return None;
        }
        let unit = unsafe { Unit::from_ptr(self.start.add(index - 1))? };
        if unsafe { (**unit).minor_unique_index == minor } {
            Some(unit)
        } else {
            None
        }
    }

    pub fn to_unique_id_opt(&self, unit: Option<Unit>) -> u32 {
        unit.map(|x| self.to_unique_id(x)).unwrap_or(0)
    }

    pub fn to_unique_id(&self, unit: Unit) -> u32 {
        unsafe {
            let long_id = self.length > 1700;
            let index =
                ((*unit as usize - self.start as usize) / mem::size_of::<bw::Unit>()) as u32;
            if long_id {
                assert!(index < (1 << 0xd));
                (index + 1) | (((**unit).minor_unique_index as u32) << 0xd)
            } else {
                assert!(index < (1 << 0xb));
                (index + 1) | (((**unit).minor_unique_index as u32) << 0xb)
            }
        }
    }
}

unsafe impl Send for Unit {}
unsafe impl Sync for Unit {}

pub const MARINE: UnitId = UnitId(0x0);
pub const GHOST: UnitId = UnitId(0x1);
pub const VULTURE: UnitId = UnitId(0x2);
pub const GOLIATH: UnitId = UnitId(0x3);
pub const GOLIATH_TURRET: UnitId = UnitId(0x4);
pub const SIEGE_TANK_TANK: UnitId = UnitId(0x5);
pub const SIEGE_TANK_TURRET: UnitId = UnitId(0x6);
pub const SCV: UnitId = UnitId(0x7);
pub const WRAITH: UnitId = UnitId(0x7);
pub const GUI_MONTAG: UnitId = UnitId(0xa);
pub const SPIDER_MINE: UnitId = UnitId(0xd);
pub const NUCLEAR_MISSILE: UnitId = UnitId(0xe);
pub const CIVILIAN: UnitId = UnitId(0xf);
pub const SARAH_KERRIGAN: UnitId = UnitId(0x10);
pub const ALAN_SCHEZAR: UnitId = UnitId(0x11);
pub const SCHEZAR_TURRET: UnitId = UnitId(0x12);
pub const JIM_RAYNOR_VULTURE: UnitId = UnitId(0x13);
pub const JIM_RAYNOR_MARINE: UnitId = UnitId(0x14);
pub const TOM_KAZANSKY: UnitId = UnitId(0x15);
pub const EDMUND_DUKE_TANK: UnitId = UnitId(0x17);
pub const EDMUND_DUKE_SIEGE: UnitId = UnitId(0x19);
pub const HYPERION: UnitId = UnitId(0x19);
pub const NORAD_II: UnitId = UnitId(0x1a);
pub const SIEGE_TANK_SIEGE: UnitId = UnitId(0x1e);
pub const FIREBAT: UnitId = UnitId(0x20);
pub const MEDIC: UnitId = UnitId(0x22);
pub const LARVA: UnitId = UnitId(0x23);
pub const EGG: UnitId = UnitId(0x24);
pub const ZERGLING: UnitId = UnitId(0x25);
pub const HYDRALISK: UnitId = UnitId(0x26);
pub const ULTRALISK: UnitId = UnitId(0x27);
pub const BROODLING: UnitId = UnitId(0x28);
pub const DRONE: UnitId = UnitId(0x29);
pub const OVERLORD: UnitId = UnitId(0x2a);
pub const MUTALISK: UnitId = UnitId(0x2b);
pub const GUARDIAN: UnitId = UnitId(0x2c);
pub const QUEEN: UnitId = UnitId(0x2d);
pub const SCOURGE: UnitId = UnitId(0x2f);
pub const TORRASQUE: UnitId = UnitId(0x30);
pub const MATRIARCH: UnitId = UnitId(0x31);
pub const INFESTED_TERRAN: UnitId = UnitId(0x32);
pub const INFESTED_KERRIGAN: UnitId = UnitId(0x33);
pub const COCOON: UnitId = UnitId(0x3b);
pub const DARK_TEMPLAR: UnitId = UnitId(0x3d);
pub const DEVOURER: UnitId = UnitId(0x3e);
pub const DARK_ARCHON: UnitId = UnitId(0x3f);
pub const DRAGOON: UnitId = UnitId(0x42);
pub const HIGH_TEMPLAR: UnitId = UnitId(0x43);
pub const ARCHON: UnitId = UnitId(0x44);
pub const ARBITER: UnitId = UnitId(0x47);
pub const CARRIER: UnitId = UnitId(0x48);
pub const INTERCEPTOR: UnitId = UnitId(0x49);
pub const FENIX_DRAGOON: UnitId = UnitId(0x4e);
pub const WARBRINGER: UnitId = UnitId(0x51);
pub const GANTRITHOR: UnitId = UnitId(0x52);
pub const REAVER: UnitId = UnitId(0x53);
pub const SCARAB: UnitId = UnitId(0x55);
pub const LURKER_EGG: UnitId = UnitId(0x61);
pub const SAMIR_DURAN: UnitId = UnitId(0x63);
pub const ALEXEI_STUKOV: UnitId = UnitId(0x64);
pub const LURKER: UnitId = UnitId(0x67);
pub const INFESTED_DURAN: UnitId = UnitId(0x68);
pub const DISRUPTION_WEB: UnitId = UnitId(0x69);
pub const COMMAND_CENTER: UnitId = UnitId(0x6a);
pub const NUCLEAR_SILO: UnitId = UnitId(0x6c);
pub const SUPPLY_DEPOT: UnitId = UnitId(0x6d);
pub const REFINERY: UnitId = UnitId(0x6e);
pub const MISSILE_TURRET: UnitId = UnitId(0x7c);
pub const BUNKER: UnitId = UnitId(0x7d);
pub const HATCHERY: UnitId = UnitId(0x83);
pub const LAIR: UnitId = UnitId(0x84);
pub const HIVE: UnitId = UnitId(0x85);
pub const HYDRALISK_DEN: UnitId = UnitId(0x87);
pub const GREATER_SPIRE: UnitId = UnitId(0x89);
pub const SPIRE: UnitId = UnitId(0x8d);
pub const CREEP_COLONY: UnitId = UnitId(0x8f);
pub const SPORE_COLONY: UnitId = UnitId(0x90);
pub const SUNKEN_COLONY: UnitId = UnitId(0x92);
pub const EXTRACTOR: UnitId = UnitId(0x95);
pub const PYLON: UnitId = UnitId(0x9c);
pub const ASSIMILATOR: UnitId = UnitId(0x9d);
pub const GATEWAY: UnitId = UnitId(0xa0);
pub const PHOTON_CANNON: UnitId = UnitId(0xa2);
pub const STARGATE: UnitId = UnitId(0xa7);
pub const MINERAL_FIELD_1: UnitId = UnitId(0xb0);
pub const MINERAL_FIELD_2: UnitId = UnitId(0xb1);
pub const MINERAL_FIELD_3: UnitId = UnitId(0xb2);
pub const VESPENE_GEYSER: UnitId = UnitId(0xbc);
pub const ZERG_BEACON: UnitId = UnitId(0xc2);
pub const TERRAN_BEACON: UnitId = UnitId(0xc3);
pub const PROTOSS_BEACON: UnitId = UnitId(0xc4);
pub const ZERG_FLAG_BEACON: UnitId = UnitId(0xc5);
pub const TERRAN_FLAG_BEACON: UnitId = UnitId(0xc6);
pub const PROTOSS_FLAG_BEACON: UnitId = UnitId(0xc7);
pub const DARK_SWARM: UnitId = UnitId(0xca);
pub const LEFT_UPPER_LEVEL_DOOR: UnitId = UnitId(0xcd);
pub const RIGHT_UPPER_LEVEL_DOOR: UnitId = UnitId(0xce);
pub const LEFT_PIT_DOOR: UnitId = UnitId(0xcf);
pub const RIGHT_PIT_DOOR: UnitId = UnitId(0xd0);
pub const NONE: UnitId = UnitId(0xe4);
pub const ANY_UNIT: UnitId = UnitId(0xe5);
pub const GROUP_MEN: UnitId = UnitId(0xe6);
pub const GROUP_BUILDINGS: UnitId = UnitId(0xe7);
pub const GROUP_FACTORIES: UnitId = UnitId(0xe8);
