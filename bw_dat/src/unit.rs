use std::mem;
use std::ptr::{NonNull};

use crate::bw;
use crate::game::Game;
use crate::sprite::Sprite;
use crate::{UnitId, TechId, OrderId, UpgradeId, RaceFlags, order, tech, upgrade};

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
        unsafe { Sprite::from_ptr((**self).flingy.sprite) }
    }

    pub fn player(self) -> u8 {
        unsafe { (**self).player }
    }

    pub fn id(self) -> UnitId {
        UnitId(unsafe { (**self).unit_id })
    }

    /// If the unit is dying, that is, no longer in active units list.
    /// Unit receives order::DIE as soon as its hp hits 0 ("is killed"), but
    /// "is dying" only after the die order has been processed. Though often
    /// just checking for order::DIE can be enough too.
    pub fn is_dying(self) -> bool {
        self.order() == order::DIE && self.order_state() == 1
    }

    /// Morphing buildings show the name/graphics of incomplete building
    pub fn building_morph_displayed_id(self) -> UnitId {
        if !self.is_completed() {
            if let Some(dest_id) = self.first_queued_unit() {
                return match dest_id.is_higher_tier_morphed_building() {
                    true => dest_id,
                    false => self.id(),
                };
            }
        }
        self.id()
    }

    pub fn is_morphing_building(self) -> bool {
        if !self.is_completed() {
            if let Some(dest_id) = self.first_queued_unit() {
                return match dest_id.is_higher_tier_morphed_building() {
                    true => true,
                    false => false,
                };
            }
        }
        false
    }

    pub fn is_completed_or_morphing_to_higher_tier(self) -> bool {
        if !self.is_completed() {
            if let Some(dest_id) = self.first_queued_unit() {
                return match dest_id.is_higher_tier_morphed_building() {
                    true => true,
                    false => false,
                };
            }
        }
        true
    }

    pub fn position(self) -> bw::Point {
        unsafe { (**self).flingy.position }
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
        unsafe { (**self).flingy.hitpoints }
    }

    pub fn hp_percent(self) -> i32 {
        self.hitpoints().saturating_mul(100)
            .checked_div(self.id().hitpoints())
            .unwrap_or(100)
    }

    pub fn hp_displayed(self) -> i32 {
        self.hitpoints().wrapping_add(0xff) >> 8
    }

    pub fn max_hp_displayed(self) -> i32 {
        let hp = self.id().hitpoints() >> 8;
        if hp != 0 {
            hp
        } else {
            let hp = self.hp_displayed();
            if hp != 0 {
                hp
            } else {
                1
            }
        }
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

    pub fn has_loaded_units(self) -> bool {
        // (unit gets removed properly from self.loaded_units even on irradiate death
        // and similar deaths inside transports)
        // Also don't have to care if high_bits are set for larger unit limit, since
        // index is fully in low bits, and index 0 means none
        unsafe { (**self).loaded_units != [0; 8] }
    }

    pub fn fighter_parent(self) -> Option<Unit> {
        unsafe {
            if self.id() == SCARAB || self.id() == INTERCEPTOR {
                Unit::from_ptr((**self).unit_specific.interceptor.parent)
            } else {
                None
            }
        }
    }

    pub fn has_nuke(self) -> bool {
        self.silo_nuke().is_some()
    }

    pub fn silo_nuke(self) -> Option<Unit> {
        unsafe {
            if self.id() == NUCLEAR_SILO {
                let nuke = Unit::from_ptr((**self).unit_specific2.nuke_silo.nuke);
                // Will a nuke even be stored here if it's not completed?
                nuke.filter(|x| x.is_completed())
            } else {
                None
            }
        }
    }

    pub fn powerup_worker(self) -> Option<Unit> {
        unsafe {
            if self.id().is_powerup() {
                Unit::from_ptr((**self).unit_specific2.powerup.worker)
            } else {
                None
            }
        }
    }

    pub fn nydus_linked(self) -> Option<Unit> {
        unsafe {
            if self.id() == NYDUS_CANAL {
                Unit::from_ptr((**self).unit_specific2.nydus.linked)
            } else {
                None
            }
        }
    }

    pub fn rally_unit(self) -> Option<Unit> {
        unsafe {
            if self.id().is_building() && self.id() != PYLON {
                Unit::from_ptr((**self).rally_pylon.rally.unit)
            } else {
                None
            }
        }
    }

    pub fn pylon_aura(self) -> Option<Sprite> {
        unsafe {
            if self.id() == PYLON {
                Sprite::from_ptr((**self).unit_specific2.pylon.aura)
            } else {
                None
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

    pub fn shields_displayed(self) -> i32 {
        self.shields() >> 8
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

    pub fn in_transport(self) -> bool {
        unsafe { (**self).flags & 0x40 != 0 }
    }

    /// Is the unit cloaked or burrowed (So it requires detection)
    pub fn is_invisible(self) -> bool {
        unsafe { (**self).flags & 0x300 != 0 }
    }

    pub fn is_invincible(self) -> bool {
        unsafe { (**self).flags & 0x04000000 != 0 }
    }

    pub fn target_pos(self) -> bw::Point {
        unsafe { (**self).order_target.pos }
    }

    pub fn target(self) -> Option<Unit> {
        unsafe { Unit::from_ptr((**self).order_target.unit) }
    }

    pub fn matches_id(self, other: UnitId) -> bool {
        self.id().matches_id(other)
    }

    pub fn collision_rect(self) -> bw::Rect {
        let collision_rect = self.id().dimensions();
        let position = self.position();
        bw::Rect {
            left: (position.x as u16).saturating_sub(collision_rect.left) as i16,
            right: position.x.saturating_add(collision_rect.right as i16).saturating_add(1),
            top: (position.y as u16).saturating_sub(collision_rect.top) as i16,
            bottom: position.y.saturating_add(collision_rect.bottom as i16).saturating_add(1),
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

    pub fn has_smart_flag(self) -> bool {
        unsafe { (**self).flags & 0x0002_0000 != 0 }
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
                Unit::from_ptr((**self).unit_specific.building.addon)
            } else {
                None
            }
        }
    }

    pub fn tech_in_progress(self) -> Option<TechId> {
        unsafe {
            if self.id().is_building() {
                TechId::optional((**self).unit_specific.building.tech.into())
            } else {
                None
            }
        }
    }

    pub fn upgrade_in_progress(self) -> Option<UpgradeId> {
        unsafe {
            if self.id().is_building() {
                UpgradeId::optional((**self).unit_specific.building.upgrade.into())
            } else {
                None
            }
        }
    }

    pub fn cargo_count(self) -> u8 {
        unsafe { (0..8).filter(|&i| (**self).loaded_units[i] != 0).count() as u8 }
    }

    pub fn is_disabled(self) -> bool {
        unsafe {
            (**self).lockdown_timer != 0 ||
                (**self).stasis_timer != 0 ||
                (**self).maelstrom_timer != 0 ||
                (**self).flags & 0x400 != 0
        }
    }

    pub fn disabled_flag(self) -> bool {
        self.flags() & 0x400 != 0
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
                Unit::from_ptr((**self).unit_specific.worker.powerup)
            }
        } else {
            None
        }
    }

    pub fn carried_resource_amount(self) -> u8 {
        if self.id().is_worker() {
            unsafe { (**self).unit_specific.worker.carried_resource_count }
        } else {
            0
        }
    }

    pub fn mine_amount(self, game: Game) -> u8 {
        let id = self.id();
        if id == VULTURE || id == JIM_RAYNOR_VULTURE {
            if id.is_hero() || game.tech_researched(self.player(), tech::SPIDER_MINES) {
                unsafe { (**self).unit_specific.vulture.mines }
            } else {
                0
            }
        } else {
            0
        }
    }

    pub fn uses_fighters(self) -> bool {
        self.id().fighter_id().is_some()
    }

    pub fn fighter_amount(self) -> u32 {
        if self.uses_fighters() {
            // Count fighters outside hangar if carrier
            unsafe {
                match self.id() {
                    CARRIER | GANTRITHOR => {
                        ((**self).unit_specific.carrier.in_hangar_count as u32)
                            .saturating_add(
                                (**self).unit_specific.carrier.out_hangar_count as u32
                            )
                    }
                    _ => (**self).unit_specific.carrier.in_hangar_count as u32
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
        unsafe { Unit::from_ptr((**self).secondary_order_target.unit) }
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
        // Note: Oddly AI SCVs can have a building in their build queue even when
        // not executing this order; checking the queue here would be wrong.
        // (Patching that could be wise though)
        self.order() == crate::order::CONSTRUCTING_BUILDING
    }

    pub fn resource_amount(self) -> u16 {
        unsafe { (**self).unit_specific2.resource.amount }
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

    fn ai_by_type<T>(self, ty: u8) -> Option<*mut T> {
        unsafe {
            if (**self).ai.is_null() {
                None
            } else {
                let ai = (**self).ai as *mut bw::GuardAi;
                if (*ai).ai_type == ty {
                    Some(ai as *mut T)
                } else {
                    None
                }
            }
        }
    }

    pub fn guard_ai(self) -> Option<*mut bw::GuardAi> {
        self.ai_by_type(1)
    }

    pub fn worker_ai(self) -> Option<*mut bw::WorkerAi> {
        self.ai_by_type(2)
    }

    pub fn building_ai(self) -> Option<*mut bw::BuildingAi> {
        self.ai_by_type(3)
    }

    pub fn military_ai(self) -> Option<*mut bw::MilitaryAi> {
        self.ai_by_type(4)
    }

    /// "Is target considered an enemy by this unit"
    pub fn is_enemy(self, game: Game, target: Unit) -> bool {
        target.is_enemy_for_player(game, self.player())
    }

    /// "Does `player` consider this unit an enemy"
    pub fn is_enemy_for_player(self, game: Game, player: u8) -> bool {
        let target_player = if self.player() == 11 {
            match self.sprite() {
                Some(s) => s.player(),
                _ => return false,
            }
        } else {
            self.player()
        };
        !game.allied(player, target_player)
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
            let speed = ((**self).flingy.next_speed).max(0) as u32;
            if speed == 0 || (**self).flingy.movement_type != 0 {
                0
            } else {
                speed.saturating_mul(speed)
                    .checked_div((**self).flingy.acceleration as u32 * 2)
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

    #[inline]
    pub fn ptr(&self) -> *mut bw::Unit {
        self.start
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    #[inline]
    pub fn get_by_index(&self, index: u32) -> Option<Unit> {
        if self.len() < index as usize {
            None
        } else {
            unsafe { Unit::from_ptr(self.ptr().add(index as usize)) }
        }
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
        // Checking for sprite prevents from accessing free units.
        // A never-initialized free unit will have is_dying() == false
        // (Order is DIE but order state is 0)
        if unit.sprite().is_some() &&
            !unit.is_dying() &&
            unsafe { (**unit).minor_unique_index == minor }
        {
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
            let index = self.to_index(unit);
            if long_id {
                assert!(index < (1 << 0xd));
                (index + 1) | (((**unit).minor_unique_index as u32) << 0xd)
            } else {
                assert!(index < (1 << 0xb));
                (index + 1) | (((**unit).minor_unique_index as u32) << 0xb)
            }
        }
    }

    #[inline]
    pub fn to_index(&self, unit: Unit) -> u32 {
        ((*unit as usize - self.start as usize) / mem::size_of::<bw::Unit>()) as u32
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
pub const WRAITH: UnitId = UnitId(0x8);
pub const SCIENCE_VESSEL: UnitId = UnitId(0x9);
pub const GUI_MONTAG: UnitId = UnitId(0xa);
pub const DROPSHIP: UnitId = UnitId(0xb);
pub const BATTLECRUISER: UnitId = UnitId(0xc);
pub const SPIDER_MINE: UnitId = UnitId(0xd);
pub const NUCLEAR_MISSILE: UnitId = UnitId(0xe);
pub const CIVILIAN: UnitId = UnitId(0xf);
pub const SARAH_KERRIGAN: UnitId = UnitId(0x10);
pub const ALAN_SCHEZAR: UnitId = UnitId(0x11);
pub const SCHEZAR_TURRET: UnitId = UnitId(0x12);
pub const JIM_RAYNOR_VULTURE: UnitId = UnitId(0x13);
pub const JIM_RAYNOR_MARINE: UnitId = UnitId(0x14);
pub const TOM_KAZANSKY: UnitId = UnitId(0x15);
pub const MAGELLAN: UnitId = UnitId(0x16);
pub const EDMUND_DUKE_TANK: UnitId = UnitId(0x17);
pub const EDMUND_DUKE_TANK_TURRET: UnitId = UnitId(0x18);
pub const EDMUND_DUKE_SIEGE: UnitId = UnitId(0x19);
pub const EDMUND_DUKE_SIEGE_TURRET: UnitId = UnitId(0x1a);
pub const ARCTURUS_MENGSK: UnitId = UnitId(0x1b);
pub const HYPERION: UnitId = UnitId(0x1c);
pub const NORAD_II: UnitId = UnitId(0x1d);
pub const SIEGE_TANK_SIEGE: UnitId = UnitId(0x1e);
pub const SIEGE_TANK_SIEGE_TURRET: UnitId = UnitId(0x1f);
pub const FIREBAT: UnitId = UnitId(0x20);
pub const SCANNER_SWEEP: UnitId = UnitId(0x21);
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
pub const DEFILER: UnitId = UnitId(0x2e);
pub const SCOURGE: UnitId = UnitId(0x2f);
pub const TORRASQUE: UnitId = UnitId(0x30);
pub const MATRIARCH: UnitId = UnitId(0x31);
pub const INFESTED_TERRAN: UnitId = UnitId(0x32);
pub const INFESTED_KERRIGAN: UnitId = UnitId(0x33);
pub const UNCLEAN_ONE: UnitId = UnitId(0x34);
pub const HUNTER_KILLER: UnitId = UnitId(0x35);
pub const DEVOURING_ONE: UnitId = UnitId(0x36);
pub const KUKULZA_MUTALISK: UnitId = UnitId(0x37);
pub const KUKULZA_GUARDIAN: UnitId = UnitId(0x38);
pub const YGGDRASILL: UnitId = UnitId(0x39);
pub const VALKYRIE: UnitId = UnitId(0x3a);
pub const COCOON: UnitId = UnitId(0x3b);
pub const CORSAIR: UnitId = UnitId(0x3c);
pub const DARK_TEMPLAR: UnitId = UnitId(0x3d);
pub const DEVOURER: UnitId = UnitId(0x3e);
pub const DARK_ARCHON: UnitId = UnitId(0x3f);
pub const PROBE: UnitId = UnitId(0x40);
pub const ZEALOT: UnitId = UnitId(0x41);
pub const DRAGOON: UnitId = UnitId(0x42);
pub const HIGH_TEMPLAR: UnitId = UnitId(0x43);
pub const ARCHON: UnitId = UnitId(0x44);
pub const SHUTTLE: UnitId = UnitId(0x45);
pub const SCOUT: UnitId = UnitId(0x46);
pub const ARBITER: UnitId = UnitId(0x47);
pub const CARRIER: UnitId = UnitId(0x48);
pub const INTERCEPTOR: UnitId = UnitId(0x49);
pub const DARK_TEMPLAR_HERO: UnitId = UnitId(0x4a);
pub const ZERATUL: UnitId = UnitId(0x4b);
pub const TASSADAR_ZERATUL: UnitId = UnitId(0x4c);
pub const FENIX_ZEALOT: UnitId = UnitId(0x4d);
pub const FENIX_DRAGOON: UnitId = UnitId(0x4e);
pub const TASSADAR: UnitId = UnitId(0x4f);
pub const MOJO: UnitId = UnitId(0x50);
pub const WARBRINGER: UnitId = UnitId(0x51);
pub const GANTRITHOR: UnitId = UnitId(0x52);
pub const REAVER: UnitId = UnitId(0x53);
pub const OBSERVER: UnitId = UnitId(0x54);
pub const SCARAB: UnitId = UnitId(0x55);
pub const DANIMOTH: UnitId = UnitId(0x56);
pub const ALDARIS: UnitId = UnitId(0x57);
pub const ARTANIS: UnitId = UnitId(0x58);
pub const RHYNADON: UnitId = UnitId(0x59);
pub const BENGALAAS: UnitId = UnitId(0x5a);
pub const CARGO_SHIP: UnitId = UnitId(0x5b);
pub const MERCENARY_GUNSHIP: UnitId = UnitId(0x5c);
pub const SCANTID: UnitId = UnitId(0x5d);
pub const KAKARU: UnitId = UnitId(0x5e);
pub const RAGNASAUR: UnitId = UnitId(0x5f);
pub const URSADON: UnitId = UnitId(0x60);
pub const LURKER_EGG: UnitId = UnitId(0x61);
pub const RASZAGAL: UnitId = UnitId(0x62);
pub const SAMIR_DURAN: UnitId = UnitId(0x63);
pub const ALEXEI_STUKOV: UnitId = UnitId(0x64);
pub const MAP_REVEALER: UnitId = UnitId(0x65);
pub const GERARD_DUGALLE: UnitId = UnitId(0x66);
pub const LURKER: UnitId = UnitId(0x67);
pub const INFESTED_DURAN: UnitId = UnitId(0x68);
pub const DISRUPTION_WEB: UnitId = UnitId(0x69);
pub const COMMAND_CENTER: UnitId = UnitId(0x6a);
pub const COMSAT_STATION: UnitId = UnitId(0x6b);
pub const NUCLEAR_SILO: UnitId = UnitId(0x6c);
pub const SUPPLY_DEPOT: UnitId = UnitId(0x6d);
pub const REFINERY: UnitId = UnitId(0x6e);
pub const BARRACKS: UnitId = UnitId(0x6f);
pub const ACADEMY: UnitId = UnitId(0x70);
pub const FACTORY: UnitId = UnitId(0x71);
pub const STARPORT: UnitId = UnitId(0x72);
pub const CONTROL_TOWER: UnitId = UnitId(0x73);
pub const SCIENCE_FACILITY: UnitId = UnitId(0x74);
pub const COVERT_OPS: UnitId = UnitId(0x75);
pub const PHYSICS_LAB: UnitId = UnitId(0x76);
pub const STARBASE: UnitId = UnitId(0x77);
pub const MACHINE_SHOP: UnitId = UnitId(0x78);
pub const REPAIR_BAY: UnitId = UnitId(0x79);
pub const ENGINEERING_BAT: UnitId = UnitId(0x7a);
pub const ARMORY: UnitId = UnitId(0x7b);
pub const MISSILE_TURRET: UnitId = UnitId(0x7c);
pub const BUNKER: UnitId = UnitId(0x7d);
pub const NORAD_II_CRASHED: UnitId = UnitId(0x7e);
pub const ION_CANNON: UnitId = UnitId(0x7f);
pub const URAJ_CRYSTAL: UnitId = UnitId(0x80);
pub const KHALIS_CRYSTAL: UnitId = UnitId(0x81);
pub const INFESTED_COMMAND_CENTER: UnitId = UnitId(0x82);
pub const HATCHERY: UnitId = UnitId(0x83);
pub const LAIR: UnitId = UnitId(0x84);
pub const HIVE: UnitId = UnitId(0x85);
pub const NYDUS_CANAL: UnitId = UnitId(0x86);
pub const HYDRALISK_DEN: UnitId = UnitId(0x87);
pub const DEFILER_MOUND: UnitId = UnitId(0x88);
pub const GREATER_SPIRE: UnitId = UnitId(0x89);
pub const QUEENS_NEST: UnitId = UnitId(0x8a);
pub const EVOLUTION_CHAMBER: UnitId = UnitId(0x8b);
pub const ULTRALISK_CAVERN: UnitId = UnitId(0x8c);
pub const SPIRE: UnitId = UnitId(0x8d);
pub const SPAWNING_POOL: UnitId = UnitId(0x8e);
pub const CREEP_COLONY: UnitId = UnitId(0x8f);
pub const SPORE_COLONY: UnitId = UnitId(0x90);
pub const UNUSED_ZERG_BUILDING_1: UnitId = UnitId(0x91);
pub const SUNKEN_COLONY: UnitId = UnitId(0x92);
pub const OVERMIND_WITH_SHELL: UnitId = UnitId(0x93);
pub const OVERMIND: UnitId = UnitId(0x94);
pub const EXTRACTOR: UnitId = UnitId(0x95);
pub const MATURE_CHRYSALIS: UnitId = UnitId(0x96);
pub const CEREBRATE: UnitId = UnitId(0x97);
pub const CEREBRATE_DAGGOTH: UnitId = UnitId(0x98);
pub const UNUSED_ZERG_BUILDING_2: UnitId = UnitId(0x99);
pub const NEXUS: UnitId = UnitId(0x9a);
pub const ROBOTICS_FACILITY: UnitId = UnitId(0x9b);
pub const PYLON: UnitId = UnitId(0x9c);
pub const ASSIMILATOR: UnitId = UnitId(0x9d);
pub const UNUSED_PROTOSS_BUILDING_1: UnitId = UnitId(0x9e);
pub const OBSERVATORY: UnitId = UnitId(0x9f);
pub const GATEWAY: UnitId = UnitId(0xa0);
pub const UNUSED_PROTOSS_BUILDING_2: UnitId = UnitId(0xa1);
pub const PHOTON_CANNON: UnitId = UnitId(0xa2);
pub const CITADEL_OF_ADUN: UnitId = UnitId(0xa3);
pub const CYBERNETICS_CORE: UnitId = UnitId(0xa4);
pub const TEMPLAR_ARCHIVES: UnitId = UnitId(0xa5);
pub const FORGE: UnitId = UnitId(0xa6);
pub const STARGATE: UnitId = UnitId(0xa7);
pub const STASIS_CELL: UnitId = UnitId(0xa8);
pub const FLEET_BEACON: UnitId = UnitId(0xa9);
pub const ARBITER_TRIBUNAL: UnitId = UnitId(0xaa);
pub const ROBOTICS_SUPPORT_BAY: UnitId = UnitId(0xab);
pub const SHIELD_BATTERY: UnitId = UnitId(0xac);
pub const KHAYDARIN_CRYSTAL_FORMATION: UnitId = UnitId(0xad);
pub const TEMPLE: UnitId = UnitId(0xae);
pub const XELNAGA_TEMPLE: UnitId = UnitId(0xaf);
pub const MINERAL_FIELD_1: UnitId = UnitId(0xb0);
pub const MINERAL_FIELD_2: UnitId = UnitId(0xb1);
pub const MINERAL_FIELD_3: UnitId = UnitId(0xb2);
pub const CAVE: UnitId = UnitId(0xb3);
pub const CAVE_IN: UnitId = UnitId(0xb4);
pub const CANTINA: UnitId = UnitId(0xb5);
pub const MINING_PLATFORM: UnitId = UnitId(0xb6);
pub const INDEPENDENT_COMMAND_CENTER: UnitId = UnitId(0xb7);
pub const INDEPENDENT_STARPORT: UnitId = UnitId(0xb8);
pub const JUMP_GATE_UNUSED: UnitId = UnitId(0xb9);
pub const RUINS: UnitId = UnitId(0xba);
pub const KYADARIN_CRYSTAL_FORMATION_UNUSED: UnitId = UnitId(0xbb);
pub const VESPENE_GEYSER: UnitId = UnitId(0xbc);
pub const WARP_GATE: UnitId = UnitId(0xbd);
pub const PSI_DISRUPTER: UnitId = UnitId(0xbe);
pub const ZERG_MARKER: UnitId = UnitId(0xbf);
pub const TERRAN_MARKER: UnitId = UnitId(0xc0);
pub const PROTOSS_MARKER: UnitId = UnitId(0xc1);
pub const ZERG_BEACON: UnitId = UnitId(0xc2);
pub const TERRAN_BEACON: UnitId = UnitId(0xc3);
pub const PROTOSS_BEACON: UnitId = UnitId(0xc4);
pub const ZERG_FLAG_BEACON: UnitId = UnitId(0xc5);
pub const TERRAN_FLAG_BEACON: UnitId = UnitId(0xc6);
pub const PROTOSS_FLAG_BEACON: UnitId = UnitId(0xc7);
pub const POWER_GENERATOR: UnitId = UnitId(0xc8);
pub const OVERMIND_COCOON: UnitId = UnitId(0xc9);
pub const DARK_SWARM: UnitId = UnitId(0xca);
pub const FLOOR_MISSILE_TRAP: UnitId = UnitId(0xcb);
pub const FLOOR_HATCH: UnitId = UnitId(0xcc);
pub const LEFT_UPPER_LEVEL_DOOR: UnitId = UnitId(0xcd);
pub const RIGHT_UPPER_LEVEL_DOOR: UnitId = UnitId(0xce);
pub const LEFT_PIT_DOOR: UnitId = UnitId(0xcf);
pub const RIGHT_PIT_DOOR: UnitId = UnitId(0xd0);
pub const FLOOR_GUN_TRAP: UnitId = UnitId(0xd1);
pub const LEFT_WALL_MISSILE_TRAP: UnitId = UnitId(0xd2);
pub const LEFT_WALL_FLAME_TRAP: UnitId = UnitId(0xd3);
pub const RIGHT_WALL_MISSILE_TRAP: UnitId = UnitId(0xd4);
pub const RIGHT_WALL_FLAME_TRAP: UnitId = UnitId(0xd5);
pub const START_LOCATION: UnitId = UnitId(0xd6);
pub const FLAG: UnitId = UnitId(0xd7);
pub const YOUNG_CHRYSALIS: UnitId = UnitId(0xd8);
pub const PSI_EMITTER: UnitId = UnitId(0xd9);
pub const DATA_DISC: UnitId = UnitId(0xda);
pub const KHAYDARIN_CRYSTAL: UnitId = UnitId(0xdb);
pub const MINERAL_CHUNK_1: UnitId = UnitId(0xdc);
pub const MINERAL_CHUNK_2: UnitId = UnitId(0xdd);
pub const VESPENE_ORB_1: UnitId = UnitId(0xde);
pub const VESPENE_ORB_2: UnitId = UnitId(0xdf);
pub const VESPENE_SAC_1: UnitId = UnitId(0xe0);
pub const VESPENE_SAC_2: UnitId = UnitId(0xe1);
pub const VESPENE_TANK_1: UnitId = UnitId(0xe2);
pub const VESPENE_TANK_2: UnitId = UnitId(0xe3);
pub const NONE: UnitId = UnitId(0xe4);
pub const ANY_UNIT: UnitId = UnitId(0xe5);
pub const GROUP_MEN: UnitId = UnitId(0xe6);
pub const GROUP_BUILDINGS: UnitId = UnitId(0xe7);
pub const GROUP_FACTORIES: UnitId = UnitId(0xe8);
