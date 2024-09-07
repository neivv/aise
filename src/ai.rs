use std::convert::TryFrom;
use std::mem;
use std::ptr::null_mut;

use libc::c_void;
use serde::{Deserialize, Serialize};

use bw_dat::{
    Game, order, unit, Pathing, Race, RaceFlags, Region, TechId, Unit, UnitArray, UnitId,
    UpgradeId,
};

use crate::aiscript::Town;
use crate::bw;
use crate::globals::{Globals, RegionIdCycle};
use crate::list::{ListEntry, ListIter};
use crate::pathing;
use crate::unit::{active_units, UnitExt, LazyUnitStrengths, UnitStrengths};
use crate::unit_search::{LazyUnitSearch, UnitSearch};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PlayerAi(pub *mut bw::PlayerAiData, pub u8);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PlayerAiArray(pub *mut bw::PlayerAiData);

impl PlayerAiArray {
    pub fn get() -> PlayerAiArray {
        PlayerAiArray(bw::player_ai(0))
    }

    pub fn player(self, player: u8) -> PlayerAi {
        assert!(player < 8);
        unsafe { PlayerAi(self.0.add(player.into()), player) }
    }
}

impl PlayerAi {
    pub fn get(player: u8) -> PlayerAi {
        assert!(player < 8);
        PlayerAi(bw::player_ai(player.into()), player)
    }

    fn train_request_priority(&self, game: Game) -> u8 {
        if game.minerals(self.1) > 1500 && game.gas(self.1) > 1500 {
            160
        } else {
            50
        }
    }

    pub fn check_train(&self, unit: Unit, game: Game) {
        let train_unit_id = unsafe { (*self.0).train_unit_id };
        if train_unit_id != 0 {
            let unit_id = UnitId(train_unit_id - 1);
            unsafe {
                (*self.0).train_unit_id = 0;
            }
            let ai_regions = bw::ai_regions(self.1 as u32);
            if let Some(region) = unit_ai_region(ai_regions, unit) {
                let priority = self.train_request_priority(game);
                self.add_train_request(unit_id, region, priority, game);
            }
        }
    }

    fn is_campaign(&self) -> bool {
        self.flags() & 0x20 != 0
    }

    fn flags(&self) -> u16 {
        unsafe { (*self.0).flags }
    }

    fn add_train_request(&self, unit: UnitId, region: *mut bw::AiRegion, priority: u8, game: Game) {
        // Note: Idle training would check for flag 0x200 being nonzero before adding,
        // but it is currently handled by BW.
        unsafe {
            (*self.0).flags |= 0x40;
        }
        self.add_spending_request(&SpendingRequest::Military(unit, region), priority, game);
    }

    pub fn is_at_limit(&self, unit: UnitId, game: Game) -> bool {
        let limit = if let Some(arr) = bw_dat::extended_array(0xc) {
            arr.read_u8(unit.0 as usize * 0xc + self.1 as usize)
        } else {
            unsafe { (*self.0).build_limits[unit.0 as usize] }
        };
        match limit {
            0 => false,
            0xff => true,
            x => count_units(self.1, unit, game) >= x.into(),
        }
    }

    fn add_spending_request(&self, req: &SpendingRequest, priority: u8, game: Game) {
        let limit_reached = match req {
            SpendingRequest::Military(unit, _) | SpendingRequest::Guard(unit, _) => {
                self.is_at_limit(*unit, game)
            }
        };
        if !limit_reached {
            self.add_spending_request_inner(req, priority, game);
        }
    }

    fn add_spending_request_inner(&self, req: &SpendingRequest, priority: u8, game: Game) {
        let cost = match req {
            SpendingRequest::Military(unit, _) | SpendingRequest::Guard(unit, _) => {
                unit_cost(*unit)
            }
        };
        unsafe {
            (*self.0).mineral_need = (*self.0).mineral_need.saturating_add(cost.minerals);
            (*self.0).gas_need = (*self.0).gas_need.saturating_add(cost.gas);
            (*self.0).supply_need = (*self.0).supply_need.saturating_add(cost.supply);
        }
        let (ty, id, val) = match *req {
            SpendingRequest::Military(unit, region) => (1, unit.0, region as *mut c_void),
            SpendingRequest::Guard(unit, ai) => (2, unit.0, ai as *mut c_void),
        };
        self.push_request(bw::AiSpendingRequest {
            priority,
            ty,
            id,
            val,
        });
        if let Some((prereq, count)) = self.prerequisite_unit(req, game) {
            for _ in 0..count {
                self.add_spending_request_inner(&prereq, priority, game);
            }
        }
    }

    fn push_request(&self, req: bw::AiSpendingRequest) {
        unsafe {
            let requests = &mut (*self.0).requests[..];
            let request_count = usize::from((*self.0).request_count);
            if request_count >= requests.len() {
                return;
            }
            (*self.0).request_count += 1;
            request_heap_push(requests, request_count, req);
        }
    }

    fn prerequisite_unit(
        &self,
        req: &SpendingRequest,
        game: Game,
    ) -> Option<(SpendingRequest, u32)> {
        use self::SpendingRequest::*;
        let (prereq, count) = match *req {
            Military(unit, _) | Guard(unit, _) => match unit {
                unit::GUARDIAN | unit::DEVOURER => (unit::MUTALISK, 1u32),
                unit::LURKER => (unit::HYDRALISK, 1),
                unit::ARCHON => (unit::HIGH_TEMPLAR, 2),
                unit::DARK_ARCHON => (unit::DARK_TEMPLAR, 2),
                _ => return None,
            },
        };
        let existing_count = count_units(self.1, prereq, game);
        let needed = count.saturating_sub(existing_count);
        if needed == 0 {
            None
        } else {
            let new_req = match *req {
                Military(_, region) => Military(prereq, region),
                Guard(_, ai) => Guard(prereq, ai),
            };
            Some((new_req, needed))
        }
    }

    // Should probs return SpendingRequest instead
    pub fn first_request(&self) -> Option<bw::AiSpendingRequest> {
        unsafe {
            if (*self.0).request_count == 0 {
                None
            } else {
                Some((*self.0).requests[0])
            }
        }
    }

    pub fn copy_requests(&self) -> SpendingRequestsCopy {
        unsafe {
            SpendingRequestsCopy {
                buffer: (*self.0).requests,
                count: (*self.0).request_count,
                deleted: 0,
            }
        }
    }

    pub fn set_to_copied_requests(&self, copy: &SpendingRequestsCopy) {
        unsafe {
            let len = copy.count as usize;
            let slice = &copy.buffer[..len];
            (&mut (*self.0).requests[..len]).copy_from_slice(slice);
            (*self.0).request_count = copy.count;
        }
    }

    pub fn has_resources(&self, game: Game, players: *mut bw::Player, cost: &Cost) -> bool {
        unsafe {
            let player = self.1;
            let race_flags = Race::from_id((*players.add(player as usize)).race)
                .map(|x| x.as_flags())
                .unwrap_or(RaceFlags::empty());
            let consider_supply = cost.races.intersects(race_flags);
            (*self.0).minerals_available >= cost.minerals &&
                (*self.0).gas_available >= cost.gas &&
                (!consider_supply || (*self.0).supply_available >= cost.supply) &&
                has_resources(game, player, cost)
        }
    }

    pub fn available_resources(&self, players: *mut bw::Player) -> AiResources {
        unsafe {
            let player = self.1;
            let race = Race::from_id((*players.add(player as usize)).race);
            AiResources {
                minerals: (*self.0).minerals_available,
                gas: (*self.0).gas_available,
                supply: (*self.0).supply_available,
                race,
            }
        }
    }

    /// spent_money is true when the ai actually built something, false on failures
    pub fn remove_resource_need(&self, cost: &Cost, spent_money: bool) {
        unsafe {
            (*self.0).mineral_need = (*self.0).mineral_need.saturating_sub(cost.minerals);
            (*self.0).gas_need = (*self.0).gas_need.saturating_sub(cost.supply);
            (*self.0).supply_need = (*self.0).supply_need.saturating_sub(cost.supply);
            if spent_money {
                (*self.0).minerals_available =
                    (*self.0).minerals_available.saturating_sub(cost.minerals);
                (*self.0).gas_available = (*self.0).gas_available.saturating_sub(cost.gas);
                (*self.0).supply_available = (*self.0).supply_available.saturating_sub(cost.supply);
            }
        }
    }

    pub fn pop_request(&self) {
        let requests;
        let new_count;
        unsafe {
            requests = &mut ((*self.0).requests)[..(*self.0).request_count as usize];
            (*self.0).request_count -= 1;
            new_count = (*self.0).request_count as usize;
            if new_count == 0 {
                return;
            }
        }
        request_heap_pop(requests);
    }

    fn is_guard_being_trained(&self, guard: *mut bw::GuardAi) -> bool {
        let requests = unsafe { &mut ((*self.0).requests)[..(*self.0).request_count as usize] };
        if requests
            .iter()
            .any(|req| req.ty == 2 && req.val as *mut bw::GuardAi == guard)
        {
            return true;
        }
        for unit in active_units().filter(|x| x.player() == self.1) {
            if let Some(ai) = unit.building_ai() {
                unsafe {
                    let iter = (*ai)
                        .train_queue_types
                        .iter_mut()
                        .zip((*ai).train_queue_values.iter_mut());
                    for (ty, val) in iter {
                        if *ty == 2 && *val != null_mut() {
                            let ai = *val as *mut bw::GuardAi;
                            if ai == guard {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        false
    }

    fn remove_from_attack_force(&self, unit_id: UnitId) {
        let unit_id = if unit_id == unit::SIEGE_TANK_SIEGE {
            unit::SIEGE_TANK_TANK
        } else {
            unit_id
        };
        unsafe {
            for val in (*self.0).attack_force.iter_mut() {
                if *val == unit_id.0 + 1 {
                    *val = unit::NONE.0 + 1;
                    return;
                }
            }
        }
    }
}

pub struct SpendingRequestsCopy {
    buffer: [bw::AiSpendingRequest; 0x3f],
    count: u8,
    /// Deleted requests are placed at end of buffer in reverse, as long
    /// as there is space for them
    deleted: u8,
}

impl SpendingRequestsCopy {
    pub fn first_request(&self) -> Option<bw::AiSpendingRequest> {
        if self.count == 0 {
            None
        } else {
            Some(self.buffer[0])
        }
    }

    pub fn pop_request_save(&mut self) {
        let request = match self.first_request() {
            Some(s) => s,
            None => return,
        };
        self.pop_request_dont_save();
        if (self.count as usize + self.deleted as usize) < self.buffer.len() {
            let free_index = self.buffer.len() - 1 - self.deleted as usize;
            self.buffer[free_index] = request;
            self.deleted += 1;
        }
    }

    pub fn pop_request_dont_save(&mut self) {
        if self.count == 0 {
            return;
        }
        request_heap_pop(&mut self.buffer[..(self.count as usize)]);
        self.count -= 1;
    }

    pub fn restore_popped_requests(&mut self) {
        while self.deleted > 0 {
            let index = self.buffer.len() - self.deleted as usize;
            let request = self.buffer[index];
            self.deleted -= 1;
            self.push_request(request);
        }
    }

    pub fn push_request(&mut self, request: bw::AiSpendingRequest) {
        let count = usize::from(self.count);
        if count >= self.buffer.len() {
            return;
        }
        self.count += 1;
        request_heap_push(&mut self.buffer, count, request);
    }
}

fn request_heap_push(
    requests: &mut [bw::AiSpendingRequest],
    current_size: usize,
    req: bw::AiSpendingRequest,
) {
    let mut pos = current_size as isize;
    let mut prev = pos;
    requests[pos as usize] = req;
    pos = (pos - 1) >> 1;
    while prev > 0 {
        if requests[prev as usize].priority <= requests[pos as usize].priority {
            break;
        }
        requests.swap(prev as usize, pos as usize);
        prev = pos;
        pos = (pos - 1) >> 1;
    }
}

fn request_heap_pop(requests: &mut [bw::AiSpendingRequest]) {
    if requests.len() < 2 {
        return;
    }
    let new_count = requests.len() - 1;
    requests[0] = requests[new_count];
    let mut prev = 0;
    let mut pos = 1;
    while pos <= new_count - 1 {
        let index =
            if pos + 1 >= new_count || requests[pos].priority > requests[pos + 1].priority {
                pos
            } else {
                pos + 1
            };
        if requests[prev].priority >= requests[index].priority {
            break;
        }
        requests.swap(prev, index);
        prev = index;
        pos = (index << 1) + 1;
    }
}

/// Returns unit ids which are counted for request satisfication for request of `unit_id`
/// So practically higher tier buildings with morph.
pub fn request_equivalent_unit_ids(unit_id: UnitId) -> [UnitId; 4] {
    use bw_dat::unit::*;
    match unit_id {
        SIEGE_TANK_TANK | SIEGE_TANK_SIEGE => [SIEGE_TANK_SIEGE, SIEGE_TANK_TANK, NONE, NONE],
        SPIRE => [SPIRE, GREATER_SPIRE, NONE, NONE],
        CREEP_COLONY => [CREEP_COLONY, SPORE_COLONY, SUNKEN_COLONY, NONE],
        HATCHERY => [HATCHERY, LAIR, HIVE, NONE],
        LAIR => [LAIR, HIVE, NONE, NONE],
        _ => [unit_id, NONE, NONE, NONE],
    }
}

fn completed_id(unit: Unit) -> Option<UnitId> {
    use bw_dat::unit::*;
    if unit.is_completed() {
        Some(unit.id())
    } else {
        let dest_id = unit.first_queued_unit()?;
        match dest_id {
            LAIR | HIVE | GREATER_SPIRE | SUNKEN_COLONY | SPORE_COLONY => Some(unit.id()),
            _ => None,
        }
    }
}

pub fn count_town_units(town: Town, unit_id: UnitId, must_be_completed: bool) -> u32 {
    let unit_ids = request_equivalent_unit_ids(unit_id);
    let workers = unsafe { town.workers().flat_map(|x| Unit::from_ptr((*x).parent)) };
    let buildings = unsafe { town.buildings().flat_map(|x| Unit::from_ptr((*x).parent)) };
    let is_worker = unit_id.is_worker();
    let units = workers.take_while(|_| is_worker)
        .chain(buildings.take_while(|_| !is_worker));
    let units_in_town = units
        .filter(|&x| {
            let id = if must_be_completed {
                match completed_id(x) {
                    Some(s) => s,
                    None => return false,
                }
            } else {
                x.building_morph_displayed_id()
            };
            unit_ids.iter().any(|&x| id == x)
        })
        .count() as u32;
    units_in_town
}

pub fn count_units(player: u8, unit_id: UnitId, game: Game) -> u32 {
    let existing = {
        let mut existing = game.unit_count(player, unit_id);
        let other_unit_id = match unit_id {
            unit::SIEGE_TANK_TANK => Some(unit::SIEGE_TANK_SIEGE),
            unit::SIEGE_TANK_SIEGE => Some(unit::SIEGE_TANK_TANK),
            unit::EDMUND_DUKE_TANK => Some(unit::EDMUND_DUKE_SIEGE),
            unit::EDMUND_DUKE_SIEGE => Some(unit::EDMUND_DUKE_TANK),
            _ => None,
        };
        if let Some(other) = other_unit_id {
            existing += game.unit_count(player, other);
        }
        existing
    };
    let birth_multiplier = match unit_id.flags() & 0x400 != 0 {
        true => 2,
        false => 1,
    };
    let mut marked_templars = Vec::new();
    let mut mark_templar = |unit: Unit| {
        if !marked_templars.iter().any(|&x| x == unit) {
            if let Some(t) = unit.target() {
                marked_templars.push(t);
                1
            } else {
                0
            }
        } else {
            0
        }
    };
    let morphing: u32 = active_units()
        .filter(|x| x.player() == player)
        .map(|unit| match unit.id() {
            unit::EGG | unit::COCOON | unit::LURKER_EGG => {
                if unit.first_queued_unit() == Some(unit_id) {
                    birth_multiplier
                } else {
                    0
                }
            }
            unit::HIGH_TEMPLAR if unit_id == unit::ARCHON => {
                match unit.order() == order::ARCHON_WARP {
                    true => mark_templar(unit),
                    false => 0,
                }
            }
            unit::DARK_TEMPLAR if unit_id == unit::DARK_ARCHON => {
                match unit.order() == order::DARK_ARCHON_MELD {
                    true => mark_templar(unit),
                    false => 0,
                }
            }
            _ => 0,
        })
        .sum();
    morphing.saturating_add(existing)
}

pub struct AiResources {
    pub minerals: u32,
    pub gas: u32,
    pub supply: u32,
    pub race: Option<Race>
}

impl AiResources {
    pub fn has_enough_for_cost(&self, cost: &Cost) -> bool {
        let race_flags = self.race.map(|x| x.as_flags()).unwrap_or(RaceFlags::empty());
        let consider_supply = cost.races.intersects(race_flags);

        self.minerals >= cost.minerals &&
            self.gas >= cost.gas &&
            (!consider_supply || self.supply >= cost.supply)
    }

    pub fn reduce_cost(&mut self, cost: &Cost) {
        let race_flags = self.race.map(|x| x.as_flags()).unwrap_or(RaceFlags::empty());
        let consider_supply = cost.races.intersects(race_flags);

        self.minerals = self.minerals.saturating_sub(cost.minerals);
        self.gas = self.gas.saturating_sub(cost.gas);
        if consider_supply {
            self.supply = self.supply.saturating_sub(cost.supply);
        }
    }
}

pub struct Cost {
    pub minerals: u32,
    pub gas: u32,
    /// Includes supply cost for both units if the unit is dual birth unit.
    pub supply: u32,
    pub races: RaceFlags,
}

pub fn request_cost(request: &bw::AiSpendingRequest, upgrade_level: u8) -> Cost {
    match request.ty {
        5 => upgrade_cost(UpgradeId(request.id), upgrade_level),
        6 => tech_cost(TechId(request.id)),
        _ => unit_cost(UnitId(request.id)),
    }
}

pub fn unit_cost(unit: UnitId) -> Cost {
    let dual_birth = unit.flags() & 0x400 != 0;
    Cost {
        minerals: unit.mineral_cost(),
        gas: unit.gas_cost(),
        supply: unit.supply_cost() * if dual_birth { 2 } else { 1 },
        races: unit.races(),
    }
}

pub fn upgrade_cost(upgrade: UpgradeId, level: u8) -> Cost {
    Cost {
        minerals: upgrade.mineral_cost().saturating_add(
            (level as u32)
                .saturating_sub(1)
                .saturating_mul(upgrade.mineral_factor()),
        ),
        gas: upgrade.gas_cost().saturating_add(
            (level as u32)
                .saturating_sub(1)
                .saturating_mul(upgrade.gas_factor()),
        ),
        supply: 0,
        races: RaceFlags::empty(),
    }
}

pub fn tech_cost(tech: TechId) -> Cost {
    Cost {
        minerals: tech.mineral_cost(),
        gas: tech.gas_cost(),
        supply: 0,
        races: RaceFlags::empty(),
    }
}

#[derive(Clone)]
enum SpendingRequest {
    Military(UnitId, *mut bw::AiRegion),
    Guard(UnitId, *mut bw::GuardAi),
}

pub fn unit_ai_region(regions: *mut bw::AiRegion, unit: Unit) -> Option<*mut bw::AiRegion> {
    ai_region(regions, unit.position())
}

pub fn ai_region(regions: *mut bw::AiRegion, position: bw::Point) -> Option<*mut bw::AiRegion> {
    if regions != null_mut() {
        if let Some(region) = bw::get_region(position) {
            unsafe { Some(regions.offset(region as isize)) }
        } else {
            None
        }
    } else {
        None
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GuardState {
    // Entry for each guard, deaths != 0 if actually used
    guards: Vec<Guard>,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Default)]
struct Guard {
    // 0 = Transparent to BW (Not created by us / pending deletion), 255 = Never delete.
    // Otherwise how many deaths the guard has left.
    // TODO: I think BW may delete guards when starting the first town for the player?
    // This doesn't see that and breaks a bit.
    deaths: u8,
    priority: u8,
}

enum GuardDeathResult {
    ZeroBwVisible,
    Nothing,
    Deleted,
}

impl GuardState {
    pub const fn new() -> GuardState {
        GuardState {
            guards: Vec::new(),
        }
    }

    fn guard(&mut self, array: *mut bw::GuardAi, ai: *mut bw::GuardAi) -> &mut Guard {
        if self.guards.is_empty() {
            // At least as of 1.22 there's still just 1000 guards?
            self.guards.resize_with(1000, Default::default);
        }
        let index = (ai as usize - array as usize) / mem::size_of::<bw::GuardAi>();
        if index >= self.guards.len() {
            // Sanity check
            assert!(index < 0x4000);
            self.guards.resize(index + 1, Default::default());
        }
        &mut self.guards[index]
    }

    pub fn add(
        &mut self,
        array: *mut bw::GuardAi,
        ai: *mut bw::GuardAi,
        death_limit: u8,
        priority: u8,
    ) {
        let guard = self.guard(array, ai);
        *guard = Guard {
            deaths: death_limit,
            priority,
        }
    }

    fn add_death(&mut self, array: *mut bw::GuardAi, ai: *mut bw::GuardAi) -> GuardDeathResult {
        let guard = self.guard(array, ai);
        match guard.deaths {
            0 => GuardDeathResult::Nothing,
            255 => GuardDeathResult::ZeroBwVisible,
            x => {
                guard.deaths = x - 1;
                if x == 1 {
                    GuardDeathResult::Deleted
                } else {
                    GuardDeathResult::ZeroBwVisible
                }
            }
        }
    }

    fn priority(&mut self, array: *mut bw::GuardAi, ai: *mut bw::GuardAi) -> u8 {
        let guard = self.guard(array, ai);
        if guard.priority == 0 {
            60
        } else {
            guard.priority
        }
    }
}

pub unsafe fn update_guard_needs(
    game: Game,
    player_ai: PlayerAiArray,
    guards: &mut GuardState,
) {
    let guard_array = (*bw::guard_array()).ais.as_mut_ptr();
    let seconds = (**game).elapsed_seconds;
    for player in 0..8 {
        let ai = player_ai.player(player);
        let ai_regions = bw::ai_regions(player as u32);
        let mut guard = bw::guard_ais(player);
        while guard != null_mut() {
            if (*guard).times_died > 0 {
                let result = guards.add_death(guard_array, guard);
                match result {
                    GuardDeathResult::ZeroBwVisible => (*guard).times_died = 0,
                    GuardDeathResult::Deleted => (*guard).times_died = 3,
                    GuardDeathResult::Nothing => (),
                };
            }
            let previous_update = (*guard).previous_update;
            // Not handling dual birth since pulling military is difficult right now
            let unit_id = UnitId((*guard).unit_id);
            let skip = (*guard).parent != null_mut() ||
                (unit_id == unit::ZERGLING || unit_id == unit::SCOURGE) ||
                (ai.is_campaign() && (*guard).times_died >= 3);
            if !skip {
                if seconds.saturating_sub(previous_update) > 5 {
                    (*guard).previous_update = seconds;
                    if let Some(region) = ai_region(ai_regions, (*guard).other_home) {
                        if region_can_rebuild_guards(region) {
                            if !ai.is_guard_being_trained(guard) {
                                debug!(
                                    "Adding guard {:x} {:x} {:x?}",
                                    player,
                                    unit_id.0,
                                    (*guard).home
                                );
                                let req = SpendingRequest::Guard(unit_id, guard);
                                let priority = guards.priority(guard_array, guard);
                                ai.add_spending_request(&req, priority, game);
                            }
                        }
                    }
                }
            }
            guard = (*guard).next;
        }
    }
}

unsafe fn region_can_rebuild_guards(region: *mut bw::AiRegion) -> bool {
    (*region).state != 3 &&
        (*region).air_target == null_mut() &&
        (*region).ground_target == null_mut() &&
        (*region).flags & 0x20 == 0
}

pub unsafe fn continue_incomplete_buildings() {
    for i in 0..8 {
        for town in ListIter(bw::first_active_ai_town(i)) {
            let regions = bw::ai_regions(u32::from((*town).player));
            let free_scvs = ListIter((*town).workers.first)
                .filter_map(|x| Unit::from_ptr((*x).parent))
                .filter(|x| x.id() == unit::SCV && x.order() == order::COMPUTER_AI);
            let incomplete_buildings = ListIter((*town).buildings.first)
                .filter_map(|x| Unit::from_ptr((*x).parent))
                .filter(|&x| {
                    !x.is_completed() &&
                        (**x).related.is_null() &&
                        x.id().group_flags() & 0x2 != 0 &&
                        is_building_safe(x, regions)
                });
            for (scv, building) in free_scvs.zip(incomplete_buildings) {
                scv.issue_order_unit(order::CONSTRUCTING_BUILDING, building);
                (**building).related = *scv;
            }
        }
    }
}

/// Fixes towns that don't have a valid main building but do have
/// a town hall in their building list.
pub unsafe fn update_town_main_buildings() {
    for i in 0..8 {
        for town in ListIter(bw::first_active_ai_town(i)) {
            let town = match Town::from_ptr(town) {
                Some(s) => s,
                None => continue,
            };
            let ok = match town.main_building() {
                // Could be false in an edge case where ai infests a cc it owns
                // that was the main building.
                Some(s) => s.id().is_town_hall(),
                None => false,
            };
            if ok {
                continue;
            }
            if !town.has_workers() {
                // Shouldn't be point in trying to pick new town halls if there are no workers
                continue;
            }
            if let Some(unit) = town.buildings()
                .filter_map(|x| Unit::from_ptr((*x).parent))
                .find(|x| x.id().is_town_hall())
            {
                (*town.0).main_building = *unit;
            }
        }
    }
}

unsafe fn is_building_safe(building: Unit, regions: *mut bw::AiRegion) -> bool {
    let region = match bw::get_region(building.position()) {
        Some(s) => regions.offset(s as isize),
        None => return true,
    };
    (*region).state != 3 &&
        (*region).ground_target.is_null() &&
        (*region).air_target.is_null() &&
        (*region).flags & 0x20 == 0
}

unsafe fn get_matching_guard_ai(unit: Unit) -> Option<*mut bw::GuardAi> {
    let mut guard = bw::guard_ais(unit.player());
    while guard != null_mut() {
        if (*guard).parent.is_null() {
            if UnitId((*guard).unit_id) == unit.id() && (*guard).other_home == unit.position() {
                return Some(guard);
            }
        }
        guard = (*guard).next;
    }
    let array = bw::guard_array();
    if (*array).first_free.is_null() {
        None
    } else {
        let ai = (*array).first_free;
        // Free array objects don't have initialized ai_type if the object
        // has never been used before.
        (*ai).ai_type = 1;
        assert!(unit.player() < 8);
        let dest = crate::samase::guard_ais().offset(unit.player() as isize);
        ListEntry::move_to(ai, &mut (*array).first_free, &mut (*dest).first);
        Some(ai)
    }
}

/// Uses an existing needed or creates a new guard ai for the unit.
/// Usually likely creates since there isn't one at precisely where the unit stands.
pub unsafe fn add_guard_ai(unit: Unit) {
    assert!(!unit.has_ai());
    // Ai flag for "don't become guard"
    if unit.id().ai_flags() & 0x2 != 0 {
        return;
    }
    assert!(!unit.id().is_building());
    if let Some(ai) = get_matching_guard_ai(unit) {
        (*ai).parent = *unit;
        (*ai).unit_id = unit.id().0;
        (*ai).home = unit.position();
        (*ai).other_home = unit.position();
        (*ai).times_died = 0;
        (**unit).ai = ai as *mut c_void;
    } else {
        warn!("Guard ai limit");
    }
}

/// NOTE: Differs from bw function in that it doesn't immediatly do one frame step.
/// If this is called somewhere else than just zerg birth order, it should be done afterwards.
pub unsafe fn add_military_ai(unit: Unit, region: *mut bw::AiRegion, always_this_region: bool) {
    assert!(!unit.has_ai());
    let region = if !always_this_region && (*region).state == 3 {
        let regions = bw::ai_regions(unit.player() as u32);
        ai_region(regions, unit.position()).expect("Unit out of bounds??")
    } else {
        region
    };

    let array = (*region).military.full_array;
    let ai = (*array).first_free;
    if ai.is_null() {
        warn!("Military ai limit");
        return;
    }
    ListEntry::move_to(ai, &mut (*array).first_free, &mut (*region).military.first);
    (*ai).ai_type = 4; // Unnecessary?
    (*ai).parent = *unit;
    (*ai).region = region;
    (**unit).ai = ai as *mut c_void;
    if unit.is_air() {
        (*region).needed_air_strength = (*region).needed_air_strength.saturating_add(1); // Why?
    }
    match (*region).state {
        1 | 2 | 8 | 9 => update_slowest_unit_in_region(region),
        _ => (),
    }
}

unsafe fn update_slowest_unit_in_region(region: *mut bw::AiRegion) {
    if !PlayerAi::get((*region).player).is_campaign() {
        return;
    }
    let mut slowest_ground = None;
    let mut slowest_air = None;
    let mut ground_speed = u32::MAX;
    let mut air_speed = u32::MAX;

    if let Some(unit) = Unit::from_ptr((*region).slowest_military) {
        if unit.is_air() {
            slowest_air = Some(unit);
            air_speed = (**unit).flingy.top_speed;
        } else {
            slowest_ground = Some(unit);
            ground_speed = (**unit).flingy.top_speed;
        }
    }
    for ai in ListIter((*region).military.first) {
        let unit = Unit::from_ptr((*ai).parent).expect("Parentless military ai");
        let speed = (**unit).flingy.top_speed;
        if unit.is_air() {
            if speed < air_speed {
                air_speed = speed;
                slowest_air = Some(unit);
            }
        } else {
            if speed < ground_speed {
                ground_speed = speed;
                slowest_ground = Some(unit);
            }
        }
    }
    (*region).slowest_military = slowest_ground
        .or(slowest_air)
        .map(|x| *x)
        .unwrap_or(null_mut());
}

pub fn has_resources(game: Game, player: u8, cost: &Cost) -> bool {
    static RACES: &[(RaceFlags, Race)] = &[
        (RaceFlags::ZERG, Race::Zerg),
        (RaceFlags::TERRAN, Race::Terran),
        (RaceFlags::PROTOSS, Race::Protoss),
    ];
    for &(flag, race) in RACES {
        if cost.races.intersects(flag) && game.supply_free(player, race) < cost.supply {
            return false;
        }
    }
    game.minerals(player) >= cost.minerals && game.gas(player) >= cost.gas
}

pub unsafe fn update_region_safety(
    region_pos: &mut RegionIdCycle,
    game: Game,
    search: &UnitSearch,
) {
    let pathing = bw::pathing();
    if !region_pos.is_inited() {
        region_pos.init((*pathing).region_count);
    }
    for (player, id_iter) in region_pos.cycle_all() {
        let regions = bw::ai_regions(player.into());
        if regions != null_mut() {
            for region_id in id_iter {
                let region = regions.add(region_id as usize);
                if (*region).flags & 0x20 != 0 {
                    // Region was marked as dangerous, check if there actually are any enemies
                    // nearby
                    let mut area = (*pathing).regions[region_id as usize].area;
                    let radius = 32 * 12;
                    area.left = area.left.saturating_sub(radius);
                    area.top = area.top.saturating_sub(radius);
                    area.right = area.right.saturating_add(radius);
                    area.bottom = area.bottom.saturating_add(radius);
                    let has_enemies = search
                        .search_iter(&area)
                        .any(|unit| !game.allied(unit.player(), player));
                    if !has_enemies {
                        (*region).flags &= !0x20;
                    }

                    // Leave rest of the regions of this player for later frames
                    break;
                }
            }
        }
    }
}

/// Helper function for remove_unit_ai. Probably call that instead.
///
/// Does ai region/military -related cleanup
unsafe fn remove_from_ai_structs(
    game: Game,
    unit_search: &UnitSearch,
    strength: &UnitStrengths,
    player_ai: &PlayerAi,
    unit: Unit,
    was_killed: bool,
) {
    let players = bw::players();
    let pathing = bw::pathing();
    let region_count = (*pathing).region_count;
    let unit_id = unit.id();
    for i in 0u8..8 {
        if (*players.add(i as usize)).player_type != 1 {
            continue;
        }
        let ai_regions = bw::ai_regions(i as u32);
        // Region 0 is not used?
        for j in 1..region_count {
            let region = ai_regions.add(j as usize);
            if (*region).air_target == *unit {
                (*region).air_target = null_mut();
            }
            if (*region).ground_target == *unit {
                (*region).ground_target = null_mut();
            }
            if (*region).slowest_military == *unit {
                (*region).slowest_military = null_mut();
            }
            if (*region).detector == *unit {
                (*region).detector = null_mut();
            }
        }
        if !game.allied(i, unit.player()) {
            if let Some(region) = unit_ai_region(ai_regions, unit) {
                let ground = u16::try_from(strength.ground(unit_id)).unwrap_or(u16::MAX);
                let air = u16::try_from(strength.air(unit_id)).unwrap_or(u16::MAX);
                (*region).needed_ground_strength =
                    (*region).needed_ground_strength.saturating_sub(ground);
                (*region).needed_air_strength =
                    (*region).needed_air_strength.saturating_sub(air);
            }
        }
    }
    if let Some(ai) = unit.military_ai() {
        let ai_regions = bw::ai_regions(unit.player() as u32);
        let region = (*ai).region;
        if (*region).state == 8 {
            player_ai.remove_from_attack_force(unit_id);
        } else if was_killed {
            let is_attack_region = match (*region).state {
                1 | 2 | 8 | 9 => true,
                3 | 4 | 5 | 6 | 7 | _ => false,
            };
            if !is_attack_region {
                if let Some(current_region) = unit_ai_region(ai_regions, unit) {
                    if current_region != region {
                        let near_town = has_worker_or_building_at_region(
                            unit_search,
                            pathing,
                            (*region).id,
                            unit.player(),
                        );
                        if !near_town {
                            bw::change_ai_region_state(region, 3);
                        }
                    }
                }
            }
        }
        let array = (*region).military.full_array;
        ListEntry::move_to(ai, &mut (*region).military.first, &mut (*array).first_free);
        (**unit).ai = null_mut();
    }
}

unsafe fn has_worker_or_building_at_region(
    unit_search: &UnitSearch,
    pathing: *mut bw::Pathing,
    region_id: u16,
    player: u8,
) -> bool {
    let region = (*pathing).regions.as_ptr().add(region_id as usize);
    let mut region_units = unit_search
        .search_iter(&(*region).area)
        .filter(|unit| bw::get_region(unit.position()) == Some(region_id));
    region_units
        .any(|unit| unit.player() == player && (unit.id().is_building() || unit.id().is_worker()))
}

/// Helper function for remove_unit_ai. Probably call that instead.
/// Does town-related cleanup.
///
/// check_delete is set false if recursing here when check_town_delete moves units
unsafe fn remove_worker_or_building_ai(
    game: Game,
    player_ai: &PlayerAi,
    unit: Unit,
    check_delete: bool,
) {
    if unit.id().is_resource_container() {
        for town in (0..8).flat_map(|player| ListIter(bw::first_active_ai_town(player))) {
            if (*town).mineral == *unit {
                (*town).mineral = null_mut();
            }
            for gas in (*town).gas_buildings.iter_mut() {
                if *gas == *unit {
                    *gas = null_mut();
                }
            }
        }
    }
    let mut used_town = None;
    if let Some(ai) = unit.worker_ai() {
        let town = (*ai).town;
        used_town = Some(town);
        ListEntry::move_to(
            ai,
            &mut (*town).workers.first,
            &mut (*(*town).workers.full_array).first_free,
        );
        (*town).worker_count = (*town).worker_count.saturating_sub(1);
        (**unit).ai = null_mut();
        if (*town).building_scv == *unit {
            (*town).building_scv = null_mut();
        }
    }
    if let Some(ai) = unit.building_ai() {
        let town = (*ai).town;
        used_town = Some(town);
        ListEntry::move_to(
            ai,
            &mut (*town).buildings.first,
            &mut (*(*town).buildings.full_array).first_free,
        );
        (**unit).ai = null_mut();
        if (*town).main_building == *unit {
            (*town).main_building = null_mut();
        }
    }
    if check_delete {
        if let Some(town) = used_town {
            check_town_delete(game, player_ai, Town(town));
        }
    }
}

/// Deletes a town if it is empty.
unsafe fn check_town_delete(game: Game, player_ai: &PlayerAi, town: Town) {
    if !prepare_town_delete(game, player_ai, town) {
        return;
    }
    for unit in active_units() {
        if unit.id().is_resource_container() {
            (**unit).unit_specific2.resource.resource_area = 0;
        }
    }
    if (*town.0).resource_area != 0 {
        // TODO
        //let resource_areas = bw::resource_areas();
        //(*resource_areas).areas[(*town.0).resource_area as usize].flags &= !0x2;
    }
    let towns = crate::samase::active_towns().add(player_ai.1 as usize);
    let town_array = (*towns).full_array;
    ListEntry::move_to(town.0, &mut (*towns).first, &mut (*town_array).first_free);
}

/// Returns true if the town is ready to be deleted.
///
/// May move workers to another town.
unsafe fn prepare_town_delete(game: Game, player_ai: &PlayerAi, town: Town) -> bool {
    let has_workers = town.workers().next().is_some();
    let has_buildings = town.buildings().next().is_some();
    if !has_workers && !has_buildings {
        return true;
    }
    let has_buildings_left = town
        .buildings()
        .map(|ai| Unit::from_ptr((*ai).parent).expect("Parentless building ai"))
        .any(|unit| unit.id() != unit::OVERLORD);
    if has_buildings_left {
        return false;
    }

    // Move units to a new town
    let new_town = ListIter(bw::first_active_ai_town(town.player()))
        .filter(|&other| other != town.0)
        .min_by_key(|&other| bw::distance((*town.0).position, (*other).position))
        .map(|x| Town(x));
    let new_town = match new_town {
        Some(s) => s,
        None => return false,
    };
    for ai in town.workers() {
        let worker = Unit::from_ptr((*ai).parent).expect("Parentless worker ai");
        remove_worker_or_building_ai(game, player_ai, worker, false);
        add_worker_ai(game, worker, new_town);
    }
    for ai in town.buildings() {
        let building = Unit::from_ptr((*ai).parent).expect("Parentless building ai");
        remove_worker_or_building_ai(game, player_ai, building, false);
        let ai_regions = bw::ai_regions(building.player() as u32);
        add_building_ai(game, player_ai, ai_regions, building, new_town);
    }
    true
}

unsafe fn add_building_ai(
    game: Game,
    player_ai: &PlayerAi,
    ai_regions: *mut bw::AiRegion,
    unit: Unit,
    town: Town,
) {
    assert!(!unit.has_ai());
    let array = (*town.0).buildings.full_array;
    if (*array).first_free.is_null() {
        warn!("Building ai limit");
        return;
    }

    let ai = (*array).first_free;
    ListEntry::move_to(ai, &mut (*array).first_free, &mut (*town.0).buildings.first);
    (*ai).parent = *unit;
    (*ai).town = town.0;
    (*ai).ai_type = 0x3;
    (*ai).train_queue_types = [0; 5];
    (*ai).train_queue_values = [null_mut(); 5];
    (**unit).ai = ai as *mut c_void;
    match unit.id() {
        unit::HATCHERY | unit::LAIR | unit::HIVE | unit::CREEP_COLONY => {
            // Bw only does hatch/lair/hive, but might as well do creep colony as
            // it doesn't do anything important.
            if unit.is_completed() {
                unit.issue_order_ground(order::COMPUTER_AI, unit.position());
                unit.issue_secondary_order(order::SPREAD_CREEP);
            }
        }
        _ => (),
    }
    fn is_minor_building(unit_id: UnitId) -> bool {
        match unit_id {
            unit::MISSILE_TURRET |
            unit::BUNKER |
            unit::CREEP_COLONY |
            unit::SUNKEN_COLONY |
            unit::SPORE_COLONY |
            unit::PYLON |
            unit::PHOTON_CANNON => true,
            _ => false,
        }
    }
    if !player_ai.is_campaign() || (unit.is_landed_building() && !is_minor_building(unit.id())) {
        create_town_region(game, player_ai, ai_regions, unit);
    }
    if (*town.0).inited == 0 {
        (*town.0).inited = 1;
        (*town.0).gas_buildings = [null_mut(); 3];
    }
    if unit.id().is_town_hall() {
        (*town.0).main_building = *unit;
    } else if is_gas_building(unit.id()) {
        if let Some(slot) = (*town.0).gas_buildings.iter_mut().find(|x| x.is_null()) {
            *slot = *unit;
        }
        if (*town.0).resource_area != 0 {
            (*town.0).resource_units_not_set = 1;
        }
    }
}

unsafe fn create_town_region(
    game: Game,
    player_ai: &PlayerAi,
    ai_regions: *mut bw::AiRegion,
    unit: Unit,
) {
    let region = unit_ai_region(ai_regions, unit).expect("Unit out of bounds?");
    let is_campaign = player_ai.is_campaign();
    if (*region).state == 0 || (*region).state == 4 {
        bw::change_ai_region_state(region, 5);
        (*region).flags |= 0x1;
        if !is_campaign {
            (*region).flags |= 0x40;
            for other_player in 0..8 {
                if game.allied(other_player, player_ai.1) {
                    let other_regions = bw::ai_regions(other_player as u32);
                    if let Some(other_region) = unit_ai_region(other_regions, unit) {
                        if (*other_region).state == 0 {
                            bw::change_ai_region_state(other_region, 4);
                        }
                    }
                }
            }
        }
    }
    if (unit.is_landed_building() && unit.id().is_town_hall()) || unit.id().is_worker() {
        if is_campaign {
            let starting_strength = (*player_ai.0).default_min_strength_for_regions as u16 * 6;
            (*region).needed_ground_strength = starting_strength;
            (*region).needed_air_strength = starting_strength;
        } else {
            (*region).flags |= 0x40;
            (*region).needed_ground_strength = 1500;
            (*region).needed_air_strength = 1000;
        }
        bw::change_ai_region_state(region, 6);
        (*region).unk_count = 45;
    }
}

pub fn is_gas_building(unit_id: UnitId) -> bool {
    use bw_dat::unit::*;
    match unit_id {
        REFINERY | EXTRACTOR | ASSIMILATOR => true,
        _ => false,
    }
}

unsafe fn add_worker_ai(game: Game, unit: Unit, town: Town) {
    assert!(!unit.has_ai());
    let array = (*town.0).workers.full_array;
    if (*array).first_free.is_null() {
        warn!("Worker ai limit");
    } else {
        let ai = (*array).first_free;
        ListEntry::move_to(ai, &mut (*array).first_free, &mut (*town.0).workers.first);
        (*ai).parent = *unit;
        (*ai).town = town.0;
        (*ai).ai_type = 0x2;
        (*ai).target_resource = 0x1;
        (*ai).reassign_count = 0;
        (*ai).wait_timer = 0;
        (*ai).last_update_second = (**game).elapsed_seconds;
        (**unit).ai = ai as *mut c_void;
        (*town.0).worker_count = (*town.0).worker_count.saturating_add(1);
    }
}

// TODO NOT FULLY COMPLETE, NEED RESAREAS
pub fn remove_unit_ai(
    game: Game,
    unit_search: &UnitSearch,
    strength: &UnitStrengths,
    unit: Unit,
    was_killed: bool,
) {
    unsafe {
        let player_ai = PlayerAi::get(unit.player());
        remove_from_ai_structs(game, unit_search, strength, &player_ai, unit, was_killed);
        remove_worker_or_building_ai(game, &player_ai, unit, true);
        if let Some(ai) = unit.guard_ai() {
            (*ai).parent = null_mut();
            // Just do what bw does and let the guard ai hooks notice this.
            // And yes, += 1 even if it wasn't killed
            (*ai).times_died += 1;
            (**unit).ai = null_mut();
        }
        if (*player_ai.0).free_medic == *unit {
            (*player_ai.0).free_medic = active_units()
                .filter(|x| x.player() == unit.player())
                .find(|x| x.id() == unit::MEDIC && x.has_ai())
                .map(|x| *x)
                .unwrap_or_else(null_mut);
        }
        if unit.has_ai() {
            let ai = (**unit).ai as *mut bw::GuardAi;
            panic!(
                "Unit's AI was not removed.\n\
                Unit 0x{:x} @ {:?} order 0x{:x} player {}\n\
                Ai type {} pointer {:p},\n\
                Ai bytes {:x?},\n\
                Unit bytes {:x?}\n",
                unit.id().0,
                unit.position(),
                unit.order().0,
                unit.player(),
                (*ai).ai_type,
                ai,
                std::slice::from_raw_parts(ai as *const u8, 0x20),
                std::slice::from_raw_parts(*unit as *const u8, std::mem::size_of::<bw::Unit>()),
            );
        }
    }
}

pub unsafe extern fn step_region_hook(
    player: u32,
    region_id: u32,
    orig: unsafe extern fn(u32, u32),
) {
    let regions = bw::ai_regions(player);
    let region = regions.add(region_id as usize);
    if (*region).state == 6 {
        // Fix melee ai wanting to set attacked region need to 1000/1000
        // unconditionally, which is really dumb.
        let old_needed_ground = (*region).needed_ground_strength;
        let old_needed_air = (*region).needed_air_strength;
        orig(player, region_id);
        (*region).needed_ground_strength = old_needed_ground;
        (*region).needed_air_strength = old_needed_air;
    } else {
        orig(player, region_id);
    }
}

pub unsafe extern fn focus_disabled_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    let skip = {
        let globals = Globals::get("focus_air");
        globals.global_ai_mode.disable_spell_focus
    };
    if !skip {
        orig(u);
    }
}

pub unsafe extern fn focus_air_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    if let Some(unit) = Unit::from_ptr(u as *mut bw::Unit) {
        let globals = Globals::get("focus_air");
        if unit.acid_spore_count() >= 4 {
            if globals.global_ai_mode.disable_acid_spore_focus {
                return;
            }
        } else {
            if globals.global_ai_mode.disable_carrier_focus {
                return;
            }
        }
    }
    orig(u);
}

/// If the ai wants to unload at a really small region, search nearby regions
/// for a larger one
pub unsafe fn unload_target_fix(
    unit: Unit,
    game: Game,
    player_ai: &PlayerAi,
    regions: &mut pathing::RegionNeighbourSearch,
    unit_search: &LazyUnitSearch,
    strength: &LazyUnitStrengths,
    pathing: Pathing,
    unit_arr: &UnitArray,
    ai_regions: *mut bw::AiRegion,
) {
    if !unit.has_loaded_units() {
        return;
    }
    let target_pos = if unit.order() == order::UNLOAD {
        unit.position()
    } else {
        if unit.order_state() == 0 {
            unit.target().map(|x| x.position()).unwrap_or_else(|| unit.target_pos())
        } else {
            unit.target_pos()
        }
    };
    let region_id = match bw::get_region(target_pos) {
        Some(s) => s,
        None => return,
    };
    let region = match pathing.region(region_id) {
        Some(s) => s,
        None => return,
    };
    // Minimum size is going to be twice the size of largest unit in transport
    let min_size = unit.loaded_units(unit_arr)
        .map(|x| {
            let rect = x.id().dimensions();
            rect.width().max(rect.height()).saturating_mul(2)
        })
        .max()
        .unwrap_or(0);
    let area = region.area();
    if area.width() >= min_size &&
        area.height() >= min_size &&
        !is_degenerate_region(pathing, region)
    {
        // No need to change target
        return;
    }
    // If all units are guards assigned to the region, don't change target.
    // Assuming that they're preplaced guards and map creator wants
    // them to be there.
    // Think the game won't suddenly assign guards to awkward locations,
    // or at most very rarely?
    let all_guards = unit.loaded_units(unit_arr).all(|unit| {
        if let Some(ai) = unit.guard_ai() {
            let home = &(*ai).home;
            area.contains_point(home)
        } else {
            false
        }
    });
    if all_guards {
        return;
    }

    for (other_id, region) in regions.neighbours(pathing, region_id, 4) {
        if (**region).walkability == 0x1ffd {
            continue;
        }
        let area = region.area();
        if area.width() < min_size ||
            area.height() < min_size ||
            is_degenerate_region(pathing, region)
        {
            continue;
        }
        trace!(
            "Redirecting transport {} of player {}, {:?} -> {:?}",
            unit_arr.to_index(unit), unit.player(), target_pos, area.center(),
        );
        unit.issue_order_ground(order::MOVE_UNLOAD, area.center());
        // Move all ground units to this new region too
        let ai_region = ai_regions.add(region_id as usize);
        let new_region = ai_regions.add(other_id as usize);
        (*ai_region).needed_ground_strength = 0;
        // If this is attack region, move all units to a better one.
        // And switch new region to be attack grouping region
        let mut move_all = false;
        let is_attack_region = matches!((*ai_region).state, 1 | 2 | 8 | 9);
        if is_attack_region {
            let mut unit_ai = (*ai_region).military.first;
            while !unit_ai.is_null() {
                let next = (*unit_ai).next;
                if let Some(unit) = Unit::from_ptr((*unit_ai).parent) {
                    if !unit.is_air() {
                        move_all = true;
                        break;
                    }
                }
                unit_ai = next;
            }
        }

        let mut unit_ai = (*ai_region).military.first;
        while !unit_ai.is_null() {
            let next = (*unit_ai).next;
            if let Some(unit) = Unit::from_ptr((*unit_ai).parent) {
                if move_all || !unit.is_air() {
                    trace!(
                        "Moving military {} (unit id {}) to new region too",
                        unit_arr.to_index(unit), unit.id().0,
                    );
                    remove_unit_ai(game, unit_search.get(), strength.get(), unit, false);
                    add_military_ai(unit, new_region, true);
                    // TODO military ai step? since add_military_ai doesn't do that
                }
            }
            unit_ai = next;
        }
        if is_attack_region {
            bw::change_ai_region_state(new_region, (*ai_region).state as u32);
            (*new_region).target_region_id = (*ai_region).target_region_id;
            bw::change_ai_region_state(ai_region, 0);
            if (*player_ai.0).attack_grouping_region == region_id.wrapping_add(1) {
                (*player_ai.0).attack_grouping_region = other_id.wrapping_add(1);
                trace!(
                    "Changed attack grouping region {} -> {}",
                    region_id, other_id,
                );
            }
        }
        return;
    }
    trace!(
        "Wanted to redirect player {} transport {} from {:?}, but no regions larger than {}x{} \
        were found",
        unit.player(), unit_arr.to_index(unit), target_pos, min_size, min_size,
    );
}

/// Returns true for regions that have their area center (Not the actual center) in other region
/// So it is C or L shaped most likely.
fn is_degenerate_region(pathing: Pathing, region: Region) -> bool {
    let area = region.area();
    let center = area.center();
    pathing.region_for_point(&center) != Some(region)
}

#[test]
fn push_pop_requests() {
    use std::mem;

    let mut ai_data: bw::PlayerAiData = unsafe { mem::zeroed() };
    let inputs = vec![1, 3, 6, 8, 8, 5, 2, 7, 2, 6, 3, 9, 24, 1, 3, 2, 10];
    for i in 1..inputs.len() {
        let mut inputs: Vec<_> = (&inputs[..i]).into();
        let player_ai = PlayerAi(&mut ai_data, 0);
        for &val in &inputs {
            player_ai.push_request(bw::AiSpendingRequest {
                priority: val,
                ty: 0,
                id: 0,
                val: null_mut(),
            });
        }
        while !inputs.is_empty() {
            let first = player_ai.first_request().unwrap();
            player_ai.pop_request();
            assert_eq!(first.priority, inputs.iter().cloned().max().unwrap());
            let pos = inputs
                .iter()
                .cloned()
                .position(|x| x == first.priority)
                .unwrap();
            inputs.remove(pos);
        }
    }
}
