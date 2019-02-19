use std::mem;
use std::ptr::null_mut;

use libc::c_void;

use bw_dat::{order, unit, RaceFlags, TechId, UnitId, UpgradeId};

use bw;
use game::{Game, Race};
use list::{ListEntry, ListIter};
use unit::{active_units, Unit};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlayerAi(pub *mut bw::PlayerAiData, pub u8);

impl PlayerAi {
    pub fn get(player: u8) -> PlayerAi {
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
            if let Some(region) = unit_ai_region(self.1, unit) {
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

    fn is_at_limit(&self, unit: UnitId, game: Game) -> bool {
        match unsafe { (*self.0).build_limits[unit.0 as usize] } {
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
        let requests = unsafe { &mut (*self.0).requests[..] };
        let mut pos = unsafe {
            if usize::from((*self.0).request_count) < requests.len() {
                (*self.0).request_count += 1;
            }
            isize::from((*self.0).request_count) - 1
        };
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
                let morph_unit =
                    unsafe { UnitId((*unit.0).build_queue[(*unit.0).current_build_slot as usize]) };
                if morph_unit == unit_id {
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
    morphing + existing
}

pub struct Cost {
    pub minerals: u32,
    pub gas: u32,
    pub supply: u32,
    pub races: RaceFlags,
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

pub fn upgrade_cost(upgrade: UpgradeId) -> Cost {
    Cost {
        minerals: upgrade.mineral_cost(),
        gas: upgrade.gas_cost(),
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

pub fn unit_ai_region(player: u8, unit: Unit) -> Option<*mut bw::AiRegion> {
    ai_region(player, unit.position())
}

pub fn ai_region(player: u8, position: bw::Point) -> Option<*mut bw::AiRegion> {
    let regions = bw::ai_regions(player.into());
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
    pub fn new() -> GuardState {
        // At least as of 1.22 there's still just 1000 guards?
        GuardState {
            guards: vec![Default::default(); 1000],
        }
    }

    fn guard(&mut self, array: *mut bw::GuardAi, ai: *mut bw::GuardAi) -> &mut Guard {
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
        if guard.deaths == 0 || guard.priority == 0 {
            60
        } else {
            guard.priority
        }
    }
}

pub unsafe fn update_guard_needs(game: Game, guards: &mut GuardState) {
    let guard_array = (*bw::guard_array()).ais.as_mut_ptr();
    let seconds = (*game.0).elapsed_seconds;
    for player in 0..8 {
        let ai = PlayerAi::get(player);
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
                    if let Some(region) = ai_region(player, (*guard).other_home) {
                        if region_can_rebuild_guards(region) {
                            if !is_guard_being_trained(player, guard) {
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

fn is_guard_being_trained(player: u8, guard: *mut bw::GuardAi) -> bool {
    for unit in active_units().filter(|x| x.player() == player) {
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
            let free_scvs = ListIter((*town).workers)
                .map(|x| Unit((*x).parent))
                .filter(|x| x.id() == unit::SCV && x.order() == order::COMPUTER_AI);
            let incomplete_buildings = ListIter((*town).buildings)
                .map(|x| Unit((*x).parent))
                .filter(|&x| {
                    !x.is_completed() &&
                        (*x.0).related.is_null() &&
                        x.id().group_flags() & 0x2 != 0 &&
                        is_building_safe(x, regions)
                });
            for (scv, building) in free_scvs.zip(incomplete_buildings) {
                scv.issue_order_unit(order::CONSTRUCTING_BUILDING, building);
                (*building.0).related = scv.0;
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
        assert!(unit.player() < 8);
        let dest = crate::samase::guard_ais().offset(unit.player() as isize);
        ListEntry::move_to(ai, &mut (*array).first_free, &mut (*dest).first);
        Some(ai)
    }
}

/// Uses an existing needed or creates a new guard ai for the unit.
/// Usually likely creates since there isn't one at precisely where the unit stands.
pub unsafe fn add_guard_ai(unit: Unit) {
    assert!((*unit.0).ai.is_null());
    // Ai flag for "don't become guard"
    if unit.id().ai_flags() & 0x2 != 0 {
        return;
    }
    assert!(!unit.id().is_building());
    if let Some(ai) = get_matching_guard_ai(unit) {
        (*ai).parent = unit.0;
        (*ai).unit_id = unit.id().0;
        (*ai).home = unit.position();
        (*ai).other_home = unit.position();
        (*ai).times_died = 0;
        (*unit.0).ai = ai as *mut c_void;
    } else {
        warn!("Guard ai limit");
    }
}

/// NOTE: Differs from bw function in that it doesn't immediatly do one frame step.
/// If this is called somewhere else than just zerg birth order, it should be done afterwards.
pub unsafe fn add_military_ai(unit: Unit, region: *mut bw::AiRegion, always_this_region: bool) {
    assert!((*unit.0).ai.is_null());
    let region = if !always_this_region && (*region).state == 3 {
        ai_region(unit.player(), unit.position()).expect("Unit out of bounds??")
    } else {
        region
    };

    let array = (*region).military.array;
    let ai = (*array).first_free;
    if ai.is_null() {
        warn!("Military ai limit");
        return;
    }
    ListEntry::move_to(ai, &mut (*array).first_free, &mut (*region).military.first);
    (*ai).ai_type = 4; // Unnecessary?
    (*ai).parent = unit.0;
    (*ai).region = region;
    (*unit.0).ai = ai as *mut c_void;
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
    let mut ground_speed = u32::max_value();
    let mut air_speed = u32::max_value();

    if let Some(unit) = Unit::from_ptr((*region).slowest_military) {
        if unit.is_air() {
            slowest_air = Some(unit);
            air_speed = (*unit.0).flingy_top_speed;
        } else {
            slowest_ground = Some(unit);
            ground_speed = (*unit.0).flingy_top_speed;
        }
    }
    for ai in ListIter((*region).military.first) {
        let unit = Unit::from_ptr((*ai).parent).expect("Parentless military ai");
        let speed = (*unit.0).flingy_top_speed;
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
        .map(|x| x.0)
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
