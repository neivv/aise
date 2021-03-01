use smallvec::SmallVec;

use bw_dat::{self, order, Game, TechId, Unit, UnitId, UpgradeId};

use crate::ai::{self, has_resources, Cost, PlayerAi};
use crate::aiscript::{AiMode, Town};
use crate::bw;
use crate::datreq::{check_dat_requirements, DatReq, ReadDatReqs};
use crate::list::ListIter;
use crate::unit::{self, UnitExt};
use crate::unit_search::UnitSearch;

pub unsafe fn frame_hook(game: Game, unit_search: &UnitSearch, ai_mode: &[AiMode; 8]) {
    for player in 0..8 {
        let ai_mode = &ai_mode[player as usize];
        let player_ai = PlayerAi::get(player);
        while let Some(request) = player_ai.first_request() {
            let can = can_satisfy_request(game, &player_ai, player, &request, ai_mode).is_ok();
            let mut handled = false;
            if can {
                // Handle building morphs since preplaced colonies don't check for requests
                // (Since their order switches to idle because they don't have a town on frame 0)
                // Could handle other requests as well, but no need at the moment.
                if handle_building_morph(game, &request, player) == Ok(()) {
                    handled = true;
                }
                // Handle military/guard training as well since train opcode is otherwise
                // bad at spreading work across buildings
                if handle_training(game, unit_search, &request, player) == Ok(()) {
                    handled = true;
                }
                if !handled {
                    break;
                }
            }
            let level = if request.ty == 5 {
                game.upgrade_level(player as u8, UpgradeId(request.id))
                    .saturating_add(1)
            } else {
                0
            };
            let cost = ai::request_cost(&request, level);
            player_ai.remove_resource_need(&cost, handled);
            player_ai.pop_request()
        }
    }
}

fn handle_building_morph(
    game: Game,
    request: &bw::AiSpendingRequest,
    player: u8,
) -> Result<(), ()> {
    if request.ty == 3 {
        let unit_id = UnitId(request.id);
        let cost = ai::unit_cost(unit_id);
        if has_resources(game, player, &cost) && game.unit_available(player, unit_id) {
            let town = Town(request.val as *mut bw::AiTown);
            let reqs = match bw::unit_dat_requirements(unit_id) {
                Some(s) => s,
                None => return Err(()),
            };
            let mut valid_unit_ids: SmallVec<[UnitId; 4]> = SmallVec::new();
            parent_unit_ids_for_request(reqs, 0, &mut valid_unit_ids);
            valid_unit_ids.retain(|&mut x| is_usable_unit_id_for_building_request(x, town));
            if valid_unit_ids.iter().any(|x| x.is_building()) {
                let unit = town
                    .buildings()
                    .map(|x| unsafe {
                        Unit::from_ptr((*x).parent).expect("Parentless building ai")
                    })
                    .find(|&unit| {
                        valid_unit_ids.iter().any(|&y| unit.matches_id(y)) &&
                            !is_busy(unit) &&
                            unsafe { check_dat_requirements(game, reqs, unit, 0) }
                    });
                if let Some(unit) = unit {
                    start_unit_building(game, unit, unit_id, &cost)?;
                    unit.issue_order_ground(order::BUILDING_MORPH, unit.position());
                    return Ok(());
                }
            }
        }
    }
    Err(())
}

fn handle_training(
    game: Game,
    unit_search: &UnitSearch,
    request: &bw::AiSpendingRequest,
    player: u8,
) -> Result<(), ()> {
    if request.ty == 1 || request.ty == 2 {
        let unit_id = UnitId(request.id);
        let cost = ai::unit_cost(unit_id);
        if has_resources(game, player, &cost) && game.unit_available(player, unit_id) {
            let reqs = match bw::unit_dat_requirements(unit_id) {
                Some(s) => s,
                None => return Err(()),
            };
            let mut valid_unit_ids: SmallVec<[UnitId; 4]> = SmallVec::new();
            parent_unit_ids_for_request(reqs, 0, &mut valid_unit_ids);
            let unit = unit::active_units()
                .filter(|&x| x.player() == player && x.first_queued_unit().is_none())
                .filter(|&x| unsafe { !(**x).ai.is_null() })
                .find(|&unit| {
                    valid_unit_ids.iter().any(|&y| unit.matches_id(y)) &&
                        unsafe { check_dat_requirements(game, reqs, unit, 0) }
                });
            if let Some(unit) = unit {
                if unit.id() == bw_dat::unit::HIGH_TEMPLAR ||
                    unit.id() == bw_dat::unit::DARK_TEMPLAR
                {
                    // todo. these should not go through start_unit_building, but
                    // just find a partner and order merge.
                    // BW should handle these fine though.
                    return Err(());
                }

                start_unit_building(game, unit, unit_id, &cost)?;
                if let Some(ai) = unit.building_ai() {
                    unsafe {
                        let slot = (**unit).current_build_slot as usize;
                        (*ai).train_queue_types[slot] = request.ty;
                        (*ai).train_queue_values[slot] = request.val;
                    }
                }
                match unit.id() {
                    bw_dat::unit::MUTALISK | bw_dat::unit::HYDRALISK => {
                        unsafe {
                            ai::remove_unit_ai(game, unit_search, unit, false);
                            if request.ty == 2 {
                                let ai = request.val as *mut bw::GuardAi;
                                assert!(!ai.is_null());
                                (*ai).home = (*ai).other_home;
                                (**unit).ai = ai as *mut _;
                                (*ai).parent = *unit;
                            } else {
                                assert_eq!(request.ty, 1);
                                let region = request.val as *mut bw::AiRegion;
                                assert!(!region.is_null());
                                ai::add_military_ai(unit, region, true);
                            }
                        }
                        unit.issue_order_ground(order::UNIT_MORPH, unit.position());
                    }
                    bw_dat::unit::LARVA => {
                        unit.issue_order_ground(order::UNIT_MORPH, unit.position());
                    }
                    _ => {
                        unit.issue_secondary_order(order::TRAIN);
                    }
                }
                return Ok(());
            }
        }
    }
    Err(())
}

pub unsafe fn can_satisfy_request(
    game: Game,
    player_ai: &PlayerAi,
    player: u8,
    request: &bw::AiSpendingRequest,
    ai_mode: &AiMode,
) -> Result<(), RequestSatisfyError> {
    let wait_resources = ai_mode.wait_for_resources;
    let town = match request.ty {
        // Overlords, upgrades, and techs can be done in any town,
        // even if their request has a town pointer.
        3 if UnitId(request.id) != bw_dat::unit::OVERLORD => {
            Town::from_ptr(request.val as *mut bw::AiTown)
        }
        4 | 7 => Town::from_ptr(request.val as *mut bw::AiTown),
        _ => None,
    };
    match request.ty {
        1 | 2 | 3 | 4 | 7 | 8 => {
            let unit_id = UnitId(request.id);
            if !game.unit_available(player, unit_id) {
                return Err(RequestSatisfyError::NotAvailable);
            }
            if player_ai.is_at_limit(unit_id, game) {
                return Err(RequestSatisfyError::BuildLimit);
            }
            if request.ty == 3 && !ai_mode.build_gas && ai::is_gas_building(unit_id) {
                let town = request.val as *mut bw::AiTown;
                let existing_gas_buildings = ListIter((*town).buildings)
                    .filter_map(|x| Unit::from_ptr((*x).parent))
                    .filter(|x| x.id() == unit_id)
                    .count();
                let explicitly_requested_buildings = (*town)
                    .town_units
                    .iter()
                    .filter(|x| x.flags_and_count & 0x6 == 0 && x.id == unit_id.0)
                    .map(|x| ((x.flags_and_count & 0xf8) >> 3) as usize)
                    .sum();
                if existing_gas_buildings >= explicitly_requested_buildings {
                    return Err(RequestSatisfyError::BuildLimit);
                }
            }
            if !wait_resources && !has_resources(game, player, &ai::unit_cost(unit_id)) {
                return Err(RequestSatisfyError::Resources);
            }
            let reqs = match bw::unit_dat_requirements(unit_id) {
                Some(s) => s,
                None => return Err(RequestSatisfyError::NeedDatReqs),
            };
            can_satisfy_dat_request(game, player, reqs, town, 0)
                .map_err(RequestSatisfyError::DatReq)?;
            if request.ty == 3 {
                let mut parents = SmallVec::new();
                parent_unit_ids_for_request(reqs, 0, &mut parents);
                let ok = parents.iter().any(|&unit_id| {
                    let town = Town(request.val as *mut bw::AiTown);
                    is_usable_unit_id_for_building_request(unit_id, town)
                });
                if !ok {
                    return Err(RequestSatisfyError::DatReq(vec![
                        DatReqSatisfyError::NeedUnit(*parents.get(0).unwrap_or(&UnitId(0))),
                    ]));
                }
            }
            Ok(())
        }
        5 => {
            let upgrade_id = UpgradeId(request.id);
            let current_level = game.upgrade_level(player as u8, upgrade_id);
            if game.upgrade_max_level(player, upgrade_id) <= current_level {
                return Err(RequestSatisfyError::NotAvailable);
            }
            if !wait_resources {
                let cost = ai::upgrade_cost(upgrade_id, current_level + 1);
                if !has_resources(game, player, &cost) {
                    return Err(RequestSatisfyError::Resources);
                }
            }
            let reqs = match bw::upgrade_dat_requirements(upgrade_id) {
                Some(s) => s,
                None => return Err(RequestSatisfyError::NeedDatReqs),
            };
            can_satisfy_dat_request(game, player, reqs, town, current_level)
                .map_err(RequestSatisfyError::DatReq)
        }
        6 => {
            let tech_id = TechId(request.id);
            if !game.tech_available(player, tech_id) {
                return Err(RequestSatisfyError::NotAvailable);
            }
            if !wait_resources && !has_resources(game, player, &ai::tech_cost(tech_id)) {
                return Err(RequestSatisfyError::Resources);
            }
            let reqs = match bw::tech_research_dat_requirements(tech_id) {
                Some(s) => s,
                None => return Err(RequestSatisfyError::NeedDatReqs),
            };
            can_satisfy_dat_request(game, player, reqs, town, 0)
                .map_err(RequestSatisfyError::DatReq)
        }
        _ => Ok(()),
    }
}

pub enum RequestSatisfyError {
    Resources,
    /// UMS disabled unit
    NotAvailable,
    /// define_max limit
    BuildLimit,
    /// One set of ors that couldn't be satisfied
    /// (Doesn't show rest if many)
    DatReq(Vec<DatReqSatisfyError>),
    /// Dat reqs are required for request type (Units default success on no reqs)
    NeedDatReqs,
}

#[derive(Copy, Clone)]
pub enum DatReqSatisfyError {
    /// This doesn't differentiate between current unit is/owns unit,
    /// but let's assume that the user can figure things out from the error.
    NeedUnit(UnitId),
    TooManyUnits(UnitId),
    NeedTech(TechId),
    NeedAddonless,
    NeedEmptySilo,
    NeedHangarSpace,
    /// Datreq is "always disabled/blank"
    Disabled,
}

enum MatchRequirement {
    Unit(UnitId),
    Addon(UnitId),
    HasHangarSpace,
    HasNoNuke,
    HasNoAddon,
    Or(Vec<MatchRequirement>),
}

impl MatchRequirement {
    fn matches(&self, unit: Unit) -> bool {
        use self::MatchRequirement::*;
        match self {
            Unit(id) => unit.matches_id(*id),
            Addon(id) => unit.addon().map(|x| x.matches_id(*id)).unwrap_or(false),
            HasHangarSpace => true, // TODO if ever cared
            HasNoNuke => !unit.has_nuke(),
            HasNoAddon => unit.addon().is_none(),
            Or(reqs) => reqs.iter().any(|x| x.matches(unit)),
        }
    }
}

fn is_busy(unit: Unit) -> bool {
    if unit.id().is_building() {
        unsafe {
            if unit.currently_building().is_some() {
                return true;
            }
            // Currently_building doesn't catch building morph,
            // so check also for first queued unit
            if unit.first_queued_unit().is_some() {
                return true;
            }
            let tech = TechId((**unit).unit_specific[0x8] as u16);
            let upgrade = UpgradeId((**unit).unit_specific[0x9] as u16);
            tech != bw_dat::tech::NONE || upgrade != bw_dat::upgrade::NONE
        }
    } else {
        // Don't consider workers ever busy for req satisfying
        false
    }
}

unsafe fn can_satisfy_dat_request(
    game: Game,
    player: u8,
    reqs: *const u16,
    town: Option<Town>,
    current_upgrade_level: u8,
) -> Result<(), Vec<DatReqSatisfyError>> {
    fn match_req(dat_req: &DatReq) -> Option<MatchRequirement> {
        match *dat_req {
            DatReq::HasHangarSpace => Some(MatchRequirement::HasHangarSpace),
            DatReq::HasNotNukeOnly => Some(MatchRequirement::HasNoNuke),
            DatReq::HasNoAddon => Some(MatchRequirement::HasNoAddon),
            DatReq::CurrentUnitIs(unit) => Some(MatchRequirement::Unit(unit)),
            DatReq::HasAddonAttached(addon) => Some(MatchRequirement::Addon(addon)),
            _ => None,
        }
    }

    let mut match_requirements: SmallVec<[(MatchRequirement, SmallVec<_>); 8]> = SmallVec::new();
    let mut dat_reqs: SmallVec<_> = SmallVec::new();
    let mut read = ReadDatReqs::new(reqs, current_upgrade_level);
    loop {
        read.next_dat_requirements(&mut dat_reqs);
        if dat_reqs.is_empty() {
            break;
        }
        let mut errors: SmallVec<[DatReqSatisfyError; 4]> = SmallVec::new();
        let pass = dat_reqs.iter().any(|req| {
            match *req {
                DatReq::Disabled | DatReq::Blank => {
                    errors.push(DatReqSatisfyError::Disabled);
                    false
                }
                DatReq::BwOnly => true, // w/e
                DatReq::IsTransport => true,
                DatReq::IsNotBusy => true,
                DatReq::IsNotConstructingAddon => true,
                DatReq::IsNotConstructingBuilding => true,
                DatReq::IsNotUpgrading => true,
                DatReq::IsLifted => true,
                DatReq::IsNotLifted => true,
                DatReq::IsNotTeching => true,
                DatReq::HasNoNydusExit => true,
                DatReq::NotBurrowedOnly => true,
                DatReq::BurrowedOnly => true,
                DatReq::NotLandedBuildingOnly => true,
                DatReq::LandedBuildingOnly => true,
                DatReq::CanMoveOnly => true,
                DatReq::CanAttackOnly => true,
                DatReq::SubunitOnly => true,
                DatReq::WorkerOnly => true,
                DatReq::FlyingBuildingOnly => true,
                DatReq::PowerupOnly => true,
                DatReq::HasSpiderMinesOnly => true,
                DatReq::CanHoldPositionOnly => true,
                DatReq::AllowOnHallucinations => true,
                DatReq::TechOnly(tech) | DatReq::TechResearched(tech) => {
                    let pass = game.tech_researched(player, tech);
                    if !pass {
                        errors.push(DatReqSatisfyError::NeedTech(tech));
                    }
                    pass
                }
                DatReq::HasUnit(unit) => {
                    let pass = game.unit_count(player, unit) != 0;
                    if !pass {
                        errors.push(DatReqSatisfyError::NeedUnit(unit));
                    }
                    pass
                }
                DatReq::Unit(unit) => {
                    let pass = game.completed_count(player, unit) != 0;
                    if !pass {
                        errors.push(DatReqSatisfyError::NeedUnit(unit));
                    }
                    pass
                }
                DatReq::UnitCountLessThan(unit, count) => {
                    let pass = game.unit_count(player, unit) < count.into();
                    if !pass {
                        errors.push(DatReqSatisfyError::TooManyUnits(unit));
                    }
                    pass
                }
                DatReq::Unknown(id) => {
                    warn!("Unknown req ty {:x}", id);
                    true
                }
                // Match reqs return false here, they are cheked only if didn't pass
                DatReq::CurrentUnitIs(_) => false,
                DatReq::HasHangarSpace => false,
                DatReq::HasNotNukeOnly => false,
                DatReq::HasNoAddon => false,
                DatReq::HasAddonAttached(_) => false,
            }
        });
        if !pass {
            // Can (and should) skip adding match reqs if already passed
            let match_req_count = dat_reqs.iter().filter(|x| match_req(x).is_some()).count();
            match match_req_count {
                0 => {
                    assert!(!errors.is_empty());
                    return Err(errors.into_iter().collect());
                }
                1 => {
                    if let Some(req) = dat_reqs.iter().filter_map(|x| match_req(x)).next() {
                        match_requirements.push((req, errors));
                    }
                }
                _ => {
                    let reqs = dat_reqs.iter().filter_map(|x| match_req(x)).collect();
                    match_requirements.push((MatchRequirement::Or(reqs), errors));
                }
            };
        };
    }
    let ok = unit::active_units()
        .filter(|x| x.player() == player && x.is_completed())
        .filter(|&x| !is_busy(x))
        .filter(|&x| {
            // If a town is specified, the constructing unit must be a
            // worker/building within the town.
            match town {
                Some(town) => match x.building_ai() {
                    Some(ai) => (*ai).town == town.0,
                    None => match x.worker_ai() {
                        Some(ai) => (*ai).town == town.0,
                        None => false,
                    },
                },
                None => true,
            }
        })
        .filter(|&x| match_requirements.iter().all(|r| r.0.matches(x)))
        .next()
        .is_some();
    if ok {
        Ok(())
    } else {
        let errors = match match_requirements.into_iter().next() {
            Some(s) => {
                fn match_req_to_error(req: &MatchRequirement, out: &mut Vec<DatReqSatisfyError>) {
                    let err = match req {
                        MatchRequirement::Unit(unit) | MatchRequirement::Addon(unit) => {
                            DatReqSatisfyError::NeedUnit(*unit)
                        }
                        MatchRequirement::HasHangarSpace => DatReqSatisfyError::NeedHangarSpace,
                        MatchRequirement::HasNoNuke => DatReqSatisfyError::NeedEmptySilo,
                        MatchRequirement::HasNoAddon => DatReqSatisfyError::NeedAddonless,
                        MatchRequirement::Or(list) => {
                            for err in list {
                                match_req_to_error(err, out);
                            }
                            return;
                        }
                    };
                    out.push(err);
                }
                let mut errors = Vec::with_capacity(4);
                match_req_to_error(&s.0, &mut errors);
                errors.extend(s.1);
                errors
            }
            // No real requirements, so any nonbusy unit would be fine..
            // Yet the player doesn't have any.
            None => vec![DatReqSatisfyError::NeedUnit(bw_dat::unit::ANY_UNIT)],
        };
        Err(errors)
    }
}

/// Starts unit building immediately. That is, it cannot be used with workers starting
/// construction.
///
/// Does not do anything ai-specific either.
fn start_unit_building(game: Game, unit: Unit, unit_id: UnitId, cost: &Cost) -> Result<(), ()> {
    let slot = match unit.empty_build_slot() {
        Some(s) => s as usize,
        None => return Err(()),
    };
    if !unit.is_completed() {
        return Err(());
    }
    let player = unit.player();
    if !has_resources(game, player, cost) {
        return Err(());
    }
    unsafe {
        (**unit).build_queue[slot] = unit_id.0;
    }
    game.reduce_minerals(player, cost.minerals);
    game.reduce_gas(player, cost.gas);
    Ok(())
}

fn parent_unit_ids_for_request(
    reqs: *const u16,
    upgrade_level: u8,
    out: &mut SmallVec<[UnitId; 4]>,
) {
    out.clear();
    let mut dat_reqs: SmallVec<_> = SmallVec::new();
    let mut read = unsafe { ReadDatReqs::new(reqs, upgrade_level) };
    loop {
        read.next_dat_requirements(&mut dat_reqs);
        if dat_reqs.is_empty() {
            break;
        }
        for req in dat_reqs.iter() {
            if let DatReq::CurrentUnitIs(unit) = req {
                out.push(*unit);
            }
        }
    }
}

fn is_usable_unit_id_for_building_request(unit_id: UnitId, town: Town) -> bool {
    use bw_dat::unit;
    let is_vanilla_morphable = match unit_id {
        unit::CREEP_COLONY | unit::HATCHERY | unit::LAIR | unit::SPIRE => true,
        _ => false,
    };
    // NOTE: using count_town_units is technically wrong since it includes sunkens/spores
    // as creeps etc, but it'll fail at other parts of the request check if there isn't
    // a creep when we need one. And hopefully this is just a short-term solution..
    let count = ai::count_town_units(town, unit_id, true);
    if !unit_id.is_building() || is_vanilla_morphable {
        count != 0
    } else {
        // Check to not morph, say sunkens away to spores if a mod enables it
        // but town has no extra sunkens
        // Morphing creeps away is fine, that's expected by BW.
        let requested = town
            .requests()
            .filter(|x| x.flags_and_count & 0x6 == 0) // Unit
            .filter(|x| x.id == unit_id.0)
            .map(|x| (x.flags_and_count & 0xf8) >> 3)
            .max()
            .unwrap_or(0);
        count > requested.into()
    }
}
