use std::ptr::null_mut;

use smallvec::SmallVec;

use bw_dat::{self, order, TechId, UnitId, UpgradeId};

use crate::ai::{self, has_resources, Cost, PlayerAi};
use crate::aiscript::{AiMode, Town};
use crate::bw;
use crate::datreq::{check_dat_requirements, DatReq, ReadDatReqs};
use crate::game::Game;
use crate::list::ListIter;
use crate::unit::{self, Unit};

pub unsafe fn frame_hook(ai_mode: &[AiMode; 8]) {
    let game = Game::get();
    for player in 0..8 {
        let ai_mode = &ai_mode[player as usize];
        let player_ai = PlayerAi::get(player);
        while let Some(request) = player_ai.first_request() {
            let can = can_satisfy_request(game, player, &request, ai_mode);
            if can {
                let mut handled = false;
                // Handle building morphs since preplaced colonies don't check for requests
                // (Since their order switches to idle because they don't have a town on frame 0)
                // Could handle other requests as well, but no need at the moment.
                if handle_building_morph(game, &request, player) == Ok(()) {
                    handled = true;
                }
                if !handled {
                    break;
                }
            }
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
            if valid_unit_ids.iter().any(|x| x.is_building()) {
                let unit = town
                    .buildings()
                    .map(|x| {
                        Unit::from_ptr(unsafe { (*x).parent }).expect("Parentless building ai")
                    })
                    .find(|&unit| {
                        valid_unit_ids.iter().any(|&y| unit.matches_id(y)) &&
                            !unit.is_constructing_building() &&
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

fn is_gas_building(unit_id: UnitId) -> bool {
    use bw_dat::unit::*;
    match unit_id {
        REFINERY | EXTRACTOR | ASSIMILATOR => true,
        _ => false,
    }
}

unsafe fn can_satisfy_request(
    game: Game,
    player: u8,
    request: &bw::AiSpendingRequest,
    ai_mode: &AiMode,
) -> bool {
    let wait_resources = ai_mode.wait_for_resources;
    match request.ty {
        1 | 2 | 3 | 4 => {
            let unit_id = UnitId(request.id);
            if request.ty == 3 && !ai_mode.build_gas && is_gas_building(unit_id) {
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
                    return false;
                }
            }
            if !wait_resources && !has_resources(game, player, &ai::unit_cost(unit_id)) {
                return false;
            }
            let reqs = match bw::unit_dat_requirements(unit_id) {
                Some(s) => s,
                None => return true,
            };
            can_satisfy_dat_request(game, player, reqs, 0)
        }
        5 => {
            let upgrade_id = UpgradeId(request.id);
            if !wait_resources && !has_resources(game, player, &ai::upgrade_cost(upgrade_id)) {
                return false;
            }
            let mut reqs = match bw::upgrade_dat_requirements(upgrade_id) {
                Some(s) => s,
                None => return true,
            };
            let level = game.upgrade_level(player as u8, upgrade_id);
            can_satisfy_dat_request(game, player, reqs, level)
        }
        6 => {
            let tech_id = TechId(request.id);
            if !wait_resources && !has_resources(game, player, &ai::tech_cost(tech_id)) {
                return false;
            }
            let mut reqs = match bw::tech_research_dat_requirements(tech_id) {
                Some(s) => s,
                None => return true,
            };
            can_satisfy_dat_request(game, player, reqs, 0)
        }
        _ => true,
    }
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
            if (*unit.0).currently_building != null_mut() {
                return true;
            }
            let tech = TechId((*unit.0).unit_specific[0x8] as u16);
            let upgrade = UpgradeId((*unit.0).unit_specific[0x9] as u16);
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
    upgrade_level: u8,
) -> bool {
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

    let mut match_requirements: SmallVec<[MatchRequirement; 8]> = SmallVec::new();
    let mut dat_reqs: SmallVec<_> = SmallVec::new();
    let mut read = ReadDatReqs::new(reqs, upgrade_level);
    loop {
        read.next_dat_requirements(&mut dat_reqs);
        if dat_reqs.is_empty() {
            break;
        }
        let pass = dat_reqs.iter().any(|req| {
            match *req {
                DatReq::Disabled => false,
                DatReq::Blank => false,
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
                DatReq::TechOnly(tech) => game.tech_researched(player, tech),
                DatReq::TechResearched(tech) => game.tech_researched(player, tech),
                DatReq::HasUnit(unit) => game.unit_count(player, unit) != 0,
                DatReq::Unit(unit) => game.completed_count(player, unit) != 0,
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
                0 => return false,
                1 => {
                    if let Some(req) = dat_reqs.iter().filter_map(|x| match_req(x)).next() {
                        match_requirements.push(req);
                    }
                }
                _ => {
                    let reqs = dat_reqs.iter().filter_map(|x| match_req(x)).collect();
                    match_requirements.push(MatchRequirement::Or(reqs));
                }
            };
        };
    }
    unit::active_units()
        .filter(|x| x.player() == player && x.is_completed())
        .filter(|&x| !is_busy(x))
        .filter(|&x| match_requirements.iter().all(|r| r.matches(x)))
        .next()
        .is_some()
}

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
        (*unit.0).build_queue[slot] = unit_id.0;
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
