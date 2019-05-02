use std::mem;

use libc::c_void;

use bw_dat::{tech, unit, upgrade, TechId, UnitId, UpgradeId};

use crate::ai::{self, PlayerAi};
use crate::ai_spending::{DatReqSatisfyError, RequestSatisfyError};
use crate::aiscript::Town;
use crate::bw;
use crate::game::Game;
use crate::globals::Globals;
use crate::list::ListIter;
use crate::unit::{active_units, Unit};

use super::support::UiList;
use super::ui::Page;
use super::{tech_name, unit_name, upgrade_name, UiInput};

pub struct MilitaryRequests {
    player: u8,
}

impl MilitaryRequests {
    pub fn input(&mut self, value: char) -> UiInput {
        match value {
            'q' => {
                self.player = self.player.saturating_sub(1);
                UiInput::Handled
            }
            'w' => {
                self.player = self.player.saturating_add(1).min(7);
                UiInput::Handled
            }
            _ => UiInput::NotHandled,
        }
    }

    pub fn draw_page(&mut self, page: &mut Page, globals: &Globals) {
        page.clear();

        let game = Game::get();
        let player_ai = PlayerAi::get(self.player);
        page.push(format!("Player {}", self.player));
        unsafe {
            if (*player_ai.0).train_unit_id != 0 {
                page.push("Training:");
                let mut indent = page.indent(4);
                let page = indent.page();
                let unit_id = UnitId((*player_ai.0).train_unit_id - 1);
                military_line(page, game, globals, self.player, unit_id);
            }
            if (*player_ai.0).attack_grouping_region != 0 {
                let region = bw::ai_regions(self.player as u32)
                    .add((*player_ai.0).attack_grouping_region as usize - 1);
                let attack_force = missing_attack_force_units(&player_ai, region);
                if !attack_force.is_empty() {
                    page.push("Attack force:");
                    let mut indent = page.indent(4);
                    let page = indent.page();
                    for &unit_id in &attack_force {
                        military_line(page, game, globals, self.player, unit_id);
                    }
                }
            }
            let mut defense_builds = (*player_ai.0)
                .ground_vs_air_build_def
                .iter()
                .chain((*player_ai.0).ground_vs_air_build_def.iter())
                .chain((*player_ai.0).air_vs_ground_build_def.iter())
                .chain((*player_ai.0).air_vs_air_build_def.iter())
                .cloned()
                .filter(|&x| x != 0)
                .map(|x| UnitId(x - 1))
                .collect::<Vec<_>>();
            defense_builds.sort();
            defense_builds.dedup();
            if !defense_builds.is_empty() {
                page.push("Defense:");
                let mut indent = page.indent(2);
                let page = indent.page();
                for &unit_id in &defense_builds {
                    military_line(page, game, globals, self.player, unit_id);
                }
            }
        }
    }
}

unsafe fn military_line(
    page: &mut Page,
    game: Game,
    globals: &Globals,
    player: u8,
    unit_id: UnitId,
) {
    if unit_id.0 >= unit::NONE.0 {
        return;
    }
    page.push(unit_name(unit_id));

    let mut indent = page.indent(2);
    let page = indent.page();
    let request = bw::AiSpendingRequest {
        priority: 0,
        ty: 1,
        id: unit_id.0,
        val: bw::ai_regions(player as u32) as *mut c_void,
    };
    request_line(page, game, globals, player, &request);
}

/// Returns informative name for the request
fn town_request_name(req: &TownRequest) -> Option<String> {
    match req.ty {
        RequestType::Unit(u) => {
            if u.0 >= unit::NONE.0 {
                None
            } else {
                Some(format!("{} {}", req.count, unit_name(u)))
            }
        }
        RequestType::Upgrade(u) => {
            if u.0 >= upgrade::NONE.0 {
                None
            } else {
                Some(format!("{} level {}", upgrade_name(u), req.count))
            }
        }
        RequestType::Tech(u) => {
            if u.0 >= tech::NONE.0 {
                None
            } else {
                Some(format!("{}", tech_name(u)))
            }
        }
    }
}

unsafe fn town_request_line(
    page: &mut Page,
    game: Game,
    globals: &Globals,
    town: Town,
    req: &TownRequest,
) {
    match town_request_name(req) {
        Some(s) => page.push(s),
        None => return,
    }
    let mut indent = page.indent(2);
    let page = indent.page();
    let request = bw::AiSpendingRequest {
        priority: 0,
        ty: match req.ty {
            RequestType::Unit(u) => match u.is_worker() {
                true => 4,
                false => 3,
            },
            RequestType::Tech(_) => 6,
            RequestType::Upgrade(_) => 5,
        },
        id: match req.ty {
            RequestType::Unit(u) => u.0,
            RequestType::Tech(u) => u.0,
            RequestType::Upgrade(u) => u.0,
        },
        val: town.0 as *mut c_void,
    };
    request_line(page, game, globals, (*town.0).player, &request);
}

unsafe fn request_line(
    page: &mut Page,
    game: Game,
    globals: &Globals,
    player: u8,
    request: &bw::AiSpendingRequest,
) {
    let result = crate::ai_spending::can_satisfy_request(
        game,
        player,
        &request,
        &globals.ai_mode[player as usize],
    );
    if let Err(e) = result {
        let msg = match e {
            RequestSatisfyError::Resources => "Not enough resources/supply",
            RequestSatisfyError::NotAvailable => "Disabled in map settings",
            RequestSatisfyError::BuildLimit => "Reached build limit",
            RequestSatisfyError::NeedDatReqs => "Dat requirements are empty",
            RequestSatisfyError::DatReq(errs) => {
                if errs.len() != 1 {
                    page.push("Couldn't satisfy any of these requirements:");
                }
                for e in errs {
                    let msg = match e {
                        DatReqSatisfyError::NeedUnit(unit) => format!("Need {}", unit_name(unit)),
                        DatReqSatisfyError::NeedTech(tech) => format!("Need {}", tech_name(tech)),
                        DatReqSatisfyError::NeedAddonless => "Need addonless unit".into(),
                        DatReqSatisfyError::NeedEmptySilo => "Need empty silo".into(),
                        DatReqSatisfyError::NeedHangarSpace => "Need hangar space".into(),
                        DatReqSatisfyError::Disabled => "(Datreq is always disabled)".into(),
                    };
                    page.push(msg);
                }
                return;
            }
        };
        page.push(msg);
    } else {
        let upgrade_level = if request.ty == 5 {
            game.upgrade_level(player, UpgradeId(request.id))
                .saturating_add(1)
        } else {
            0
        };
        if !ai::has_resources(game, player, &ai::request_cost(&request, upgrade_level)) {
            page.push("Not enough resources/supply");
        }
    }
}

pub unsafe fn missing_attack_force_units(ai: &PlayerAi, region: *mut bw::AiRegion) -> Vec<UnitId> {
    if (*region).state != 8 {
        return Vec::new();
    }
    let mut attack_force = (*ai.0)
        .attack_force
        .iter()
        .cloned()
        .take_while(|&x| x != 0)
        .map(|x| UnitId(x - 1))
        .collect::<Vec<_>>();
    for military_ai in ListIter((*region).military.first) {
        let unit = Unit::from_ptr((*military_ai).parent).expect("Broken military ai");
        if let Some(pos) = attack_force.iter().position(|&x| unit.id() == x) {
            attack_force.swap_remove(pos);
        }
    }
    // Also remove ones that are being trained
    let building_ais = active_units()
        .filter(|x| x.player() == ai.1)
        .flat_map(|x| x.building_ai());
    for building_ai in building_ais {
        for i in 0..5 {
            let matching = (*building_ai).train_queue_types[i] == 1 &&
                (*building_ai).train_queue_values[i] == region as *mut c_void;
            if matching {
                let unit = Unit::from_ptr((*building_ai).parent).expect("Broken building ai");
                let unit_id = UnitId((*unit.0).build_queue[i]);
                if let Some(pos) = attack_force.iter().position(|&x| unit_id == x) {
                    attack_force.swap_remove(pos);
                }
            }
        }
    }
    attack_force
}

enum RequestSatisfied {
    Yes,
    No,
    /// The current town is working towards this request.
    /// (All units/etc have already been started, just need time)
    WorkingHere,
    /// Another town is working towards this request.
    WorkingElsewhere,
}

fn is_request_satisfied(game: Game, town: Town, req: &TownRequest) -> RequestSatisfied {
    let player = unsafe { (*town.0).player };
    match req.ty {
        RequestType::Unit(unit_id) => {
            let count = if req.only_if_needed {
                ai::request_equivalent_unit_ids(unit_id)
                    .into_iter()
                    .map(|unit_id| game.completed_count(player, unit_id))
                    .sum::<u32>()
            } else {
                ai::count_town_units(town, unit_id, true)
            };
            if count >= req.count as u32 {
                return RequestSatisfied::Yes;
            }
            let incomplete_current_town = ai::count_town_units(town, unit_id, false) -
                ai::count_town_units(town, unit_id, true);
            if count + incomplete_current_town >= req.count as u32 {
                RequestSatisfied::WorkingHere
            } else if req.only_if_needed && game.unit_count(player, unit_id) >= req.count as u32 {
                RequestSatisfied::WorkingElsewhere
            } else {
                RequestSatisfied::No
            }
        }
        RequestType::Upgrade(upgrade_id) => {
            let current_level = game.upgrade_level(player, upgrade_id);
            if current_level >= req.count {
                RequestSatisfied::Yes
            } else {
                let working = active_units()
                    .any(|x| x.player() == player && x.upgrade_in_progress() == upgrade_id);
                if working {
                    // Upgrades/techs aren't really associated with a town,
                    // so don't report WorkingElsewhere
                    if current_level == req.count - 1 {
                        // Shows working on lower levels as "not working on", I guess
                        // that makes most sense.
                        RequestSatisfied::WorkingHere
                    } else {
                        RequestSatisfied::No
                    }
                } else {
                    RequestSatisfied::No
                }
            }
        }
        RequestType::Tech(tech_id) => {
            if game.tech_researched(player, tech_id) {
                RequestSatisfied::Yes
            } else {
                let working =
                    active_units().any(|x| x.player() == player && x.tech_in_progress() == tech_id);
                if working {
                    // Upgrades/techs aren't really associated with a town
                    // so don't report WorkingElsewhere
                    RequestSatisfied::WorkingHere
                } else {
                    RequestSatisfied::No
                }
            }
        }
    }
}

pub struct TownRequests {
    towns: UiList<*mut bw::AiTown>,
}

struct TownRequest {
    count: u8,
    only_if_needed: bool,
    ty: RequestType,
    priority: u8,
}

enum RequestType {
    Unit(UnitId),
    Upgrade(UpgradeId),
    Tech(TechId),
}

impl TownRequests {
    pub fn draw_page(&mut self, page: &mut Page, globals: &Globals) {
        page.clear();
        self.towns.update();
        let town = match self.towns.current() {
            Some(&s) => s,
            None => return,
        };

        let game = Game::get();
        let town_array = bw::town_array_start();
        unsafe {
            let town_id = (town as usize - town_array as usize) / mem::size_of::<bw::AiTown>();
            page.push(format!(
                "Town #{}/{} id {:x}: {:p} player {:x} pos ({}, {})",
                self.towns.current_pos() + 1,
                self.towns.len(),
                town_id,
                town,
                (*town).player,
                (*town).position.x,
                (*town).position.y,
            ));
            page.push("Incomplete requests:");
            let mut indent = page.indent(2);
            let page = indent.page();
            // Keep duplicate/reduntant requests due to priorities.

            let mut requests = (*town)
                .town_units
                .iter()
                .take_while(|&x| x.flags_and_count != 0)
                .map(|x| TownRequest {
                    count: (x.flags_and_count & 0xf8) >> 3,
                    only_if_needed: x.flags_and_count & 0x1 != 0,
                    ty: match x.flags_and_count & 0x6 {
                        0x2 => RequestType::Upgrade(UpgradeId(x.id)),
                        0x4 => RequestType::Tech(TechId(x.id)),
                        _ => RequestType::Unit(UnitId(x.id)),
                    },
                    priority: x.priority,
                })
                .collect::<Vec<_>>();
            requests.sort_by(|a, b| b.priority.cmp(&a.priority));
            for req in &requests {
                let town = Town(town);
                match is_request_satisfied(game, town, req) {
                    RequestSatisfied::Yes => (),
                    RequestSatisfied::No => town_request_line(page, game, globals, town, req),
                    RequestSatisfied::WorkingHere => {
                        if let Some(name) = town_request_name(req) {
                            page.push(format!("{} (Being worked on)", name));
                        }
                    }
                    RequestSatisfied::WorkingElsewhere => {
                        if let Some(name) = town_request_name(req) {
                            page.push(format!("{} (Being worked on at another town)", name));
                        }
                    }
                }
            }
        }
    }

    pub fn input(&mut self, value: char) -> UiInput {
        match value {
            'q' => {
                self.towns.page_back(1);
                UiInput::Handled
            }
            'w' => {
                self.towns.page_forward(1);
                UiInput::Handled
            }
            _ => UiInput::NotHandled,
        }
    }
}

pub struct AiRequests {
    pub military: MilitaryRequests,
    pub towns: TownRequests,
}

impl AiRequests {
    pub fn new() -> AiRequests {
        AiRequests {
            military: MilitaryRequests {
                player: 0,
            },
            towns: TownRequests {
                towns: UiList::new(),
            },
        }
    }
}
