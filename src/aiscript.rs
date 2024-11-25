use std::ffi::CString;
use std::fmt;
use std::fs::{self, File};
use std::mem;
use std::path::{Component, Path, PathBuf};
use std::ptr::null_mut;
use std::slice;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use directories::UserDirs;
use parking_lot::Mutex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use bw_dat::{Game, TechId, Unit, UnitId, UpgradeId};

use crate::ai::{self};
use crate::block_alloc::BlockAllocSet;
use crate::bw;
use crate::datreq::check_dat_requirements;
use crate::feature_disabled;
use crate::globals::{
    self, BankKey, BaseLayout, BuildMax, BunkerCondition, BunkerDecl, Globals,
    LiftLandBuilding, Queues, RenameStatus, RevealState, RevealType, UnitQueue,
    UnitReplace,
};
use crate::list::ListIter;
use crate::order::{self, OrderId};
use crate::rng::Rng;
use crate::samase;
use crate::swap_retain::SwapRetain;
use crate::unit::{self, UnitExt};
use crate::unit_search::UnitSearch;

#[cfg(target_pointer_width = "32")]
use crate::globals::{LiftLand, LiftLandStage};


pub fn init_save_mapping() {}

pub fn clear_save_mapping() {}

pub fn init_load_mapping() {}

pub fn clear_load_mapping() {}

// Cache unit search accesses from script commands of same frame, as the script commands
// won't change unit positioning (Except create_unit, but oh well).
// CACHED_UNIT_SEARCH_FRAME has to be set to !0 on loading and game init to surely
// invalidate the cache.
// Arc for simplicity, but obviously getting the search from thread other than main is
// pretty much always racy.

static CACHED_UNIT_SEARCH: Mutex<Option<Arc<UnitSearch>>> = parking_lot::const_mutex(None);
static CACHED_UNIT_SEARCH_FRAME: AtomicUsize = AtomicUsize::new(0);

pub fn invalidate_cached_unit_search() {
    CACHED_UNIT_SEARCH_FRAME.store(!0, Ordering::Relaxed);
}

unsafe fn aiscript_unit_search(game: Game) -> Arc<UnitSearch> {
    let frame = game.frame_count() as usize;
    if CACHED_UNIT_SEARCH_FRAME.load(Ordering::Relaxed) != frame {
        let search = Arc::new(UnitSearch::from_bw());
        *CACHED_UNIT_SEARCH.lock() = Some(search.clone());
        CACHED_UNIT_SEARCH_FRAME.store(frame, Ordering::Relaxed);
        search
    } else {
        let guard = CACHED_UNIT_SEARCH.lock();
        guard.as_ref().unwrap().clone()
    }
}

unsafe fn attack_to_pos(player: u8, grouping: bw::Point, target: bw::Point) {
    let grouping_region = match bw::get_region(grouping) {
        Some(s) => s,
        None => {
            bw_print!(
                "Aiscript attackto (player {}): invalid grouping coordinates {:?}",
                player,
                grouping,
            );
            return;
        }
    };
    let target_region = match bw::get_region(target) {
        Some(s) => s,
        None => {
            bw_print!(
                "Aiscript attackto (player {}): invalid target coordinates {:?}",
                player,
                target,
            );
            return;
        }
    };
    let ai_data = bw::player_ai(player.into());
    (*ai_data).last_attack_second = bw::elapsed_seconds();
    (*ai_data).attack_grouping_region = grouping_region + 1;
    let region = ai_region(player.into(), grouping_region);
    bw::change_ai_region_state(region, 8);
    (*region).target_region_id = target_region; // Yes, 0-based
}

pub unsafe extern fn attack_to(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let grouping = read.read_position();
    let target = read.read_position();
    if feature_disabled("attack_to") || read.is_invalid() {
        return;
    }
    attack_to_pos((*script).player as u8, grouping.center, target.center);
}

pub unsafe extern fn attack_to_deaths(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let grouping_x_unit = read.read_u16();
    let grouping_x_player = read.read_u16() as u8;
    let grouping_y_unit = read.read_u16();
    let grouping_y_player = read.read_u16() as u8;
    let target_x_unit = read.read_u16();
    let target_x_player = read.read_u16() as u8;
    let target_y_unit = read.read_u16();
    let target_y_player = read.read_u16() as u8;
    if feature_disabled("attack_to") || read.is_invalid() {
        return;
    }
    let game = bw::game();
    let group_x = game.unit_deaths(grouping_x_player, UnitId(grouping_x_unit));
    let group_y = game.unit_deaths(grouping_y_player, UnitId(grouping_y_unit));
    let target_x = game.unit_deaths(target_x_player, UnitId(target_x_unit));
    let target_y = game.unit_deaths(target_y_player, UnitId(target_y_unit));
    let grouping = bw::Point {
        x: group_x as i16,
        y: group_y as i16,
    };
    let target = bw::Point {
        x: target_x as i16,
        y: target_y as i16,
    };
    attack_to_pos((*script).player as u8, grouping, target);
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct AttackTimeoutState {
    value: Option<u32>,
    original_start_second: Option<(u32, u32)>,
}

impl AttackTimeoutState {
    pub const fn new() -> AttackTimeoutState {
        AttackTimeoutState {
            value: None,
            original_start_second: None,
        }
    }
}

pub unsafe extern fn attack_timeout(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let timeout = read.read_u32();
    if feature_disabled("attack_timeout") || read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais attack_timeout");
    globals.attack_timeouts[(*script).player as usize].value = Some(timeout);
}

pub(crate) unsafe fn attack_timeouts_frame_hook(globals: &mut Globals, game: Game) {
    let seconds = (**game).elapsed_seconds;
    for i in 0..8 {
        let player_ai = bw::player_ai(i as u32);
        let last_attack_second = (*player_ai).last_attack_second;
        if last_attack_second == 0 {
            globals.attack_timeouts[i].value = None;
        }
        let new = if last_attack_second == 0 {
            Some(!400)
        } else {
            if let Some(timeout) = globals.attack_timeouts[i].value {
                if last_attack_second.saturating_add(timeout) < seconds {
                    Some(!400)
                } else {
                    // Don't want to accidentally set it to 0
                    Some(seconds.max(1))
                }
            } else {
                None
            }
        };
        if let Some(new) = new {
            (*player_ai).last_attack_second = new;
            globals.attack_timeouts[i].original_start_second = Some((last_attack_second, new));
        }
    }
}

pub(crate) unsafe fn attack_timeouts_frame_hook_after(globals: &mut Globals) {
    // Revert old value for teippi debug, only if it wasn't changed during frame step
    for i in 0..8 {
        if let Some((previous, new)) = globals.attack_timeouts[i].original_start_second.take() {
            let player_ai = bw::player_ai(i as u32);
            if (*player_ai).last_attack_second == new {
                (*player_ai).last_attack_second = previous;
            }
        }
    }
}

pub unsafe extern fn issue_order(script: *mut bw::AiScript) {
    // issue_order(order, count, unit_id, srcx, srcy, src_range, tgtx, tgty, tgt_range,
    //      tgt_param, flags)
    // Flag 0x1 = Target enemies,
    //      0x2 = Target own,
    //      0x4 = Target allies,
    //      0x8 = Target single unit
    //      0x10 = Target each unit once
    //      0x20 = Do not issue if unit is busy (unimplemented?? Going to leave unused)
    //      0x40 = Ignore datreqs
    let mut read = ScriptData::new(script);
    let order = OrderId(read.read_u8());
    let limit = read.read_u16();
    let unit_id = read.read_unit_match();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let mut target = read.read_position();
    let tgt_radius = read.read_u16();
    target.extend_area(tgt_radius as i16);
    let target_misc = read.read_unit_match();
    let flags = read.read_u16();
    if feature_disabled("issue_order") || read.is_invalid() {
        return;
    }
    if flags & 0xff80 != 0 {
        bw_print!("Aiscript issue_order: Unknown flags 0x{:x}", flags);
        return;
    }
    let check_datreqs = flags & 0x40 == 0;
    let game = bw::game();
    let search = aiscript_unit_search(game);
    let units = search
        .search_iter(&src.area)
        .filter(|u| u.player() as u32 == (*script).player && unit_id.matches(u))
        .filter(|u| u.can_issue_order(order))
        .take(limit as usize);

    let targets = if flags & 0x7 != 0 {
        let mut acceptable_players = [false; 12];
        for i in 0..12 {
            if i == (*script).player {
                acceptable_players[i as usize] = flags & 0x2 != 0;
            } else {
                if !game.allied((*script).player as u8, i as u8) {
                    acceptable_players[i as usize] = flags & 0x1 != 0;
                } else {
                    acceptable_players[i as usize] = flags & 0x4 != 0;
                }
            }
        }

        let limit = match flags & 0x8 != 0 {
            true => 1,
            false => !0,
        };
        let targets = search
            .search_iter(&target.area)
            .filter(|u| acceptable_players[u.player() as usize] && target_misc.matches(u))
            .take(limit)
            .collect::<Vec<_>>();

        Some(targets)
    } else {
        None
    };
    if targets.as_ref().map(|x| x.is_empty()).unwrap_or(false) {
        return;
    }
    let mut target_pos = 0;
    for unit in units {
        if order.is_secondary() {
            // Not sure how to handle cases where a train overrides another train in queue.
            unit.issue_secondary_order(order);
        } else {
            if let Some(ref targets) = targets {
                if target_pos == targets.len() {
                    if flags & 0x10 != 0 {
                        break;
                    }
                    target_pos = 0;
                }
                unit.issue_order_unit(order, targets[target_pos]);
                target_pos += 1;
            } else {
                unit.issue_order_ground(order, target.center);
            }
        }
        match order {
            order::id::PLACE_ADDON | order::id::BUILD_ADDON => {
                let unit_id = target_misc.get_one();
                if check_datreqs {
                    let reqs = match bw::unit_dat_requirements(unit_id) {
                        Some(s) => s,
                        None => return,
                    };
                    if !check_dat_requirements(game, reqs, unit, 0) {
                        return;
                    }
                }
                (**unit).unit_specific.building.build_addon_unit_id = unit_id.0;
            }
            order::id::DRONE_BUILD |
            order::id::SCV_BUILD |
            order::id::PROBE_BUILD |
            order::id::UNIT_MORPH |
            order::id::BUILDING_MORPH |
            order::id::TRAIN |
            order::id::TRAIN_FIGHTER |
            order::id::BUILD_NYDUS_EXIT => {
                let unit_id = target_misc.get_one();
                if check_datreqs {
                    let reqs = match bw::unit_dat_requirements(unit_id) {
                        Some(s) => s,
                        None => return,
                    };
                    if !check_dat_requirements(game, reqs, unit, 0) {
                        return;
                    }
                }
                (**unit).build_queue[(**unit).current_build_slot as usize] = unit_id.0;
            }
            _ => (),
        }
    }
}

pub unsafe extern fn if_attacking(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let dest = read.read_jump_pos();
    if feature_disabled("if_attacking") || read.is_invalid() {
        return;
    }
    let ai = bw::player_ai((*script).player);
    if (*ai).attack_grouping_region != 0 {
        (*script).pos = dest;
    }
}

pub unsafe extern fn unstart_campaign(script: *mut bw::AiScript) {
    if feature_disabled("unstart_campaign") {
        return;
    }
    let ai = bw::player_ai((*script).player);
    (*ai).flags &= !0x20;
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TownId {
    town: Town,
    id: u8,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MaxWorkers {
    town: Town,
    count: u8,
}

fn towns() -> Vec<Town> {
    let mut result = Vec::with_capacity(32);
    for unit in unit::active_units() {
        let town = if let Some(ai) = unit.building_ai() {
            unsafe { Town::from_ptr((*ai).town) }
        } else if let Some(ai) = unit.worker_ai() {
            unsafe { Town::from_ptr((*ai).town) }
        } else {
            None
        };
        if let Some(town) = town {
            if !result.iter().any(|&x| x == town) {
                result.push(town);
            }
        }
    }
    let mut script = bw::first_ai_script();
    while !script.is_null() {
        let town = unsafe { Town::from_ptr((*script).town) };
        if let Some(town) = town {
            if !result.iter().any(|&x| x == town) {
                result.push(town);
            }
        }
        script = unsafe { (*script).next };
    }
    result
}

pub(crate) fn update_towns(globals: &mut Globals) {
    let old = mem::replace(&mut globals.towns, towns());
    for old in old {
        if !globals.towns.iter().any(|&x| x == old) {
            globals.max_workers.swap_retain(|x| x.town != old);
            globals.town_ids.swap_retain(|x| x.town != old);
            globals
                .lift_lands
                .structures
                .swap_retain(|x| x.town_src != old && x.town_tgt != old);
            globals.queues.queue.swap_retain(|x| x.town != Some(old));
            globals.build_at.remove(&old);
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Town(pub *mut bw::AiTown);

impl Town {
    pub fn from_ptr(ptr: *mut bw::AiTown) -> Option<Town> {
        if ptr == null_mut() {
            None
        } else {
            Some(Town(ptr))
        }
    }

    pub fn player(self) -> u8 {
        unsafe { (*self.0).player }
    }

    pub fn has_building(self, unit: Unit) -> bool {
        unsafe { self.buildings().any(|x| (*x).parent == *unit) }
    }

    pub fn buildings(self) -> impl Iterator<Item = *mut bw::BuildingAi> {
        unsafe { ListIter((*self.0).buildings.first) }
    }

    pub fn workers(self) -> impl Iterator<Item = *mut bw::WorkerAi> {
        unsafe { ListIter((*self.0).workers.first) }
    }

    pub fn has_workers(self) -> bool {
        self.workers().next().is_some()
    }

    pub fn main_building(self) -> Option<Unit> {
        unsafe { Unit::from_ptr((*self.0).main_building) }
    }

    pub fn requests(self) -> impl Iterator<Item = bw::TownReq> {
        unsafe {
            (*self.0)
                .town_units
                .iter()
                .copied()
                .take_while(|x| x.flags_and_count != 0)
        }
    }
}

unsafe impl Send for Town {}

impl Serialize for Town {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;

        let array = bw::town_array() as *mut bw::AiTown;
        if array.is_null() {
            Err(S::Error::custom("Saving is not supported"))
        } else {
            let index = (self.0 as usize - array as usize) / mem::size_of::<bw::AiTown>();
            (index as u32).serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for Town {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;

        let id = u32::deserialize(deserializer)?;
        let array = bw::town_array() as *mut bw::AiTown;
        if array.is_null() {
            Err(S::Error::custom("Saving is not supported"))
        } else {
            unsafe { Ok(Town(array.offset(id as isize))) }
        }
    }
}

pub unsafe extern fn set_town_id(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let id = read.read_u8();
    if feature_disabled("set_town_id") || read.is_invalid() {
        return;
    }
    if id == 255 {
        bw_print!("Unsupported id {} in set_id", id);
        return;
    }
    let town = match Town::from_ptr((*script).town) {
        Some(s) => s,
        None => {
            bw_print!("Used `set_id {}` without town", id);
            return;
        }
    };
    let mut globals = Globals::get("ais set_id");
    globals.town_ids.swap_retain(|x| x.town != town);
    globals.town_ids.push(TownId {
        town,
        id,
    });
}

pub unsafe extern fn remove_build(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let mut unit_id = read.read_unit_match();
    let id = read.read_u8();
    if feature_disabled("remove_build") || read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais remove_build");
    let town = town_from_id(script, &mut globals, id);
    if let Some(town) = town {
        remove_build_from_town(town, &mut unit_id, amount);
    }
}

unsafe fn remove_build_from_town(town: Town, unit_id: &mut UnitMatch, amount: u8) {
    let builds = town.requests().collect::<Vec<_>>();
    let town = town.0;

    let mut write_units = 0;
    for mut elem in builds {
        let mut write = true;
        let flags_and_count = elem.flags_and_count;
        if flags_and_count != 0 {
            if flags_and_count & 0x4 != 0 {
                //tech - implement later
            } else if flags_and_count & 0x2 != 0 {
                //upgrade - implement later
            } else {
                for unit in unit_id.iter_flatten_groups() {
                    if unit.0 == elem.id {
                        let mut count = flags_and_count / 8;
                        count = count.saturating_sub(amount);
                        if count != 0 {
                            elem.flags_and_count = (flags_and_count & 0x7) | (count << 3);
                        } else {
                            write = false;
                        }
                    }
                }
            }
        }
        if write {
            (*town).town_units[write_units] = elem;
            write_units += 1;
        }
    }
    for i in write_units..64 {
        if (*town).town_units[i].flags_and_count != 0 {
            (*town).town_units[i].flags_and_count = 0;
            (*town).town_units[i].id = 0;
            (*town).town_units[i].priority = 0;
        }
    }
}

pub unsafe extern fn max_workers(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let count = read.read_u8();
    if feature_disabled("max_workers") || read.is_invalid() {
        return;
    }
    let town = match Town::from_ptr((*script).town) {
        Some(s) => s,
        None => {
            bw_print!("Used `max_workers {}` without town", count);
            return;
        }
    };
    let mut globals = Globals::get("ais max_workers");
    globals.max_workers.swap_retain(|x| x.town != town);
    if count != 255 {
        globals.max_workers.push(MaxWorkers {
            town,
            count,
        });
    }
}

pub(crate) fn max_workers_for(globals: &mut Globals, town: *mut bw::AiTown) -> Option<u8> {
    globals
        .max_workers
        .iter()
        .find(|x| x.town.0 == town)
        .map(|x| x.count)
}

pub unsafe extern fn under_attack(script: *mut bw::AiScript) {
    // 0 = Never, 1 = Default, 2 = Always
    let mut read = ScriptData::new(script);
    let mode = read.read_u8();
    if feature_disabled("under_attack") || read.is_invalid() {
        return;
    }
    let player = (*script).player as usize;
    let mut globals = Globals::get("ais under_attack");
    globals.under_attack_mode[player] = match mode {
        0 => Some(false),
        1 => None,
        2 => Some(true),
        _ => {
            bw_print!("Invalid `under_attack` mode: {}", mode);
            return;
        }
    };
}

unsafe fn subtract_cost(game: Game, id: UnitId, player: u8) {
    let cost = ai::unit_cost(id);
    game.reduce_minerals(player, cost.minerals);
    game.reduce_gas(player, cost.gas);
}

pub unsafe fn queues_frame_hook(
    queues: &mut Queues,
    unit_search: &UnitSearch,
    game: Game,
    players: *mut bw::Player,
) {
    use bw_dat::order::{ARCHON_WARP, DARK_ARCHON_MELD, TRAIN, UNIT_MORPH};
    use bw_dat::unit::{ARCHON, DARK_ARCHON};
    queues.queue.swap_retain(|x| x.current_quantity > 0);
    for queue in &mut queues.queue {
        let units = unit_search
            .search_iter(&queue.pos)
            .filter(|x| queue.can_train(*x))
            .collect::<Vec<_>>();
        for u in units {
            let ai = ai::PlayerAi::get(u.player());
            if ai.has_resources(game, players, &ai::unit_cost(queue.unit_id)) {
                queue.current_quantity = queue.current_quantity.saturating_sub(1);
                match u.id().is_building() {
                    true => {
                        u.issue_secondary_order(TRAIN);
                        (**u).build_queue[(**u).current_build_slot as usize] = queue.unit_id.0;
                        subtract_cost(game, queue.unit_id, queue.player);
                    }
                    false => {
                        match queue.unit_id {
                            ARCHON | DARK_ARCHON => {
                                let order = match queue.unit_id {
                                    ARCHON => ARCHON_WARP,
                                    _ => DARK_ARCHON_MELD,
                                };
                                let find_pair = unit_search.find_nearest(u.position(), |x| {
                                    x != u && x.id() == u.id() && x.order() != order
                                });
                                if let Some((unit, _distance)) = find_pair {
                                    unit.issue_order_unit(order, u);
                                    u.issue_order_unit(order, unit);
                                    //archons have no cost
                                }
                            }
                            _ => {
                                u.issue_order(UNIT_MORPH, u.position(), None);
                                (**u).build_queue[0] = queue.unit_id.0;
                                subtract_cost(game, queue.unit_id, queue.player);
                            }
                        }
                    }
                }
                if queue.current_quantity == 0 {
                    break;
                }
            }
        }
    }
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn lift_land_hook(lift_lands: &mut LiftLand, search: &UnitSearch, game: Game) {
    use bw_dat::order::{BUILDING_LAND, LIFTOFF, MOVE};
    lift_lands
        .structures
        .swap_retain(|x| x.stage() != LiftLandStage::End);
    for lift_land in &mut lift_lands.structures {
        if lift_land.state.is_none() {
            let unit = search
                .search_iter(&lift_land.src)
                .find(|x| x.id() == lift_land.unit_id && lift_land.town_src.has_building(*x));
            if let Some(unit) = unit {
                lift_land.init_state(unit);
            }
        }

        if let Some(ref mut state) = lift_land.state {
            let unit = state.unit.0;
            let target_area = match state.is_returning {
                false => lift_land.tgt,
                true => lift_land.src,
            };
            match state.stage {
                LiftLandStage::LiftOff_Start => {
                    if unit.hp_percent() >= lift_land.return_hp_percent as i32 {
                        unit.issue_order_ground(LIFTOFF, unit.position());
                        state.stage = LiftLandStage::LiftOff_End;
                    }
                }
                LiftLandStage::LiftOff_End => {
                    if unit.order() != LIFTOFF {
                        state.stage = LiftLandStage::Fly;
                        state.is_returning = false;
                    }
                }
                LiftLandStage::Fly => {
                    if unit.hp_percent() < lift_land.return_hp_percent as i32 {
                        state.is_returning = true;
                    }
                    if unit_in_area(unit, target_area) {
                        state.stage = LiftLandStage::FindLocation;
                    } else {
                        unit.issue_order_ground(MOVE, target_area.center());
                    }
                }
                LiftLandStage::FindLocation => {
                    let rect_x = target_area.right / 32;
                    let rect_y = target_area.bottom / 32;
                    let pos_x = target_area.left / 32;
                    let pos_y = target_area.top / 32;

                    let offset_x = target_area.left - (pos_x * 32);
                    let offset_y = target_area.top - (pos_y * 32);
                    'outer: for i in pos_x..rect_x + 1 {
                        for j in pos_y..rect_y + 1 {
                            if check_placement(game, search, unit, i, j, unit.id()) {
                                state.target = bw::Point {
                                    x: (i * 32) + offset_x,
                                    y: (j * 32) + offset_y,
                                };
                                unit.issue_order_ground(MOVE, state.target);
                                state.stage = LiftLandStage::Land;
                                break 'outer;
                            }
                        }
                    }
                }
                LiftLandStage::Land => {
                    if unit_in_area(unit, bw::Rect::from_point(state.target)) {
                        if unit.is_air() {
                            let placement_ok = check_placement(
                                game,
                                search,
                                unit,
                                state.target.x / 32,
                                state.target.y / 32,
                                unit.id(),
                            );
                            if placement_ok {
                                if unit.order() != BUILDING_LAND {
                                    unit.issue_order_ground(BUILDING_LAND, state.target);
                                }
                            } else {
                                state.stage = LiftLandStage::FindLocation;
                            }
                        } else {
                            match state.is_returning {
                                false => {
                                    let old_town = lift_land.town_src.0;
                                    let new_town = lift_land.town_tgt.0;
                                    if old_town != new_town {
                                        if unit.building_ai().is_some() {
                                            bw::remove_unit_ai(*unit, 0);
                                            bw::add_town_unit_ai(*unit, new_town);
                                        }
                                    }
                                    state.stage = LiftLandStage::End;
                                }
                                true => state.stage = LiftLandStage::LiftOff_Start,
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

pub unsafe fn bunker_fill_hook(
    bunker_states: &mut BunkerCondition,
    picked_unit: Unit,
    search: &UnitSearch,
) {
    use bw_dat::order::*;
    if bunker_states.in_list(picked_unit) {
        return;
    }

    match picked_unit.order() {
        ENTER_TRANSPORT | AI_ATTACK_MOVE => {
            return;
        }
        _ => {}
    }
    for (decl, state) in &mut bunker_states.bunker_states {
        if decl.unit_id.matches(&picked_unit) {
            let units = search.search_iter(&decl.pos).filter(|u| {
                u.player() == decl.player &&
                    decl.bunker_id.matches(u) &&
                    u.player() == picked_unit.player()
            });
            let mut used_bunkers = 0;
            for bunker in units {
                let cargo_capacity = bunker.id().cargo_space_provided();
                let cargo_amount = bunker.cargo_count();
                let units_targeted = state.count_associated_units(bunker).unwrap_or(0);
                let free_slots =
                    cargo_capacity.saturating_sub(u32::from(cargo_amount + units_targeted));

                let full = u32::from(cargo_amount) == cargo_capacity;
                if units_targeted > 0 || (full && units_targeted == 0) {
                    used_bunkers += 1;
                    if used_bunkers >= decl.bunker_quantity {
                        return;
                    }
                }
                if free_slots > 0 && decl.quantity > 0 {
                    picked_unit.issue_order_unit(ENTER_TRANSPORT, bunker);
                    state.add_targeter(picked_unit, bunker);
                    debug!(
                        "Add Targeter: uid {} at {} {} to pos {} {}",
                        bunker.id().0,
                        picked_unit.position().x,
                        picked_unit.position().y,
                        bunker.position().x,
                        bunker.position().y
                    );
                    return;
                }
            }
        }
    }
}

pub(crate) unsafe fn under_attack_frame_hook(globals: &mut Globals) {
    for (player, mode) in globals.under_attack_mode.iter().cloned().enumerate() {
        match mode {
            Some(true) => {
                (*bw::player_ai(player as u32)).previous_building_hit_second =
                    bw::elapsed_seconds().wrapping_sub(1);
            }
            Some(false) => {
                (*bw::player_ai(player as u32)).previous_building_hit_second = 0;
            }
            None => (),
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
pub struct AiMode {
    pub wait_for_resources: bool,
    pub build_gas: bool,
    pub retaliation: bool,
    pub focus_disabled_units: bool,
}

impl AiMode {
    pub const fn default() -> AiMode {
        AiMode {
            wait_for_resources: true,
            build_gas: true,
            retaliation: true,
            focus_disabled_units: true,
        }
    }
}

#[derive(Serialize, Deserialize, Copy, Clone)]
pub struct GlobalAiMode {
    pub disable_spell_focus: bool,
    pub disable_carrier_focus: bool,
    pub disable_acid_spore_focus: bool,
}

impl GlobalAiMode {
    pub const fn default() -> GlobalAiMode {
        GlobalAiMode {
            disable_spell_focus: false,
            disable_carrier_focus: false,
            disable_acid_spore_focus: false,
        }
    }
}

pub unsafe extern fn aicontrol(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mode = read.read_u8();
    if feature_disabled("aicontrol") || read.is_invalid() {
        return;
    }
    let player = (*script).player as usize;
    let mut globals = Globals::get("ais aicontrol");
    let globals = &mut *globals;
    let out = &mut globals.ai_mode[player];
    let global_out = &mut globals.global_ai_mode;

    match mode {
        0 => out.wait_for_resources = true,
        1 => out.wait_for_resources = false,
        2 => out.build_gas = true,
        3 => out.build_gas = false,
        4 => out.retaliation = true,
        5 => out.retaliation = false,
        6 => out.focus_disabled_units = true,
        7 => out.focus_disabled_units = false,
        8 => global_out.disable_spell_focus = false,
        9 => global_out.disable_spell_focus = true,
        0xa => global_out.disable_acid_spore_focus = false,
        0xb => global_out.disable_acid_spore_focus = true,
        0xc => global_out.disable_carrier_focus = false,
        0xd => global_out.disable_carrier_focus = true,
        _ => panic!("Invalid aicontrol {:x}", mode),
    };
}

pub unsafe extern fn goto(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let new_pos = read.read_jump_pos();
    if read.is_invalid() {
        return;
    }
    (*script).pos = new_pos;
}

pub unsafe extern fn call(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let dest = read.read_u16() as u32;
    if feature_disabled("everything_else") || read.is_invalid() {
        return;
    }
    let ret = (*script).pos;
    (*script).pos = dest;
    let globals = Globals::get("ais call");
    (*Script::ptr_from_bw(script)).call_stack_push(ret, &globals.ai_scripts);
}

pub unsafe extern fn ret(script: *mut bw::AiScript) {
    let script = Script::ptr_from_bw(script);
    let globals = Globals::get("ais ret");
    match (*script).call_stack_pop(&globals.ai_scripts) {
        Some(s) => {
            (*script).bw.pos = s;
        }
        None => {
            bw_print!(
                "Script {} used return without call",
                (*script).debug_string()
            );
            (*script).bw.wait = !1;
            (*script).bw.pos -= 1;
        }
    }
}

pub unsafe extern fn do_morph(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let unit_id = UnitId(read.read_u16());
    if read.is_invalid() {
        return;
    }
    let globals = Globals::get("ais do_morph");
    let unit_id = globals.unit_replace.replace_check(unit_id); // replace_requests
    let player = (*script).player as u8;
    let game = bw::game();
    if ai::count_units(player, unit_id, game) < u32::from(amount) {
        let ai = ai::PlayerAi::get(player);
        // If the train will get ignored due to define_max limit, don't clobber train_unit_id
        // in case there is another script issuing train too that is expected to success.
        if !ai.is_at_limit(unit_id, game) {
            (*ai.0).train_unit_id = unit_id.0 + 1;
        }
    }
}

pub unsafe extern fn train(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let unit_id = UnitId(read.read_u16());
    if read.is_invalid() {
        return;
    }
    let globals = Globals::get("ais train");
    let unit_id = globals.unit_replace.replace_check(unit_id); // replace_requests
    let player = (*script).player as u8;
    let game = bw::game();
    if ai::count_units(player, unit_id, game) < u32::from(amount) {
        let ai = ai::PlayerAi::get(player);
        // If the train will get ignored due to define_max limit, don't clobber train_unit_id
        // in case there is another script issuing train too that is expected to success.
        if !ai.is_at_limit(unit_id, game) {
            (*ai.0).train_unit_id = unit_id.0 + 1;
        }
        (*script).pos -= 4;
        (*script).wait = 30;
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct UnitMatch {
    units: Vec<UnitId>,
    has_groups: bool,
}

impl UnitMatch {
    pub fn single(unit_id: UnitId) -> UnitMatch {
        let has_groups = unit_id.0 >= unit::id::ANY_UNIT.0 &&
            unit_id.0 <= unit::id::GROUP_FACTORIES.0;
        UnitMatch {
            units: vec![unit_id],
            has_groups,
        }
    }

    pub fn iter_flatten_groups<'a>(&'a mut self) -> impl Iterator<Item = UnitId> + 'a {
        // Ineffective but w/e, simpler than ignoring duplicates
        if self.has_groups && self.units.iter().any(|&x| x == unit::id::ANY_UNIT) {
            self.units = (0..UnitId::entry_amount())
                .flat_map(|x| UnitId::optional(x))
                .collect();
        } else if self.has_groups {
            let mut group_flags = 0;
            for &id in &self.units {
                group_flags |= match id {
                    unit::id::GROUP_MEN => 0x8,
                    unit::id::GROUP_BUILDINGS => 0x10,
                    unit::id::GROUP_FACTORIES => 0x20,
                    _ => 0x0,
                };
            }
            if group_flags != 0 {
                self.units.retain(|&unit_id| match unit_id {
                    x if UnitId::optional(x.0 as u32).is_none() => false,
                    x => x.group_flags() & group_flags == 0,
                });
                self.has_groups = false;
                let new_units = (0..UnitId::entry_amount())
                    .flat_map(|x| UnitId::optional(x))
                    .filter(|x| x.group_flags() & group_flags != 0);
                self.units.extend(new_units);
            }
        }
        self.units.iter().cloned()
    }

    pub fn has_groups(&self) -> bool {
        self.has_groups
    }

    pub fn as_slice(&self) -> &[UnitId] {
        &self.units
    }

    pub fn matches(&self, unit: &Unit) -> bool {
        self.matches_id(unit.id())
    }

    pub fn matches_id(&self, id: UnitId) -> bool {
        if !self.has_groups() {
            self.units.iter().any(|&x| x == id)
        } else {
            self.units.iter().any(|&x| id.matches_id(x))
        }
    }

    pub fn matches_match(&self, other: &UnitMatch) -> bool {
        self.units.iter().all(|&x| {
            match x {
                unit::id::ANY_UNIT => true,
                unit::id::GROUP_MEN | unit::id::GROUP_BUILDINGS | unit::id::GROUP_FACTORIES => {
                    other.units.iter().any(|&y| {
                        x == y
                    })
                }
                _ => other.matches_id(x),
            }
        })
    }

    pub fn count(&self) -> usize {
        self.units.len()
    }

    pub fn get_one(&self) -> UnitId {
        self.units
            .iter()
            .cloned()
            .flat_map(|x| UnitId::optional(x.0 as u32))
            .next()
            .unwrap_or(UnitId(0))
    }

    fn replace_ids(&mut self, unit_replace: &mut UnitReplace) {
        for id in &mut self.units {
            *id = unit_replace.replace_check(*id);
        }
    }
}

impl fmt::Display for UnitMatch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let limit = 10;
        write!(f, "[")?;
        for (i, &id) in self.units.iter().take(limit).enumerate() {
            if i != 0 {
                write!(f, ", {}", id.0)?;
            } else {
                write!(f, "{}", id.0)?;
            }
        }
        if self.units.len() > limit {
            write!(f, ", ...")?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Serialize, Deserialize)]
pub struct PlayerMatch {
    players: [bool; 12],
}

impl PlayerMatch {
    pub fn matches(&self, player: u8) -> bool {
        self.players.get(player as usize).cloned().unwrap_or(false)
    }

    pub fn players<'a>(&'a self) -> impl Iterator<Item = u8> + 'a {
        self.players
            .iter()
            .enumerate()
            .filter(|x| *x.1 == true)
            .map(|x| x.0 as u8)
    }
}

pub unsafe extern fn supply(script: *mut bw::AiScript) {
    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum Race {
        Zerg,
        Terran,
        Protoss,
        Any,
    }

    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum SupplyType {
        Used,
        Provided,
        Max,
        InUnits,
    }

    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum Mode {
        Sum,
        Max,
    }
    let old_pos = (*script).pos - 1;
    let mut globals = Globals::get("ais supply");
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = (read.read_u16() as u32) * 2;
    let supply_type = read.read_u8();
    let units = read.read_unit_match();
    let race = read.read_u8();
    let dest = read.read_jump_pos();
    if feature_disabled("supply") || read.is_invalid() {
        return;
    }
    let supply_type = match supply_type {
        0 => SupplyType::Provided,
        1 => SupplyType::Used,
        2 => SupplyType::Max,
        3 => SupplyType::InUnits,
        x => {
            bw_print!("Unsupported supply type in supply: {:x}", x);
            return;
        }
    };

    let (race, mode) = match race {
        0 => (Race::Zerg, Mode::Sum),
        1 => (Race::Terran, Mode::Sum),
        2 => (Race::Protoss, Mode::Sum),
        16 => (Race::Any, Mode::Sum),
        17 => (Race::Any, Mode::Max),
        x => {
            bw_print!("Unsupported race in supply: {:x}", x);
            return;
        }
    };

    match modifier.ty {
        ModifierType::Read(read) => {
            let mut sum = 0;
            match supply_type {
                SupplyType::InUnits => {
                    for unit in unit::active_units().chain(unit::hidden_units()) {
                        if players.matches(unit.player()) && units.matches(&unit) {
                            sum += unit.id().supply_cost();
                        }
                    }
                }
                _ => {
                    for race_id in 0..3 {
                        let race_matches = match race {
                            Race::Zerg => race_id == 0,
                            Race::Terran => race_id == 1,
                            Race::Protoss => race_id == 2,
                            Race::Any => true,
                        };
                        if race_matches {
                            for player in players.players() {
                                let supplies = &((**game).supplies[race_id]);
                                let amount = match supply_type {
                                    SupplyType::Provided => supplies.provided[player as usize],
                                    SupplyType::Used => supplies.used[player as usize],
                                    SupplyType::Max => supplies.max[player as usize],
                                    _ => 0,
                                };
                                if mode == Mode::Max {
                                    sum = sum.max(amount);
                                } else {
                                    sum += amount;
                                }
                            }
                        }
                    }
                }
            }
            read.compare_and_act(sum, amount, script, dest, old_pos, &globals.ai_scripts);
        }
        ModifierType::Write(write) => {
            let race_i = match race {
                Race::Zerg => 0,
                Race::Terran => 1,
                Race::Protoss => 2,
                Race::Any => {
                    bw_print!("Only specific race supply can be modified.");
                    return;
                }
            };
            if supply_type == SupplyType::Max {
                for player in players.players() {
                    let supply_val = (**game).supplies[race_i].max.get_mut(player as usize);
                    if let Some(supply_val) = supply_val {
                        *supply_val = write.apply(*supply_val, amount, &mut globals.rng);
                    }
                }
            } else {
                bw_print!("Only Max supply can be modified.");
                return;
            }
        }
    }
}

pub unsafe extern fn resources_command(script: *mut bw::AiScript) {
    #[derive(Copy, Clone)]
    enum Resource {
        Ore,
        Gas,
    }
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut globals = Globals::get("ais resources");
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let res = read.read_u8();
    let amount = read.read_u32();
    let dest = read.read_jump_pos();
    if feature_disabled("resources") || read.is_invalid() {
        return;
    }
    let resources_to_check: &[_] = match res {
        // Matching trigger conditions
        0 => &[Resource::Ore],
        1 => &[Resource::Gas],
        2 => &[Resource::Ore, Resource::Gas],
        x => {
            bw_print!("Unsupported resource type in resources: {:x}", x);
            return;
        }
    };

    match modifier.ty {
        ModifierType::Read(read) => {
            let jump = players.players().any(|player| {
                resources_to_check.iter().any(|&res| {
                    let resvalue = match res {
                        Resource::Ore => game.minerals(player),
                        Resource::Gas => game.gas(player),
                    };
                    read.compare(resvalue, amount)
                })
            });

            let read_req = read.action.get_read_req();
            if jump == read_req {
                read.action.do_action(script, dest, old_pos, &globals.ai_scripts);
            }
        }
        ModifierType::Write(write) => {
            for player in players.players() {
                for &res in resources_to_check {
                    let resources = match res {
                        Resource::Ore => (**game).minerals.get_mut(player as usize),
                        Resource::Gas => (**game).gas.get_mut(player as usize),
                    };
                    if let Some(resources) = resources {
                        *resources = write.apply(*resources, amount, &mut globals.rng);
                    }
                }
            }
        }
    }
}

pub(crate) unsafe fn reveal_vision_hook(globals: &mut Globals, game: Game, tile_flags: *mut u32) {
    for rev in &mut globals.reveal_states {
        if rev.time != !0 {
            rev.time -= 1;
            if rev.time == 0 {
                reveal(game, tile_flags, rev.pos, rev.players, rev.reveal_type, false);
            } else {
                reveal(game, tile_flags, rev.pos, rev.players, rev.reveal_type, true);
            }
        }
    }
    globals.reveal_states.swap_retain(|x| x.time > 0);
}

unsafe fn reveal(
    game: Game,
    tile_flags: *mut u32,
    area: bw::Rect,
    players: PlayerMatch,
    reveal_type: RevealType,
    reveal: bool,
) {
    fn clamp_x(x: i16, game: Game) -> i16 {
        x.clamp(0i16, game.map_width_tiles() as i16 - 1)
    }

    fn clamp_y(y: i16, game: Game) -> i16 {
        y.clamp(0i16, game.map_height_tiles() as i16 - 1)
    }

    let tile_x = clamp_x(area.left / 32, game);
    let tile_y = clamp_y(area.top / 32, game);
    // This maybe should be (area.right + 31) / 32 with exclusive range?
    // Currently passing in 1x1 tile location reveals 2x2 area which is somewhat weird.
    let limit_x = clamp_x(area.right / 32, game);
    let limit_y = clamp_y(area.bottom / 32, game);
    let map_width = game.map_width_tiles();
    let base_mask = match (reveal_type, reveal) {
        (RevealType::RevealFull, false) => 0x1,
        (RevealType::RevealFull, true) => 0x101,
        (RevealType::RevealFog, _) => 0x100,
    };
    for player in players.players().filter(|&x| x < 8) {
        let player_mask = base_mask << player;

        for i in tile_x..=limit_x {
            for j in tile_y..=limit_y {
                let tile_flag = tile_flags.add(i as usize + map_width as usize * j as usize);
                if reveal {
                    *tile_flag &= !player_mask;
                } else {
                    *tile_flag |= player_mask;
                }
            }
        }
    }
}

pub unsafe extern fn reveal_area(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let game = bw::game();
    let mut globals = Globals::get("ais reveal_area");
    let players = read.read_player_match(game);
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let time = read.read_u16();
    let flag = read.read_u8();
    if feature_disabled("reveal_area") || read.is_invalid() {
        return;
    }
    let reveal_type = match flag {
        0 => RevealType::RevealFog,
        1 => RevealType::RevealFull,
        x => {
            bw_print!("Unsupported flag modifier: {:x}", x);
            return;
        }
    };
    if time != 0 || reveal_type == RevealType::RevealFull {
        let reveal_state = RevealState {
            pos: src.area,
            time: if time == 0 {
                !0
            } else {
                time
            },
            reveal_type,
            players,
        };
        globals.reveal_states.push(reveal_state);
    }
    reveal(game, bw::tile_flags(), src.area, players, reveal_type, true);
}

fn get_bank_path(name: &str) -> Option<PathBuf> {
    let name = Path::new(name);
    let bad_path = name.components().any(|x| match x {
        Component::Prefix(..) | Component::RootDir | Component::ParentDir => true,
        Component::CurDir | Component::Normal(..) => false,
    });
    if bad_path {
        return None;
    }
    let root = if crate::is_scr() {
        UserDirs::new()
            .and_then(|user_dirs| user_dirs.document_dir().map(|s| s.join("Starcraft")))
            .unwrap_or_else(|| ".".into())
    } else {
        ".".into()
    };
    Some(root.join("save").join(name))
}

pub unsafe extern fn save_bank(script: *mut bw::AiScript) {
    let orig_pos = (*script).pos;
    let mut read = ScriptData::new(script);
    let name = read.read_string();
    if read.is_invalid() {
        return;
    }
    let globals = Globals::get("ais save_bank");
    let name = match std::str::from_utf8(name) {
        Ok(o) if !o.is_empty() => o,
        _ => {
            bw_print!(
                "Invalid bank path at offset {:x} / {}",
                orig_pos, (*Script::ptr_from_bw(script)).debug_string(),
            );
            (*script).wait = !1;
            (*script).pos = orig_pos;
            return;
        }
    };
    let path = match get_bank_path(&name) {
        Some(s) => s,
        None => return,
    };
    let folder = Path::parent(&path);
    if let Some(s) = folder {
        let _ = fs::create_dir(s);
    }
    let mut file = match File::create(&path) {
        Ok(o) => o,
        Err(e) => {
            bw_print!("Bank load error: {}", e);
            return;
        }
    };
    if let Err(e) = bincode::serialize_into(&mut file, &globals.bank) {
        bw_print!("Bank save error: {}", e);
    }
}

pub unsafe extern fn load_bank(script: *mut bw::AiScript) {
    let orig_pos = (*script).pos;
    let mut read = ScriptData::new(script);
    let name = read.read_string();
    if feature_disabled("load_bank") || read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais load_bank");
    globals.bank.reset();
    let name = match std::str::from_utf8(name) {
        Ok(o) if !o.is_empty() => o,
        _ => {
            bw_print!(
                "Invalid bank path at offset {:x} / {}",
                orig_pos, (*Script::ptr_from_bw(script)).debug_string(),
            );
            (*script).wait = !1;
            (*script).pos = orig_pos;
            return;
        }
    };
    let path = match get_bank_path(&name) {
        Some(s) => s,
        None => return,
    };
    if path.exists() {
        let mut file = match File::open(path) {
            Ok(o) => o,
            Err(e) => {
                bw_print!("Bank load error: {}", e);
                return;
            }
        };
        globals.bank = match bincode::deserialize_from(&mut file) {
            Ok(o) => o,
            Err(e) => {
                bw_print!("Bank load error: {}", e);
                return;
            }
        };
    }
}

unsafe fn add_bank_data(
    script: *mut bw::AiScript,
    old_pos: u32,
    modifier: TriggerModifier,
    key: BankKey,
    amount: u32,
    dest: u32,
) {
    let mut globals = Globals::get("ais bank_data");
    let globals = &mut *globals;
    match modifier.ty {
        ModifierType::Read(read) => {
            let value = globals.bank.get(&key);
            read.compare_and_act(value, amount, script, dest, old_pos, &globals.ai_scripts);
        }
        ModifierType::Write(write) => {
            let rng = &mut globals.rng;
            globals
                .bank
                .update(key, |val| write.apply(val, amount, rng));
        }
    }
}

pub unsafe extern fn bank_data_old(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let mut read = ScriptData::new(script);
    let modifier = read.read_modifier();
    let category = read.read_string();
    let label = read.read_string();
    let amount = read.read_u32();
    let dest = read.read_jump_pos();
    if read.is_invalid() {
        return;
    }
    let key = BankKey {
        label: String::from_utf8_lossy(label).to_string(),
        category: String::from_utf8_lossy(category).to_string(),
    };
    add_bank_data(script, old_pos, modifier, key, amount, dest);
}

pub unsafe extern fn bank_data(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let mut read = ScriptData::new(script);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let category = read.read_string();
    let label = read.read_string();
    let dest = read.read_jump_pos();
    if read.is_invalid() {
        return;
    }
    let key = BankKey {
        label: String::from_utf8_lossy(label).to_string(),
        category: String::from_utf8_lossy(category).to_string(),
    };
    add_bank_data(script, old_pos, modifier, key, amount, dest);
}

pub unsafe extern fn remove_creep(script: *mut bw::AiScript) {
    #![cfg_attr(target_pointer_width = "64", allow(unused_variables, unused_mut))]
    let mut read = ScriptData::new(script);
    let mut src = read.read_position();
    let radius = read.read_u16();
    if crate::is_scr() {
        bw_print!("remove_creep is not supported in SCR");
        return;
    }
    if feature_disabled("remove_creep") || read.is_invalid() {
        return;
    }
    #[cfg(target_pointer_width = "32")]
    {
        src.extend_area(radius as i16);
        let rect_x = src.area.right / 32;
        let rect_y = src.area.bottom / 32;
        let pos_x = src.area.left / 32;
        let pos_y = src.area.top / 32;

        for x_tile in pos_x..(rect_x + 1) {
            for y_tile in pos_y..(rect_y + 1) {
                let left = x_tile as u32 * 32;
                let top = y_tile as u32 * 32;
                unsafe extern "stdcall" fn nop(
                    _x_tile: u32,
                    _y_tile: u32,
                    _area: *mut bw::Rect32,
                ) -> u32 {
                    0
                }
                bw::remove_creep_at_unit(left, top, unit::id::EGG.0 as u32, nop);
            }
        }
    }
}

pub unsafe extern fn time_command(script: *mut bw::AiScript) {
    enum TimeType {
        Frames,
        Minutes,
    }
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let time_mod = read.read_u8();
    let dest = read.read_jump_pos();
    if feature_disabled("time") || read.is_invalid() {
        return;
    }
    let time_mod = match time_mod {
        // Matching trigger conditions
        0 => TimeType::Frames,
        1 => TimeType::Minutes,
        x => {
            bw_print!("Unsupported time modifier in time: {:x}", x);
            return;
        }
    };
    let amount = match time_mod {
        TimeType::Minutes => amount * 960,
        TimeType::Frames => amount,
    };
    let time = game.frame_count();
    let read = match modifier.ty {
        ModifierType::Read(r) => r,
        ModifierType::Write(w) => {
            bw_print!("Used writing modifier {:?} in time", w);
            return;
        }
    };
    let globals = Globals::get("time_command");
    read.compare_and_act(time, amount, script, dest, old_pos, &globals.ai_scripts);
}

pub unsafe extern fn attacking(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let mut read = ScriptData::new(script);
    let modifier = read.read_bool_modifier();
    let dest = read.read_jump_pos();
    if feature_disabled("attacking") || read.is_invalid() {
        return;
    }
    let ai = bw::player_ai((*script).player);
    let r_compare = ((*ai).attack_grouping_region != 0) == modifier.value;
    if r_compare == modifier.action.get_read_req() {
        let globals = Globals::get("attacking");
        modifier.action.do_action(script, dest, old_pos, &globals.ai_scripts);
    }
}

#[cfg(target_pointer_width = "32")]
fn unit_in_area(source: Unit, area: bw::Rect) -> bool {
    source.collision_rect().overlaps(&area)
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn add_spending_request_hook(
    priority: u32,
    c: *mut libc::c_void,
    unit_id: u16,
    ai_type: u32,
    player: u8,
    orig: unsafe extern fn(u32, *mut libc::c_void, u16, u32, u8),
) {
    let mut globals = Globals::get("add_spending_request");
    let unit_id = UnitId(unit_id);
    for maxbuild in &mut globals.build_max.buildmax {
        if let Some(town) = maxbuild.town {
            if maxbuild.unit_id == unit_id {
                let units_in_town = ai::count_town_units(town, unit_id, false);
                if units_in_town >= maxbuild.quantity as u32 {
                    return;
                }
            }
        }
    }
    drop(globals);
    orig(priority, c, unit_id.0, ai_type, player);
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn ai_attack_focus_hook(
    unit: *mut bw::Unit,
    func_param: u32,
    orig: unsafe extern fn(*mut bw::Unit, u32) -> u32,
) -> u32 {
    if (*unit).player < 8 {
        let globals = Globals::get("ai focus unit hook");
        let ai_mode = globals.ai_mode[(*unit).player as usize];
        if !ai_mode.focus_disabled_units {
            return 0;
        }
    }
    orig(unit, func_param)
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn unit_name_hook(unit_id: u32, orig: unsafe extern fn(u32) -> *const u8) -> *const u8 {
    let unit = bw::client_selection[0];
    let unit = match Unit::from_ptr(unit) {
        Some(s) => s,
        None => return orig(unit_id),
    };
    let globals = Globals::get("unit name hook");
    let name_match = globals
        .renamed_units
        .states
        .iter()
        .find(|x| {
            unit_id == x.unit_id.0 as u32 &&
                x.players.matches(unit.player()) &&
                (unit_in_area(unit, x.area))
        })
        .map(|x| &x.name);

    if let Some(name_match) = name_match {
        name_match.as_ptr() as *const u8
    } else {
        orig(unit_id)
    }
}

pub unsafe extern fn unit_name(script: *mut bw::AiScript) {
    enum NameStatus {
        Enable,
        Disable,
    }
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let unit = read.read_u16();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let replacement_string = read.read_string();
    let flag = read.read_u8();
    if feature_disabled("unit_name") || read.is_invalid() {
        return;
    }
    let flag = match flag {
        0 => NameStatus::Enable,
        1 => NameStatus::Disable,
        x => {
            bw_print!("Unsupported flag modifier in unit_name: {:x}", x);
            return;
        }
    };
    if crate::is_scr() {
        bw_print!("unit_name is not supported in SCR");
        return;
    }
    let mut globals = Globals::get("ais unit_name");

    let rename_status = RenameStatus {
        area: src.area,
        unit_id: UnitId(unit),
        name: CString::new(replacement_string).unwrap(),
        players,
    };
    match flag {
        NameStatus::Enable => globals.renamed_units.try_add(rename_status),
        NameStatus::Disable => globals.renamed_units.try_remove(&rename_status),
    }
}
pub unsafe extern fn wait_build(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let mut unit_id = UnitId(read.read_u16());
    if read.is_invalid() {
        return;
    }
    let town = Town::from_ptr((*script).town);
    let globals = Globals::get("ais wait_build");
    unit_id = globals.unit_replace.replace_check(unit_id);

    if let Some(town) = town {
        if unit_id.is_worker() && (*town.0).worker_limit <= (*town.0).worker_count {
            return;
        }
        let units_in_town = ai::count_town_units(town, unit_id, true);
        if units_in_town < amount as u32 {
            (*script).pos = old_pos;
            (*script).wait = 30;
        }
    }
}
pub unsafe extern fn wait_buildstart(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let mut unit_id = UnitId(read.read_u16());
    if read.is_invalid() {
        return;
    }
    let town = Town::from_ptr((*script).town);
    let globals = Globals::get("ais wait_buildstart");
    unit_id = globals.unit_replace.replace_check(unit_id);
    if let Some(town) = town {
        if unit_id.is_worker() {
            let workers_being_built = town.buildings()
                .filter_map(|ai| {
                    let unit = Unit::from_ptr((*ai).parent)?;
                    unit.currently_building()
                        .filter(|x| x.id().is_worker())
                })
                .count();
            let count = ((*town.0).worker_count as u32)
                .saturating_add(workers_being_built as u32);
            let limit = ((*town.0).worker_limit).min(amount);
            if count >= limit as u32 {
                return;
            }
        } else {
            let units_in_town = ai::count_town_units(town, unit_id, false);
            if units_in_town >= amount as u32 {
                return;
            }
        }
        (*script).pos = old_pos;
        (*script).wait = 30;
    }
}

pub unsafe extern fn deaths(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    // deaths(player, modifier, amount, unit, dest)
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let mut units = read.read_unit_match();
    let dest = read.read_jump_pos();
    if feature_disabled("deaths") || read.is_invalid() {
        return;
    }

    let mut globals = Globals::get("ais deaths");
    match modifier.ty {
        ModifierType::Read(read) => {
            let sum = units
                .iter_flatten_groups()
                .map(|unit_id| {
                    players
                        .players()
                        .map(|player| {
                            game.unit_deaths(player, unit_id)
                        })
                        .sum::<u32>()
                })
                .sum::<u32>();
            read.compare_and_act(sum, amount, script, dest, old_pos, &globals.ai_scripts);
        }
        ModifierType::Write(write) => {
            for unit_id in units.iter_flatten_groups() {
                for player in players.players() {
                    let deaths = game.unit_deaths(player, unit_id);
                    let new = write.apply(deaths, amount, &mut globals.rng);
                    game.set_unit_deaths(player, unit_id, new);
                }
            }
        }
    }
}

pub unsafe extern fn bw_kills(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    // kills(player, modifier, amount, unit, dest)
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let mut units = read.read_unit_match();
    let dest = read.read_jump_pos();
    if feature_disabled("bw_kills") || read.is_invalid() {
        return;
    }

    let mut globals = Globals::get("ais bw kills");
    match modifier.ty {
        ModifierType::Read(read) => {
            let sum = units
                .iter_flatten_groups()
                .map(|unit_id| {
                    players
                        .players()
                        .map(|player| {
                            game.unit_kills(player, unit_id)
                        })
                        .sum::<u32>()
                })
                .sum::<u32>();
            read.compare_and_act(sum, amount, script, dest, old_pos, &globals.ai_scripts);
        }
        ModifierType::Write(write) => {
            for unit_id in units.iter_flatten_groups() {
                for player in players.players() {
                    let kills = game.unit_kills(player, unit_id);
                    let new = write.apply(kills, amount, &mut globals.rng);
                    game.set_unit_kills(player, unit_id, new);
                }
            }
        }
    }
}

pub unsafe extern fn build_at(script: *mut bw::AiScript) {
    // build_at(unit_group, pos, flags)
    let mut read = ScriptData::new(script);
    let units = read.read_unit_match();
    let mut pos = read.read_position().center;
    let flags = read.read_u32();

    let town = match Town::from_ptr((*script).town) {
        Some(s) => s,
        None => return,
    };
    if pos.x as u16 == 65500 {
        pos = (*town.0).position;
    }
    let mut globals = Globals::get("ais build_at");
    let vec = globals.build_at.entry(town)
        .or_insert_with(|| Vec::new());
    if flags & 0x8000_0000 != 0 {
        vec.retain(|x| {
            x.position != pos || !x.unit_match.matches_match(&units)
        })
    } else {
        vec.push(globals::BuildAt {
            unit_match: units,
            flags,
            position: pos,
        });
    }
}

pub unsafe extern fn wait_rand(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mut r1 = read.read_u32();
    let mut r2 = read.read_u32();
    if read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais wait_rand");
    if r1 > r2 {
        mem::swap(&mut r1, &mut r2);
    }
    (*script).wait = globals.rng.synced_rand(r1..r2 + 1);
}

pub unsafe extern fn kills_command(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    // kills(player1, player2, modifier, amount, unit, dest)
    let mut read = ScriptData::new(script);
    let player1 = read.read_player_match(game);
    let player2 = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let mut units = read.read_unit_match();
    let dest = read.read_jump_pos();
    if feature_disabled("kills_command") || read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais kills");

    match modifier.ty {
        ModifierType::Read(read) => {
            let sum = units
                .iter_flatten_groups()
                .map(|unit_id| {
                    player1
                        .players()
                        .map(|p1| {
                            player2
                                .players()
                                .map(|p2| globals.kills_table.count_kills(p1, p2, unit_id.0))
                                .sum::<u32>()
                        })
                        .sum::<u32>()
                })
                .sum::<u32>();
            read.compare_and_act(sum, amount, script, dest, old_pos, &globals.ai_scripts);
        }
        ModifierType::Write(write) => {
            for unit_id in units.iter_flatten_groups() {
                for p1 in player1.players() {
                    for p2 in player2.players() {
                        let kpos = globals::KCPos::new(p1, p2, unit_id.0);
                        match write {
                            WriteModifier::Set => globals.kills_table.try_set(kpos, amount),
                            WriteModifier::Add => globals.kills_table.try_add(kpos, amount),
                            WriteModifier::Subtract => globals.kills_table.try_sub(kpos, amount),
                            WriteModifier::Randomize => {
                                if amount != 0 {
                                    let random = globals.rng.synced_rand(0..amount);
                                    globals.kills_table.try_set(kpos, random);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn increment_deaths(
    target: *mut bw::Unit,
    attacker_p_id: u8,
    orig: unsafe extern fn(*mut bw::Unit, u8),
) {
    let unit_id = (*target).unit_id;
    let amount = 1;
    let player = (*target).player;
    {
        let mut globals = Globals::get("increment deaths hook");
        let kpos = globals::KCPos::new(attacker_p_id, player, unit_id);
        globals.kills_table.try_add(kpos, amount);
    }
    orig(target, attacker_p_id);
}

pub unsafe extern fn print_command(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let msg = read.read_string();
    if read.is_invalid() {
        return;
    }
    // The null isn't included in returned slice, but it still is always there for ptr.
    samase::print_text(msg.as_ptr());
}

pub unsafe extern fn aise_debug(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let msg = read.read_string();
    if read.is_invalid() {
        return;
    }
    debug!("{}", String::from_utf8_lossy(msg));
}

pub unsafe extern fn debug_name(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let name = read.read_string();
    if read.is_invalid() {
        return;
    }

    if crate::debug_ui_active() {
        let globals = Globals::get("debug_name");
        let script = Script::ptr_from_bw(script);
        let name = String::from_utf8_lossy(name);
        if globals.ai_scripts.contains(script) {
            (*script).debug_name = name.into();
        } else {
            bw_print!("Wait 1 before using debug_name {}", name);
        }
    }
}

pub unsafe extern fn ping(script: *mut bw::AiScript) {
    #![cfg_attr(target_pointer_width = "64", allow(unused_variables))]
    let mut read = ScriptData::new(script);
    let x = read.read_u16();
    let y = read.read_u16();
    let color = read.read_u8();
    if crate::is_scr() || read.is_invalid() {
        bw_print!("ping is not supported in SCR");
        return;
    }
    #[cfg(target_pointer_width = "32")]
    bw::ping_minimap(x as u32, y as u32, color);
}

pub unsafe extern fn player_jump(script: *mut bw::AiScript) {
    #![cfg_attr(target_pointer_width = "64", allow(unused_variables))]
    let mut read = ScriptData::new(script);
    let player = read.read_string();
    let dest = read.read_jump_pos();
    if feature_disabled("player_jump") || read.is_invalid() {
        return;
    }
    if crate::is_scr() {
        bw_print!("player_jump is not supported in SCR");
        return;
    }
    #[cfg(target_pointer_width = "32")]
    {
        if *bw::is_multiplayer != 0 {
            // Not doing this since it'd desync
            return;
        }
        let player_name = {
            let len = bw::player_name
                .iter()
                .position(|&x| x == 0)
                .unwrap_or_else(|| bw::player_name.len());
            &bw::player_name[..len]
        };
        if player_name.eq_ignore_ascii_case(&player) {
            (*script).pos = dest;
        }
    }
}

pub unsafe extern fn upgrade_jump(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let upgrade = UpgradeId(read.read_u16());
    let level = read.read_u8();
    let dest = read.read_jump_pos();
    if feature_disabled("upgrade_jump") || read.is_invalid() {
        return;
    }
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = r.action.get_read_req();
            let jump = players.players().any(|player| {
                let up_lev = game.upgrade_level(player, upgrade);
                r.compare(u32::from(up_lev), u32::from(level))
            });

            if jump == read_req {
                let globals = Globals::get("ais upgrade jump");
                r.action.do_action(script, dest, old_pos, &globals.ai_scripts);
            }
        }
        ModifierType::Write(w) => {
            let mut globals = Globals::get("ais upgrade_jump");
            for player in players.players() {
                let old = game.upgrade_level(player, upgrade);
                let new = w.apply(u32::from(old), u32::from(level), &mut globals.rng);
                game.set_upgrade_level(player, upgrade, new as u8);
            }
        }
    };
}

pub unsafe extern fn load_bunkers(script: *mut bw::AiScript) {
    // load_bunkers(area, load_unit, quantity, bunker_unit, bunker_quantity, priority)
    let mut globals = Globals::get("ais load_bunkers");
    let mut read = ScriptData::new(script);
    let player = (*script).player;
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let mut unit_id = read.read_unit_match();
    let quantity = read.read_u8();
    let mut bunker_id = read.read_unit_match();
    let bunker_quantity = read.read_u8();
    let priority = read.read_u8();
    unit_id.replace_ids(&mut globals.unit_replace);
    bunker_id.replace_ids(&mut globals.unit_replace);

    if feature_disabled("load_bunkers") || read.is_invalid() {
        return;
    }
    let decl = BunkerDecl {
        pos: src.area,
        unit_id: unit_id,
        bunker_id: bunker_id,
        quantity: quantity,
        player: player as u8,
        priority: priority,
        bunker_quantity: bunker_quantity,
    };

    globals.bunker_states.add(decl);
}

pub unsafe extern fn unit_avail(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut globals = Globals::get("ais unit_avail");
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let avail_modifier = read.read_u8();
    let unit = UnitId(read.read_u16());
    let dest = read.read_jump_pos();
    if feature_disabled("unit_avail") || read.is_invalid() {
        return;
    }
    if avail_modifier >= 2 {
        bw_print!("Invalid modifier in unit_avail");
        return;
    }
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = r.action.get_read_req();
            let jump = players.players().any(|player| {
                let avail = game.unit_available(player, unit);
                r.compare(avail as u32, u32::from(avail_modifier))
            });
            if jump == read_req {
                r.action.do_action(script, dest, old_pos, &globals.ai_scripts);
            }
        }
        ModifierType::Write(w) => {
            for player in players.players() {
                let new_value = match w {
                    WriteModifier::Add | WriteModifier::Subtract => {
                        bw_print!("Add/subtract modifier is not supported in unit_avail");
                        return;
                    }
                    WriteModifier::Set => avail_modifier,
                    WriteModifier::Randomize => globals.rng.synced_rand(0..2) as u8,
                };
                game.set_unit_availability(player, unit, new_value != 0);
            }
        }
    };
}

pub unsafe extern fn tech_jump(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let tech = TechId(read.read_u16());
    let level = read.read_u8();
    let dest = read.read_jump_pos();
    if feature_disabled("tech_jump") || read.is_invalid() {
        return;
    }
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = r.action.get_read_req();
            let jump = players.players().any(|player| {
                let up_lev = game.tech_researched(player, tech);
                r.compare(u32::from(up_lev), u32::from(level))
            });
            if jump == read_req {
                let globals = Globals::get("ais tech_jump");
                r.action.do_action(script, dest, old_pos, &globals.ai_scripts);
            }
        }
        ModifierType::Write(w) => {
            let mut globals = Globals::get("ais tech_jump");
            for player in players.players() {
                let old = game.tech_researched(player, tech);
                let new = w.apply(u32::from(old), u32::from(level), &mut globals.rng);
                game.set_tech_level(player, tech, new as u8);
            }
        }
    };
}

pub unsafe extern fn tech_avail(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let tech = TechId(read.read_u16());
    let level = read.read_u8();
    let dest = read.read_jump_pos();
    if feature_disabled("tech_avail") || read.is_invalid() {
        return;
    }
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = r.action.get_read_req();
            let jump = players.players().any(|player| {
                let up_lev = game.tech_available(player, tech);
                r.compare(u32::from(up_lev), u32::from(level))
            });
            if jump == read_req {
                let globals = Globals::get("ais tech_avail");
                r.action.do_action(script, dest, old_pos, &globals.ai_scripts);
            }
        }
        ModifierType::Write(w) => {
            let mut globals = Globals::get("ais tech_avail");
            for player in players.players() {
                let old = game.tech_available(player, tech);
                let new = w.apply(u32::from(old), u32::from(level), &mut globals.rng);
                game.set_tech_availability(player, tech, new as u8);
            }
        }
    };
}

pub unsafe extern fn random_call(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let chance = read.read_u8();
    let dest = read.read_jump_pos();
    if feature_disabled("random_call") || read.is_invalid() {
        return;
    }

    let mut globals = Globals::get("ais random_call");
    let random = globals.rng.synced_rand(0..256);
    if u32::from(chance) > random {
        let ret = (*script).pos;
        (*script).pos = dest;
        (*Script::ptr_from_bw(script)).call_stack_push(ret, &globals.ai_scripts);
    }
}

pub unsafe extern fn attack_rand(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mut r1 = read.read_u8() as u32;
    let mut r2 = read.read_u8() as u32;
    let unit = read.read_u16();
    if feature_disabled("attack_rand") || read.is_invalid() {
        return;
    }
    if r1 > r2 {
        mem::swap(&mut r1, &mut r2);
    }
    let mut globals = Globals::get("ais attack_rand");
    let random = globals.rng.synced_rand(r1..r2 + 1);
    add_to_attack_force(&globals, (*script).player as u8, UnitId(unit), random);
}

unsafe fn add_to_attack_force(globals: &Globals, player: u8, unit: UnitId, amount: u32) {
    let unit = globals.unit_replace.replace_check(unit);
    let ai = ai::PlayerAi::get(player);
    // Reuse slots that may have been deleted during the attack
    let attack_force = &mut (*ai.0).attack_force[..];
    let free_slots = attack_force
        .iter_mut()
        .filter(|&&mut x| x == 0 || x == unit::id::NONE.0 + 1)
        .take(amount as usize);
    for out in free_slots {
        *out = unit.0 + 1;
    }
}

pub unsafe extern fn bring_jump(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = bw::game();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let unit_id = read.read_unit_match();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let dest = read.read_jump_pos();
    if feature_disabled("bring_jump") || read.is_invalid() {
        return;
    }

    let read = match modifier.ty {
        ModifierType::Read(r) => r,
        ModifierType::Write(w) => {
            bw_print!("Used writing modifier {:?} in bring_jump", w);
            return;
        }
    };
    let search = aiscript_unit_search(game);
    let count = search
        .search_iter(&src.area)
        .filter(|u| players.matches(u.player()) && unit_id.matches(u))
        .count() as u32;
    let globals = Globals::get("ais bring_jump");
    read.compare_and_act(count, amount, script, dest, old_pos, &globals.ai_scripts);
}

unsafe fn ai_region(player: u32, region: u16) -> *mut bw::AiRegion {
    bw::ai_regions(player).offset(region as isize)
}

#[derive(Eq, PartialEq)]
pub struct Position {
    pub center: bw::Point,
    pub area: bw::Rect,
}

impl Position {
    pub fn from_point(x: i16, y: i16) -> Position {
        Position {
            center: bw::Point {
                x,
                y,
            },
            area: bw::Rect {
                left: x,
                right: x.saturating_add(1),
                top: y,
                bottom: y.saturating_add(1),
            },
        }
    }

    pub fn from_rect32(rect: &bw::Rect32) -> Position {
        Position {
            center: bw::Point {
                x: (rect.left + (rect.right - rect.left) / 2) as i16,
                y: (rect.top + (rect.bottom - rect.top) / 2) as i16,
            },
            area: bw::Rect {
                left: rect.left as i16,
                right: rect.right as i16,
                top: rect.top as i16,
                bottom: rect.bottom as i16,
            },
        }
    }

    pub fn extend_area(&mut self, amt: i16) {
        self.area.left = self.area.left.saturating_sub(amt);
        self.area.right = self.area.right.saturating_add(amt);
        self.area.top = self.area.top.saturating_sub(amt);
        self.area.bottom = self.area.bottom.saturating_add(amt);
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bw::Rect {
            left,
            right,
            top,
            bottom,
        } = self.area;

        if left == right - 1 && top == bottom - 1 {
            write!(f, "{}, {}", left, right)
        } else {
            write!(f, "{}, {}, {}, {}", left, top, right, bottom)
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ReadModifierType {
    AtLeast,
    AtMost,
    Exactly,
}

impl ReadModifierType {
    pub fn compare(&self, value: u32, constant: u32) -> bool {
        match self {
            ReadModifierType::AtLeast => value >= constant,
            ReadModifierType::AtMost => value <= constant,
            ReadModifierType::Exactly => value == constant,
        }
    }
}

impl ReadModifier {
    pub fn compare(&self, value: u32, constant: u32) -> bool {
        self.ty.compare(value, constant)
    }

    pub unsafe fn compare_and_act(
        &self,
        value: u32,
        constant: u32,
        script: *mut bw::AiScript,
        dest: u32,
        old_pos: u32,
        scripts: &BlockAllocSet<Script>,
    ) {
        let read_req = self.action.get_read_req();
        if self.ty.compare(value, constant) == read_req {
            self.action.do_action(script, dest, old_pos, scripts);
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum WriteModifier {
    Set,
    Add,
    Subtract,
    Randomize,
}

impl WriteModifier {
    pub fn apply(self, old: u32, operand: u32, rng: &mut Rng) -> u32 {
        match self {
            WriteModifier::Add => old.saturating_add(operand),
            WriteModifier::Subtract => old.saturating_sub(operand),
            WriteModifier::Set => operand,
            WriteModifier::Randomize => {
                if operand != 0 {
                    rng.synced_rand(0..operand)
                } else {
                    bw_print!("Cannot randomize with 0 cases");
                    !0
                }
            }
        }
    }
}

struct ReadModifier {
    ty: ReadModifierType,
    action: ModifierAction,
}

enum ModifierType {
    Read(ReadModifier),
    Write(WriteModifier),
}

struct TriggerModifier {
    ty: ModifierType,
}

#[derive(Copy, Clone, Debug)]
enum ModifierAction {
    Jump,
    Call,
    Wait,
}

impl ModifierAction {
    /// Whether the action should be done on comparision being false or true.
    pub fn get_read_req(&self) -> bool {
        match self {
            ModifierAction::Jump | ModifierAction::Call => true,
            ModifierAction::Wait => false,
        }
    }

    pub unsafe fn do_action(
        &self,
        script: *mut bw::AiScript,
        dest: u32,
        old_pos: u32,
        scripts: &BlockAllocSet<Script>,
    ) {
        match self {
            ModifierAction::Jump => {
                (*script).pos = dest;
            }
            ModifierAction::Call => {
                let ret = (*script).pos;
                (*script).pos = dest;
                (*Script::ptr_from_bw(script)).call_stack_push(ret, scripts);
            }
            ModifierAction::Wait => {
                (*script).pos = old_pos;
                (*script).wait = 30;
            }
        }
    }
}

struct BoolModifier {
    value: bool,
    action: ModifierAction,
}

// For reading aiscript.bin or bwscript.bin (or some other?) bytes
pub struct ScriptData {
    start: *const u8,
    pos: *const u8,
    orig_pos: u32,
    length: u32,
    script: *mut bw::AiScript,
    invalid: bool,
    mutate: bool,
}

impl ScriptData {
    pub unsafe fn new(script: *mut bw::AiScript) -> ScriptData {
        Self::new_with_pos(script, (*script).pos, true)
    }

    pub unsafe fn new_with_pos(script: *mut bw::AiScript, pos: u32, mutate: bool) -> ScriptData {
        let (script_bytes, script_len) = match (*script).flags & 0x1 != 0 {
            false => bw::aiscript_bin(),
            true => bw::bwscript_bin(),
        };
        let invalid = pos < 4 || pos >= script_len;
        if invalid && pos == (*script).pos && mutate {
            bw_print!(
                "Invalid script pos {:x} / {}",
                pos, (*Script::ptr_from_bw(script)).debug_string(),
            );
            (*script).wait = !1;
        }
        ScriptData {
            start: script_bytes,
            orig_pos: pos,
            pos: script_bytes.add(pos as usize) as *const u8,
            length: script_len,
            script,
            invalid,
            mutate,
        }
    }

    pub fn is_invalid(&self) -> bool {
        self.invalid
    }

    pub unsafe fn read_player_match(&mut self, game: Game) -> PlayerMatch {
        let mut cont = true;
        let mut result = PlayerMatch {
            players: [false; 12],
        };
        let current_player = (*self.script).player as u8;
        while cont {
            let byte = self.read_u8();
            cont = byte & 0x80 != 0;
            let player = byte & 0x7f;
            match player {
                x @ 0..=11 => result.players[x as usize] = true,
                13 => result.players[current_player as usize] = true,
                // Foes, allies
                14 | 15 => {
                    let allies = player == 15;
                    let players = (0..12)
                        .filter(|&x| x != current_player)
                        .filter(|&x| game.allied(current_player, x) == allies);
                    for player in players {
                        result.players[player as usize] = true;
                    }
                }
                // All players
                17 => result.players = [true; 12],
                // Forces
                18 | 19 | 20 | 21 => {
                    // Forces are 1-based
                    let force = 1 + player - 18;
                    for player in (0..8).filter(|&x| (**game).player_forces[x as usize] == force) {
                        result.players[player as usize] = true;
                    }
                }
                x => {
                    bw_print!("Unsupported player: {:x}", x);
                }
            };
        }
        result
    }

    fn read_bool_modifier(&mut self) -> BoolModifier {
        let val = self.read_u8();
        let action = match val >> 1 {
            0x20 => ModifierAction::Wait,
            0x40 => ModifierAction::Call,
            0x0 => ModifierAction::Jump,
            _ => {
                bw_print!("Unsupported modifier: {:x}", val);
                ModifierAction::Jump
            }
        };

        BoolModifier {
            action,
            value: val & 1 != 0,
        }
    }

    fn read_modifier(&mut self) -> TriggerModifier {
        let val = self.read_u8();
        let action = match val & 0xc0 {
            0x40 => ModifierAction::Wait,
            0x80 => ModifierAction::Call,
            0x0 => ModifierAction::Jump,
            _ => {
                bw_print!("Unsupported modifier: {:x}", val);
                ModifierAction::Jump
            }
        };
        let read_modifier = |ty, action| ReadModifier {
            ty,
            action,
        };
        TriggerModifier {
            ty: match val & 0x1f {
                // Matching triggers in chk
                0 => ModifierType::Read(read_modifier(ReadModifierType::AtLeast, action)),
                1 => ModifierType::Read(read_modifier(ReadModifierType::AtMost, action)),
                7 => ModifierType::Write(WriteModifier::Set),
                8 => ModifierType::Write(WriteModifier::Add),
                9 => ModifierType::Write(WriteModifier::Subtract),
                10 => ModifierType::Read(read_modifier(ReadModifierType::Exactly, action)),
                11 => ModifierType::Write(WriteModifier::Randomize),
                x => {
                    bw_print!("Unsupported modifier: {:x}", x);
                    ModifierType::Read(read_modifier(ReadModifierType::AtLeast, action))
                }
            },
        }
    }

    pub fn read_unit_match(&mut self) -> UnitMatch {
        let val = self.read_u16();
        if val > 0xff00 {
            let repeat = val & 0xff;
            let units = (0..repeat).map(|_| UnitId(self.read_u16())).collect::<Vec<_>>();
            let has_groups = units.iter().any(|x| {
                x.0 >= unit::id::ANY_UNIT.0 && x.0 <= unit::id::GROUP_FACTORIES.0
            });
            UnitMatch {
                units,
                has_groups,
            }
        } else if val < 0x1000 {
            UnitMatch::single(UnitId(val))
        } else {
            bw_print!("Invalid script encoding: unit match {:x}", val);
            UnitMatch {
                units: vec![],
                has_groups: false,
            }
        }
    }

    fn read_position(&mut self) -> Position {
        let x = self.read_u16();
        let y = self.read_u16();
        if x == 65535 {
            let location = if y >= 255 {
                bw_print!("Invalid location id 0x{:x} used", y);
                bw::location(63)
            } else {
                bw::location(y as u8)
            };
            Position::from_rect32(&location.area)
        } else if x == 65534 {
            let area = unsafe { (*self.script).area };
            Position::from_rect32(&area)
        } else {
            // !1, !1 is used for inherit position in create_script
            Position::from_point(x as i16, y as i16)
        }
    }

    pub fn read_jump_pos(&mut self) -> u32 {
        let long_jumps = unsafe { (self.start as *const u32).read_unaligned() >= 0x10000 };
        let pos = if long_jumps {
            self.read_u32()
        } else {
            self.read_u16().into()
        };

        if pos >= self.length && !self.invalid {
            if self.mutate {
                unsafe {
                    bw_print!(
                        "Invalid jump dest at offset {:x} / {}",
                        self.orig_pos, (*Script::ptr_from_bw(self.script)).debug_string(),
                    );
                    (*self.script).wait = !1;
                }
            }
            self.invalid = true;
            0x8000_0000
        } else {
            pos
        }
    }

    pub fn read_u8(&mut self) -> u8 {
        self.read()
    }

    pub fn read_u16(&mut self) -> u16 {
        self.read()
    }

    pub fn read_u32(&mut self) -> u32 {
        self.read()
    }

    // Maybe not a good idea to inline(never) but saves 1k in binary size
    #[inline(never)]
    pub fn read<T: Copy>(&mut self) -> T {
        unsafe {
            let size = mem::size_of::<T>();
            let val = (self.pos as *const T).read_unaligned();
            self.pos = self.pos.add(size);
            if self.mutate {
                (*self.script).pos = (*self.script).pos.wrapping_add(size as u32);
            }
            val
        }
    }

    pub unsafe fn read_string(&mut self) -> &'static [u8] {
        let length = (0usize..).position(|x| *self.pos.add(x) == 0).unwrap_or(0);
        let val = slice::from_raw_parts(self.pos, length);
        self.pos = self.pos.add(length + 1);
        if self.mutate {
            (*self.script).pos += length as u32 + 1;
        }
        val
    }
}

#[derive(Serialize, Deserialize)]
#[repr(C)]
pub struct Script {
    #[serde(serialize_with = "serialize_bw_script")]
    #[serde(deserialize_with = "deserialize_bw_script")]
    bw: bw::AiScript,
    delete_mark: bool,
    pub call_stack: Vec<u32>,
    debug_name: String,
}

#[derive(Serialize, Deserialize)]
struct SerializeBwScript {
    pos: u32,
    wait: u32,
    player: u32,
    area: bw::Rect32,
    center: bw::Point32,
    town: Option<Town>,
    flags: u32,
}

fn serialize_bw_script<S: Serializer>(script: &bw::AiScript, s: S) -> Result<S::Ok, S::Error> {
    let bw::AiScript {
        next: _,
        prev: _,
        pos,
        wait,
        player,
        area,
        center,
        town,
        flags,
    } = *script;
    SerializeBwScript {
        pos,
        wait,
        player,
        area,
        center,
        town: Town::from_ptr(town),
        flags,
    }
    .serialize(s)
}

// The deserialization of next/prev is handled in the container deserialization.
fn deserialize_bw_script<'de, D: Deserializer<'de>>(d: D) -> Result<bw::AiScript, D::Error> {
    let result: SerializeBwScript = SerializeBwScript::deserialize(d)?;
    Ok(bw::AiScript {
        next: null_mut(),
        prev: null_mut(),
        pos: result.pos,
        wait: result.wait,
        player: result.player,
        area: result.area,
        center: result.center,
        town: result.town.map(|x| x.0).unwrap_or_else(null_mut),
        flags: result.flags,
    })
}

pub fn serialize_scripts<S: Serializer>(
    scripts: &BlockAllocSet<Script>,
    s: S,
) -> Result<S::Ok, S::Error> {
    use serde::ser::SerializeSeq;

    let mut state = globals::save_state("serialize_scripts");
    let state = state
        .as_mut()
        .expect("Serializing AI scripts without state init");
    debug!("Serializing {} scripts", scripts.len());
    let mut s = s.serialize_seq(Some(scripts.len()))?;
    let mut script = state.first_ai_script.0;
    while !script.is_null() {
        unsafe {
            s.serialize_element(&*Script::ptr_from_bw(script))?;
            script = (*script).next;
        }
    }
    s.end()
}

pub fn deserialize_scripts<'de, D: Deserializer<'de>>(
    d: D,
) -> Result<BlockAllocSet<Script>, D::Error> {
    struct Visitor;
    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = (*mut bw::AiScript, BlockAllocSet<Script>);
        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(formatter, "a sequence of AI scripts")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let mut first_script: *mut bw::AiScript = null_mut();
            let mut latest = null_mut();
            let mut container = BlockAllocSet::new();
            while let Some(script) = seq.next_element::<Script>()? {
                let ptr = container.alloc(script);
                unsafe {
                    if first_script.is_null() {
                        first_script = &mut (*ptr).bw;
                    }
                    (*ptr).bw.prev = latest;
                    if !latest.is_null() {
                        (*latest).next = &mut (*ptr).bw;
                    }
                    latest = &mut (*ptr).bw;
                }
            }
            Ok((first_script, container))
        }
    }
    let (first_script, container) = d.deserialize_seq(Visitor)?;
    debug!(
        "Deserialized {} scripts, first {:?}",
        container.len(),
        first_script
    );
    bw::set_first_ai_script(first_script);
    Ok(container)
}

impl Script {
    pub fn ptr_from_bw(bw: *mut bw::AiScript) -> *mut Script {
        bw as *mut Script
    }

    pub fn debug_string(&self) -> String {
        if let Some(name) = self.debug_name() {
            format!(
                "'{}', player {}, pos {}, {}",
                name, self.bw.player, self.bw.center.x, self.bw.center.y,
            )
        } else {
            format!(
                "Player {}, pos {}, {}",
                self.bw.player, self.bw.center.x, self.bw.center.y,
            )
        }
    }

    fn call_stack_push(&mut self, pos: u32, scripts: &BlockAllocSet<Script>) {
        if !scripts.contains(self as *const _ as *mut _) {
            panic!("Aiscript call used on BW object.\nUse wait 1 before any calls.");
        }
        self.call_stack.push(pos);
    }

    fn call_stack_pop(&mut self, scripts: &BlockAllocSet<Script>) -> Option<u32> {
        if !scripts.contains(self as *const _ as *mut _) {
            return None;
        }
        self.call_stack.pop()
    }

    pub fn debug_name(&self) -> Option<&str> {
        if self.debug_name.len() != 0 {
            Some(&self.debug_name)
        } else {
            None
        }
    }
}

const AISCRIPT_LIMIT: usize = 8192;

pub(crate) fn claim_bw_allocated_scripts(globals: &mut Globals) {
    unsafe {
        let first = bw::first_ai_script();
        let first_free = bw::first_free_ai_script();

        if globals.ai_scripts.len() >= AISCRIPT_LIMIT {
            let (_, new_first_free) = clean_free_scripts(&globals.ai_scripts, first_free, !0);
            if let Some(x) = new_first_free {
                bw::set_first_free_ai_script(x);
            }
            return;
        }

        if first_free.is_null() {
            bw_print!(
                "Warning: ran out of AI scripts for the frame, some of the scripts \
                 may not have started.",
            );
        }
        let (new_first, mut new_first_free) =
            take_bw_allocated_scripts(&mut globals.ai_scripts, first, first_free);
        if new_first != first {
            bw::set_first_ai_script(new_first);
        }
        let (deleted, even_newer_first_free) =
            clean_free_scripts(&globals.ai_scripts, new_first_free, !0);
        if deleted != 0 {
            if let Some(x) = even_newer_first_free {
                new_first_free = x;
            }
            let delete_count = clear_deleted_scripts(&mut globals.ai_scripts, new_first);
            assert_eq!(delete_count, deleted);
        }
        if new_first_free != first_free {
            bw::set_first_free_ai_script(new_first_free);
        }
    }
}

// Remove aise objects from bw's free list
unsafe fn clean_free_scripts(
    scripts: &BlockAllocSet<Script>,
    free_list_pos: *mut bw::AiScript,
    delete_count: usize,
) -> (usize, Option<*mut bw::AiScript>) {
    let mut script = free_list_pos;
    let mut new_first_free = None;
    let mut deleted = 0;
    while !script.is_null() && delete_count != deleted {
        if scripts.contains(Script::ptr_from_bw(script)) {
            if !(*script).prev.is_null() {
                (*(*script).prev).next = (*script).next;
            } else {
                new_first_free = Some((*script).next);
            }
            if !(*script).next.is_null() {
                (*(*script).next).prev = (*script).prev;
            }
            deleted += 1;
        }
        script = (*script).next;
    }
    (deleted, new_first_free)
}

// return delete count
unsafe fn clear_deleted_scripts(
    scripts: &mut BlockAllocSet<Script>,
    bw_list: *mut bw::AiScript,
) -> usize {
    for script in scripts.iter() {
        (*script).delete_mark = true;
    }
    let mut script = bw_list;
    let mut count = 0;
    while script != null_mut() {
        assert!(scripts.contains(Script::ptr_from_bw(script)));
        (*Script::ptr_from_bw(script)).delete_mark = false;
        script = (*script).next;
        count += 1;
    }
    let delete_count = scripts.len() - count;
    if delete_count != 0 {
        scripts.retain(|x| !(*x).delete_mark);
    }
    delete_count
}

unsafe fn take_bw_allocated_scripts(
    scripts: &mut BlockAllocSet<Script>,
    first: *mut bw::AiScript,
    first_free: *mut bw::AiScript,
) -> (*mut bw::AiScript, *mut bw::AiScript) {
    let mut script = first;
    let mut last_new_free: Option<*mut bw::AiScript> = None;
    let mut first_new_free = None;
    let mut first_new: Option<*mut bw::AiScript> = None;
    let mut prev: Option<*mut bw::AiScript> = None;
    while !script.is_null() {
        if scripts.len() == AISCRIPT_LIMIT {
            bw_print!("AI script limit reached.");
            break;
        }
        if !scripts.contains(Script::ptr_from_bw(script)) {
            let taken = scripts.alloc(Script {
                bw: *script,
                delete_mark: false,
                call_stack: Vec::new(),
                debug_name: String::new(),
            });
            if let Some(prev) = prev {
                (*prev).next = &mut (*taken).bw;
            }
            (*taken).bw.prev = prev.unwrap_or_else(null_mut);
            if first_new.is_none() {
                first_new = Some(&mut (*taken).bw);
            }
            if first_new_free.is_none() {
                first_new_free = Some(script);
            }
            prev = Some(&mut (*taken).bw);
            if let Some(last_new_free) = last_new_free {
                (*last_new_free).next = script;
                (*script).prev = last_new_free;
            } else {
                (*script).prev = null_mut();
            }
            last_new_free = Some(script);
        } else {
            if first_new.is_none() {
                first_new = Some(script);
            }
            if let Some(prev) = prev {
                (*prev).next = script;
            }
            (*script).prev = prev.unwrap_or_else(null_mut);
            prev = Some(script);
        }
        script = (*script).next;
    }
    if let Some(prev) = prev {
        (*prev).next = script;
        if !script.is_null() {
            (*script).prev = prev;
        }
    }
    if let Some(new_free) = last_new_free {
        (*new_free).next = first_free;
        if !first_free.is_null() {
            (*first_free).prev = new_free;
        }
    }
    (
        first_new.unwrap_or(first),
        first_new_free.unwrap_or(first_free),
    )
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn ai_spellcast_hook(
    revenge: bool,
    unit: *mut bw::Unit,
    orig: unsafe extern fn(bool, *mut bw::Unit) -> u32,
) -> u32 {
    let globals = Globals::get("ai spellcast hook");
    let ai_mode = &globals.ai_mode[(*unit).player as usize];
    if !ai_mode.retaliation && revenge {
        0
    } else {
        orig(revenge, unit)
    }
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn update_placement_hook(
    builder: *mut bw::Unit,
    player: u8,
    x_tile: u32,
    y_tile: u32,
    unit_id: u16,
    placement_state_entry: u8,
    check_vision: u8,
    also_invisible: u8,
    without_vision: u8,
    orig: unsafe extern fn(*mut bw::Unit, u8, u32, u32, u16, u8, u8, u8, u8) -> u32,
) -> u32 {
    let result = orig(
        builder,
        player,
        x_tile,
        y_tile,
        unit_id,
        placement_state_entry,
        check_vision,
        also_invisible,
        without_vision,
    );
    result
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn choose_building_placement(
    unit_id: u32,
    position_xy: u32,
    out_pos: *mut bw::Point,
    area_tiles: u32,
    builder: *mut bw::Unit,
    orig: unsafe extern fn(u32, u32, *mut bw::Point, u32, *mut bw::Unit) -> u32,
) -> u32 {
    let result = orig(unit_id, position_xy, out_pos, area_tiles, builder);

    if out_pos.is_null() {
        return result;
    }
    let builder = match Unit::from_ptr(builder) {
        Some(s) => s,
        None => return result,
    };
    let ai = match builder.worker_ai() {
        Some(s) => s,
        None => return result,
    };
    let town = (*ai).town;
    let unit_id = UnitId(unit_id as u16);
    let player = builder.player();

    let globals = Globals::get("place building hook");
    let unit_search = UnitSearch::from_bw();
    if let Some(town_id) = globals.town_ids.iter().find(|x| x.town.0 == town) {
        let game = bw::game();
        let layouts = globals
            .base_layouts
            .layouts
            .iter()
            .filter(|x| {
                x.unit_id == unit_id &&
                    (x.town_id == town_id.id || (x.town_id == 255 && x.town.0 == town)) &&
                    x.player == player
            })
            .filter(|layout| {
                let existing_count = unit_search
                    .search_iter(&layout.pos)
                    .filter(|u| u.player() == layout.player && u.id() == layout.unit_id)
                    .count();
                existing_count < usize::from(layout.amount)
            });
        for layout in layouts {
            //uses tiles instead of pixels
            let rect_x = layout.pos.right / 32;
            let rect_y = layout.pos.bottom / 32;
            let pos_x = layout.pos.left / 32;
            let pos_y = layout.pos.top / 32;
            let offset_x = layout.pos.left - (pos_x * 32);
            let offset_y = layout.pos.top - (pos_y * 32);
            for i in pos_x..rect_x + 1 {
                for j in pos_y..rect_y + 1 {
                    let mut ok = check_placement(game, &unit_search, builder, i, j, unit_id);
                    if ok {
                        let mut workers = (*town).workers.first;
                        while workers != null_mut() {
                            let current = (*workers).parent;
                            if current != *builder {
                                let point = bw::Point { x: i * 32, y: j * 32 };
                                if (*current).order_target.pos == point {
                                    ok = false;
                                    break;
                                }
                            }
                            workers = (*workers).next;
                        }
                    }
                    if ok {
                        debug!(
                            "Placing {:x} to {:x}, {:x} for player {:x}",
                            unit_id.0, i, j, player
                        );
                        // modify coordinates by offset for buildings (offset will be 0 is width/height has even number of tiles)
                        // to avoid placement of structures unaligned to tile grid
                        (*out_pos).x = (i * 32) + offset_x;
                        (*out_pos).y = (j * 32) + offset_y;
                        return result;
                    }
                }
            }
        }
    }
    result
}

#[cfg(target_pointer_width = "32")]
unsafe fn check_placement(
    game: Game,
    unit_search: &UnitSearch,
    builder: Unit,
    x_tile: i16,
    y_tile: i16,
    unit_id: UnitId,
) -> bool {
    let map_width = game.map_width_tiles();
    let map_height = game.map_height_tiles();
    let zerg = unit_id.group_flags() & 0x1 != 0;
    let require_creep = unit_id.require_creep();
    let forbid_creep = !require_creep && !zerg;

    let placement = unit_id.placement();

    let area = bw::Rect {
        left: (x_tile * 32),
        top: (y_tile * 32),
        right: (x_tile * 32) + (placement.width as i16 / 2),
        bottom: (y_tile * 32) + (placement.height as i16 / 2),
    };
    let blocked = unit_search
        .search_iter(&area)
        .any(|u| u != builder && !u.is_air());
    if blocked {
        return false;
    }

    let width_tiles = placement.width / 32;
    let height_tiles = placement.height / 32;
    if x_tile as u16 + width_tiles > map_width || y_tile as u16 + height_tiles > map_height {
        return false;
    }

    if unit_id.is_town_hall() {
        let area = bw::Rect {
            left: (x_tile * 32).saturating_sub(3 * 32) - (placement.width as i16 / 2),
            top: (y_tile * 32).saturating_sub(3 * 32),
            right: (x_tile * 32) + placement.width as i16 + (3 * 32) - (placement.width as i16 / 2),
            bottom: (y_tile * 32) + placement.height as i16 + 3 * 32,
        };
        let res_units = unit_search
            .search_iter(&area)
            .any(|u| u.id().is_resource_container());
        if res_units {
            return false;
        }
    }

    let tile_flags = bw::tile_flags();
    for px in 0..width_tiles {
        for py in 0..height_tiles {
            let tile = *tile_flags.offset(
                (px + x_tile as u16 - 1) as isize + (map_width * (py + y_tile as u16 - 1)) as isize,
            );
            if tile & 0x0080_0000 != 0 {
                //unbuildable
                return false;
            }
            if tile & 0x1000_0000 != 0 {
                //creep disappearing
                if forbid_creep || require_creep {
                    return false;
                }
            }
            let creep_tile = tile & 0x0040_0000 != 0;
            if (!creep_tile && require_creep) || (creep_tile && forbid_creep) {
                return false;
            }
        }
    }
    if unit_id.require_psi() {
        let powered = bw::is_powered(
            (x_tile as u32) * 32,
            (y_tile as u32) * 32,
            builder.player(),
            unit_id.0 as u32,
        );
        if powered == 0 {
            return false;
        }
    }
    true
}

unsafe fn add_layout(
    script: *mut bw::AiScript,
    unit_id: UnitId,
    layout_modifier: u8,
    src: Position,
    amount: u8,
    town_id: u8,
    priority: u8,
) {
    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum LayoutModifier {
        Set,
        Remove,
    }
    if feature_disabled("base_layout") {
        return;
    }
    let layout_modifier = match layout_modifier {
        0 => LayoutModifier::Set,
        1 => LayoutModifier::Remove,
        x => {
            bw_print!("Unsupported layout modifier in base_layout: {:x}", x);
            return;
        }
    };

    let mut globals = Globals::get("ais base_layout");
    let town = Town::from_ptr((*script).town);
    if let Some(town) = town {
        let layout = BaseLayout {
            pos: src.area,
            player: (*script).player as u8,
            unit_id,
            amount,
            town_id,
            town,
            priority,
        };

        match layout_modifier {
            LayoutModifier::Set => globals.base_layouts.try_add(layout),
            LayoutModifier::Remove => globals.base_layouts.try_remove(&layout),
        }
    }
}

unsafe fn town_from_id(script: *mut bw::AiScript, globals: &mut Globals, id: u8) -> Option<Town> {
    let town_src = match id {
        255 => Town::from_ptr((*script).town),
        _ => globals
            .town_ids
            .iter()
            .find(|x| id == x.id && u32::from((*x.town.0).player) == (*script).player)
            .map(|x| x.town),
    };
    town_src
}

pub unsafe extern fn max_build(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mut globals = Globals::get("ais maxbuild");
    let town_id = read.read_u8();
    let unit_id = UnitId(read.read_u16());
    let quantity = read.read_u16();
    if feature_disabled("max_build") || read.is_invalid() {
        return;
    }
    let id = town_from_id(script, &mut globals, town_id);
    let town = match id {
        None => {
            bw_print!("town id {:x} for local queue do not exist", town_id);
            return;
        }
        Some(s) => Some(s),
    };
    let build = BuildMax {
        town,
        quantity,
        unit_id,
    };
    globals.build_max.add(build);
}

pub unsafe extern fn queue(script: *mut bw::AiScript) {
    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum LocalModifier {
        Local,
        Global,
    }
    //quantity unit_id factory_id id modifier location priority
    let mut read = ScriptData::new(script);
    let mut globals = Globals::get("ais queue");
    let quantity = read.read_u8();
    let unit_id = UnitId(read.read_u16());
    let factory_id = UnitId(read.read_u16());
    let town_id = read.read_u8();
    let modifier = read.read_u8();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let priority = read.read_u8();
    if feature_disabled("queue") || read.is_invalid() {
        return;
    }
    let modifier = match modifier {
        0 => LocalModifier::Local,
        1 => LocalModifier::Global,
        x => {
            bw_print!("Unsupported local modifier in queue: {:x}", x);
            return;
        }
    };
    let town = match modifier {
        LocalModifier::Global => None,
        LocalModifier::Local => {
            let id = town_from_id(script, &mut globals, town_id);
            match id {
                None => {
                    bw_print!("town id {:x} for local queue do not exist", town_id);
                    return;
                }
                Some(s) => Some(s),
            }
        }
    };
    let queue = UnitQueue {
        player: (*script).player as u8,
        current_quantity: quantity,
        unit_id,
        factory_id,
        town,
        pos: src.area,
        priority,
    };
    globals.queues.add(queue);
}

pub unsafe extern fn defense_command(script: *mut bw::AiScript) {
    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum DefenseType {
        Build,
        Use,
    }
    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    enum DefenseUnit {
        Ground,
        Air,
    }
    let mut read = ScriptData::new(script);
    let amount = read.read_u16();
    let unit = UnitId(read.read_u16());
    let defense_type = read.read_u8();
    let first = read.read_u8();
    let second = read.read_u8();
    if read.is_invalid() {
        return;
    }
    let ai_data = bw::player_ai((*script).player);
    let globals = Globals::get("ais defense");
    let unit = globals.unit_replace.replace_check(unit); // replace_requests
    let defense_type = match defense_type {
        0 => DefenseType::Build,
        1 => DefenseType::Use,
        x => {
            bw_print!("Unsupported defense type in defense command: {:x}", x);
            return;
        }
    };
    let first = match first {
        0 => DefenseUnit::Ground,
        1 => DefenseUnit::Air,
        x => {
            bw_print!("Unsupported defense direction in defense command: {:x}", x);
            return;
        }
    };
    let second = match second {
        0 => DefenseUnit::Ground,
        1 => DefenseUnit::Air,
        x => {
            bw_print!("Unsupported defense direction in defense command: {:x}", x);
            return;
        }
    };

    let defense_list: &mut [u16] = match defense_type {
        DefenseType::Build => {
            if first == DefenseUnit::Ground && second == DefenseUnit::Ground {
                &mut (*ai_data).defense_units[0]
            } else if first == DefenseUnit::Ground && second == DefenseUnit::Air {
                &mut (*ai_data).defense_units[1]
            } else if first == DefenseUnit::Air && second == DefenseUnit::Ground {
                &mut (*ai_data).defense_units[2]
            } else {
                &mut (*ai_data).defense_units[3]
            }
        }
        DefenseType::Use => {
            if first == DefenseUnit::Ground && second == DefenseUnit::Ground {
                &mut (*ai_data).defense_units[4]
            } else if first == DefenseUnit::Ground && second == DefenseUnit::Air {
                &mut (*ai_data).defense_units[5]
            } else if first == DefenseUnit::Air && second == DefenseUnit::Ground {
                &mut (*ai_data).defense_units[6]
            } else {
                &mut (*ai_data).defense_units[7]
            }
        }
    };
    let free_slots = defense_list
        .iter_mut()
        .skip_while(|x| **x != 0)
        .take(amount as usize);
    for defense_entry in free_slots {
        *defense_entry = unit.0 + 1;
    }
}
pub unsafe extern fn replace_requests(script: *mut bw::AiScript) {
    // Replacing guard, attack_add, defense, guard, do_morph, train, load_bunkers
    let mut read = ScriptData::new(script);
    let id_first = UnitId(read.read_u16());
    let id_second = UnitId(read.read_u16());
    if read.is_invalid() {
        return;
    }
    debug!("Replace unit {:x} with {:x}", id_first.0, id_second.0);
    let ai_data = bw::player_ai((*script).player);
    //replace build requests
    //unit_id
    let mut globals = Globals::get("ais replace_requests");
    globals.unit_replace.add(id_first, id_second);

    let replace_defense_requests = |list: &mut [u16]| {
        for x in list {
            if *x == id_first.0 + 1 {
                *x = id_second.0 + 1;
            }
        }
    };
    for i in 0..8 {
        replace_defense_requests(&mut (*ai_data).defense_units[i]);
    }
    let mut guard = bw::guard_ais((*script).player as u8);
    while guard != null_mut() {
        if (*guard).unit_id == id_first.0 {
            (*guard).unit_id = id_second.0;
        }
        guard = (*guard).next;
    }
}

pub unsafe extern fn lift_land(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    //unitId quantity liftLocation landLocation lifttownid landtownid hpval
    let unit_id = UnitId(read.read_u16());
    let amount = read.read_u8();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let mut tgt = read.read_position();
    let radius_target = read.read_u16();
    tgt.extend_area(radius_target as i16);
    let id_source = read.read_u8();
    let id_target = read.read_u8();
    let return_hp_percent = read.read_u8();
    if feature_disabled("lift_land") || read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais lift_land");
    let town_src = town_from_id(script, &mut globals, id_source);
    let town_tgt = town_from_id(script, &mut globals, id_target);
    if let Some(town_src) = town_src {
        if let Some(town_tgt) = town_tgt {
            let lift_land = LiftLandBuilding {
                player: (*script).player as u8,
                unit_id: unit_id,
                src: src.area,
                tgt: tgt.area,
                town_src,
                town_tgt,
                return_hp_percent,
                state: Default::default(),
            };
            globals.lift_lands.add(lift_land, amount);
        } else {
            debug!("No target town");
        }
    } else {
        debug!("No src town");
    }
}

pub unsafe extern fn base_layout(script: *mut bw::AiScript) {
    // base_layout(unit, modifier, src_area, amount, town_id)
    let mut read = ScriptData::new(script);
    let unit = UnitId(read.read_u16());
    let layout_modifier = read.read_u8();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let amount = read.read_u8();
    let town_id = read.read_u8();
    let priority = read.read_u8();
    if read.is_invalid() {
        return;
    }
    add_layout(
        script,
        unit,
        layout_modifier,
        src,
        amount,
        town_id,
        priority,
    );
}
pub unsafe extern fn base_layout_old(script: *mut bw::AiScript) {
    // base_layout(unit, modifier, src_area, amount, town_id)
    let mut read = ScriptData::new(script);
    let unit = UnitId(read.read_u16());
    let layout_modifier = read.read_u8();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let amount = read.read_u8();
    let town_id = read.read_u8();
    if read.is_invalid() {
        return;
    }
    add_layout(script, unit, layout_modifier, src, amount, town_id, 50);
}

pub unsafe extern fn guard_command(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let unit = read.read_u16();
    let target = read.read_position();
    let quantity = read.read_u8();
    let death_limit = read.read_u8();
    let priority = read.read_u8();
    if feature_disabled("guard") || read.is_invalid() {
        return;
    }
    let mut globals = Globals::get("ais guard");
    let unit = globals.unit_replace.replace_check(UnitId(unit)); // replace_requests
    for _n in 0..quantity {
        let guards = samase::guard_ais().add((*script).player as usize);
        let old_first_active = (*guards).first;
        let new_ai = (*(*guards).full_array).first_free;
        if new_ai.is_null() {
            // Guard AI limit
            break;
        }
        (*new_ai) = bw::GuardAi {
            next: (*new_ai).next,
            prev: (*new_ai).prev,
            ai_type: 1,
            times_died: 0,
            parent: null_mut(),
            unit_id: unit.0,
            home: target.center,
            other_home: target.center,
            previous_update: 0,
        };
        let new_first_free = (*new_ai).next;
        (*(*guards).full_array).first_free = new_first_free;
        if !new_first_free.is_null() {
            (*new_first_free).prev = null_mut();
        }
        (*new_ai).next = old_first_active;
        if !old_first_active.is_null() {
            (*old_first_active).prev = new_ai;
        }
        (*guards).first = new_ai;
        globals.guards.add(
            (*(*guards).full_array).ais.as_mut_ptr(),
            new_ai,
            death_limit,
            priority,
        );
    }
}

pub unsafe extern fn create_script(script: *mut bw::AiScript) {
    // create_script(pos, player, area, town, resarea)
    let mut read = ScriptData::new(script);
    let pos = read.read_jump_pos();
    let player = match read.read_u8() {
        255 => (*script).player as u8,
        x => x,
    };
    let mut area = read.read_position();
    let radius = read.read_u16();
    area.extend_area(radius as i16);
    if area.center.x == -2 && area.center.y == -2 {
        area = Position::from_rect32(&(*script).area)
    }
    let town = read.read_u8();
    let resarea = match read.read_u8() {
        255 => (((*script).flags >> 3) & 0xff) as u8,
        x => x,
    };
    if feature_disabled("create_script") || read.is_invalid() {
        return;
    }
    let town = match town {
        0 => null_mut(),
        255 => (*script).town,
        _ => {
            bw_print!("Invalid town in create_script");
            return;
        }
    };

    let mut globals = Globals::get("ais create_script");
    let flags = ((*script).flags & 1) | ((resarea as u32) << 3);
    let first_ai_script = bw::first_ai_script();
    let script = globals.ai_scripts.alloc(Script {
        bw: bw::AiScript {
            next: first_ai_script,
            prev: null_mut(),
            pos,
            wait: 0,
            player: player as u32,
            area: bw::Rect32 {
                left: i32::from(area.area.left),
                top: i32::from(area.area.top),
                right: i32::from(area.area.right),
                bottom: i32::from(area.area.bottom),
            },
            center: bw::Point32 {
                x: i32::from(area.center.x),
                y: i32::from(area.center.y),
            },
            town,
            flags,
        },
        delete_mark: false,
        call_stack: Vec::new(),
        debug_name: String::new(),
    });
    (*first_ai_script).prev = &mut (*script).bw;
    bw::set_first_ai_script(&mut (*script).bw);
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn take_scripts_none() {
        unsafe {
            let mut scripts = BlockAllocSet::new();
            let (new_first, first_free) =
                take_bw_allocated_scripts(&mut scripts, null_mut(), null_mut());
            assert!(new_first.is_null());
            assert!(first_free.is_null());
            assert_eq!(scripts.len(), 0);
            clear_deleted_scripts(&mut scripts, null_mut());
            assert_eq!(scripts.len(), 0);
        }
    }

    fn dummy_script(num: u32) -> bw::AiScript {
        let mut script: bw::AiScript = unsafe { mem::zeroed() };
        script.pos = num;
        script
    }

    unsafe fn dummy_list(amt: usize, base: u32) -> Vec<bw::AiScript> {
        let mut external = Vec::new();
        for i in 0..amt {
            external.push(dummy_script(base + i as u32));
        }
        let mut prev = null_mut();
        for i in 0..amt {
            external[i].prev = prev;
            if i != 0 {
                external[i - 1].next = &mut external[i];
            }
            prev = &mut external[i];
        }
        validate_links(&mut external[0], amt);
        external
    }

    unsafe fn validate_links(start: *mut bw::AiScript, expected_len: usize) {
        let mut len = 0;
        let mut script = start;
        assert!((*script).prev.is_null());
        while !script.is_null() {
            if !(*script).prev.is_null() {
                assert_eq!((*(*script).prev).next, script, "Script {} prevnext", len);
            }
            if !(*script).next.is_null() {
                assert_eq!((*(*script).next).prev, script, "Script {} nextprev", len);
            }
            script = (*script).next;
            len += 1;
        }
        assert_eq!(len, expected_len);
    }

    #[test]
    fn take_scripts() {
        unsafe {
            let mut scripts = BlockAllocSet::new();
            let mut external = dummy_list(10, 0);

            let mut lone_free = dummy_script(123);
            let (new_first, first_free) =
                take_bw_allocated_scripts(&mut scripts, &mut external[0], &mut lone_free);
            assert!(!new_first.is_null());
            assert!(external
                .iter_mut()
                .any(|x| (&mut *x) as *mut _ == first_free));
            assert_eq!(scripts.len(), 10);
            validate_links(new_first, 10);
            validate_links(first_free, 11);

            let mut external = dummy_list(20, 100);
            external[19].next = new_first;
            (*new_first).prev = &mut external[19];
            let (new_first, first_free) =
                take_bw_allocated_scripts(&mut scripts, &mut external[0], first_free);
            assert!(!new_first.is_null());
            assert!(external
                .iter_mut()
                .any(|x| (&mut *x) as *mut _ == first_free));
            assert_eq!(scripts.len(), 30);
            validate_links(new_first, 30);
            validate_links(first_free, 31);

            let mut script = new_first;
            let mut i = 0;
            while !script.is_null() {
                if i < 20 {
                    assert_eq!((*script).pos, 100 + i);
                } else {
                    assert_eq!((*script).pos, i - 20);
                }
                script = (*script).next;
                i += 1;
            }
        }
    }

    #[test]
    fn delete_scripts() {
        unsafe {
            let mut scripts = BlockAllocSet::new();
            let mut external = dummy_list(10, 0);

            let (new_first, first_free) =
                take_bw_allocated_scripts(&mut scripts, &mut external[0], null_mut());
            assert_eq!(scripts.len(), 10);
            validate_links(new_first, 10);
            validate_links(first_free, 10);

            let (new_first, first_free) =
                take_bw_allocated_scripts(&mut scripts, new_first, first_free);
            assert_eq!(scripts.len(), 10);
            validate_links(new_first, 10);
            validate_links(first_free, 10);

            // Remove the first script
            let second = (*new_first).next;
            (*new_first).next = first_free;
            (*first_free).prev = new_first;
            let first_free = new_first;
            let new_first = second;
            (*new_first).prev = null_mut();
            validate_links(new_first, 9);
            validate_links(first_free, 11);
            clear_deleted_scripts(&mut scripts, new_first);
            assert_eq!(scripts.len(), 9);

            // Remove the second and fourth script
            let second = (*new_first).next;
            let fourth = (*(*second).next).next;
            (*(*second).next).prev = (*second).prev;
            (*(*second).prev).next = (*second).next;
            (*(*fourth).next).prev = (*fourth).prev;
            (*(*fourth).prev).next = (*fourth).next;
            // Too lazy to add them to free list D:
            validate_links(new_first, 7);
            clear_deleted_scripts(&mut scripts, new_first);
            assert_eq!(scripts.len(), 7);

            // Remove the 4th, 6th and 7th scripts
            let second = (*new_first).next;
            let fourth = (*(*second).next).next;
            let fifth = (*fourth).next;
            (*(*fourth).next).prev = (*fourth).prev;
            (*(*fourth).prev).next = (*fourth).next;
            (*fifth).next = null_mut();
            validate_links(new_first, 4);
            clear_deleted_scripts(&mut scripts, new_first);
            assert_eq!(scripts.len(), 4);
        }
    }

    #[test]
    fn town_buildreq_removal() {
        // Always removes unit id 8
        unsafe fn check(mut town: bw::AiTown, remove_count: u8, remaining: &[bw::TownReq]) {
            let mut units = UnitMatch::single(UnitId(8));
            println!("Remove count is: {:#?}", remove_count);

            remove_build_from_town(Town(&mut town), &mut units, remove_count);
            for i in 0..remaining.len() {
                println!(
                    "Flags and count is: {:#?}",
                    town.town_units[i].flags_and_count
                );
                assert_ne!(town.town_units[i].flags_and_count >> 3, 0);
            }
            assert_eq!(town.town_units[remaining.len()].flags_and_count >> 3, 0);
            for rem in remaining {
                let ok = (0..remaining.len()).any(|i| town.town_units[i] == *rem);
                if !ok {
                    panic!(
                        "(line {}) Couldn't find {:#x?} after removal, remaining were {:#x?}",
                        line!(),
                        rem,
                        &town.town_units[..remaining.len()],
                    );
                }
            }
        }
        unsafe {
            let req = |amt: u8, id: u16, flag| bw::TownReq {
                flags_and_count: if flag { 0x1 } else { 0x0 } | (amt << 3),
                priority: 50,
                id,
            };
            println!("Unsafe test");
            let mut town: bw::AiTown = mem::zeroed();
            town.town_units[0] = req(5, 8, false);
            town.town_units[1] = req(8, 8, false);
            // Removes one, reduces one
            let remaining = vec![req(2, 8, false)];
            check(town, 6, &remaining);

            let mut town: bw::AiTown = mem::zeroed();
            town.town_units[0] = req(5, 8, false);
            town.town_units[1] = req(9, 8, false);
            town.town_units[2] = req(10, 7, true);
            town.town_units[3] = req(5, 9, false);
            town.town_units[4] = req(8, 8, true);
            town.town_units[5] = req(5, 1, false);
            // Removes all of the unit 8
            let remaining = vec![town.town_units[2], town.town_units[3], town.town_units[5]];
            check(town, 11, &remaining);

            // Check that two of the unit 8 reqs stay
            let mut town: bw::AiTown = mem::zeroed();
            town.town_units[0] = req(5, 8, false);
            town.town_units[1] = req(9, 8, false);
            town.town_units[2] = req(5, 8, false);
            town.town_units[3] = req(10, 7, true);
            town.town_units[4] = req(5, 9, false);
            town.town_units[5] = req(8, 8, true);
            town.town_units[6] = req(1, 8, true);
            town.town_units[7] = req(5, 1, false);
            town.town_units[8] = req(1, 8, true);
            let remaining = vec![
                town.town_units[3],
                town.town_units[4],
                town.town_units[7],
                req(3, 8, false),
                req(2, 8, true),
            ];
            check(town, 6, &remaining);
        }
    }

    #[test]
    fn script_data_reading() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x123).unwrap();
        buf.write_u32::<LE>(43242).unwrap();
        buf.write_u16::<LE>(12345).unwrap();
        for &c in b"test test \0".iter() {
            buf.push(c);
        }
        buf.write_u16::<LE>(941).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
                orig_pos: 4,
                length: buf.len() as u32,
                invalid: false,
                mutate: true,
            };
            assert_eq!(read.read_u32(), 43242);
            assert_eq!(read.read_u16(), 12345);
            assert_eq!(read.read_string(), b"test test ");
            assert_eq!(read.read_u16(), 941);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }

    #[test]
    fn script_data_unit_match() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x123).unwrap();
        buf.write_u16::<LE>(0x33).unwrap();
        buf.write_u16::<LE>(0xff04).unwrap();
        buf.write_u16::<LE>(0x123).unwrap();
        buf.write_u16::<LE>(0x110).unwrap();
        buf.write_u16::<LE>(0x30).unwrap();
        buf.write_u16::<LE>(0x70).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
                orig_pos: 4,
                length: buf.len() as u32,
                invalid: false,
                mutate: true,
            };
            assert_eq!(read.read_unit_match().units, vec![UnitId(0x33)]);
            let eq = vec![UnitId(0x123), UnitId(0x110), UnitId(0x30), UnitId(0x70)];
            assert_eq!(read.read_unit_match().units, eq);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }

    #[test]
    fn script_data_long_jumps() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x123456).unwrap();
        buf.write_u32::<LE>(0x0000_0005).unwrap();
        buf.write_u32::<LE>(0x0000_0009).unwrap();
        buf.write_u32::<LE>(0x3311_3322).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
                orig_pos: 4,
                length: buf.len() as u32,
                invalid: false,
                mutate: true,
            };
            assert_eq!(read.read_jump_pos(), 0x0000_0005);
            assert_eq!(read.read_jump_pos(), 0x0000_0009);
            assert_eq!(read.read_jump_pos(), 0x8000_0000);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }

    #[test]
    fn script_data_short_jumps() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x1234).unwrap();
        buf.write_u32::<LE>(0x0000_0005).unwrap();
        buf.write_u32::<LE>(0x0000_0009).unwrap();
        buf.write_u32::<LE>(0x3311_3322).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
                orig_pos: 4,
                length: buf.len() as u32,
                invalid: false,
                mutate: true,
            };
            assert_eq!(read.read_jump_pos(), 0x0005);
            assert_eq!(read.read_jump_pos(), 0x0000);
            assert_eq!(read.read_jump_pos(), 0x0009);
            assert_eq!(read.read_jump_pos(), 0x0000);
            assert_eq!(read.read_jump_pos(), 0x8000_0000);
            let _ = read.read_jump_pos(); // Currently only returns error once, idk if makes sense
            assert_eq!(script.pos, buf.len() as u32);
        }
    }
}

pub unsafe extern fn panic_opcode(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let offset = read.read_u16();
    if read.is_invalid() {
        return;
    }
    if (*script).flags & 0x1 != 0 {
        let script = Script::ptr_from_bw(script);
        bw_print!(
            "Script {}: Cannot use panic in script placed to bwscript.bin",
            (*script).debug_string(),
        );
        return;
    }
    let player = (*script).player as u8;
    let ai = ai::PlayerAi::get(player);
    (*ai.0).panic_script_pos = offset;
}
