use std::fmt;
use std::mem;
use std::ptr::null_mut;
use std::slice;

use byteorder::{WriteBytesExt, LE};
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use bw_dat::{self, TechId, UnitId, UpgradeId};

use ai;
use block_alloc::BlockAllocSet;
use bw;
use datreq::{DatReq, ReadDatReqs};
use game::Game;
use globals::{self, BaseLayout, BunkerCondition, BunkerState, Globals, RevealState, RevealType};
use list::ListIter;
use order::{self, OrderId};
use rng::Rng;
use samase;
use swap_retain::SwapRetain;
use unit::{self, Unit};

pub fn init_save_mapping() {}

pub fn clear_save_mapping() {}

pub fn init_load_mapping() {}

pub fn clear_load_mapping() {}

pub unsafe extern fn attack_to(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let grouping = read.read_position();
    let target = read.read_position();
    let grouping_region = match bw::get_region(grouping.center) {
        Some(s) => s,
        None => {
            bw::print_text(format!(
                "Aiscript attackto (player {}): invalid grouping coordinates {}",
                (*script).player,
                grouping,
            ));
            return;
        }
    };
    let target_region = match bw::get_region(target.center) {
        Some(s) => s,
        None => {
            bw::print_text(format!(
                "Aiscript attackto (player {}): invalid target coordinates {}",
                (*script).player,
                target,
            ));
            return;
        }
    };
    let ai_data = bw::player_ai((*script).player);
    (*ai_data).last_attack_second = bw::elapsed_seconds();
    (*ai_data).attack_grouping_region = grouping_region + 1;
    let region = ai_region((*script).player, grouping_region);
    bw::change_ai_region_state(region, 8);
    (*region).target_region_id = target_region; // Yes, 0-based
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct AttackTimeoutState {
    value: Option<u32>,
    original_start_second: Option<(u32, u32)>,
}

impl AttackTimeoutState {
    pub fn new() -> AttackTimeoutState {
        AttackTimeoutState {
            value: None,
            original_start_second: None,
        }
    }
}

pub unsafe extern fn attack_timeout(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let timeout = read.read_u32();
    Globals::get().attack_timeouts[(*script).player as usize].value = Some(timeout);
}

pub unsafe fn attack_timeouts_frame_hook(globals: &mut Globals, game: Game) {
    let seconds = (*game.0).elapsed_seconds;
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

pub unsafe fn attack_timeouts_frame_hook_after(globals: &mut Globals) {
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
    if flags & 0xffe0 != 0 {
        bw::print_text(format!("Aiscript issue_order: Unknown flags 0x{:x}", flags));
        return;
    }
    let mut count = 0;
    let units = unit::find_units(&src.area, |u| {
        if count == limit {
            return false;
        }
        let ok = u.player() as u32 == (*script).player && unit_id.matches(u);
        if ok {
            count += 1;
        }
        ok
    });
    let game = Game::get();
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
        let mut count = 0;
        Some(unit::find_units(&target.area, |u| {
            if flags & 0x8 != 0 && count != 0 {
                return false;
            }
            let ok = acceptable_players[u.player() as usize] && target_misc.matches(u);
            if ok {
                count += 1;
            }
            ok
        }))
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
                let target = &targets[target_pos];
                target_pos += 1;
                bw::issue_order(unit.0, order, target.position(), target.0, unit::id::NONE);
            } else {
                bw::issue_order(unit.0, order, target.center, null_mut(), unit::id::NONE);
            }
        }
        match order {
            order::id::PLACE_ADDON | order::id::BUILD_ADDON => {
                let unit_id = target_misc.get_one();
                (&mut (*unit.0).unit_specific[4..])
                    .write_u16::<LE>(unit_id.0)
                    .unwrap();
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
                (*unit.0).build_queue[(*unit.0).current_build_slot as usize] = unit_id.0;
            }
            _ => (),
        }
    }
}

pub unsafe extern fn if_attacking(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let dest = read.read_u16();
    let ai = bw::player_ai((*script).player);
    if (*ai).attack_grouping_region != 0 {
        (*script).pos = dest as u32;
    }
}

pub unsafe extern fn unstart_campaign(script: *mut bw::AiScript) {
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

pub fn update_towns(globals: &mut Globals) {
    let old = mem::replace(&mut globals.towns, towns());
    for old in old {
        if !globals.towns.iter().any(|&x| x == old) {
            globals.max_workers.swap_retain(|x| x.town != old);
            globals.town_ids.swap_retain(|x| x.town != old);
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Town(*mut bw::AiTown);

impl Town {
    fn from_ptr(ptr: *mut bw::AiTown) -> Option<Town> {
        if ptr == null_mut() {
            None
        } else {
            Some(Town(ptr))
        }
    }
}

unsafe impl Send for Town {}

impl Serialize for Town {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;

        let array = bw::town_array_start();
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
        let array = bw::town_array_start();
        if array.is_null() {
            Err(S::Error::custom("Saving is not supported"))
        } else {
            unsafe { Ok(Town(bw::town_array_start().offset(id as isize))) }
        }
    }
}

pub unsafe extern fn set_town_id(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let id = read.read_u8();
    if id == 255 {
        bw::print_text(format!("Unsupported id {} in set_id", id));
        return;
    }
    let town = match Town::from_ptr((*script).town) {
        Some(s) => s,
        None => {
            bw::print_text(format!("Used `set_id {}` without town", id));
            return;
        }
    };
    let mut globals = Globals::get();
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
    let globals = Globals::get();
    let town = match id {
        255 => Town::from_ptr((*script).town),
        _ => globals
            .town_ids
            .iter()
            .find(|x| id == x.id && u32::from((*x.town.0).player) == (*script).player)
            .map(|x| x.town),
    };
    if let Some(town) = town {
        remove_build_from_town(town, &mut unit_id, amount);
    }
}

unsafe fn remove_build_from_town(town: Town, unit_id: &mut UnitMatch, amount: u8) {
    let town = town.0;
    let builds = (*town)
        .town_units
        .iter()
        .cloned()
        .take_while(|x| x.flags_and_count != 0)
        .collect::<Vec<_>>();

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
    let town = match Town::from_ptr((*script).town) {
        Some(s) => s,
        None => {
            bw::print_text(format!("Used `max_workers {}` without town", count));
            return;
        }
    };
    let mut globals = Globals::get();
    globals.max_workers.swap_retain(|x| x.town != town);
    if count != 255 {
        globals.max_workers.push(MaxWorkers {
            town,
            count,
        });
    }
}

pub extern fn max_workers_for(globals: &mut Globals, town: *mut bw::AiTown) -> Option<u8> {
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
    let player = (*script).player as usize;
    let mut globals = Globals::get();
    globals.under_attack_mode[player] = match mode {
        0 => Some(false),
        1 => None,
        2 => Some(true),
        _ => {
            bw::print_text(format!("Invalid `under_attack` mode: {}", mode));
            return;
        }
    };
}

pub unsafe fn bunker_fill_hook(bunker_states: &mut BunkerCondition, picked_unit: Unit) {
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
    for state in &mut bunker_states.bunker_states {
        if state.unit_id.matches(&picked_unit) {
            let units = unit::find_units(&state.pos, |u| {
                u.player() == state.player &&
                    state.bunker_id.matches(u) &&
                    u.player() == (*picked_unit.0).player
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
                    if used_bunkers >= state.bunker_quantity {
                        return;
                    }
                }
                if free_slots > 0 && state.quantity > 0 {
                    bw::issue_order(
                        picked_unit.0,
                        ENTER_TRANSPORT,
                        picked_unit.position(),
                        bunker.0,
                        unit::id::NONE,
                    );
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

pub unsafe fn under_attack_frame_hook(globals: &mut Globals) {
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
}

impl Default for AiMode {
    fn default() -> AiMode {
        AiMode {
            wait_for_resources: true,
            build_gas: true,
        }
    }
}

pub unsafe extern fn aicontrol(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mode = read.read_u8();
    let player = (*script).player as usize;
    let mut globals = Globals::get();
    let out = &mut globals.ai_mode[player];
    match mode {
        0 => out.wait_for_resources = true,
        1 => out.wait_for_resources = false,
        2 => out.build_gas = true,
        3 => out.build_gas = false,
        _ => panic!("Invalid aicontrol {:x}", mode),
    };
}

pub unsafe extern fn call(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let dest = read.read_u16() as u32;
    let ret = (*script).pos;
    (*script).pos = dest;
    (*Script::ptr_from_bw(script)).call_stack.push(ret);
}

pub unsafe extern fn ret(script: *mut bw::AiScript) {
    let script = Script::ptr_from_bw(script);
    match (*script).call_stack.pop() {
        Some(s) => {
            (*script).bw.pos = s;
        }
        None => {
            bw::print_text(format!(
                "Script {} used return without call",
                (*script).debug_string()
            ));
            (*script).bw.wait = !1;
            (*script).bw.pos -= 1;
        }
    }
}

pub unsafe extern fn do_morph(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let unit_id = UnitId(read.read_u16());
    let player = (*script).player as u8;
    if ai::count_units(player, unit_id, Game::get()) < u32::from(amount) {
        let ai = ai::PlayerAi::get(player);
        (*ai.0).train_unit_id = unit_id.0 + 1;
    }
}

pub unsafe extern fn train(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let amount = read.read_u8();
    let unit_id = UnitId(read.read_u16());
    let player = (*script).player as u8;
    if ai::count_units(player, unit_id, Game::get()) < u32::from(amount) {
        let ai = ai::PlayerAi::get(player);
        (*ai.0).train_unit_id = unit_id.0 + 1;
        (*script).pos -= 4;
        (*script).wait = 30;
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct UnitMatch {
    units: Vec<UnitId>,
}

impl UnitMatch {
    pub fn iter_flatten_groups<'a>(&'a mut self) -> impl Iterator<Item = UnitId> + 'a {
        // Ineffective but w/e, simpler than ignoring duplicates
        if self.units.iter().any(|&x| x == unit::id::ANY_UNIT) {
            self.units = (0..unit::id::NONE.0).map(UnitId).collect();
        } else {
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
                    x if x.0 >= unit::id::NONE.0 => false,
                    x => x.group_flags() & group_flags == 0,
                });
                let new_units = (0..unit::id::NONE.0)
                    .map(UnitId)
                    .filter(|x| x.group_flags() & group_flags != 0);
                self.units.extend(new_units);
            }
        }
        self.units.iter().cloned()
    }

    pub fn matches(&self, unit: &Unit) -> bool {
        self.units.iter().any(|&x| unit.matches_id(x))
    }

    pub fn get_one(&self) -> UnitId {
        self.units
            .iter()
            .cloned()
            .filter(|x| x.0 < unit::id::NONE.0)
            .next()
            .unwrap_or(UnitId(0))
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
    let mut globals = Globals::get();
    let game = Game::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = (read.read_u16() as u32) * 2;
    let supply_type = read.read_u8();
    let units = read.read_unit_match();
    let race = read.read_u8();
    let dest = read.read_u16();
    let supply_type = match supply_type {
        0 => SupplyType::Provided,
        1 => SupplyType::Used,
        2 => SupplyType::Max,
        3 => SupplyType::InUnits,
        x => {
            bw::print_text(format!("Unsupported supply type in supply: {:x}", x));
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
            bw::print_text(format!("Unsupported race in supply: {:x}", x));
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
                                let supplies = &((*game.0).supplies[race_id]);
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
            let read_req = modifier.get_read_req();
            if read.compare(sum, amount) == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(write) => {
            let race_i = match race {
                Race::Zerg => 0,
                Race::Terran => 1,
                Race::Protoss => 2,
                Race::Any => {
                    bw::print_text("Only specific race supply can be modified.");
                    return;
                }
            };
            if supply_type == SupplyType::Max {
                for player in players.players() {
                    let supply_val = (*game.0).supplies[race_i].max.get_mut(player as usize);
                    if let Some(supply_val) = supply_val {
                        *supply_val = write.apply(*supply_val, amount, &mut globals.rng);
                    }
                }
            } else {
                bw::print_text("Only Max supply can be modified.");
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
    let game = Game::get();
    let mut globals = Globals::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let res = read.read_u8();
    let amount = read.read_u32();
    let dest = read.read_u16();
    let resources_to_check: &[_] = match res {
        // Matching trigger conditions
        0 => &[Resource::Ore],
        1 => &[Resource::Gas],
        2 => &[Resource::Ore, Resource::Gas],
        x => {
            bw::print_text(format!("Unsupported resource type in resources: {:x}", x));
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

            let read_req = modifier.get_read_req();
            if jump == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(write) => {
            for player in players.players() {
                for &res in resources_to_check {
                    let resources = match res {
                        Resource::Ore => (*game.0).minerals.get_mut(player as usize),
                        Resource::Gas => (*game.0).gas.get_mut(player as usize),
                    };
                    if let Some(resources) = resources {
                        *resources = write.apply(*resources, amount, &mut globals.rng);
                    }
                }
            }
        }
    }
}

pub unsafe fn reveal_vision_hook(globals: &mut Globals, game: Game) {
    for rev in &mut globals.reveal_states {
        if rev.time != 0 {
            rev.time -= 1;
            if rev.time == 0 {
                reveal(game, rev.pos, rev.players, false);
            }
        }
    }
    globals.reveal_states.swap_retain(|x| x.time > 0);
}

unsafe extern fn reveal(game: Game, area: bw::Rect, players: PlayerMatch, reveal: bool) {
    let tile_x = area.left / 32;
    let tile_y = area.top / 32;
    let limit_x = area.right / 32;
    let limit_y = area.bottom / 32;
    let map_width = (*game.0).map_width_tiles;
    for player in players.players().filter(|&x| x < 8) {
        for i in tile_x..=limit_x {
            for j in tile_y..=limit_y {
                let tile_flag =
                    (*bw::tile_flags).offset(i as isize + (map_width * j as u16) as isize);
                if reveal {
                    *tile_flag &= 0x1 << player;
                } else {
                    *tile_flag |= 0x100 << player;
                    *tile_flag |= 0x1 << player;
                }
            }
        }
    }
}

pub unsafe extern fn reveal_area(script: *mut bw::AiScript) {
    if bw::is_scr() {
        bw::print_text("reveal_area is not supported in SCR");
        return;
    }
    let mut read = ScriptData::new(script);
    let game = Game::get();
    let mut globals = Globals::get();
    let players = read.read_player_match(game);
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let time = read.read_u16();
    let flag = read.read_u8();
    let reveal_type = match flag {
        0 => RevealType::RevealFog,
        x => {
            bw::print_text(format!("Unsupported flag modifier: {:x}", x));
            return;
        }
    };
    if time != 0 {
        let reveal_state = RevealState {
            pos: src.area,
            time,
            reveal_type,
            players,
        };
        globals.reveal_states.push(reveal_state);
    }
    reveal(game, src.area, players, true);
}

pub unsafe extern fn remove_creep(script: *mut bw::AiScript) {
    if bw::is_scr() {
        bw::print_text("remove_creep is not supported in SCR");
        return;
    }
    let mut read = ScriptData::new(script);
    let mut src = read.read_position();
    let radius = read.read_u16();
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

pub unsafe extern fn time_command(script: *mut bw::AiScript) {
    enum TimeType {
        Frames,
        Minutes,
    }
    let old_pos = (*script).pos - 1;
    let game = Game::get();
    let mut read = ScriptData::new(script);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let time_mod = read.read_u8();
    let dest = read.read_u16();
    let time_mod = match time_mod {
        // Matching trigger conditions
        0 => TimeType::Frames,
        1 => TimeType::Minutes,
        x => {
            bw::print_text(format!("Unsupported time modifier in time: {:x}", x));
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
            bw::print_text(format!("Used writing modifier {:?} in time", w));
            return;
        }
    };
    let read_req = modifier.get_read_req();
    if read.compare(time, amount) == read_req {
        match modifier.action {
            ModifierAction::Jump => {
                (*script).pos = dest as u32;
            }
            ModifierAction::Call => {
                let ret = (*script).pos;
                (*script).pos = dest as u32;
                (*Script::ptr_from_bw(script)).call_stack.push(ret);
            }
            ModifierAction::Wait => {
                (*script).pos = old_pos;
                (*script).wait = 30;
            }
        }
    }
}

pub unsafe extern fn attacking(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let mut read = ScriptData::new(script);
    let modifier = read.read_bool_modifier();
    let dest = read.read_u16();
    let ai = bw::player_ai((*script).player);
    let r_compare = ((*ai).attack_grouping_region != 0) == modifier.value;

    match modifier.action {
        ModifierAction::Jump => {
            if r_compare {
                (*script).pos = dest as u32;
            }
        }
        ModifierAction::Call => {
            if r_compare {
                let ret = (*script).pos;
                (*script).pos = dest as u32;
                (*Script::ptr_from_bw(script)).call_stack.push(ret);
            }
        }
        ModifierAction::Wait => {
            if !r_compare {
                (*script).pos = old_pos;
                (*script).wait = 30;
            }
        }
    }
}

pub unsafe extern fn deaths(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = Game::get();
    // deaths(player, modifier, amount, unit, dest)
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let mut units = read.read_unit_match();
    let dest = read.read_u16();

    let mut globals = Globals::get();
    match modifier.ty {
        ModifierType::Read(read) => {
            let sum = units
                .iter_flatten_groups()
                .map(|unit_id| {
                    players
                        .players()
                        .map(|player| {
                            (*game.0)
                                .deaths
                                .get_mut(unit_id.0 as usize)
                                .and_then(|x| x.get_mut(player as usize))
                                .cloned()
                                .unwrap_or(0)
                        })
                        .sum::<u32>()
                })
                .sum::<u32>();
            let read_req = modifier.get_read_req();
            if read.compare(sum, amount) == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(write) => {
            for unit_id in units.iter_flatten_groups() {
                for player in players.players() {
                    let deaths = (*game.0)
                        .deaths
                        .get_mut(unit_id.0 as usize)
                        .and_then(|x| x.get_mut(player as usize));
                    if let Some(deaths) = deaths {
                        *deaths = write.apply(*deaths, amount, &mut globals.rng);
                    }
                }
            }
        }
    }
}

pub unsafe extern fn wait_rand(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mut r1 = read.read_u32();
    let mut r2 = read.read_u32();
    let mut globals = Globals::get();
    if r1 > r2 {
        mem::swap(&mut r1, &mut r2);
    }
    (*script).wait = globals.rng.synced_rand(r1..r2 + 1);
}

pub unsafe extern fn kills_command(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = Game::get();
    // kills(player1, player2, modifier, amount, unit, dest)
    let mut read = ScriptData::new(script);
    let player1 = read.read_player_match(game);
    let player2 = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let mut units = read.read_unit_match();
    let dest = read.read_u16();
    let mut globals = Globals::get();

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
            let read_req = modifier.get_read_req();
            if read.compare(sum, amount) == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(write) => {
            for unit_id in units.iter_flatten_groups() {
                for p1 in player1.players() {
                    for p2 in player2.players() {
                        let mut kpos = globals::KCPos::new(p1, p2, unit_id.0);
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

pub unsafe fn increment_deaths(
    target: *mut bw::Unit,
    attacker_p_id: u8,
    orig: &Fn(*mut bw::Unit, u8),
) {
    let unit_id = (*target).unit_id;
    let amount = 1;
    let player = (*target).player;
    {
        let mut globals = Globals::get();
        let kpos = globals::KCPos::new(attacker_p_id, player, unit_id);
        globals.kills_table.try_add(kpos, amount);
    }
    orig(target, attacker_p_id);
}

pub unsafe extern fn print_command(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let msg = read.read_string();
    let s = String::from_utf8_lossy(&msg);
    bw::print_text(s);
}

pub unsafe extern fn ping(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let x = read.read_u16();
    let y = read.read_u16();
    let color = read.read_u8();
    if bw::is_scr() {
        bw::print_text("ping is not supported in SCR");
        return;
    }
    bw::ping_minimap(x as u32, y as u32, color);
}

pub unsafe extern fn player_jump(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let player = read.read_string();
    let dest = read.read_u16();
    if bw::is_scr() {
        bw::print_text("player_jump is not supported in SCR");
        return;
    }
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
        (*script).pos = dest as u32;
    }
}

pub unsafe extern fn upgrade_jump(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = Game::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let upgrade = UpgradeId(read.read_u16());
    let level = read.read_u8();
    let dest = read.read_u16();
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = modifier.get_read_req();
            let jump = players.players().any(|player| {
                let up_lev = game.upgrade_level(player, upgrade);
                r.compare(u32::from(up_lev), u32::from(level))
            });
            if jump == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(w) => {
            let mut globals = Globals::get();
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
    let mut globals = Globals::get();
    let mut read = ScriptData::new(script);
    let player = (*script).player;
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let unit_id = read.read_unit_match();
    let quantity = read.read_u8();
    let bunker_id = read.read_unit_match();
    let bunker_quantity = read.read_u8();
    let priority = read.read_u8();
    let bunker_state = BunkerState {
        pos: src.area,
        unit_id: unit_id,
        bunker_id: bunker_id,
        quantity: quantity,
        player: player as u8,
        priority: priority,
        bunker_quantity: bunker_quantity,
        single_bunker_states: Vec::new(),
    };

    globals.bunker_states.add(bunker_state);
}

pub unsafe extern fn unit_avail(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = Game::get();
    let mut globals = Globals::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let avail_modifier = read.read_u8();
    let unit = UnitId(read.read_u16());
    let dest = read.read_u16();
    if avail_modifier >= 2 {
        bw::print_text("Invalid modifier in unit_avail");
        return;
    }
    match modifier.ty {
        ModifierType::Read(r) => match r {
            ReadModifier::AtLeast | ReadModifier::AtMost => {
                bw::print_text("AtLeast/AtMost modifier is not supported in unit_avail");
                return;
            }
            ReadModifier::Exactly => {
                let read_req = modifier.get_read_req();
                let jump = players.players().any(|player| {
                    let avail = game.unit_available(player, unit);
                    r.compare(avail as u32, u32::from(avail_modifier))
                });
                if jump == read_req {
                    match modifier.action {
                        ModifierAction::Jump => {
                            (*script).pos = dest as u32;
                        }
                        ModifierAction::Call => {
                            let ret = (*script).pos;
                            (*script).pos = dest as u32;
                            (*Script::ptr_from_bw(script)).call_stack.push(ret);
                        }
                        ModifierAction::Wait => {
                            (*script).pos = old_pos;
                            (*script).wait = 30;
                        }
                    }
                }
            }
        },
        ModifierType::Write(w) => {
            for player in players.players() {
                let new_value = match w {
                    WriteModifier::Add | WriteModifier::Subtract => {
                        bw::print_text("Add/subtract modifier is not supported in unit_avail");
                        return;
                    }
                    WriteModifier::Set => avail_modifier,
                    WriteModifier::Randomize => globals.rng.synced_rand(0..2) as u8,
                };
                game.set_unit_availability(player, unit, new_value);
            }
        }
    };
}

pub unsafe extern fn tech_jump(script: *mut bw::AiScript) {
    let old_pos = (*script).pos - 1;
    let game = Game::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let tech = TechId(read.read_u16());
    let level = read.read_u8();
    let dest = read.read_u16();
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = modifier.get_read_req();
            let jump = players.players().any(|player| {
                let up_lev = game.tech_researched(player, tech);
                r.compare(u32::from(up_lev), u32::from(level))
            });
            if jump == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(w) => {
            let mut globals = Globals::get();
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
    let game = Game::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let tech = TechId(read.read_u16());
    let level = read.read_u8();
    let dest = read.read_u16();
    match modifier.ty {
        ModifierType::Read(r) => {
            let read_req = modifier.get_read_req();
            let jump = players.players().any(|player| {
                let up_lev = game.tech_available(player, tech);
                r.compare(u32::from(up_lev), u32::from(level))
            });
            if jump == read_req {
                match modifier.action {
                    ModifierAction::Jump => {
                        (*script).pos = dest as u32;
                    }
                    ModifierAction::Call => {
                        let ret = (*script).pos;
                        (*script).pos = dest as u32;
                        (*Script::ptr_from_bw(script)).call_stack.push(ret);
                    }
                    ModifierAction::Wait => {
                        (*script).pos = old_pos;
                        (*script).wait = 30;
                    }
                }
            }
        }
        ModifierType::Write(w) => {
            let mut globals = Globals::get();
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
    let dest = read.read_u16() as u32;

    let mut globals = Globals::get();
    let random = globals.rng.synced_rand(0..256);
    if u32::from(chance) > random {
        let ret = (*script).pos;
        (*script).pos = dest as u32;
        (*Script::ptr_from_bw(script)).call_stack.push(ret);
    }
}

pub unsafe extern fn attack_rand(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let mut r1 = read.read_u8() as u32;
    let mut r2 = read.read_u8() as u32;
    let unit = read.read_u16();
    if r1 > r2 {
        mem::swap(&mut r1, &mut r2);
    }
    let mut globals = Globals::get();
    let random = globals.rng.synced_rand(r1..r2 + 1);
    add_to_attack_force((*script).player as u8, UnitId(unit), random);
}

unsafe fn add_to_attack_force(player: u8, unit: UnitId, amount: u32) {
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
    let game = Game::get();
    let mut read = ScriptData::new(script);
    let players = read.read_player_match(game);
    let modifier = read.read_modifier();
    let amount = read.read_u32();
    let unit_id = read.read_unit_match();
    let mut src = read.read_position();
    let radius = read.read_u16();
    src.extend_area(radius as i16);
    let dest = read.read_u16();

    let read = match modifier.ty {
        ModifierType::Read(r) => r,
        ModifierType::Write(w) => {
            bw::print_text(format!("Used writing modifier {:?} in bring_jump", w));
            return;
        }
    };
    let units = unit::find_units(&src.area, |u| {
        players.matches(u.player()) && unit_id.matches(u)
    });
    let count = units.len() as u32;
    let read_req = modifier.get_read_req();
    if read.compare(count, amount) == read_req {
        match modifier.action {
            ModifierAction::Jump => {
                (*script).pos = dest as u32;
            }
            ModifierAction::Call => {
                let ret = (*script).pos;
                (*script).pos = dest as u32;
                (*Script::ptr_from_bw(script)).call_stack.push(ret);
            }
            ModifierAction::Wait => {
                (*script).pos = old_pos;
                (*script).wait = 30;
            }
        }
    }
}

unsafe fn ai_region(player: u32, region: u16) -> *mut bw::AiRegion {
    bw::ai_regions(player).offset(region as isize)
}

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
pub enum ReadModifier {
    AtLeast,
    AtMost,
    Exactly,
}

impl ReadModifier {
    pub fn compare(self, value: u32, constant: u32) -> bool {
        match self {
            ReadModifier::AtLeast => value >= constant,
            ReadModifier::AtMost => value <= constant,
            ReadModifier::Exactly => value == constant,
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
                    bw::print_text("Cannot randomize with 0 cases");
                    !0
                }
            }
        }
    }
}

enum ModifierType {
    Read(ReadModifier),
    Write(WriteModifier),
}

struct TriggerModifier {
    ty: ModifierType,
    action: ModifierAction,
}

impl TriggerModifier {
    pub fn get_read_req(&self) -> bool {
        match self.action {
            ModifierAction::Jump | ModifierAction::Call => true,
            ModifierAction::Wait => false,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ModifierAction {
    Jump,
    Call,
    Wait,
}

struct BoolModifier {
    value: bool,
    action: ModifierAction,
}

pub unsafe fn clean_unsatisfiable_requests(ai_mode: &[AiMode; 8]) {
    let game = Game::get();
    for player in 0..8 {
        let ai_mode = &ai_mode[player as usize];
        let ai_data = bw::player_ai(player as u32);
        let requests = &mut ((*ai_data).requests)[..(*ai_data).request_count as usize];
        let mut in_pos = 0;
        let mut out_pos = 0;
        // I actually think this is some sort of heap layout instead of a straight array,
        // so this breaks the priority ordering if things are dropped :/
        while in_pos < requests.len() {
            let can = can_satisfy_request(game, player, &requests[in_pos], ai_mode);
            if can {
                requests[out_pos] = requests[in_pos];
                out_pos += 1;
            } else {
                let req = requests[in_pos];
                debug!(
                    "Player {} can't satisfy request {:x}/{:x}",
                    player, req.ty, req.id
                );
            }
            in_pos += 1;
        }
        (*ai_data).request_count = out_pos as u8;
    }
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

fn has_resources(game: Game, player: u8, cost: &ai::Cost) -> bool {
    // TODO supply
    game.minerals(player) >= cost.minerals && game.gas(player) >= cost.gas
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

// For reading aiscript.bin or bwscript.bin (or some other?) bytes
pub struct ScriptData(*const u8, *mut bw::AiScript);

impl ScriptData {
    pub unsafe fn new(script: *mut bw::AiScript) -> ScriptData {
        let script_bytes = match (*script).flags & 0x1 != 0 {
            false => bw::aiscript_bin(),
            true => bw::bwscript_bin(),
        };
        ScriptData(
            script_bytes.offset((*script).pos as isize) as *const u8,
            script,
        )
    }

    pub unsafe fn read_player_match(&mut self, game: Game) -> PlayerMatch {
        let mut cont = true;
        let mut result = PlayerMatch {
            players: [false; 12],
        };
        let current_player = (*self.1).player as u8;
        while cont {
            let byte = self.read_u8();
            cont = byte & 0x80 != 0;
            let player = byte & 0x7f;
            match player {
                x @ 0...11 => result.players[x as usize] = true,
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
                    for player in (0..8).filter(|&x| (*game.0).player_forces[x as usize] == force) {
                        result.players[player as usize] = true;
                    }
                }
                x => {
                    bw::print_text(format!("Unsupported player: {:x}", x));
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
                bw::print_text(format!("Unsupported modifier: {:x}", val));
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
                bw::print_text(format!("Unsupported modifier: {:x}", val));
                ModifierAction::Jump
            }
        };
        TriggerModifier {
            action,
            ty: match val & 0x1f {
                // Matching triggers in chk
                0 => ModifierType::Read(ReadModifier::AtLeast),
                1 => ModifierType::Read(ReadModifier::AtMost),
                7 => ModifierType::Write(WriteModifier::Set),
                8 => ModifierType::Write(WriteModifier::Add),
                9 => ModifierType::Write(WriteModifier::Subtract),
                10 => ModifierType::Read(ReadModifier::Exactly),
                11 => ModifierType::Write(WriteModifier::Randomize),
                x => {
                    bw::print_text(format!("Unsupported modifier: {:x}", x));
                    ModifierType::Read(ReadModifier::AtLeast)
                }
            },
        }
    }

    pub fn read_unit_match(&mut self) -> UnitMatch {
        let val = self.read_u16();
        if val > 0xff00 {
            let repeat = val & 0xff;
            let units = (0..repeat).map(|_| UnitId(self.read_u16())).collect();
            UnitMatch {
                units,
            }
        } else if val < 0x1000 {
            UnitMatch {
                units: vec![UnitId(val)],
            }
        } else {
            bw::print_text(format!("Invalid script encoding: unit match {:x}", val));
            UnitMatch {
                units: vec![],
            }
        }
    }

    fn read_position(&mut self) -> Position {
        let x = self.read_u16();
        let y = self.read_u16();
        if x == !0 {
            assert!(y < 255);
            let location = if y >= 255 {
                bw::print_text(format!("Invalid location id 0x{:x} used", y));
                bw::location(63)
            } else {
                bw::location(y as u8)
            };
            Position::from_rect32(&location.area)
        } else {
            // !1, !1 is used for inherit position in create_script
            Position::from_point(x as i16, y as i16)
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
            let val = (self.0 as *const T).read_unaligned();
            self.0 = self.0.add(size);
            (*self.1).pos = (*self.1).pos.wrapping_add(size as u32);
            val
        }
    }

    pub unsafe fn read_string(&mut self) -> &'static [u8] {
        let length = (0usize..).position(|x| *self.0.add(x) == 0).unwrap_or(0);
        let val = slice::from_raw_parts(self.0, length);
        self.0 = self.0.add(length + 1);
        (*self.1).pos += length as u32 + 1;
        val
    }
}

#[derive(Serialize, Deserialize)]
pub struct Script {
    #[serde(serialize_with = "serialize_bw_script")]
    #[serde(deserialize_with = "deserialize_bw_script")]
    bw: bw::AiScript,
    delete_mark: bool,
    call_stack: Vec<u32>,
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

    let mut state = globals::save_state();
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
        ((bw as usize) - offset_of!(Script, bw)) as *mut Script
    }

    fn debug_string(&self) -> String {
        unsafe {
            format!(
                "Player {}, pos {}, {}",
                self.bw.player, self.bw.center.x, self.bw.center.y,
            )
        }
    }
}

const AISCRIPT_LIMIT: usize = 8192;

pub fn claim_bw_allocated_scripts(globals: &mut Globals) {
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
            bw::print_text(
                "Warning: ran out of AI scripts for the frame, some of the scripts
                may not have started.",
            );
        }
        let (new_first, mut new_first_free) =
            take_bw_allocated_scripts(&mut globals.ai_scripts, first, first_free);
        if new_first != first {
            bw::set_first_ai_script(new_first);
        }
        let delete_count = clear_deleted_scripts(&mut globals.ai_scripts, new_first);
        if delete_count != 0 {
            let (deleted, even_newer_first_free) =
                clean_free_scripts(&globals.ai_scripts, first_free, delete_count);
            if let Some(x) = even_newer_first_free {
                new_first_free = x;
            }
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
    // Break out once we run into a script which is already owned by us,
    // bw inserts new scripts at start of the list.
    while !script.is_null() {
        if scripts.len() == AISCRIPT_LIMIT {
            bw::print_text("AI script limit reached.");
            break;
        }
        if !scripts.contains(Script::ptr_from_bw(script)) {
            let taken = scripts.alloc(Script {
                bw: *script,
                delete_mark: false,
                call_stack: Vec::new(),
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
    orig: &Fn(*mut bw::Unit, u8, u32, u32, u16, u8, u8, u8, u8) -> u32,
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

pub unsafe fn choose_building_placement(
    unit_id: u32,
    position_xy: u32,
    out_pos: *mut bw::Point,
    area_tiles: u32,
    builder: *mut bw::Unit,
    orig: &Fn(u32, u32, *mut bw::Point, u32, *mut bw::Unit) -> u32,
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

    let globals = Globals::get();
    if let Some(town_id) = globals.town_ids.iter().find(|x| x.town.0 == town) {
        let game = Game::get();
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
                let units = unit::find_units(&layout.pos, |u| {
                    u.player() == layout.player && u.id() == layout.unit_id
                });
                units.len() < usize::from(layout.amount)
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
                    let mut ok = check_placement(game, builder, i, j, unit_id);
                    if ok {
                        let mut workers = (*town).workers;
                        while workers != null_mut() {
                            let current = (*workers).parent;
                            if current != builder.0 {
                                if (*current).order_target_pos.x == i * 32 &&
                                    (*current).order_target_pos.y == j * 32
                                {
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

unsafe fn check_placement(
    game: Game,
    builder: Unit,
    x_tile: i16,
    y_tile: i16,
    unit_id: UnitId,
) -> bool {
    let map_width = (*game.0).map_width_tiles;
    let map_height = (*game.0).map_height_tiles;
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
    let units = unit::find_units(&area, |&u| u != builder);
    if !units.is_empty() {
        return false;
    }

    let width_tiles = placement.width / 32;
    let height_tiles = placement.height / 32;
    if x_tile as u16 + width_tiles > map_width || y_tile as u16 + height_tiles > map_height {
        return false;
    }

    if unit_id.is_town_hall() {
        let area = bw::Rect {
            left: (x_tile * 32).saturating_sub(3 * 32),
            top: (y_tile * 32).saturating_sub(3 * 32),
            right: (x_tile * 32) + placement.width as i16 + 3 * 32,
            bottom: (y_tile * 32) + placement.height as i16 + 3 * 32,
        };
        let res_units = unit::find_units(&area, |u| u.id().is_resource_container());
        if !res_units.is_empty() {
            return false;
        }
    }

    for px in 0..width_tiles {
        for py in 0..height_tiles {
            let tile = *(*bw::tile_flags).offset(
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

unsafe extern fn add_layout(
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
    let layout_modifier = match layout_modifier {
        0 => LayoutModifier::Set,
        1 => LayoutModifier::Remove,
        x => {
            bw::print_text(format!(
                "Unsupported layout modifier in base_layout: {:x}",
                x
            ));
            return;
        }
    };

    let mut globals = Globals::get();
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
    add_layout(script, unit, layout_modifier, src, amount, town_id, 50);
}

pub unsafe extern fn guard_command(script: *mut bw::AiScript) {
    let mut read = ScriptData::new(script);
    let unit = read.read_u16();
    let target = read.read_position();
    let quantity = read.read_u8();
    let death_limit = read.read_u8();
    let priority = read.read_u8();
    let mut globals = Globals::get();
    for _n in 0..quantity {
        let guards = samase::guard_ais().add((*script).player as usize);
        let old_first_active = (*guards).first;
        let new_ai = (*(*guards).array).first_free;
        (*new_ai) = bw::GuardAi {
            next: (*new_ai).next,
            prev: (*new_ai).prev,
            ai_type: 1,
            times_died: 0,
            dca: [0, 0],
            parent: null_mut(),
            unit_id: unit,
            home: target.center,
            other_home: target.center,
            padding1a: [0, 0],
            previous_update: 0,
        };
        let new_first_free = (*new_ai).next;
        (*(*guards).array).first_free = new_first_free;
        if !new_first_free.is_null() {
            (*new_first_free).prev = null_mut();
        }
        (*new_ai).next = old_first_active;
        if !old_first_active.is_null() {
            (*old_first_active).prev = new_ai;
        }
        (*guards).first = new_ai;
        globals.guards.add(
            (*(*guards).array).ais.as_mut_ptr(),
            new_ai,
            death_limit,
            priority,
        );
    }
}

pub unsafe extern fn create_script(script: *mut bw::AiScript) {
    // create_script(pos, player, area, town, resarea)
    let mut read = ScriptData::new(script);
    let pos = read.read_u16();
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
    let town = match town {
        0 => null_mut(),
        255 => (*script).town,
        _ => {
            bw::print_text("Invalid town in create_script");
            return;
        }
    };

    let mut globals = Globals::get();
    let flags = ((*script).flags & 1) | ((resarea as u32) << 3);
    let first_ai_script = bw::first_ai_script();
    let script = globals.ai_scripts.alloc(Script {
        bw: bw::AiScript {
            next: first_ai_script,
            prev: null_mut(),
            pos: pos as u32,
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
            assert!(
                external
                    .iter_mut()
                    .any(|x| (&mut *x) as *mut _ == first_free)
            );
            assert_eq!(scripts.len(), 10);
            validate_links(new_first, 10);
            validate_links(first_free, 11);

            let mut external = dummy_list(20, 100);
            external[19].next = new_first;
            (*new_first).prev = &mut external[19];
            let (new_first, first_free) =
                take_bw_allocated_scripts(&mut scripts, &mut external[0], first_free);
            assert!(!new_first.is_null());
            assert!(
                external
                    .iter_mut()
                    .any(|x| (&mut *x) as *mut _ == first_free)
            );
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
            let mut units = UnitMatch {
                units: vec![UnitId(8)],
            };
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
        buf.write_u32::<LE>(43242).unwrap();
        buf.write_u16::<LE>(12345).unwrap();
        for &c in b"test test \0".iter() {
            buf.push(c);
        }
        buf.write_u16::<LE>(941).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            let mut read = ScriptData(buf.as_ptr(), &mut script);
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
        buf.write_u16::<LE>(0x33).unwrap();
        buf.write_u16::<LE>(0xff04).unwrap();
        buf.write_u16::<LE>(0x123).unwrap();
        buf.write_u16::<LE>(0x110).unwrap();
        buf.write_u16::<LE>(0x30).unwrap();
        buf.write_u16::<LE>(0x70).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            let mut read = ScriptData(buf.as_ptr(), &mut script);
            assert_eq!(read.read_unit_match().units, vec![UnitId(0x33)]);
            let eq = vec![UnitId(0x123), UnitId(0x110), UnitId(0x30), UnitId(0x70)];
            assert_eq!(read.read_unit_match().units, eq);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }
}
