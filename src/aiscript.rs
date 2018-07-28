use std::cell::RefCell;
use std::fmt;
use std::mem;
use std::ptr::null_mut;

use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};
use smallvec::SmallVec;

use bw_dat::{self, TechId, UnitId, UpgradeId};

use ai;
use block_alloc::BlockAllocSet;
use bw;
use game::Game;
use globals::{self, Globals};
use order::{self, OrderId};
use swap_retain::SwapRetain;
use unit::{self, Unit};

fn wait_for_resources(globals: &mut Globals, player: u8) -> bool {
    globals.wait_for_resources[player as usize]
}

ome2_thread_local! {
    SAVE_TOWNS: RefCell<Vec<Town>> = town_id_mapping(RefCell::new(Vec::new()));
}

pub fn init_save_mapping() {
    *town_id_mapping().borrow_mut() = towns();
}

pub fn clear_save_mapping() {
    town_id_mapping().borrow_mut().clear();
}

pub fn init_load_mapping() {
    *town_id_mapping().borrow_mut() = towns();
}

pub fn clear_load_mapping() {
    town_id_mapping().borrow_mut().clear();
}

pub unsafe extern fn attack_to(script: *mut bw::AiScript) {
    let grouping = read_position(script);
    let target = read_position(script);
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
    let timeout = read_u32(script);
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
    let order = OrderId(read_u8(script));
    let limit = read_u16(script);
    let unit_id = read_unit_match(script);
    let mut src = read_position(script);
    let radius = read_u16(script);
    src.extend_area(radius as i16);
    let mut target = read_position(script);
    let tgt_radius = read_u16(script);
    target.extend_area(tgt_radius as i16);
    let target_misc = read_unit_match(script);
    let flags = read_u16(script);
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
    let dest = read_u16(script);
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
    result
}

pub fn update_towns(globals: &mut Globals) {
    let old = mem::replace(&mut globals.towns, towns());
    for old in old {
        if !globals.towns.iter().any(|&x| x == old) {
            globals.max_workers.swap_retain(|x| x.town != old);
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
        match town_id_mapping()
            .borrow()
            .iter()
            .enumerate()
            .find(|&(_, x)| x == self)
        {
            Some((id, _)) => (id as u32).serialize(serializer),
            None => Err(S::Error::custom(format!(
                "Couldn't get id for town {:?}",
                self
            ))),
        }
    }
}

impl<'de> Deserialize<'de> for Town {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match town_id_mapping().borrow().get(id as usize) {
            Some(&town) => Ok(town),
            None => Err(S::Error::custom(format!(
                "Couldn't get town for id {:?}",
                id
            ))),
        }
    }
}

pub unsafe extern fn max_workers(script: *mut bw::AiScript) {
    let count = read_u8(script);
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
    let mode = read_u8(script);
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

pub unsafe extern fn aicontrol(script: *mut bw::AiScript) {
    let mode = read_u8(script);
    let player = (*script).player as usize;
    let mut globals = Globals::get();
    globals.wait_for_resources[player] = match mode {
        0 => true,
        1 => false,
        _ => panic!("Invalid aicontrol {:x}", mode),
    };
}

pub unsafe extern fn call(script: *mut bw::AiScript) {
    let dest = read_u16(script) as u32;
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
    let amount = read_u8(script);
    let unit_id = UnitId(read_u16(script));
    let player = (*script).player as u8;
    if ai::count_units(player, unit_id, Game::get()) < u32::from(amount) {
        let ai = ai::PlayerAi::get(player);
        (*ai.0).train_unit_id = unit_id.0 + 1;
    }
}

pub unsafe extern fn train(script: *mut bw::AiScript) {
    let amount = read_u8(script);
    let unit_id = UnitId(read_u16(script));
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

struct PlayerMatch {
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

unsafe fn read_player_match(script: *mut bw::AiScript, game: Game) -> PlayerMatch {
    let mut cont = true;
    let mut result = PlayerMatch {
        players: [false; 12],
    };
    let current_player = (*script).player as u8;
    while cont {
        let byte = read_u8(script);
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

pub unsafe extern fn deaths(script: *mut bw::AiScript) {
    enum Modifier {
        AtLeast,
        AtMost,
        Set,
        Add,
        Subtract,
        Exactly,
        Randomize,
    }
    let game = Game::get();
    // deaths(player, modifier, amount, unit, dest)
    let players = read_player_match(script, game);
    let modifier = read_u8(script);
    let amount = read_u32(script);
    let mut units = read_unit_match(script);
    let dest = read_u16(script);
    let modifier = match modifier {
        // Matching trigger conditions
        0 => Modifier::AtLeast,
        1 => Modifier::AtMost,
        7 => Modifier::Set,
        8 => Modifier::Add,
        9 => Modifier::Subtract,
        10 => Modifier::Exactly,
        11 => Modifier::Randomize,
        x => {
            bw::print_text(format!("Unsupported modifier in deaths: {:x}", x));
            return;
        }
    };
    let mut globals = Globals::get();
    match modifier {
        Modifier::AtLeast | Modifier::AtMost | Modifier::Exactly => {
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

            let jump = match modifier {
                Modifier::AtLeast => sum >= amount,
                Modifier::AtMost => sum <= amount,
                Modifier::Exactly => sum == amount,
                _ => false,
            };
            if jump {
                (*script).pos = dest as u32;
            }
        }
        Modifier::Set | Modifier::Add | Modifier::Subtract | Modifier::Randomize => {
            for unit_id in units.iter_flatten_groups() {
                for player in players.players() {
                    let deaths = (*game.0)
                        .deaths
                        .get_mut(unit_id.0 as usize)
                        .and_then(|x| x.get_mut(player as usize));
                    if let Some(deaths) = deaths {
                        match modifier {
                            Modifier::Set => *deaths = amount,
                            Modifier::Add => *deaths = deaths.saturating_add(amount),
                            Modifier::Subtract => *deaths = deaths.saturating_sub(amount),
                            Modifier::Randomize => {
                                if amount != 0 {
                                    *deaths = globals.rng.synced_rand(0..amount);
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
    }
}

pub unsafe extern fn wait_rand(script: *mut bw::AiScript) {
    let mut r1 = read_u32(script);
    let mut r2 = read_u32(script);
    let mut globals = Globals::get();
    if r1 > r2 {
        mem::swap(&mut r1, &mut r2);
    }
    (*script).wait = globals.rng.synced_rand(r1..r2 + 1);
}

pub unsafe extern fn kills_command(script: *mut bw::AiScript) {
    enum Modifier {
        AtLeast,
        AtMost,
        Set,
        Add,
        Subtract,
        Exactly,
        Randomize,
    }
    let game = Game::get();
    // kills(player1, player2, modifier, amount, unit, dest)
    let player1 = read_player_match(script, game);
    let player2 = read_player_match(script, game);
    let modifier = read_u8(script);
    let amount = read_u32(script);
    let mut units = read_unit_match(script);
    let dest = read_u16(script);
    let mut globals = Globals::get();
    let modifier = match modifier {
        // Matching trigger conditions
        0 => Modifier::AtLeast,
        1 => Modifier::AtMost,
        7 => Modifier::Set,
        8 => Modifier::Add,
        9 => Modifier::Subtract,
        10 => Modifier::Exactly,
        11 => Modifier::Randomize,
        x => {
            bw::print_text(format!("Unsupported modifier in kills: {:x}", x));
            return;
        }
    };

    match modifier {
        Modifier::AtLeast | Modifier::AtMost | Modifier::Exactly => {
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

            let jump = match modifier {
                Modifier::AtLeast => sum >= amount,
                Modifier::AtMost => sum <= amount,
                Modifier::Exactly => sum == amount,
                _ => false,
            };
            if jump {
                (*script).pos = dest as u32;
            }
        }
        Modifier::Set | Modifier::Add | Modifier::Subtract | Modifier::Randomize => {
            for unit_id in units.iter_flatten_groups() {
                for p1 in player1.players() {
                    for p2 in player2.players() {
                        let mut kpos = globals::KCPos::new(p1, p2, unit_id.0);
                        match modifier {
                            Modifier::Set => globals.kills_table.try_set(kpos, amount),
                            Modifier::Add => globals.kills_table.try_add(kpos, amount),
                            Modifier::Subtract => globals.kills_table.try_sub(kpos, amount),
                            Modifier::Randomize => {
                                if amount != 0 {
                                    let random = globals.rng.synced_rand(0..amount);
                                    globals.kills_table.try_set(kpos, random);
                                }
                            }
                            _ => (),
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

pub unsafe extern fn player_jump(script: *mut bw::AiScript) {
    let player = read_string(script);
    let dest = read_u16(script);
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

pub unsafe extern fn bring_jump(script: *mut bw::AiScript) {
    enum Modifier {
        AtLeast,
        AtMost,
        Exactly,
    }
    let game = Game::get();
    let players = read_player_match(script, game);
    let modifier = read_u8(script);
    let amount = read_u32(script);
    let unit_id = read_unit_match(script);
    let mut src = read_position(script);
    let radius = read_u16(script);
    src.extend_area(radius as i16);
    let dest = read_u16(script);

    let modifier = match modifier {
        // Matching trigger conditions
        0 => Modifier::AtLeast,
        1 => Modifier::AtMost,
        10 => Modifier::Exactly,
        x => {
            bw::print_text(format!("Unsupported modifier in bringjump: {:x}", x));
            return;
        }
    }; //
    let units = unit::find_units(&src.area, |u| {
        players.matches(u.player()) && unit_id.matches(u)
    });
    let count = units.len() as u32;
    let jump = match modifier {
        Modifier::AtLeast => count >= amount,
        Modifier::AtMost => count <= amount,
        Modifier::Exactly => count == amount,
    };
    if jump {
        (*script).pos = dest as u32;
    }
}

unsafe fn ai_region(player: u32, region: u16) -> *mut bw::AiRegion {
    bw::ai_regions(player).offset(region as isize)
}

struct Position {
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

pub unsafe fn read_unit_match(script: *mut bw::AiScript) -> UnitMatch {
    let val = read_u16(script);
    if val > 0xff00 {
        let repeat = val & 0xff;
        let units = (0..repeat).map(|_| UnitId(read_u16(script))).collect();
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

unsafe fn read_position(script: *mut bw::AiScript) -> Position {
    let x = read_u16(script);
    let y = read_u16(script);
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

pub unsafe fn read_u8(script: *mut bw::AiScript) -> u8 {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let val = *(script_bytes.offset((*script).pos as isize) as *const u8);
    (*script).pos += 1;
    val
}

pub unsafe fn read_u16(script: *mut bw::AiScript) -> u16 {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let val = *(script_bytes.offset((*script).pos as isize) as *const u16);
    (*script).pos += 2;
    val
}

pub unsafe fn read_u32(script: *mut bw::AiScript) -> u32 {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let val = *(script_bytes.offset((*script).pos as isize) as *const u32);
    (*script).pos += 4;
    val
}

unsafe fn read_string(script: *mut bw::AiScript) -> Vec<u8> {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let mut result = Vec::new();
    loop {
        let val = *(script_bytes.offset((*script).pos as isize) as *const u8);
        (*script).pos += 1;
        if val == 0 {
            break;
        } else {
            result.push(val);
        }
    }
    result
}

pub unsafe fn clean_unsatisfiable_requests(globals: &mut Globals) {
    let game = Game::get();
    for player in 0..8 {
        let wait = wait_for_resources(globals, player);
        let ai_data = bw::player_ai(player as u32);
        let requests = &mut ((*ai_data).requests)[..(*ai_data).request_count as usize];
        let remove_count = requests
            .iter()
            .take_while(|x| {
                let can = can_satisfy_request(game, player, x, wait);
                if !can {
                    debug!(
                        "Player {} can't satisfy request {:x}/{:x}",
                        player, x.ty, x.id
                    );
                }
                !can
            })
            .count();
        (*ai_data).request_count -= remove_count as u8;
        for i in 0..(*ai_data).request_count as usize {
            requests[i] = requests[i + remove_count];
        }
    }
}

unsafe fn can_satisfy_request(
    game: Game,
    player: u8,
    request: &bw::AiSpendingRequest,
    wait_resources: bool,
) -> bool {
    match request.ty {
        1 | 2 | 3 | 4 => {
            let unit_id = UnitId(request.id);
            if !wait_resources && !has_resources(game, player, &ai::unit_cost(unit_id)) {
                return false;
            }
            can_satisfy_unit_request(game, player, unit_id)
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
            can_satisfy_nonunit_request(game, player, reqs, level)
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
            can_satisfy_nonunit_request(game, player, reqs, 0)
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
    Or(Vec<MatchRequirement>),
}

impl MatchRequirement {
    fn matches(&self, unit: Unit) -> bool {
        use self::MatchRequirement::*;
        match self {
            Unit(id) => unit.matches_id(*id),
            Addon(id) => unit.addon().map(|x| x.matches_id(*id)).unwrap_or(false),
            HasHangarSpace => true, // TODO if ever cared
            HasNoNuke => {
                if unit.id() == bw_dat::unit::NUCLEAR_SILO {
                    unsafe { (&(*unit.0).unit_specific2[..]).read_u32::<LE>().unwrap() == 0 }
                } else {
                    true
                }
            }
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

unsafe fn can_satisfy_unit_request(game: Game, player: u8, unit_id: UnitId) -> bool {
    use datreq::UnitReq;

    let mut reqs = match bw::unit_dat_requirements(unit_id) {
        Some(s) => s,
        None => return true,
    };
    let mut match_requirements: SmallVec<[MatchRequirement; 8]> = SmallVec::new();
    'outer: loop {
        let mut pass = false;
        let match_req_prev_len = match_requirements.len();
        loop {
            let val = UnitReq::from_raw(*reqs);
            reqs = reqs.offset(1);
            pass |= match val {
                UnitReq::End => break 'outer,
                UnitReq::Disabled => false,
                UnitReq::Blank => false,
                UnitReq::BwOnly => true,       // w/e
                UnitReq::BurrowedOnly => true, // ???
                UnitReq::NotBurrowedOnly => true,
                // Assuming IsNotBusy/IsNotLifted/IsNotConstructingAddon/etc
                // for anything in matching for now
                UnitReq::IsNotBusy => true,
                UnitReq::IsNotLifted => true,
                UnitReq::IsNotConstructingAddon => true,
                UnitReq::IsNotUpgrading => true,
                UnitReq::IsNotTeching => true,
                UnitReq::IsNotConstructingBuilding => true,
                UnitReq::HasHangarSpace => {
                    match_requirements.push(MatchRequirement::HasHangarSpace);
                    true
                }
                UnitReq::HasNotNukeOnly => {
                    match_requirements.push(MatchRequirement::HasNoNuke);
                    true
                }
                UnitReq::HasNoAddon => {
                    match_requirements.push(MatchRequirement::HasNoNuke);
                    true
                }
                UnitReq::CurrentUnitIs => {
                    let unit = match UnitId::optional(*reqs as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown unit {:x}", *reqs);
                            return true;
                        }
                    };
                    reqs = reqs.offset(1);
                    match_requirements.push(MatchRequirement::Unit(unit));
                    true
                }
                UnitReq::HasAddonAttached => {
                    let addon = match UnitId::optional(*reqs as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown unit {:x}", *reqs);
                            return true;
                        }
                    };
                    reqs = reqs.offset(1);
                    match_requirements.push(MatchRequirement::Addon(addon));
                    true
                }
                UnitReq::TechOnly => {
                    let tech = match TechId::optional(*reqs as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown tech {:x}", *reqs);
                            return true;
                        }
                    };
                    reqs = reqs.offset(1);
                    game.tech_researched(player, tech)
                }
                UnitReq::HasUnit => {
                    let unit = match UnitId::optional(*reqs as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown unit {:x}", *reqs);
                            return true;
                        }
                    };
                    reqs = reqs.offset(1);
                    game.unit_count(player, unit) != 0
                }
                UnitReq::Unit(id) => {
                    let unit = match UnitId::optional(id as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown unit {:x}", id);
                            return true;
                        }
                    };
                    game.completed_count(player, unit) != 0
                }
                UnitReq::Unknown(id) => {
                    warn!("Unknown unit req ty {:x} for unit {:?}", id, unit_id);
                    return true;
                }
            };
            // 0xff01 is or
            if *reqs != 0xff01 {
                break;
            } else {
                reqs = reqs.offset(1);
            }
        }
        let added_match_reqs = match_requirements.len() - match_req_prev_len;
        if added_match_reqs > 2 {
            let mut or_list = Vec::new();
            for _ in 0..added_match_reqs {
                or_list.push(match_requirements.pop().unwrap());
            }
            match_requirements.push(MatchRequirement::Or(or_list));
        }
        if !pass {
            return false;
        }
    }
    unit::active_units()
        .filter(|x| x.player() == player && x.is_completed())
        .filter(|&x| !is_busy(x))
        .filter(|&x| match_requirements.iter().all(|r| r.matches(x)))
        .next()
        .is_some()
}

unsafe fn can_satisfy_nonunit_request(
    game: Game,
    player: u8,
    mut reqs: *const u16,
    upgrade_level: u8,
) -> bool {
    use datreq::NonUnitReq;

    let mut match_requirements: SmallVec<[MatchRequirement; 8]> = SmallVec::new();
    'outer: loop {
        let mut pass = false;
        let match_req_prev_len = match_requirements.len();
        loop {
            let val = NonUnitReq::from_raw(*reqs);
            reqs = reqs.offset(1);
            pass |= match val {
                NonUnitReq::End => break 'outer,
                NonUnitReq::Disabled => false,
                NonUnitReq::Blank => false,
                NonUnitReq::BwOnly => true, // w/e
                NonUnitReq::IsTransport => true,
                NonUnitReq::IsNotBusy => true,
                NonUnitReq::IsNotConstructingAddon => true,
                NonUnitReq::IsNotUpgrading => true,
                NonUnitReq::IsLifted => true,
                NonUnitReq::IsNotLifted => true,
                NonUnitReq::IsNotTeching => true,
                NonUnitReq::HasNoNydusExit => true,
                NonUnitReq::NotBurrowedOnly => true,
                NonUnitReq::BurrowedOnly => true,
                NonUnitReq::NotLandedBuildingOnly => true,
                NonUnitReq::LandedBuildingOnly => true,
                NonUnitReq::CanMoveOnly => true,
                NonUnitReq::CanAttackOnly => true,
                NonUnitReq::SubunitOnly => true,
                NonUnitReq::WorkerOnly => true,
                NonUnitReq::FlyingBuildingOnly => true,
                NonUnitReq::PowerupOnly => true,
                NonUnitReq::HasSpiderMinesOnly => true,
                NonUnitReq::CanHoldPositionOnly => true,
                NonUnitReq::AllowOnHallucinations => true,
                NonUnitReq::TechResearched => {
                    let tech = match TechId::optional(*reqs as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown tech {:x}", *reqs);
                            return true;
                        }
                    };
                    reqs = reqs.offset(1);
                    game.tech_researched(player, tech)
                }
                NonUnitReq::CurrentUnitIs => {
                    let unit = UnitId(*reqs);
                    reqs = reqs.offset(1);
                    match_requirements.push(MatchRequirement::Unit(unit));
                    true
                }
                NonUnitReq::HasUnit => {
                    let unit = match UnitId::optional(*reqs as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown unit {:x}", *reqs);
                            return true;
                        }
                    };
                    reqs = reqs.offset(1);
                    game.unit_count(player, unit) != 0
                }
                NonUnitReq::UpgradeLevelJump => {
                    if upgrade_level == 1 {
                        while *reqs != 0xff20 {
                            reqs = reqs.offset(1);
                        }
                        reqs = reqs.offset(1);
                    } else if upgrade_level > 1 {
                        while *reqs != 0xff21 {
                            reqs = reqs.offset(1);
                        }
                        reqs = reqs.offset(1);
                    }
                    true
                }
                NonUnitReq::Unit(id) => {
                    let unit = match UnitId::optional(id as u32) {
                        Some(s) => s,
                        None => {
                            warn!("Unknown unit {:x}", id);
                            return true;
                        }
                    };
                    game.completed_count(player, unit) != 0
                }
                NonUnitReq::Unknown(id) => {
                    warn!("Unknown req ty {:x}", id);
                    return true;
                }
            };
            // 0xff01 is or
            if *reqs != 0xff01 {
                break;
            } else {
                reqs = reqs.offset(1);
            }
        }
        let added_match_reqs = match_requirements.len() - match_req_prev_len;
        if added_match_reqs > 2 {
            let mut or_list = Vec::new();
            for _ in 0..added_match_reqs {
                or_list.push(match_requirements.pop().unwrap());
            }
            match_requirements.push(MatchRequirement::Or(or_list));
        }
        if !pass {
            return false;
        }
    }
    unit::active_units()
        .filter(|x| x.player() == player && x.is_completed())
        .filter(|&x| !is_busy(x))
        .filter(|&x| match_requirements.iter().all(|r| r.matches(x)))
        .next()
        .is_some()
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
    }.serialize(s)
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

pub unsafe extern fn create_script(script: *mut bw::AiScript) {
    // create_script(pos, player, area, town, resarea)
    let pos = read_u16(script);
    let player = match read_u8(script) {
        255 => (*script).player as u8,
        x => x,
    };
    let mut area = read_position(script);
    let radius = read_u16(script);
    area.extend_area(radius as i16);
    if area.center.x == -2 && area.center.y == -2 {
        area = Position::from_rect32(&(*script).area)
    }
    let town = read_u8(script);
    let resarea = match read_u8(script) {
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
}
