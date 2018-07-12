use std::cell::RefCell;
use std::fmt;
use std::mem;
use std::ptr::null_mut;
use std::slice;
use std::sync::Mutex;

use bincode;
use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use serde::{Serializer, Serialize, Deserializer, Deserialize};
use smallvec::SmallVec;

use bw_dat::{self, UnitId, UpgradeId, TechId};

use ai;
use bw;
use game::Game;
use idle_orders::IdleOrders;
use order::{self, OrderId};
use rng;
use unit::{self, Unit};
use swap_retain::SwapRetain;

lazy_static! {
    static ref ATTACK_TIMEOUTS: Mutex<[AttackTimeoutState; 8]> =
        Mutex::new([AttackTimeoutState::new(); 8]);
    pub static ref IDLE_ORDERS: Mutex<IdleOrders> = Mutex::new(Default::default());
    static ref MAX_WORKERS: Mutex<Vec<MaxWorkers>> = Mutex::new(Default::default());
    static ref UNDER_ATTACK_MODE: Mutex<[Option<bool>; 8]> = Mutex::new(Default::default());
    static ref WAIT_FOR_RESOURCES: Mutex<[bool; 8]> = Mutex::new([true; 8]);
    // For tracking deleted towns.
    // If the tracking is updated after step_objects, it shouldn't be possible for a town
    // to be deleted and recreated in the same frame. (As recreation happens in scripts,
    // and deletion happens on last unit dying) Better solutions won't obviously hurt though.
    static ref TOWNS: Mutex<Vec<Town>> = Mutex::new(Vec::new());
    static ref CALL_STACKS: Mutex<CallStacks> = Mutex::new(CallStacks {
        stacks: Vec::new(),
    });
}

fn wait_for_resources(player: u8) -> bool {
    WAIT_FOR_RESOURCES.lock().unwrap()[player as usize]
}

ome2_thread_local! {
    SAVE_TOWNS: RefCell<Vec<Town>> = town_id_mapping(RefCell::new(Vec::new()));
    SAVE_SCRIPTS: RefCell<Vec<Script>> = script_id_mapping(RefCell::new(Vec::new()));
}

pub fn init_save_mapping() {
    *town_id_mapping().borrow_mut() = towns();
    let scripts = scripts();
    CALL_STACKS.lock().unwrap().retain_only(&scripts);
    *script_id_mapping().borrow_mut() = scripts;
}

pub fn clear_save_mapping() {
    town_id_mapping().borrow_mut().clear();
    script_id_mapping().borrow_mut().clear();
}

pub fn init_load_mapping() {
    *town_id_mapping().borrow_mut() = towns();
    *script_id_mapping().borrow_mut() = scripts();
}

pub fn clear_load_mapping() {
    town_id_mapping().borrow_mut().clear();
    script_id_mapping().borrow_mut().clear();
}

#[derive(Serialize, Deserialize, Debug)]
struct SaveData {
    attack_timeouts: [AttackTimeoutState; 8],
    idle_orders: IdleOrders,
    max_workers: Vec<MaxWorkers>,
    under_attack_mode: [Option<bool>; 8],
    wait_for_resources: [bool; 8],
    towns: Vec<Town>,
    call_stacks: CallStacks,
    rng: Option<rng::Rng>,
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    unit::init_save_mapping();
    defer!(unit::clear_save_mapping());
    init_save_mapping();
    defer!(clear_save_mapping());
    let save = SaveData {
        attack_timeouts: ATTACK_TIMEOUTS.lock().unwrap().clone(),
        idle_orders: IDLE_ORDERS.lock().unwrap().clone(),
        max_workers: MAX_WORKERS.lock().unwrap().clone(),
        under_attack_mode: UNDER_ATTACK_MODE.lock().unwrap().clone(),
        wait_for_resources: WAIT_FOR_RESOURCES.lock().unwrap().clone(),
        towns: TOWNS.lock().unwrap().clone(),
        call_stacks: CALL_STACKS.lock().unwrap().clone(),
        rng: rng::save_rng(),
    };
    match bincode::serialize(&save) {
        Ok(o) => {
            set_data(o.as_ptr(), o.len());
        }
        Err(e) => {
            error!("Couldn't save game: {}", e);
            bw::print_text(format!("(Aise) Couldn't save game: {}", e));
        }
    }
}

pub unsafe extern fn load(ptr: *const u8, len: usize) -> u32 {
    unit::init_load_mapping();
    defer!(unit::clear_load_mapping());
    init_load_mapping();
    defer!(clear_load_mapping());

    let slice = slice::from_raw_parts(ptr, len);
    let data: SaveData = match bincode::deserialize(slice) {
        Ok(o) => o,
        Err(e) => {
            error!("Couldn't load game: {}", e);
            return 0
        }
    };
    let SaveData {
        attack_timeouts,
        idle_orders,
        max_workers,
        under_attack_mode,
        wait_for_resources,
        towns,
        call_stacks,
        rng,
    } = data;
    *ATTACK_TIMEOUTS.lock().unwrap() = attack_timeouts;
    *IDLE_ORDERS.lock().unwrap() = idle_orders;
    *MAX_WORKERS.lock().unwrap() = max_workers;
    *UNDER_ATTACK_MODE.lock().unwrap() = under_attack_mode;
    *WAIT_FOR_RESOURCES.lock().unwrap() = wait_for_resources;
    *TOWNS.lock().unwrap() = towns;
    *CALL_STACKS.lock().unwrap() = call_stacks;
    rng::load_rng(rng);
    1
}

pub unsafe extern fn init_game() {
    *ATTACK_TIMEOUTS.lock().unwrap() = [AttackTimeoutState::new(); 8];
    *IDLE_ORDERS.lock().unwrap() = Default::default();
    *MAX_WORKERS.lock().unwrap() = Default::default();
    *UNDER_ATTACK_MODE.lock().unwrap() = Default::default();
    *WAIT_FOR_RESOURCES.lock().unwrap() = [true; 8];
    *TOWNS.lock().unwrap() = Default::default();
    *CALL_STACKS.lock().unwrap() = Default::default();
    rng::init_rng();
}

pub unsafe extern fn attack_to(script: *mut bw::AiScript) {
    let grouping = read_position(script);
    let target = read_position(script);
    let grouping_region = match bw::get_region(grouping.center) {
        Some(s) => s,
        None => {
            bw::print_text(&format!(
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
            bw::print_text(&format!(
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
struct AttackTimeoutState {
    value: Option<u32>,
    original_start_second: Option<(u32, u32)>,
}

impl AttackTimeoutState {
    fn new() -> AttackTimeoutState {
        AttackTimeoutState {
            value: None,
            original_start_second: None,
        }
    }
}

pub unsafe extern fn attack_timeout(script: *mut bw::AiScript) {
    let timeout = read_u32(script);
    ATTACK_TIMEOUTS.lock().unwrap()[(*script).player as usize].value = Some(timeout);
}

pub unsafe fn attack_timeouts_frame_hook(game: Game) {
    let mut timeouts = ATTACK_TIMEOUTS.lock().unwrap();
    let seconds = (*game.0).elapsed_seconds;
    for i in 0..8 {
        let player_ai = bw::player_ai(i as u32);
        let last_attack_second = (*player_ai).last_attack_second;
        if last_attack_second == 0 {
            timeouts[i].value = None;
        }
        let new = if last_attack_second == 0 {
            Some(!400)
        } else {
            if let Some(timeout) = timeouts[i].value {
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
            timeouts[i].original_start_second = Some((last_attack_second, new));
        }
    }
}

pub unsafe fn attack_timeouts_frame_hook_after() {
    // Revert old value for teippi debug, only if it wasn't changed during frame step
    let mut timeouts = ATTACK_TIMEOUTS.lock().unwrap();
    for i in 0..8 {
        if let Some((previous, new)) = timeouts[i].original_start_second.take() {
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
        bw::print_text(&format!("Aiscript issue_order: Unknown flags 0x{:x}", flags));
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
                    .write_u16::<LE>(unit_id.0).unwrap();
            }
            order::id::DRONE_BUILD | order::id::SCV_BUILD | order::id::PROBE_BUILD |
                order::id::UNIT_MORPH | order::id::BUILDING_MORPH | order::id::TRAIN |
                order::id::TRAIN_FIGHTER | order::id::BUILD_NYDUS_EXIT =>
            {
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
struct MaxWorkers {
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

fn scripts() -> Vec<Script> {
    let mut result = Vec::with_capacity(32);
    let mut script = bw::first_ai_script();
    while script != null_mut() {
        result.push(Script(script));
        unsafe {
            script = (*script).next;
        }
    }
    result
}

pub fn update_towns() {
    let mut towns_global = TOWNS.lock().unwrap();
    let mut max_workers = MAX_WORKERS.lock().unwrap();
    let old = mem::replace(&mut *towns_global, towns());
    for old in old {
        if !towns_global.iter().any(|&x| x == old) {
            max_workers.swap_retain(|x| x.town != old);
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Town(*mut bw::AiTown);

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
        match town_id_mapping().borrow().iter().enumerate().find(|&(_, x)| x == self) {
            Some((id, _)) => (id as u32).serialize(serializer),
            None => Err(S::Error::custom(format!("Couldn't get id for town {:?}", self))),
        }
    }
}

impl<'de> Deserialize<'de> for Town {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match town_id_mapping().borrow().get(id as usize) {
            Some(&town) => Ok(town),
            None => Err(S::Error::custom(format!("Couldn't get town for id {:?}", id))),
        }
    }
}

pub unsafe extern fn max_workers(script: *mut bw::AiScript) {
    let count = read_u8(script);
    let town = match Town::from_ptr((*script).town) {
        Some(s) => s,
        None => {
            bw::print_text(&format!("Used `max_workers {}` without town", count));
            return;
        }
    };
    let mut workers = MAX_WORKERS.lock().unwrap();
    workers.swap_retain(|x| x.town != town);
    if count != 255 {
        workers.push(MaxWorkers {
            town,
            count,
        });
    }
}

pub extern fn max_workers_for(town: *mut bw::AiTown) -> Option<u8> {
    let workers = MAX_WORKERS.lock().unwrap();
    workers.iter().find(|x| x.town.0 == town).map(|x| x.count)
}

pub unsafe extern fn under_attack(script: *mut bw::AiScript) {
    // 0 = Never, 1 = Default, 2 = Always
    let mode = read_u8(script);
    let player = (*script).player as usize;
    let mut under_attack = UNDER_ATTACK_MODE.lock().unwrap();
    match mode {
        0 => under_attack[player] = Some(false),
        1 => under_attack[player] = None,
        2 => under_attack[player] = Some(true),
        _ => {
            bw::print_text(&format!("Invalid `under_attack` mode: {}", mode));
            return;
        }
    }
}

pub unsafe fn under_attack_frame_hook() {
    let under_attack = UNDER_ATTACK_MODE.lock().unwrap();
    for (player, mode) in under_attack.iter().cloned().enumerate() {
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
    let mut wait = WAIT_FOR_RESOURCES.lock().unwrap();
    match mode {
        0 => wait[player] = true,
        1 => wait[player] = false,
        _ => panic!("Invalid aicontrol {:x}", mode),
    }
}

pub unsafe extern fn call(script: *mut bw::AiScript) {
    let dest = read_u16(script) as u32;
    let ret = (*script).pos;
    (*script).pos = dest;
    let mut stacks = CALL_STACKS.lock().unwrap();
    stacks.push(Script(script), ret);
}

pub unsafe extern fn ret(script: *mut bw::AiScript) {
    let mut stacks = CALL_STACKS.lock().unwrap();
    let script = Script(script);
    match stacks.pop(script) {
        Some(s) => {
            (*script.0).pos = s;
        }
        None => {
            bw::print_text(format!("Script {} used return without call", script.debug_string()));
            (*script.0).wait = !1;
            (*script.0).pos -= 1;
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Script(*mut bw::AiScript);

unsafe impl Send for Script {}

impl Script {
    fn debug_string(&self) -> String {
        unsafe {
            format!(
                "Player {}, pos {}, {}",
                (*self.0).player, (*self.0).center[0], (*self.0).center[1],
            )
        }
    }
}

impl Serialize for Script {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::Error;
        match script_id_mapping().borrow().iter().enumerate().find(|&(_, x)| x == self) {
            Some((id, _)) => (id as u32).serialize(serializer),
            None => Err(S::Error::custom(format!("Couldn't get id for script {:?}", self))),
        }
    }
}

impl<'de> Deserialize<'de> for Script {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let id = u32::deserialize(deserializer)?;
        match script_id_mapping().borrow().get(id as usize) {
            Some(&script) => Ok(script),
            None => Err(S::Error::custom(format!("Couldn't get script for id {:?}", id))),
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct CallStacks {
    stacks: Vec<(Script, Vec<u32>)>,
}

impl CallStacks {
    fn push(&mut self, script: Script, ret: u32) {
        let index = match self.stacks.iter().position(|x| x.0 == script) {
            Some(s) => s,
            None => {
                self.stacks.push((script, Vec::new()));
                self.stacks.len() - 1
            }
        };
        self.stacks[index].1.push(ret);
    }

    fn pop(&mut self, script: Script) -> Option<u32> {
        match self.stacks.iter().position(|x| x.0 == script) {
            Some(s) => {
                let result = self.stacks[s].1.pop();
                if self.stacks[s].1.is_empty() {
                    self.stacks.swap_remove(s);
                }
                result
            }
            None => None,
        }
    }

    fn retain_only(&mut self, scripts: &[Script]) {
        self.stacks.retain(|x| scripts.iter().any(|&y| y == x.0))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct UnitMatch {
    units: Vec<UnitId>,
}

impl UnitMatch {
    pub fn matches(&self, unit: &Unit) -> bool {
        self.units.iter().any(|&x| unit.matches_id(x))
    }

    pub fn get_one(&self) -> UnitId {
        self.units.iter().cloned().filter(|x| x.0 < unit::id::NONE.0).next().unwrap_or(UnitId(0))
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
        self.players.iter().enumerate().filter(|x| *x.1 == true).map(|x| x.0 as u8)
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
            x @ 0 ... 11 => result.players[x as usize] = true,
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
    let unit_id = read_u16(script);
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
    match modifier {
        Modifier::AtLeast | Modifier::AtMost | Modifier::Exactly => {
            let sum: u32 = players.players().map(|player| {
                (*game.0).deaths
                    .get_mut(unit_id as usize)
                    .and_then(|x| x.get_mut(player as usize))
                    .cloned()
                    .unwrap_or(0)
            }).sum();
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
            for player in players.players() {
                let deaths = (*game.0).deaths
                    .get_mut(unit_id as usize)
                    .and_then(|x| x.get_mut(player as usize));
                if let Some(deaths) = deaths {
                    match modifier {
                        Modifier::Set => *deaths = amount,
                        Modifier::Add => *deaths = deaths.saturating_add(amount),
                        Modifier::Subtract => *deaths = deaths.saturating_sub(amount),
                        Modifier::Randomize => {
                            if amount != 0 {
                                *deaths = rng::synced_rand(0..amount);
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
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
            center: bw::Point { x, y },
            area: bw::Rect {
                left: x,
                right: x.saturating_add(1),
                top: y,
                bottom: y.saturating_add(1),
            }
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
            }
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
        let bw::Rect { left, right, top, bottom } = self.area;

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
        let units = (0..repeat).map(|_| {
            UnitId(read_u16(script))
        }).collect();
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

pub unsafe fn clean_unsatisfiable_requests() {
    let game = Game::get();
    for player in 0..8 {
        let wait = wait_for_resources(player);
        let ai_data = bw::player_ai(player as u32);
        let requests = &mut ((*ai_data).requests)[..(*ai_data).request_count as usize];
        let remove_count = requests.iter()
            .take_while(|x| {
                let can = can_satisfy_request(game, player, x, wait);
                if !can {
                    debug!("Player {} can't satisfy request {:x}/{:x}", player, x.ty, x.id);
                }
                !can
            }).count();
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
    game.minerals(player) >= cost.minerals &&
        game.gas(player) >= cost.gas
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
                UnitReq::BwOnly => true, // w/e
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
        .next().is_some()
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
        .next().is_some()
}
