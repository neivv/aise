use std::cell::RefCell;
use std::io::Write;
use std::fmt;
use std::mem;
use std::ptr::null_mut;
use std::slice;
use std::sync::atomic::{Ordering, AtomicBool, ATOMIC_BOOL_INIT};
use std::sync::Mutex;

use bincode;
use byteorder::{WriteBytesExt, LittleEndian};
use serde::{Serializer, Serialize, Deserializer, Deserialize};
use winapi::um::heapapi::{HeapAlloc, HeapCreate};
use winapi::um::winnt::{HANDLE, HEAP_CREATE_ENABLE_EXECUTE};
use whack;

use bw;
use order::{self, OrderId};
use unit::{self, Unit, UnitId};

pub const IDLE_ORDERS_DISABLED: AtomicBool = ATOMIC_BOOL_INIT;

struct AiScriptOpcodes {
    opcodes: Vec<OpcodeHook>,
}

struct OpcodeHook {
    // Has to be position-independent
    asm: Vec<u8>,
    id: u32,
}

lazy_static! {
    static ref EXEC_HEAP: usize = unsafe {
        HeapCreate(HEAP_CREATE_ENABLE_EXECUTE, 0, 0) as usize
    };
    static ref ATTACK_TIMEOUTS: Mutex<[u32; 8]> = Mutex::new([!0; 8]);
    static ref ATTACK_TIMEOUT_USED: Mutex<[bool; 8]> = Mutex::new([false; 8]);
    static ref IDLE_ORDERS: Mutex<IdleOrders> = Mutex::new(Default::default());
    static ref MAX_WORKERS: Mutex<Vec<MaxWorkers>> = Mutex::new(Default::default());
    static ref UNDER_ATTACK_MODE: Mutex<[Option<bool>; 8]> = Mutex::new(Default::default());
    static ref QUEUED_AI_OPCODES: Mutex<Vec<(u32, unsafe extern fn(*mut bw::AiScript))>> =
        Mutex::new(Default::default());
    // For tracking deleted towns.
    // If the tracking is updated after step_objects, it shouldn't be possible for a town
    // to be deleted and recreated in the same frame. (As recreation happens in scripts,
    // and deletion happens on last unit dying) Better solutions won't obviously hurt though.
    static ref TOWNS: Mutex<Vec<Town>> = Mutex::new(Vec::new());
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

#[derive(Serialize, Deserialize)]
struct SaveData {
    attack_timeouts: [u32; 8],
    attack_timeout_used: [bool; 8],
    idle_orders: IdleOrders,
    max_workers: Vec<MaxWorkers>,
    under_attack_mode: [Option<bool>; 8],
    towns: Vec<Town>,
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    unit::init_save_mapping();
    defer!(unit::clear_save_mapping());
    init_save_mapping();
    defer!(clear_save_mapping());
    let save = SaveData {
        attack_timeouts: ATTACK_TIMEOUTS.lock().unwrap().clone(),
        attack_timeout_used: ATTACK_TIMEOUT_USED.lock().unwrap().clone(),
        idle_orders: IDLE_ORDERS.lock().unwrap().clone(),
        max_workers: MAX_WORKERS.lock().unwrap().clone(),
        under_attack_mode: UNDER_ATTACK_MODE.lock().unwrap().clone(),
        towns: TOWNS.lock().unwrap().clone(),
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
        attack_timeout_used,
        idle_orders,
        max_workers,
        under_attack_mode,
        towns,
    } = data;
    *ATTACK_TIMEOUTS.lock().unwrap() = attack_timeouts;
    *ATTACK_TIMEOUT_USED.lock().unwrap() = attack_timeout_used;
    *IDLE_ORDERS.lock().unwrap() = idle_orders;
    *MAX_WORKERS.lock().unwrap() = max_workers;
    *UNDER_ATTACK_MODE.lock().unwrap() = under_attack_mode;
    *TOWNS.lock().unwrap() = towns;
    1
}

pub unsafe extern fn init_game() {
    *ATTACK_TIMEOUTS.lock().unwrap() = [!0; 8];
    *ATTACK_TIMEOUT_USED.lock().unwrap() = [false; 8];
    *IDLE_ORDERS.lock().unwrap() = Default::default();
    *MAX_WORKERS.lock().unwrap() = Default::default();
    *UNDER_ATTACK_MODE.lock().unwrap() = Default::default();
}

unsafe fn exec_alloc(size: usize) -> *mut u8 {
    HeapAlloc(*EXEC_HEAP as HANDLE, 0, size) as *mut u8
}

impl AiScriptOpcodes {
    fn new() -> AiScriptOpcodes {
        AiScriptOpcodes {
            opcodes: Vec::new(),
        }
    }

    fn add_hook(&mut self, id: u32, fun: unsafe extern fn(*mut bw::AiScript)) {
        let mut asm = Vec::new();
        asm.write_all(&[
            0x60, // pushad
            0x56, // push esi (aiscript)
            0xb8, // mov eax, ...
        ]).unwrap();
        asm.write_u32::<LittleEndian>(unsafe { mem::transmute(fun) }).unwrap();
        asm.write_all(&[
            0xff, 0xd0, // call eax
            0x59, // pop ecx
            0x61, // popad
            0xc7, 0x44, 0xe4, 0xfc, // Mov [esp - 4], dword ...
        ]).unwrap();
        asm.write_u32::<LittleEndian>(bw::v1161::ProgressAiScript_Loop as u32).unwrap();
        // jmp dword [esp - 4]
        asm.write_all(&[0xff, 0x64, 0xe4, 0xfc]).unwrap();
        self.opcodes.push(OpcodeHook {
            asm,
            id,
        });
    }

    unsafe fn apply(&self, patcher: &mut whack::ModulePatcher) {
        let largest_opcode = self.opcodes.iter().map(|x| x.id).max().unwrap_or(0) as u8;
        let mem_size: usize = self.opcodes.iter().map(|x| x.asm.len()).sum();
        assert!(largest_opcode <= 0x7f, "Opcodes larger than 0x7f are not supported");
        let old_largest_opcode = *(bw::v1161::ProgressAiScript_SwitchLimit as *const u8);
        if old_largest_opcode < largest_opcode {
            patcher.replace_val(bw::v1161::ProgressAiScript_SwitchLimit, largest_opcode);
        }

        // Always redirect the switch table to avoid having to worry about write access w/ bw's
        // .rdata
        let mut new_switch_table = vec![0; largest_opcode as usize + 1];
        let old_table = slice::from_raw_parts(
            *(bw::v1161::ProgressAiScript_SwitchTable as *mut *mut usize),
            old_largest_opcode as usize + 1,
        );

        (&mut new_switch_table[..old_largest_opcode as usize + 1]).copy_from_slice(old_table);
        let memory_ptr = exec_alloc(mem_size);
        let mut memory = slice::from_raw_parts_mut(memory_ptr, mem_size);
        for hook in &self.opcodes {
            (&mut memory[..hook.asm.len()]).copy_from_slice(&hook.asm);
            new_switch_table[hook.id as usize] = memory.as_ptr() as usize;
            memory = &mut {memory}[hook.asm.len()..];
        }

        patcher.replace_val(bw::v1161::ProgressAiScript_SwitchTable, new_switch_table.as_mut_ptr());
        mem::forget(new_switch_table);
    }
}

pub fn add_ai_opcode_1161(opcode: u32, hook: unsafe extern fn(*mut bw::AiScript)) {
    QUEUED_AI_OPCODES.lock().unwrap().push((opcode, hook));
}

pub unsafe fn add_aiscript_opcodes(patcher: &mut whack::ModulePatcher) {
    let mut hooks = AiScriptOpcodes::new();
    for &(op, hook) in QUEUED_AI_OPCODES.lock().unwrap().iter() {
        hooks.add_hook(op, hook);
    }
    patcher.hook_opt(bw::v1161::Ai_IsAttackTimedOut, is_attack_timed_out);
    hooks.apply(patcher);
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

pub unsafe extern fn attack_timeout(script: *mut bw::AiScript) {
    let timeout = read_u32(script);
    ATTACK_TIMEOUTS.lock().unwrap()[(*script).player as usize] = timeout;
}

pub unsafe fn attack_timeouts_frame_hook() {
    // Just force timeouts if attack timeout has ever happened.
    // Unlike is_attack_timed_out, this code also runs for SCR.
    // Actually only for SCR for now due to timeout 1 breaking teippi display
    // and most likely should not cause difference here
    if bw::scr() {
        let mut timeouts_used = ATTACK_TIMEOUT_USED.lock().unwrap();
        for i in 0..8 {
            let player_ai = bw::player_ai(i);
            if (*player_ai).last_attack_second != 0 {
                timeouts_used[i as usize] = true;
            }
            if (*player_ai).last_attack_second == 0 && timeouts_used[i as usize] {
                (*player_ai).last_attack_second = !100;
            }
        }
    }
}

unsafe fn is_attack_timed_out(player: u32, orig: &Fn(u32) -> u32) -> u32 {
    let ai_data = bw::player_ai(player);
    let last_attack_second = (*ai_data).last_attack_second;
    if last_attack_second == 0 {
        return 1;
    }

    let timeout = ATTACK_TIMEOUTS.lock().unwrap()[player as usize];
    if timeout == !0 {
        orig(player)
    } else {
        let ai_data = bw::player_ai(player);
        let timeout_second = last_attack_second.saturating_add(timeout);
        if bw::elapsed_seconds() > timeout_second {
            ATTACK_TIMEOUTS.lock().unwrap()[player as usize] = !0;
            (*ai_data).last_attack_second = 0;
            1
        } else {
            0
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
    let unit_id = UnitId(read_u16(script));
    let mut src = read_position(script);
    let radius = read_u16(script);
    src.extend_area(radius as i16);
    let mut target = read_position(script);
    let tgt_radius = read_u16(script);
    target.extend_area(tgt_radius as i16);
    let target_misc = read_u16(script);
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
        let ok = u.player() as u32 == (*script).player && u.matches_id(unit_id);
        if ok {
            count += 1;
        }
        ok
    });
    let targets = if flags & 0x7 != 0 {
        let mut acceptable_players = [false; 12];
        for i in 0..12 {
            if i == (*script).player {
                acceptable_players[i as usize] = flags & 0x2 != 0;
            } else {
                if (*bw::game()).alliances[(*script).player as usize][i as usize] == 0 {
                    acceptable_players[i as usize] = flags & 0x1 != 0;
                } else {
                    acceptable_players[i as usize] = flags & 0x4 != 0;
                }
            }
        }
        let mut count = 0;
        let target_unit_id = UnitId(target_misc);
        Some(unit::find_units(&target.area, |u| {
            if flags & 0x8 != 0 && count != 0 {
                return false;
            }
            let ok = acceptable_players[u.player() as usize] && u.matches_id(target_unit_id);
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
                (&mut (*unit.0).unit_specific[4..])
                    .write_u16::<LittleEndian>(target_misc).unwrap();
            }
            order::id::DRONE_BUILD | order::id::SCV_BUILD | order::id::PROBE_BUILD |
                order::id::UNIT_MORPH | order::id::BUILDING_MORPH | order::id::TRAIN |
                order::id::TRAIN_FIGHTER | order::id::BUILD_NYDUS_EXIT =>
            {
                (*unit.0).build_queue[(*unit.0).current_build_slot as usize] = target_misc;
            }
            _ => (),
        }
    }
}

pub unsafe extern fn idle_orders(script: *mut bw::AiScript) {
    // idle_orders(order, rate, count, unit_id, radius, target_id, priority, flags)
    // Flag 0x1 = Don't target enemies,
    //      0x2 = Target own,
    //      0x4 = Target allies,
    //      0x8 = Pick unseen targets
    //      0x10 = Pick invisible targets
    //      0x20 = In combat
    //      0x4000 = Remove matching, no error on mismatch
    //      0x8000 = Remove matching
    let order = OrderId(read_u8(script));
    let rate = read_u16(script);
    let limit = read_u16(script);
    let unit_id = UnitId(read_u16(script));
    let radius = read_u16(script);
    let target_unit_id = UnitId(read_u16(script));
    let priority = read_u8(script);
    let delete_flags;
    let flags = {
        let mut flags = IdleOrderFlags {
            simple: 0,
            status_required: 0,
            status_not: 0,
            numeric: Vec::new(),
        };
        loop {
            let mut val = read_u16(script);
            match (val & 0x2f00) >> 8 {
                0 => {
                    flags.simple = (val & 0xff) as u8;
                    delete_flags = val & 0xc000;
                    break;
                }
                1 => flags.status_required = (val & 0xff) as u8,
                2 => flags.status_not = (val & 0xff) as u8,
                3 => {
                    let amount = read_u32(script) as i32;
                    let comparision = match val & 0xf {
                        0 => Comparision::LessThan,
                        1 => Comparision::GreaterThan,
                        2 => Comparision::LessThanPercentage,
                        3 => Comparision::GreaterThanPercentage,
                        _ => {
                            bw::print_text("idle_orders: invalid encoding");
                            return;
                        }
                    };
                    let ty = match (val >> 4) & 0xf {
                        0 => IdleOrderNumeric::Hp,
                        1 => IdleOrderNumeric::Shields,
                        2 => IdleOrderNumeric::Health,
                        3 => IdleOrderNumeric::Energy,
                        _ => {
                            bw::print_text("idle_orders: invalid encoding");
                            return;
                        }
                    };
                    flags.numeric.push((ty, comparision, amount));
                }
                _ => bw::print_text("idle_orders: invalid encoding"),
            }
        }
        flags
    };
    if IDLE_ORDERS_DISABLED.load(Ordering::Acquire) == true {
        return;
    }
    if order.0 >= 254 {
        if order.0 == 255 {
            // Disable default spellcasting
            (*bw::player_ai((*script).player)).spell_cooldown = 250;
        } else {
            // Enable
            (*bw::player_ai((*script).player)).spell_cooldown = 0;
        }
        return;
    }
    let mut orders = IDLE_ORDERS.lock().unwrap();
    let idle_order_list = &mut orders.orders;
    if delete_flags != 0 {
        let silent_fail = delete_flags & 0x4000 != 0;
        let matchee = IdleOrder {
            order,
            limit,
            unit_id,
            target_unit_id,
            radius,
            priority,
            flags,
            rate,
            player: (*script).player as u8,
        };
        match idle_order_list.iter().position(|x| x.0 == matchee) {
            Some(s) => {
                idle_order_list.remove(s);
            }
            None => if !silent_fail {
                bw::print_text(
                    &format!("idle_orders: Unable to find match to remove for {:#?}", matchee),
                );
            },
        }
    } else {
        let pos = idle_order_list.binary_search_by_key(&priority, |x| x.0.priority)
            .unwrap_or_else(|x| x);
        idle_order_list.insert(pos, (IdleOrder {
            order,
            limit,
            unit_id,
            target_unit_id,
            radius,
            priority,
            flags,
            rate,
            player: (*script).player as u8,
        }, IdleOrderState::new()));
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

pub fn update_towns() {
    let mut towns_global = TOWNS.lock().unwrap();
    let mut max_workers = MAX_WORKERS.lock().unwrap();
    let old = mem::replace(&mut *towns_global, towns());
    for old in old {
        if !towns_global.iter().any(|&x| x == old) {
            max_workers.retain(|x| x.town != old);
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
    workers.retain(|x| x.town != town);
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct IdleOrders {
    orders: Vec<(IdleOrder, IdleOrderState)>,
    // user, target, return point
    ongoing: Vec<OngoingOrder>,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct OngoingOrder {
    user: Unit,
    target: Option<Unit>,
    home: bw::Point,
    order: OrderId,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrder {
    priority: u8,
    order: OrderId,
    limit: u16,
    unit_id: UnitId,
    target_unit_id: UnitId,
    radius: u16,
    flags: IdleOrderFlags,
    rate: u16,
    player: u8,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrderFlags {
    simple: u8,
    status_required: u8,
    status_not: u8,
    numeric: Vec<(IdleOrderNumeric, Comparision, i32)>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
enum IdleOrderNumeric {
    Hp,
    Shields,
    Energy,
    Health,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
enum Comparision {
    LessThanPercentage,
    GreaterThanPercentage,
    LessThan,
    GreaterThan,
}

impl IdleOrderFlags {
    fn match_status(&self, unit: &Unit) -> bool {
        unsafe {
            let status_ok = if self.status_required != 0 || self.status_not != 0 {
                let flags = (if (*unit.0).ensnare_timer != 0 { 1 } else { 0 } << 0) |
                    (if (*unit.0).plague_timer != 0 { 1 } else { 0 } << 1) |
                    (if (*unit.0).lockdown_timer != 0 { 1 } else { 0 } << 2) |
                    (if (*unit.0).irradiate_timer != 0 { 1 } else { 0 } << 3) |
                    (if (*unit.0).parasited_by_players != 0 { 1 } else { 0 } << 4) |
                    (if (*unit.0).is_blind != 0 { 1 } else { 0 } << 5) |
                    (if (*unit.0).matrix_timer != 0 { 1 } else { 0 } << 6) |
                    (if (*unit.0).maelstrom_timer != 0 { 1 } else { 0 } << 7);
                self.status_required & flags == self.status_required &&
                    self.status_not & flags == 0
            } else {
                true
            };
            if !status_ok {
                return false;
            }
            self.numeric.iter().all(|&(ty, compare, amount)| {
                let id = unit.id();
                let (val, max) = match ty {
                    IdleOrderNumeric::Hp => (unit.hitpoints(), id.hitpoints()),
                    IdleOrderNumeric::Shields => {
                        if !id.has_shields() {
                            return false;
                        }
                        (unit.shields(), id.shields())
                    },
                    IdleOrderNumeric::Health => (
                        unit.hitpoints().saturating_add(unit.shields()),
                        id.hitpoints().saturating_add(id.shields()),
                    ),
                    // TODO max energy
                    IdleOrderNumeric::Energy => (unit.energy() as i32, 250 * 256),
                };
                match compare {
                    Comparision::LessThan => val < amount,
                    Comparision::GreaterThan => val > amount,
                    Comparision::LessThanPercentage => {
                        val.saturating_mul(100).checked_div(max).unwrap_or(0) < amount
                    }
                    Comparision::GreaterThanPercentage => {
                        val.saturating_mul(100).checked_div(max).unwrap_or(0) > amount
                    }
                }
            })
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct IdleOrderState {
    next_frame: u32,
}

unsafe impl Send for OngoingOrder {}

impl IdleOrderState {
    fn new() -> IdleOrderState {
        IdleOrderState {
            next_frame: 0,
        }
    }
}

impl IdleOrder {
    fn unit_valid(&self, user: &Unit) -> bool {
        let energy_cost = self.order.tech()
            .map(|x| x.energy_cost() << 8)
            .unwrap_or(0);
        user.player() == self.player &&
            user.matches_id(self.unit_id) &&
            user.order() != self.order &&
            user.energy() as u32 >= energy_cost
    }
}

pub fn remove_from_idle_orders(unit: &Unit) {
    let mut orders = IDLE_ORDERS.lock().unwrap();
    for x in orders.ongoing.iter().filter(|x| x.target == Some(*unit)) {
        unsafe {
            bw::issue_order(x.user.0, order::id::MOVE, x.home, null_mut(), unit::id::NONE);
        }
    }
    orders.ongoing.retain(|x| *unit != x.user && Some(*unit) != x.target);
}

pub unsafe fn step_idle_orders() {
    for i in 0..8 {
        let ai = bw::player_ai(i);
        if (*ai).spell_cooldown > 200 {
            // Keep spell cooldown active if default spellcasting was disabled
            (*ai).spell_cooldown = 250;
        }
    }
    let current_frame = (*bw::game()).frame_count;
    let mut orders = IDLE_ORDERS.lock().unwrap();
    let orders = &mut *orders;
    let ongoing = &mut orders.ongoing;
    // Yes, it may consider an order ongoing even if the unit is targeting the
    // target for other reasons. Acceptable?
    ongoing.retain(|o| {
        let retain = match o.target {
            None => o.user.orders().any(|x| x.id == o.order),
            Some(target) => o.user.orders().filter_map(|x| x.target).any(|x| x == target),
        };
        if !retain {
            bw::issue_order(o.user.0, order::id::MOVE, o.home, null_mut(), unit::id::NONE);
        }
        retain
    });
    for &mut (ref decl, ref mut state) in orders.orders.iter_mut().rev() {
        if state.next_frame <= current_frame {
            let unit = unit::active_units()
                .find(|u| decl.unit_valid(u) && !ongoing.iter().any(|x| x.user == *u));
            if let Some(unit) = unit {
                // Instead of a *perfect* solution of trying to find closest user-target pair,
                // find closest target for the first unit, and then find closest user for
                // the target (if the distance is large enough for it to matter)
                let (target, distance) = match find_idle_order_target(&unit, decl, &ongoing) {
                    None => continue,
                    Some(s) => s,
                };
                let (user, distance) = {
                    if distance > 16 * 32 || distance > decl.radius as u32 {
                        match find_idle_order_user(&target, decl, &ongoing) {
                            None => (unit, distance),
                            Some(s) => s,
                        }
                    } else {
                        (unit, distance)
                    }
                };
                if distance < decl.radius as u32 {
                    let pos = target.position();
                    let no_detection = target.is_invisible() &&
                        (*target.0).detection_status & (1 << decl.player) as u32 == 0;
                    let order_target = if no_detection {
                        null_mut()
                    } else {
                        target.0
                    };
                    let home = match user.order() {
                        order::id::MOVE => (*user.0).order_target_pos,
                        _ => user.position(),
                    };
                    bw::issue_order(user.0, decl.order, pos, order_target, unit::id::NONE);
                    ongoing.push(OngoingOrder {
                        user,
                        target: Unit::from_ptr(order_target),
                        home,
                        order: decl.order,
                    });
                    // Round to multiple of decl.rate so that priority is somewhat useful.
                    // Adds [rate, rate * 2) frames of wait.
                    let rate = decl.rate as u32;
                    state.next_frame = current_frame.saturating_sub(1)
                        .checked_div(rate).unwrap_or(current_frame)
                        .saturating_add(2) *
                        rate;
                } else {
                    state.next_frame = current_frame + 24 * 10;
                }
            }
        }
    }
}

unsafe fn find_idle_order_target(
    user: &Unit,
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
) -> Option<(Unit, u32)> {
    let accept_enemies = decl.flags.simple & 0x2 == 0;
    let accept_own = decl.flags.simple & 0x2 != 0;
    let accept_allies = decl.flags.simple & 0x4 != 0;
    let accept_unseen = decl.flags.simple & 0x8 != 0;
    let accept_invisible = decl.flags.simple & 0x10 != 0;
    let in_combat = decl.flags.simple & 0x20 != 0;
    let player_mask = 1 << decl.player;
    let mut acceptable_players = [false; 12];
    let game = bw::game();
    for i in 0..12 {
        if i == decl.player {
            acceptable_players[i as usize] = accept_own;
        } else {
            if (*game).alliances[decl.player as usize][i as usize] == 0 {
                acceptable_players[i as usize] = accept_enemies;
            } else {
                acceptable_players[i as usize] = accept_allies;
            }
        }
    }
    unit::find_nearest(user.position(), |unit| {
        if unit.is_invincible() {
            return false;
        }
        if !decl.flags.match_status(unit) {
            return false;
        }
        if !unit.matches_id(decl.target_unit_id) || !acceptable_players[unit.player() as usize] {
            return false;
        }
        if !accept_unseen {
            if unit.sprite().map(|s| (*s).visibility_mask & player_mask == 0).unwrap_or(true) {
                return false;
            }
        }
        if !accept_invisible {
            if unit.is_invisible() && (*unit.0).detection_status & player_mask as u32 == 0 {
                return false;
            }
        }
        if in_combat {
            let ok = unit.target().map(|x| {
                let targeting_enemy =
                    (*game).alliances[unit.player() as usize][x.player() as usize] == 0;
                targeting_enemy && unit.order().is_attack_order()
            }).unwrap_or(false);
            if !ok {
                return false;
            }
        }
        let already_targeted_count = ongoing.iter()
            .filter(|x| x.target == Some(*unit) && x.order == decl.order)
            .count();
        already_targeted_count < decl.limit as usize
    })
}

fn find_idle_order_user(
    target: &Unit,
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
) -> Option<(Unit, u32)> {
    unit::find_nearest(target.position(), |unit| {
        decl.unit_valid(unit) && !ongoing.iter().any(|x| x.user == *unit)
    })
}

pub unsafe extern fn deaths(script: *mut bw::AiScript) {
    enum Modifier {
        AtLeast,
        AtMost,
        Set,
        Add,
        Subtract,
        Exactly,
    }
    // deaths(player, modifier, amount, unit, dest)
    let player = read_u8(script);
    let modifier = read_u8(script);
    let amount = read_u32(script);
    let unit_id = read_u16(script);
    let dest = read_u16(script);
    let player = match player {
        x @ 0 ... 11 => x,
        13 => (*script).player as u8,
        x => {
            bw::print_text(format!("Unsupported player in deaths: {:x}", x));
            return;
        }
    };
    let modifier = match modifier {
        // Matching trigger conditions
        0 => Modifier::AtLeast,
        1 => Modifier::AtMost,
        7 => Modifier::Set,
        8 => Modifier::Add,
        9 => Modifier::Subtract,
        10 => Modifier::Exactly,
        x => {
            bw::print_text(format!("Unsupported modifier in deaths: {:x}", x));
            return;
        }
    };
    let deaths = (*bw::game())
        .deaths.get_mut(unit_id as usize).and_then(|x| x.get_mut(player as usize));
    if let Some(deaths) = deaths {
        let jump = match modifier {
            Modifier::AtLeast => Some(*deaths >= amount),
            Modifier::AtMost => Some(*deaths <= amount),
            Modifier::Exactly => Some(*deaths == amount),
            Modifier::Set => {
                *deaths = amount;
                None
            }
            Modifier::Add => {
                *deaths = deaths.saturating_add(amount);
                None
            }
            Modifier::Subtract => {
                *deaths = deaths.saturating_sub(amount);
                None
            }
        };
        if jump == Some(true) {
            (*script).pos = dest as u32;
        }
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

unsafe fn read_u8(script: *mut bw::AiScript) -> u8 {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let val = *(script_bytes.offset((*script).pos as isize) as *const u8);
    (*script).pos += 1;
    val
}

unsafe fn read_u16(script: *mut bw::AiScript) -> u16 {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let val = *(script_bytes.offset((*script).pos as isize) as *const u16);
    (*script).pos += 2;
    val
}

unsafe fn read_u32(script: *mut bw::AiScript) -> u32 {
    let script_bytes = match (*script).flags & 0x1 != 0 {
        false => bw::aiscript_bin(),
        true => bw::bwscript_bin(),
    };
    let val = *(script_bytes.offset((*script).pos as isize) as *const u32);
    (*script).pos += 4;
    val
}
