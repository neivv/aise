use std::io::Write;
use std::fmt;
use std::mem;
use std::ptr::null_mut;
use std::slice;
use std::sync::Mutex;

use byteorder::{WriteBytesExt, LittleEndian};
use kernel32;
use winapi;
use whack;

use bw;
use order::{self, OrderId};
use unit::{self, UnitId};

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
        kernel32::HeapCreate(winapi::HEAP_CREATE_ENABLE_EXECUTE, 0, 0) as usize
    };
    static ref ATTACK_TIMEOUTS: Mutex<[u32; 8]> = Mutex::new([!0; 8]);
}

unsafe fn exec_alloc(size: usize) -> *mut u8 {
    kernel32::HeapAlloc(*EXEC_HEAP as winapi::HANDLE, 0, size as u32) as *mut u8
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

pub unsafe fn add_aiscript_opcodes(patcher: &mut whack::ModulePatcher) {
    let mut hooks = AiScriptOpcodes::new();
    hooks.add_hook(0x71, attack_to);
    hooks.add_hook(0x72, attack_timeout);
    hooks.add_hook(0x73, issue_order);
    hooks.add_hook(0x74, deaths);
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
