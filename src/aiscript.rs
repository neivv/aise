use std::io::Write;
use std::mem;
use std::slice;
use std::sync::Mutex;

use byteorder::{WriteBytesExt, LittleEndian};
use kernel32;
use winapi;
use whack;

use bw;

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
    patcher.hook_opt(bw::v1161::Ai_IsAttackTimedOut, is_attack_timed_out);
    hooks.apply(patcher);
}

pub unsafe extern fn attack_to(script: *mut bw::AiScript) {
    let grouping_x = read_u16(script);
    let grouping_y = read_u16(script);
    let target_x = read_u16(script);
    let target_y = read_u16(script);
    let grouping_region = match bw::get_region(grouping_x, grouping_y) {
        Some(s) => s,
        None => {
            bw::print_text(&format!(
                "Aiscript attackto (player {}): invalid grouping coordinates {}, {}",
                (*script).player,
                grouping_x,
                grouping_y,
            ));
            return;
        }
    };
    let target_region = match bw::get_region(target_x, target_y) {
        Some(s) => s,
        None => {
            bw::print_text(&format!(
                "Aiscript attackto (player {}): invalid target coordinates {}, {}",
                (*script).player,
                target_x,
                target_y,
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

unsafe fn ai_region(player: u32, region: u16) -> *mut bw::AiRegion {
    bw::ai_regions(player).offset(region as isize)
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
