#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use samase;

pub mod structs;

pub use self::structs::*;

pub mod v1161 {
    use super::structs::*;
    whack_hooks!(stdcall, 0x00400000,
        0x00447090 => Ai_IsAttackTimedOut(@ecx u32) -> u32;
    );

    whack_funcs!(stdcall, init_funcs, 0x00400000,
        0x0048D1C0 => print_text(*const u8, u32, @eax u32);
        0x0049C9F0 => get_region(@edi u32, @ecx u32) -> u32;
        0x004390A0 => change_ai_region_state(@esi *mut AiRegion, @ebx u32);
    );

    whack_vars!(init_vars, 0x00400000,
        0x0068C104 => aiscript_bin: *mut u8;
        0x0068C108 => bwscript_bin: *mut u8;

        0x00628450 => map_width: u16;
        0x006284B4 => map_height: u16;
        0x0058D6F8 => elapsed_seconds: u32;
        0x0068FEE8 => player_ai: [PlayerAiData; 0x8];
        0x0069A604 => ai_regions: [*mut AiRegion; 0x8];
    );

    pub const ProgressAiScript_SwitchLimit: usize = 0x0045B885;
    pub const ProgressAiScript_SwitchTable: usize = 0x0045B892;
    pub const ProgressAiScript_Loop: usize = 0x0045B860;
}

fn scr() -> bool {
    use std::sync::atomic::Ordering;
    ::SAMASE_INIT.load(Ordering::Acquire)
}

pub fn player_ai(player: u32) -> *mut PlayerAiData {
    unsafe {
        match scr() {
            true => samase::player_ai(player),
            false => &mut v1161::player_ai[player as usize],
        }
    }
}

pub fn ai_regions(player: u32) -> *mut AiRegion {
    unsafe {
        match scr() {
            true => samase::ai_regions(player),
            false => v1161::ai_regions[player as usize],
        }
    }
}

pub fn get_region(x: u16, y: u16) -> Option<u16> {
    unsafe {
        let bounds = match scr() {
            true => {
                let game = samase::game();
                ((*game).map_width_tiles * 32, (*game).map_height_tiles * 32)
            }
            false => (*v1161::map_width, *v1161::map_height),
        };
        if bounds.0 <= x || bounds.1 <= y {
            None
        } else {
            Some(match scr() {
                true => samase::get_region(x as u32, y as u32) as u16,
                false => v1161::get_region(x as u32, y as u32) as u16,
            })
        }
    }
}

pub fn elapsed_seconds() -> u32 {
    unsafe {
        match scr() {
            true => (*samase::game()).elapsed_seconds,
            false => *v1161::elapsed_seconds,
        }
    }
}

pub fn change_ai_region_state(region: *mut AiRegion, state: u32) {
    unsafe {
        match scr() {
            true => samase::change_ai_region_state(region, state),
            false => v1161::change_ai_region_state(region, state),
        }
    }
}

lazy_static! {
    static ref SAMASE_AISCRIPT_BIN: usize =
        samase::read_file("scripts\\aiscript.bin").unwrap() as usize;
    static ref SAMASE_BWSCRIPT_BIN: usize =
        samase::read_file("scripts\\bwscript.bin").unwrap() as usize;
}

pub fn aiscript_bin() -> *mut u8 {
    unsafe {
        match scr() {
            true => *SAMASE_AISCRIPT_BIN as *mut u8,
            false => *v1161::aiscript_bin,
        }
    }
}

pub fn bwscript_bin() -> *mut u8 {
    unsafe {
        match scr() {
            true => *SAMASE_BWSCRIPT_BIN as *mut u8,
            false => *v1161::bwscript_bin,
        }
    }
}

pub fn print_text(msg: &str) {
    unsafe {
        if scr() {
            return;
        } else {
            let mut buf: Vec<u8> = msg.as_bytes().into();
            buf.push(0);
            v1161::print_text(buf.as_ptr(), 0, 8);
        }
    }
}
