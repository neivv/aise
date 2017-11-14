#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use libc::c_void;

pub mod structs;

pub use self::structs::*;

whack_hooks!(stdcall, 0x00400000,
    0x00447090 => Ai_IsAttackTimedOut(@ecx u32) -> u32;
);

whack_funcs!(stdcall, init_funcs, 0x00400000,
    0x0048D1C0 => print_text(*const u8, u32, @eax u32);
    0x0049C9F0 => get_region(@edi u32, @ecx u32) -> u32;
    0x004390A0 => change_ai_region_state(@esi *mut AiRegion, @ebx u32);
);

whack_vars!(init_vars, 0x00400000,
    0x0068C104 => aiscript_bin: *mut c_void;
    0x0068C108 => bwscript_bin: *mut c_void;

    0x00628450 => map_width: u16;
    0x006284B4 => map_height: u16;
    0x0058D6F8 => elapsed_seconds: u32;
    0x0068FEE8 => player_ai: [PlayerAiData; 0x8];
    0x0069A604 => ai_regions: [*mut AiRegion; 0x8];
);

pub const ProgressAiScript_SwitchLimit: usize = 0x0045B885;
pub const ProgressAiScript_SwitchTable: usize = 0x0045B892;
pub const ProgressAiScript_Loop: usize = 0x0045B860;
