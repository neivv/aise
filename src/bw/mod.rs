#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::ptr::{null, null_mut};
use std::sync::atomic::Ordering;

use bw_dat::{OrderId, TechId, UnitId, UpgradeId};

use samase;

pub mod structs;

pub use self::structs::*;

pub fn is_scr() -> bool {
    crate::IS_1161.load(Ordering::Acquire) == false
}

pub fn player_ai(player: u32) -> *mut PlayerAiData {
    samase::player_ai(player)
}

pub fn ai_regions(player: u32) -> *mut AiRegion {
    samase::ai_regions(player)
}

pub fn pathing() -> *mut Pathing {
    samase::pathing()
}

pub fn get_region(pos: Point) -> Option<u16> {
    let Point {
        x,
        y,
    } = pos;
    unsafe {
        let game = game();
        let bounds = ((*game).map_width_tiles * 32, (*game).map_height_tiles * 32);
        if bounds.0 as i16 <= x || bounds.1 as i16 <= y {
            None
        } else {
            Some(samase::get_region(x as u32, y as u32) as u16)
        }
    }
}

pub fn game() -> *mut Game {
    samase::game()
}

pub fn rng_seed() -> u32 {
    samase::rng_seed().unwrap_or_else(|| {
        // Oh well, rng.rs only uses this for the initial seed
        unsafe { (*game()).frame_count.wrapping_add(1234) }
    })
}

pub fn elapsed_seconds() -> u32 {
    unsafe { (*game()).elapsed_seconds }
}

pub fn location(location: u8) -> Location {
    unsafe { (*game()).locations[location as usize] }
}

pub fn point_from_rect(rect: Rect) -> Point {
    Point {
        x: rect.left + (rect.right - rect.left) / 2,
        y: rect.top + (rect.bottom - rect.top) / 2,
    }
}

pub unsafe fn issue_order(
    unit: *mut Unit,
    order: OrderId,
    pos: Point,
    target: *mut Unit,
    fow_unit: UnitId,
) {
    samase::issue_order(unit, order, pos.x as u32, pos.y as u32, target, fow_unit)
}

pub fn first_active_unit() -> *mut Unit {
    samase::first_active_unit()
}

pub fn first_hidden_unit() -> *mut Unit {
    samase::first_hidden_unit()
}

pub fn first_ai_script() -> *mut AiScript {
    samase::first_ai_script()
}

pub fn set_first_ai_script(script: *mut AiScript) {
    samase::set_first_ai_script(script)
}

pub fn first_free_ai_script() -> *mut AiScript {
    samase::first_free_ai_script()
}

pub fn set_first_free_ai_script(script: *mut AiScript) {
    samase::set_first_free_ai_script(script)
}

pub fn guard_ais(player: u8) -> *mut GuardAi {
    unsafe {
        assert!(player < 8);
        (*samase::guard_ais().offset(player as isize)).first
    }
}

pub fn guard_array() -> *mut GuardAiArray {
    unsafe { (*samase::guard_ais()).array }
}

pub fn change_ai_region_state(region: *mut AiRegion, state: u32) {
    samase::change_ai_region_state(region, state);
}

lazy_static! {
    static ref SAMASE_AISCRIPT_BIN: usize =
        samase::read_file("scripts\\aiscript.bin").unwrap().0 as usize;
    static ref SAMASE_BWSCRIPT_BIN: usize =
        samase::read_file("scripts\\bwscript.bin").unwrap().0 as usize;
}

pub fn aiscript_bin() -> *mut u8 {
    *SAMASE_AISCRIPT_BIN as *mut u8
}

pub fn bwscript_bin() -> *mut u8 {
    *SAMASE_BWSCRIPT_BIN as *mut u8
}

pub fn unit_dat_requirements(unit: UnitId) -> Option<*const u16> {
    let result = samase::requirements(0, unit.0 as u32);
    if result == null() {
        None
    } else {
        Some(result)
    }
}

pub fn upgrade_dat_requirements(upgrade: UpgradeId) -> Option<*const u16> {
    let result = samase::requirements(1, upgrade.0 as u32);
    if result == null() {
        None
    } else {
        Some(result)
    }
}

pub fn tech_research_dat_requirements(tech: TechId) -> Option<*const u16> {
    let result = samase::requirements(2, tech.0 as u32);
    if result == null() {
        None
    } else {
        Some(result)
    }
}

// BW algorithm
pub fn distance(a: Point, b: Point) -> u32 {
    let x = (a.x as i32).wrapping_sub(b.x as i32).abs() as u32;
    let y = (a.y as i32).wrapping_sub(b.y as i32).abs() as u32;
    let (greater, lesser) = (x.max(y), x.min(y));
    if greater / 4 > lesser {
        greater
    } else {
        greater * 59 / 64 + lesser * 99 / 256
    }
}

pub fn rect_distance(a: &Rect, b: &Rect) -> u32 {
    let horizontal_overlap = a.left < b.right && a.right > b.left;
    let vertical_overlap = a.top < b.bottom && a.bottom > b.top;
    let x_diff = match horizontal_overlap {
        true => 0,
        false => match a.left < b.left {
            true => b.left - a.right,
            false => a.left - b.right,
        },
    };
    let y_diff = match vertical_overlap {
        true => 0,
        false => match a.top < b.top {
            true => b.top - a.bottom,
            false => a.top - b.bottom,
        },
    };

    distance(
        Point {
            x: 0,
            y: 0,
        },
        Point {
            x: x_diff,
            y: y_diff,
        },
    )
}

pub fn town_array_start() -> *mut AiTown {
    let ptr = samase::active_towns();
    if ptr.is_null() {
        null_mut()
    } else {
        unsafe { (*ptr).array }
    }
}

pub fn first_active_ai_town(player: u8) -> *mut AiTown {
    unsafe { (*samase::active_towns().add(player as usize)).first }
}

whack_hooks!(stdcall, 0x00400000,
    0x00488AF0 => increment_death_scores(@edi *mut Unit, @edx u8);
    0x004465C0 => choose_placement_position(u32, u32, *mut Point, u32, @ecx *mut Unit) -> u32;
    0x00473FB0 => update_building_placement_state_hook(*mut Unit, u8, u32, u32, u16, u8, u8, u8, u8) -> u32;
    0x004A13C0 => ai_spellcast(bool, @eax *mut Unit) -> u32;
    0x0047B090 => get_unit_name(@ecx u32) -> *const u8;
    0x0043FCF0 => ai_focus_unit_check(@ecx *mut Unit, @edx u32) -> u32;
);

whack_funcs!(stdcall, init_funcs, 0x00400000,
    0x00473FB0 => update_building_placement_state(*mut Unit, u8, u32, u32, u16, u8, u8, u8, u8) -> u32;
    0x004936B0 => is_powered(u32, u32, u8, @eax u32) -> u32;
    0x004A34C0 => ping_minimap(u32, u32, u8);
    0x00433DD0 => add_town_unit_ai(@ebx *mut Unit, @edi *mut AiTown);
    0x004A1E50 => remove_unit_ai(@ecx *mut Unit, @edx u32);
    0x00414560 => remove_creep_at_unit(
        @ecx u32,
        @eax u32,
        @edx u32,
        unsafe extern "stdcall" fn(u32, u32, *mut Rect32) -> u32
    );
);

whack_vars!(init_vars, 0x00400000,
    0x0057EE9C => player_name: [u8; 0x19];
    0x0057F0B4 => is_multiplayer: u8;
    0x006D1260 => tile_flags: *mut u32;
    0x00597208 => client_selection: [*mut Unit; 0xc];

);
