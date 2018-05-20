#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::ptr::null;

use bw_dat::{UnitId, OrderId, TechId, UpgradeId};

use samase;

pub mod structs;

pub use self::structs::*;

pub fn player_ai(player: u32) -> *mut PlayerAiData {
    samase::player_ai(player)
}

pub fn ai_regions(player: u32) -> *mut AiRegion {
    samase::ai_regions(player)
}

pub fn pathing_region(pos: Point) -> Option<*mut Region> {
    Some(region(get_region(pos)?))
}

pub fn region(id: u16) -> *mut Region {
    unsafe {
        &mut (*samase::pathing()).regions[id as usize]
    }
}

pub fn get_region(pos: Point) -> Option<u16> {
    let Point { x, y } = pos;
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

pub fn elapsed_seconds() -> u32 {
    unsafe { (*game()).elapsed_seconds }
}

pub fn location(location: u8) -> Location {
    unsafe { (*game()).locations[location as usize] }
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

pub fn guard_ai_list(player: u8) -> *mut GuardAiList {
    unsafe {
        assert!(player < 8);
        samase::guard_ais().offset(player as isize)
    }
}

pub fn guard_ais(player: u8) -> *mut GuardAi {
    unsafe {
        assert!(player < 8);
        (*samase::guard_ais().offset(player as isize)).first
    }
}

pub fn change_ai_region_state(region: *mut AiRegion, state: u32) {
    samase::change_ai_region_state(region, state);
}

lazy_static! {
    static ref SAMASE_AISCRIPT_BIN: usize =
        samase::read_file("scripts\\aiscript.bin").unwrap() as usize;
    static ref SAMASE_BWSCRIPT_BIN: usize =
        samase::read_file("scripts\\bwscript.bin").unwrap() as usize;
    static ref SAMASE_UNITS_DAT: usize =
        samase::read_file("arr\\units.dat").unwrap() as usize;
}

pub fn collision_rect(unit: UnitId) -> Rect {
    unsafe {
        assert!(unit.0 < 0xe4);
        let dat = *SAMASE_UNITS_DAT as *const u8;
        *(dat.offset(0x3124 + unit.0 as isize * 8) as *const Rect)
    }
}

pub fn aiscript_bin() -> *mut u8 {
    *SAMASE_AISCRIPT_BIN as *mut u8
}

pub fn bwscript_bin() -> *mut u8 {
    *SAMASE_BWSCRIPT_BIN as *mut u8
}

pub fn print_text<M: AsRef<str>>(msg: M) {
    let mut buf: Vec<u8> = msg.as_ref().as_bytes().into();
    buf.push(0);
    samase::print_text(buf.as_ptr());
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
    let (greater, lesser) = if x > y {
        (x, y)
    } else {
        (y, x)
    };
    if greater / 4 > lesser {
        greater
    } else {
        greater * 59 / 64 + lesser * 99 / 256
    }
}
