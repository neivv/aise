use bw_dat::{Unit, UnitId};

use crate::aiscript::{Town};
use crate::bw;
use crate::globals::{Globals};

pub unsafe extern fn placement_position_hook(
    unit: *mut bw::Unit,
    state: *mut u8,
    player: u8,
    unit_id: u16,
    state_center: u32,
    search_pos: u32,
    out: *mut u32,
    orig: unsafe extern fn(*mut bw::Unit, *mut u8, u8, u16, u32, u32, *mut u32) -> u32,
) -> u32 {
    let mut search_pos = search_pos;
    if let Some(unit) = Unit::from_ptr(unit) {
        if let Some(ai) = unit.worker_ai() {
            if let Some(town) = Town::from_ptr((*ai).town) {
                let globals = Globals::get("placement pos hook");
                let unit_id = UnitId(unit_id);
                if let Some(vec) = globals.build_at.get(&town) {
                    for entry in vec {
                        if entry.unit_match.matches_id(unit_id) {
                            search_pos = entry.position.pack_u32();
                            break;
                        }
                    }
                }
            }
        }
    }
    orig(unit, state, player, unit_id, state_center, search_pos, out)
}

pub unsafe extern fn placement_flags_hook(
    unit: *mut bw::Unit,
    player: u8,
    unit_id: u16,
    orig: unsafe extern fn(*mut bw::Unit, u8, u16) -> u32,
) -> u32 {
    let mut flags = orig(unit, player, unit_id);
    if let Some(unit) = Unit::from_ptr(unit) {
        if let Some(ai) = unit.worker_ai() {
            if let Some(town) = Town::from_ptr((*ai).town) {
                let globals = Globals::get("placement flags hook");
                let unit_id = UnitId(unit_id);
                if let Some(vec) = globals.build_at.get(&town) {
                    for entry in vec {
                        if entry.unit_match.matches_id(unit_id) {
                            flags &= !(entry.flags & 0xffff);
                            flags |= (entry.flags >> 16) & 0xffff;
                            break;
                        }
                    }
                }
            }
        }
    }
    flags
}
