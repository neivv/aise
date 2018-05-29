use std::ptr::null_mut;

use libc::c_void;

use bw_dat::{order, unit, UnitId};

use bw;
use game::Game;
use unit::{active_units, Unit};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlayerAi(pub *mut bw::PlayerAiData, pub u8);

impl PlayerAi {
    pub fn get(player: u8) -> PlayerAi {
        PlayerAi(bw::player_ai(player.into()), player)
    }

    fn train_request_priority(&self, game: Game) -> u8 {
        if game.minerals(self.1) > 1500 && game.gas(self.1) > 1500 {
            160
        } else {
            50
        }
    }

    pub fn check_train(&self, unit: Unit, game: Game) {
        let train_unit_id = unsafe { (*self.0).train_unit_id };
        if train_unit_id != 0 {
            let unit_id = UnitId(train_unit_id - 1);
            unsafe {
                (*self.0).train_unit_id = 0;
            }
            if let Some(region) = unit_ai_region(self.1, unit) {
                let priority = self.train_request_priority(game);
                self.add_train_request(unit_id, region, priority, game);
            }
        }
    }

    fn is_campaign(&self) -> bool {
        self.flags() & 0x20 != 0
    }

    fn flags(&self) -> u16 {
        unsafe {
            (*self.0).flags
        }
    }

    fn add_train_request(
        &self,
        unit: UnitId,
        region: *mut bw::AiRegion,
        priority: u8,
        game: Game,
    ) {
        if self.flags() & 0x200 != 0 {
            return;
        }
        unsafe {
            (*self.0).flags |= 0x40;
        }
        self.add_spending_request(&SpendingRequest::Military(unit, region), priority, game);
    }

    fn is_at_limit(&self, unit: UnitId, game: Game) -> bool {
        match unsafe { (*self.0).build_limits[unit.0 as usize] } {
            0 => false,
            0xff => true,
            x => count_units(self.1, unit, game) >= x.into(),
        }
    }

    fn add_spending_request(&self, req: &SpendingRequest, priority: u8, game: Game) {
        let limit_reached = match req {
            SpendingRequest::Military(unit, _) | SpendingRequest::Guard(unit, _) => {
                self.is_at_limit(*unit, game)
            }
        };
        if !limit_reached {
            self.add_spending_request_inner(req, priority, game);
        }
    }

    fn add_spending_request_inner(&self, req: &SpendingRequest, priority: u8, game: Game) {
        let cost = match req {
            SpendingRequest::Military(unit, _) | SpendingRequest::Guard(unit, _) => {
                unit_cost(*unit)
            }
        };
        unsafe {
            (*self.0).mineral_need = (*self.0).mineral_need.saturating_add(cost.minerals);
            (*self.0).gas_need = (*self.0).gas_need.saturating_add(cost.gas);
            (*self.0).supply_need = (*self.0).supply_need.saturating_add(cost.supply);
        }
        let (ty, id, val) = match *req {
            SpendingRequest::Military(unit, region) => (1, unit.0, region as *mut c_void),
            SpendingRequest::Guard(unit, ai) => (2, unit.0, ai as *mut c_void),
        };
        let raw_req = bw::AiSpendingRequest {
            priority,
            ty,
            id,
            val,
        };
        {
            let requests = unsafe { &mut (*self.0).requests[..] };
            let mut pos = unsafe {
                if usize::from((*self.0).request_count) < requests.len() {
                    (*self.0).request_count += 1;
                }
                isize::from((*self.0).request_count) - 1
            };
            let mut prev = pos;
            requests[pos as usize] = raw_req;
            pos = (pos - 1) / 2;
            while prev > 0 {
                if requests[prev as usize].priority <= requests[pos as usize].priority {
                    break;
                }
                requests.swap(prev as usize, pos as usize);
                prev = pos;
                pos = (pos - 1) / 2;
            }
        }
        if let Some((prereq, count)) = self.prerequisite_unit(req, game) {
            for _ in 0..count {
                self.add_spending_request_inner(&prereq, priority, game);
            }
        }
    }

    fn prerequisite_unit(
        &self,
        req: &SpendingRequest,
        game: Game,
    ) -> Option<(SpendingRequest, u32)> {
        use self::SpendingRequest::*;
        let (prereq, count) = match *req {
            Military(unit, _) | Guard(unit, _)  => {
                match unit {
                    unit::GUARDIAN | unit::DEVOURER => (unit::MUTALISK, 1u32),
                    unit::LURKER => (unit::HYDRALISK, 1),
                    unit::ARCHON => (unit::HIGH_TEMPLAR, 2),
                    unit::DARK_ARCHON => (unit::DARK_TEMPLAR, 2),
                    _ => return None
                }
            }
        };
        let existing_count = count_units(self.1, prereq, game);
        let needed = count.saturating_sub(existing_count);
        if needed == 0 {
            None
        } else {
            let new_req = match *req {
                Military(_, region) => Military(prereq, region),
                Guard(_, ai) => Guard(prereq, ai),
            };
            Some((new_req, needed))
        }
    }
}

pub fn count_units(player: u8, unit_id: UnitId, game: Game) -> u32 {
    let existing = game.unit_count(player, unit_id);
    let birth_multiplier = match unit_id.flags() & 0x400 != 0 {
        true => 2,
        false => 1,
    };
    let mut marked_templars = Vec::new();
    let mut mark_templar = |unit: Unit| {
        if !marked_templars.iter().any(|&x| x == unit) {
            if let Some(t) = unit.target() {
                marked_templars.push(t);
                1
            } else {
                0
            }
        } else {
            0
        }
    };
    let morphing: u32 = active_units().filter(|x| x.player() == player).map(|unit| {
        match unit.id() {
            unit::EGG | unit::COCOON | unit::LURKER_EGG => {
                let morph_unit = unsafe {
                    UnitId((*unit.0).build_queue[(*unit.0).current_build_slot as usize])
                };
                if morph_unit == unit_id {
                    birth_multiplier
                } else {
                    0
                }
            }
            unit::HIGH_TEMPLAR if unit_id == unit::ARCHON => {
                match unit.order() == order::ARCHON_WARP {
                    true => mark_templar(unit),
                    false => 0,
                }
            }
            unit::DARK_TEMPLAR if unit_id == unit::DARK_ARCHON => {
                match unit.order() == order::DARK_ARCHON_MELD {
                    true => mark_templar(unit),
                    false => 0,
                }
            }
            _ => 0,
        }
    }).sum();
    morphing + existing
}

struct Cost {
    minerals: u32,
    gas: u32,
    supply: u32,
}

fn unit_cost(unit: UnitId) -> Cost {
    let dual_birth = unit.flags() & 0x400 != 0;
    Cost {
        minerals: unit.mineral_cost(),
        gas: unit.gas_cost(),
        supply: unit.supply_cost() * if dual_birth { 2 } else { 1 },
    }
}

#[derive(Clone)]
enum SpendingRequest {
    Military(UnitId, *mut bw::AiRegion),
    Guard(UnitId, *mut bw::GuardAi),
}

pub fn unit_ai_region(player: u8, unit: Unit) -> Option<*mut bw::AiRegion> {
    ai_region(player, unit.position())
}

pub fn ai_region(player: u8, position: bw::Point) -> Option<*mut bw::AiRegion> {
    let regions = bw::ai_regions(player.into());
    if regions != null_mut() {
        if let Some(region) = bw::get_region(position) {
            unsafe {
                Some(regions.offset(region as isize))
            }
        } else {
            None
        }
    } else {
        None
    }
}

pub unsafe fn update_guard_needs(game: Game) {
    let seconds = (*game.0).elapsed_seconds;
    for player in 0..8 {
        let ai = PlayerAi::get(player);
        let mut guard = bw::guard_ais(player);
        while guard != null_mut() {
            let previous_update = (*guard).previous_update;
            // Not handling dual birth since pulling military is difficult right now
            let unit_id = UnitId((*guard).unit_id);
            let skip = (*guard).parent != null_mut() ||
                (unit_id == unit::ZERGLING || unit_id == unit::SCOURGE) ||
                (ai.is_campaign() && (*guard).times_died >= 3);
            if !skip {
                if seconds.saturating_sub(previous_update) > 5 {
                    (*guard).previous_update = seconds;
                    if let Some(region) = ai_region(player, (*guard).other_home) {
                        if region_can_rebuild_guards(region) {
                            if !is_guard_being_trained(player, guard) {
                                let req = SpendingRequest::Guard(unit_id, guard);
                                ai.add_spending_request(&req, 60, game);
                            }
                        }
                    }
                }
            }
            guard = (*guard).next;
        }
    }
}

fn is_guard_being_trained(player: u8, guard: *mut bw::GuardAi) -> bool {
    for unit in active_units().filter(|x| x.player() == player) {
        if let Some(ai) = unit.building_ai() {
            unsafe {
                let iter = (*ai).train_queue_types.iter_mut()
                    .zip((*ai).train_queue_values.iter_mut());
                for (ty, val) in iter {
                    if *ty == 2 && *val != null_mut() {
                        let ai = *val as *mut bw::GuardAi;
                        if ai == guard {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

unsafe fn region_can_rebuild_guards(region: *mut bw::AiRegion) -> bool {
    (*region).state != 3 &&
        (*region).air_target == null_mut() &&
        (*region).ground_target == null_mut() &&
        (*region).flags & 0x20 == 0
}