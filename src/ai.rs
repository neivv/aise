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

    pub fn spending_requests(&self) -> impl Iterator<Item = &bw::AiSpendingRequest> {
        unsafe {
            (&(*self.0).requests[..(*self.0).request_count as usize]).iter()
        }
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
            let regions = bw::ai_regions(self.1 as u32);
            if let Some(region) = unit_ai_region(unit, regions) {
                let priority = self.train_request_priority(game);
                self.add_train_request(unit_id, region, priority, game);
            }
        }
    }

    pub fn is_campaign(&self) -> bool {
        self.flags() & 0x20 != 0
    }

    fn flags(&self) -> u16 {
        unsafe {
            (*self.0).flags
        }
    }

    pub fn add_military_request(
        &self,
        unit: UnitId,
        region: *mut bw::AiRegion,
        priority: u8,
        game: Game,
    ) {
        self.add_spending_request(&SpendingRequest::Military(unit, region), priority, game);
    }

    /// Idle training
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
        self.add_military_request(unit, region, priority, game);
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
    let existing = {
        let extra = match unit_id {
            unit::SIEGE_TANK_TANK => game.unit_count(player, unit::SIEGE_TANK_SIEGE),
            unit::SIEGE_TANK_SIEGE => game.unit_count(player, unit::SIEGE_TANK_TANK),
            unit::EDMUND_DUKE_TANK => game.unit_count(player, unit::EDMUND_DUKE_SIEGE),
            unit::EDMUND_DUKE_SIEGE => game.unit_count(player, unit::EDMUND_DUKE_TANK),
            _ => 0,
        };
        game.unit_count(player, unit_id) + extra
    };
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

pub fn unit_ai_region(
    unit: Unit,
    regions: *mut bw::AiRegion,
) -> Option<*mut bw::AiRegion> {
    ai_region(unit.position(), regions)
}

pub fn ai_region(
    position: bw::Point,
    regions: *mut bw::AiRegion,
) -> Option<*mut bw::AiRegion> {
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
        let regions = bw::ai_regions(player as u32);
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
                    if let Some(region) = ai_region((*guard).other_home, regions) {
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

trait LinkedListObject {
    unsafe fn next(this: *mut Self) -> *mut *mut Self;
    unsafe fn prev(this: *mut Self) -> *mut *mut Self;
}

unsafe fn linked_list_move<T: LinkedListObject>(
    this: *mut T,
    old_list: *mut *mut T,
    new_list: *mut *mut T,
) {
    let next = T::next(this);
    let prev = T::prev(this);
    if *next != null_mut() {
        let next_prev = T::prev(*next);
        assert!(*next_prev == this);
        *next_prev = *prev;
    }
    if *prev != null_mut() {
        let prev_next = T::next(*prev);
        assert!(*prev_next == this);
        *prev_next = *next;
    } else {
        assert!(*old_list == this);
        *old_list = *next;
    }
    *prev = null_mut();
    if *new_list != null_mut() {
        let old_head_prev = T::prev(*new_list);
        *old_head_prev = this;
    }
    *next = *new_list;
    *new_list = this;
}

impl LinkedListObject for bw::GuardAi {
    unsafe fn next(this: *mut Self) -> *mut *mut bw::GuardAi {
        &mut (*this).next
    }

    unsafe fn prev(this: *mut Self) -> *mut *mut bw::GuardAi {
        &mut (*this).prev
    }
}

impl LinkedListObject for bw::MilitaryAi {
    unsafe fn next(this: *mut Self) -> *mut *mut bw::MilitaryAi {
        &mut (*this).next
    }

    unsafe fn prev(this: *mut Self) -> *mut *mut bw::MilitaryAi {
        &mut (*this).prev
    }
}

pub unsafe fn move_military(
    unit: Unit,
    region: *mut bw::AiRegion,
    player_ai: &PlayerAi,
    guards: *mut bw::GuardAiList,
    game: Game,
) {
    let mut was_detector = false;
    if let Some(guard) = unit.guard_ai() {
        linked_list_move(guard, &mut (*guards).first, &mut (*(*guards).free).first_free);
        (*unit.0).ai = null_mut();
    } else if let Some(military) = unit.military_ai() {
        let old_region = (*military).region;
        linked_list_move(
            military,
            &mut (*old_region).first_military,
            &mut (*(*old_region).free_ais).first_free
        );
        (*unit.0).ai = null_mut();
        if unit.0 == (*old_region).slowest_military {
            (*old_region).slowest_military = null_mut();
        }
        if unit.0 == (*old_region).detector {
            (*old_region).detector = null_mut();
            was_detector = true;
        }
        if !unit.is_air() {
            (*old_region).ground_unit_count = (*old_region).ground_unit_count.saturating_sub(1);
        }
    } else {
        // w/e
        warn!("Couldn't move unit as military since it wasn't military or guard");
        return;
    }
    let first_free = &mut (*(*region).free_ais).first_free;
    if *first_free != null_mut() {
        let ai = *first_free;
        linked_list_move(ai, first_free, &mut (*region).first_military);
        (*ai).region = region;
        (*ai).parent = unit.0;
        (*ai).ai_type = 4; // Yea this has to be done
        (*unit.0).ai = ai as *mut c_void;
        if (*region).detector == null_mut() && was_detector {
            (*region).detector = unit.0;
        }
        if !unit.is_air() {
            (*region).ground_unit_count = (*region).ground_unit_count.saturating_add(1);
        }
        let move_order = match unit.id() {
            unit::MEDIC => order::HEAL_MOVE,
            _ => order::AI_ATTACK_MOVE,
        };
        let target_pathing_region = bw::region((*region).id);
        let region_unit_count = region_military(region).count();
        let region_center = bw::Point {
            x: ((*target_pathing_region).x >> 8) as i16,
            y: ((*target_pathing_region).y >> 8) as i16,
        };
        let target_pos = offset_pos(&region_center, region_unit_count as u32, game);
        let target_pos = {
            if let Some(offset_pathing_region) = bw::pathing_region(target_pos) {
                if (*target_pathing_region).group == (*offset_pathing_region).group {
                    target_pos
                } else {
                    region_center
                }
            } else {
                region_center
            }
        };
        bw::issue_order(unit.0, move_order, target_pos, null_mut(), unit::NONE);
        if player_ai.is_campaign() {
            unsafe fn slow_key(u: Unit) -> (u8, u32) {
                (if u.is_air() { 1 } else { 0 }, (*u.0).flingy_top_speed)
            }
            let slowest = region_military(region)
                .filter_map(|x| Unit::from_ptr((*x).parent))
                .map(|u| (u, slow_key(u)))
                .min_by_key(|x| x.1);
            if let Some((slowest, amount)) = slowest {
                if let Some(unit) = Unit::from_ptr((*region).slowest_military) {
                    if slow_key(unit) > amount {
                        (*region).slowest_military = slowest.0;
                    }
                } else {
                    (*region).slowest_military = slowest.0;
                }
            } else {
                (*region).slowest_military = null_mut();
            }
        }
    } else {
        warn!("Couldn't allocate military ai");
    }
}

fn spread_offset(index: u32) -> Option<bw::Point> {
    let odd_sqrts = [1u32, 9, 25, 49, 81, 121, 169, 225];
    let distance = odd_sqrts.iter().position(|&x| x > index)?;
    let layer_index_count =
        odd_sqrts.get(distance)? - odd_sqrts.get(distance.checked_sub(1)?)?;
    let pos_in_layer = index - odd_sqrts.get(distance.checked_sub(1)?)?;
    let other_distance = (pos_in_layer % (layer_index_count / 4)) as i16 -
        (layer_index_count / 8) as i16;
    let ret = match pos_in_layer / (layer_index_count / 4) {
        0 => bw::Point {
            x: other_distance * 16,
            y: distance as i16 * 16,
        },
        1 => bw::Point {
            x: distance as i16 * 16,
            y: other_distance * 16,
        },
        2 => bw::Point {
            x: 0 - other_distance * 16,
            y: 0i16 - distance as i16 * 16,
        },
        3 | _ => bw::Point {
            x: 0 - distance as i16 * 16,
            y: 0 - other_distance * 16,
        },
    };
    Some(ret)
}

fn offset_pos(pos: &bw::Point, index: u32, game: Game) -> bw::Point {
    fn inner(pos: &bw::Point, index: u32, game: Game) -> Option<bw::Point> {
        let ret = spread_offset(index)?;
        let map_width_pixels = unsafe { (*game.0).map_width_tiles } * 0x20;
        let map_height_pixels = unsafe { (*game.0).map_height_tiles } * 0x20;
        Some(bw::Point {
            x: pos.x.wrapping_add(ret.x).max(0).min(map_width_pixels as i16),
            y: pos.y.wrapping_add(ret.y).max(0).min(map_height_pixels as i16),
        })
    }
    inner(pos, index, game).unwrap_or_else(|| pos.clone())
}

#[test]
fn test_offset_pos() {
    if let Some(pos) = spread_offset(0) {
        assert_eq!(pos, bw::Point { x: 0, y: 0 });
    }
    assert_eq!(spread_offset(1).unwrap().y, 16);
    assert_eq!(spread_offset(2).unwrap().y, 16);
    assert_eq!(spread_offset(2).unwrap().x, 0);
    assert_eq!(spread_offset(5).unwrap().y, -16);
    assert_eq!(spread_offset(6).unwrap().y, -16);
    assert_eq!(spread_offset(6).unwrap().x, 0);
    // TODO actually check that the corners go right, sigh
}


pub fn region_military(region: *mut bw::AiRegion) -> RegionMilitary {
    unsafe {
        RegionMilitary((*region).first_military)
    }
}

pub struct RegionMilitary(*mut bw::MilitaryAi);

impl Iterator for RegionMilitary {
    type Item = *mut bw::MilitaryAi;
    fn next(&mut self) -> Option<*mut bw::MilitaryAi> {
        let next = match self.0 == null_mut() {
            true => None,
            false => Some(self.0),
        };
        if let Some(next) = next {
            unsafe { self.0 = (*next).next };
        }
        next
    }
}
