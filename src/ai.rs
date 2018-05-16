use libc::c_void;

use bw_dat::{unit, UnitId};

use bw;
use game::Game;
use unit::Unit;

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
            x => game.unit_count(self.1, unit) >= x.into(),
        }
    }

    fn add_spending_request(&self, req: &SpendingRequest, priority: u8, game: Game) {
        let limit_reached = match req {
            SpendingRequest::Military(unit, _) => self.is_at_limit(*unit, game),
        };
        if !limit_reached {
            self.add_spending_request_inner(req, priority, game);
        }
    }

    fn add_spending_request_inner(&self, req: &SpendingRequest, priority: u8, game: Game) {
        let cost = match req {
            SpendingRequest::Military(unit, _) => unit_cost(*unit),
        };
        unsafe {
            (*self.0).mineral_need = (*self.0).mineral_need.saturating_add(cost.minerals);
            (*self.0).gas_need = (*self.0).gas_need.saturating_add(cost.gas);
            (*self.0).supply_need = (*self.0).supply_need.saturating_add(cost.supply);
        }
        let (ty, id, val) = match *req {
            SpendingRequest::Military(unit, region) => (1, unit.0, region as *mut c_void),
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
        match *req {
            SpendingRequest::Military(unit, region) => {
                let (prereq, count) = match unit {
                    unit::GUARDIAN | unit::DEVOURER => (unit::MUTALISK, 1u32),
                    unit::LURKER => (unit::HYDRALISK, 1),
                    unit::ARCHON => (unit::HIGH_TEMPLAR, 2),
                    unit::DARK_ARCHON => (unit::DARK_TEMPLAR, 2),
                    _ => return None
                };
                let existing_count = game.unit_count(self.1, prereq);
                let needed = count.saturating_sub(existing_count);
                if needed == 0 {
                    None
                } else {
                    Some((SpendingRequest::Military(prereq, region), needed))
                }
            }
        }
    }
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
}

pub fn unit_ai_region(player: u8, unit: Unit) -> Option<*mut bw::AiRegion> {
    let regions = bw::ai_regions(player.into());
    if let Some(region) = bw::get_region(unit.position()) {
        unsafe {
            Some(regions.offset(region as isize))
        }
    } else {
        None
    }
}
