use std::ptr::{NonNull};

use crate::{Race, TechId, UnitId, UpgradeId};
use crate::bw;

#[derive(Copy, Clone)]
pub struct Game(NonNull<bw::Game>);

impl std::ops::Deref for Game {
    type Target = *mut bw::Game;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::mem::transmute(&self.0)
        }
    }
}

impl Game {
    pub unsafe fn from_ptr(game: *mut bw::Game) -> Game {
        Game(NonNull::new(game).unwrap())
    }

    pub fn minerals(self, player: u8) -> u32 {
        unsafe { (**self).minerals[player as usize] }
    }

    pub fn gas(self, player: u8) -> u32 {
        unsafe { (**self).gas[player as usize] }
    }

    pub fn reduce_minerals(self, player: u8, amount: u32) {
        unsafe {
            (**self).minerals[player as usize] -= amount;
        }
    }

    pub fn reduce_gas(self, player: u8, amount: u32) {
        unsafe {
            (**self).gas[player as usize] -= amount;
        }
    }

    pub fn frame_count(self) -> u32 {
        unsafe { (**self).frame_count }
    }

    pub fn unit_available(self, player: u8, unit: UnitId) -> bool {
        unsafe { (**self).unit_availability[player as usize][unit.0 as usize] != 0 }
    }

    pub fn set_unit_availability(self, player: u8, unit: UnitId, available: bool) {
        unsafe {
            (**self).unit_availability[player as usize][unit.0 as usize] = available as u8;
        }
    }

    pub fn supply_used(self, player: u8, race: Race) -> u32 {
        let index = race.id() as usize;
        unsafe {
            let supplies = &(**self).supplies[index];
            supplies.provided[player as usize]
        }
    }

    pub fn supply_provided(self, player: u8, race: Race) -> u32 {
        let index = race.id() as usize;
        unsafe {
            let supplies = &(**self).supplies[index];
            supplies.provided[player as usize]
        }
    }

    pub fn supply_max(self, player: u8, race: Race) -> u32 {
        let index = race.id() as usize;
        unsafe {
            let supplies = &(**self).supplies[index];
            supplies.max[player as usize]
        }
    }

    pub fn supply_free(self, player: u8, race: Race) -> u32 {
        let index = race.id() as usize;
        unsafe {
            let supplies = &(**self).supplies[index];
            supplies.provided[player as usize].saturating_sub(supplies.used[player as usize])
        }
    }

    pub fn upgrade_level(self, player: u8, upgrade: UpgradeId) -> u8 {
        unsafe {
            let upgrade = upgrade.0;
            assert!(player < 0xc);
            if upgrade >= 0x2e {
                (**self).upgrade_level_bw[player as usize][upgrade as usize - 0x2e]
            } else {
                (**self).upgrade_level_sc[player as usize][upgrade as usize]
            }
        }
    }

    pub fn upgrade_max_level(self, player: u8, upgrade: UpgradeId) -> u8 {
        unsafe {
            let upgrade = upgrade.0;
            assert!(player < 0xc);
            if upgrade >= 0x2e {
                (**self).upgrade_limit_bw[player as usize][upgrade as usize - 0x2e]
            } else {
                (**self).upgrade_limit_sc[player as usize][upgrade as usize]
            }
        }
    }

    pub fn set_upgrade_level(self, player: u8, upgrade: UpgradeId, level: u8) {
        unsafe {
            let upgrade = upgrade.0;
            assert!(player < 0xc);
            if upgrade >= 0x2e {
                (**self).upgrade_level_bw[player as usize][upgrade as usize - 0x2e] = level;
            } else {
                (**self).upgrade_level_sc[player as usize][upgrade as usize] = level;
            }
        }
    }

    pub fn tech_researched(self, player: u8, tech: TechId) -> bool {
        unsafe {
            let tech = tech.0;
            assert!(player < 0xc);
            if tech >= 0x18 {
                (**self).tech_level_bw[player as usize][tech as usize - 0x18] != 0
            } else {
                (**self).tech_level_sc[player as usize][tech as usize] != 0
            }
        }
    }

    pub fn set_tech_level(self, player: u8, tech: TechId, level: u8) {
        unsafe {
            let tech = tech.0;
            assert!(player < 0xc);
            if tech >= 0x18 {
                (**self).tech_level_bw[player as usize][tech as usize - 0x18] = level;
            } else {
                (**self).tech_level_sc[player as usize][tech as usize] = level;
            }
        }
    }

    pub fn tech_available(self, player: u8, tech: TechId) -> bool {
        unsafe {
            let tech = tech.0;
            assert!(player < 0xc);
            if tech >= 0x18 {
                (**self).tech_availability_bw[player as usize][tech as usize - 0x18] != 0
            } else {
                (**self).tech_availability_sc[player as usize][tech as usize] != 0
            }
        }
    }

    pub fn set_tech_availability(self, player: u8, tech: TechId, level: u8) {
        unsafe {
            let tech = tech.0;
            assert!(player < 0xc);
            if tech >= 0x18 {
                (**self).tech_availability_bw[player as usize][tech as usize - 0x18] = level;
            } else {
                (**self).tech_availability_sc[player as usize][tech as usize] = level;
            }
        }
    }

    pub fn unit_count(self, player: u8, unit: UnitId) -> u32 {
        unsafe { (**self).all_units_count[unit.0 as usize][player as usize] }
    }

    pub fn completed_count(self, player: u8, unit: UnitId) -> u32 {
        unsafe { (**self).completed_units_count[unit.0 as usize][player as usize] }
    }

    pub fn allied(self, player: u8, other: u8) -> bool {
        unsafe { (**self).alliances[player as usize][other as usize] != 0 }
    }

    pub fn map_width_tiles(self) -> u16 {
        unsafe { (**self).map_width_tiles }
    }

    pub fn map_height_tiles(self) -> u16 {
        unsafe { (**self).map_height_tiles }
    }
}
