use std::ptr::{NonNull};

use crate::{Race, TechId, UnitId, UpgradeId, extended_array};
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
            let upgrade = upgrade.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(0) {
                arr.read_u8(upgrade * 12 + player as usize)
            } else {
                if upgrade >= 0x2e {
                    (**self).upgrade_level_bw[player as usize][upgrade - 0x2e]
                } else {
                    (**self).upgrade_level_sc[player as usize][upgrade]
                }
            }
        }
    }

    pub fn upgrade_max_level(self, player: u8, upgrade: UpgradeId) -> u8 {
        unsafe {
            let upgrade = upgrade.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(1) {
                arr.read_u8(upgrade * 12 + player as usize)
            } else {
                if upgrade >= 0x2e {
                    (**self).upgrade_limit_bw[player as usize][upgrade - 0x2e]
                } else {
                    (**self).upgrade_limit_sc[player as usize][upgrade]
                }
            }
        }
    }

    pub fn set_upgrade_level(self, player: u8, upgrade: UpgradeId, level: u8) {
        unsafe {
            let upgrade = upgrade.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(0) {
                arr.write_u8(upgrade * 12 + player as usize, level);
            } else {
                if upgrade >= 0x2e {
                    (**self).upgrade_level_bw[player as usize][upgrade - 0x2e] = level;
                } else {
                    (**self).upgrade_level_sc[player as usize][upgrade] = level;
                }
            }
        }
    }

    pub fn tech_researched(self, player: u8, tech: TechId) -> bool {
        unsafe {
            let tech = tech.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(2) {
                arr.read_u8(tech * 12 + player as usize) != 0
            } else {
                if tech >= 0x18 {
                    (**self).tech_level_bw[player as usize][tech - 0x18] != 0
                } else {
                    (**self).tech_level_sc[player as usize][tech] != 0
                }
            }
        }
    }

    pub fn set_tech_level(self, player: u8, tech: TechId, level: u8) {
        unsafe {
            assert!(player < 0xc);
            let tech = tech.0 as usize;
            if let Some(arr) = extended_array(2) {
                arr.write_u8(tech * 12 + player as usize, level);
            } else {
                if tech >= 0x18 {
                    (**self).tech_level_bw[player as usize][tech - 0x18] = level;
                } else {
                    (**self).tech_level_sc[player as usize][tech] = level;
                }
            }
        }
    }

    pub fn tech_available(self, player: u8, tech: TechId) -> bool {
        unsafe {
            let tech = tech.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(3) {
                arr.read_u8(tech * 12 + player as usize) != 0
            } else {
                if tech >= 0x18 {
                    (**self).tech_availability_bw[player as usize][tech - 0x18] != 0
                } else {
                    (**self).tech_availability_sc[player as usize][tech] != 0
                }
            }
        }
    }

    pub fn set_tech_availability(self, player: u8, tech: TechId, level: u8) {
        unsafe {
            let tech = tech.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(3) {
                arr.write_u8(tech * 12 + player as usize, level);
            } else {
                if tech >= 0x18 {
                    (**self).tech_availability_bw[player as usize][tech - 0x18] = level;
                } else {
                    (**self).tech_availability_sc[player as usize][tech] = level;
                }
            }
        }
    }

    pub fn unit_count(self, player: u8, unit: UnitId) -> u32 {
        unsafe {
            let unit = unit.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(7) {
                arr.read_u32(unit * 12 + player as usize)
            } else {
                (**self).all_units_count[unit][player as usize]
            }
        }
    }

    pub fn completed_count(self, player: u8, unit: UnitId) -> u32 {
        unsafe {
            let unit = unit.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(8) {
                arr.read_u32(unit * 12 + player as usize)
            } else {
                (**self).completed_units_count[unit][player as usize]
            }
        }
    }

    pub fn set_completed_count(self, player: u8, unit: UnitId, value: u32) {
        unsafe {
            let unit = unit.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(8) {
                arr.write_u32(unit * 12 + player as usize, value)
            } else {
                (**self).completed_units_count[unit][player as usize] = value;
            }
        }
    }

    pub fn unit_kills(self, player: u8, unit: UnitId) -> u32 {
        unsafe {
            let unit = unit.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(9) {
                arr.read_u32(unit * 12 + player as usize)
            } else {
                (**self).unit_kills[unit][player as usize]
            }
        }
    }

    pub fn unit_deaths(self, player: u8, unit: UnitId) -> u32 {
        unsafe {
            let unit = unit.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(0xa) {
                arr.read_u32(unit * 12 + player as usize)
            } else {
                (**self).deaths[unit][player as usize]
            }
        }
    }

    pub fn set_unit_deaths(self, player: u8, unit: UnitId, value: u32) {
        unsafe {
            let unit = unit.0 as usize;
            assert!(player < 0xc);
            if let Some(arr) = extended_array(0xa) {
                arr.write_u32(unit * 12 + player as usize, value);
            } else {
                (**self).deaths[unit][player as usize] = value;
            }
        }
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
