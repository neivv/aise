use std::ptr::null_mut;

use bw_dat::{TechId, UnitId, UpgradeId};

use bw;

#[derive(Copy, Clone)]
pub struct Game(pub *mut bw::Game);

impl Game {
    pub fn get() -> Game {
        let game = bw::game();
        assert!(game != null_mut());
        Game(game)
    }

    pub fn minerals(self, player: u8) -> u32 {
        unsafe { (*self.0).minerals[player as usize] }
    }

    pub fn gas(self, player: u8) -> u32 {
        unsafe { (*self.0).gas[player as usize] }
    }

    pub fn frame_count(self) -> u32 {
        unsafe { (*self.0).frame_count }
    }

    pub fn upgrade_level(self, player: u8, upgrade: UpgradeId) -> u8 {
        unsafe {
            let upgrade = upgrade.0;
            assert!(player < 0xc);
            if upgrade >= 0x2e {
                (*self.0).upgrade_level_bw[player as usize][upgrade as usize - 0x2e]
            } else {
                (*self.0).upgrade_level_sc[player as usize][upgrade as usize]
            }
        }
    }

    pub fn set_upgrade_level(self, player: u8, upgrade: UpgradeId, level: u8) {
        unsafe {
            let upgrade = upgrade.0;
            assert!(player < 0xc);
            if upgrade >= 0x2e {
                (*self.0).upgrade_level_bw[player as usize][upgrade as usize - 0x2e] = level;
            } else {
                (*self.0).upgrade_level_sc[player as usize][upgrade as usize] = level;
            }
        }
    }

    pub fn tech_researched(self, player: u8, tech: TechId) -> bool {
        unsafe {
            let tech = tech.0;
            assert!(player < 0xc);
            if tech >= 0x18 {
                (*self.0).tech_level_bw[player as usize][tech as usize - 0x18] != 0
            } else {
                (*self.0).tech_level_sc[player as usize][tech as usize] != 0
            }
        }
    }

    pub fn unit_count(self, player: u8, unit: UnitId) -> u32 {
        unsafe { (*self.0).all_units_count[unit.0 as usize][player as usize] }
    }

    pub fn completed_count(self, player: u8, unit: UnitId) -> u32 {
        unsafe { (*self.0).completed_units_count[unit.0 as usize][player as usize] }
    }

    pub fn allied(self, player: u8, other: u8) -> bool {
        unsafe { (*self.0).alliances[player as usize][other as usize] != 0 }
    }
}
