use smallvec::SmallVec;

use bw_dat::{self, TechId, UnitId};

impl DatReq {
    unsafe fn read(pos: &mut *const u16) -> DatReq {
        fn opt_unit_id(id: u16) -> UnitId {
            match UnitId::optional(id as u32) {
                Some(s) => s,
                None => {
                    warn!("Unknown unit {:x}", id);
                    return bw_dat::unit::NONE;
                }
            }
        }

        fn opt_tech_id(id: u16) -> TechId {
            match TechId::optional(id as u32) {
                Some(s) => s,
                None => {
                    warn!("Unknown tech {:x}", id);
                    return bw_dat::tech::NONE;
                }
            }
        }

        use self::DatReq::*;
        let val = **pos;
        *pos = pos.add(1);
        match val {
            0xff02 => {
                let id = **pos;
                *pos = pos.add(1);
                CurrentUnitIs(opt_unit_id(id))
            }
            0xff03 => {
                let id = **pos;
                *pos = pos.add(1);
                HasUnit(opt_unit_id(id))
            }
            0xff04 => {
                let id = **pos;
                *pos = pos.add(1);
                HasAddonAttached(opt_unit_id(id))
            }
            0xff05 => IsNotLifted,
            0xff06 => IsLifted,
            0xff07 => IsNotBusy,
            0xff08 => IsNotConstructingAddon,
            0xff09 => IsNotTeching,
            0xff0a => IsNotUpgrading,
            0xff0b => IsNotConstructingBuilding,
            0xff0c => HasNoAddon,
            0xff0d => HasNoNydusExit,
            0xff0e => HasHangarSpace,
            0xff0f => {
                let id = **pos;
                *pos = pos.add(1);
                TechResearched(opt_tech_id(id))
            }
            0xff10 => HasNotNukeOnly,
            0xff11 => NotBurrowedOnly,
            0xff12 => NotLandedBuildingOnly,
            0xff13 => LandedBuildingOnly,
            0xff14 => CanMoveOnly,
            0xff15 => CanAttackOnly,
            0xff16 => WorkerOnly,
            0xff17 => FlyingBuildingOnly,
            0xff18 => IsTransport,
            0xff19 => PowerupOnly,
            0xff1a => SubunitOnly,
            0xff1b => HasSpiderMinesOnly,
            0xff1d => CanHoldPositionOnly,
            0xff1e => AllowOnHallucinations,
            0xff22 => Disabled,
            0xff23 => Blank,
            0xff24 => BwOnly,
            0xff25 => {
                let id = **pos;
                *pos = pos.add(1);
                TechOnly(opt_tech_id(id))
            }
            0xff26 => BurrowedOnly,
            x => {
                if x < 0xff00 {
                    Unit(opt_unit_id(val))
                } else {
                    Unknown(x)
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DatReq {
    CurrentUnitIs(UnitId),
    HasUnit(UnitId), // Even incomplete
    HasAddonAttached(UnitId),
    IsNotLifted,
    IsLifted,
    IsNotBusy,
    IsNotConstructingAddon,
    IsNotTeching,
    IsNotUpgrading,
    IsNotConstructingBuilding,
    HasNoAddon,
    HasNoNydusExit,
    HasHangarSpace,
    TechResearched(TechId),
    HasNotNukeOnly,
    NotBurrowedOnly,
    NotLandedBuildingOnly,
    LandedBuildingOnly,
    CanMoveOnly,
    CanAttackOnly,
    WorkerOnly,
    FlyingBuildingOnly,
    IsTransport,
    PowerupOnly,
    SubunitOnly,
    HasSpiderMinesOnly,
    CanHoldPositionOnly,
    AllowOnHallucinations,
    Disabled,
    Blank,
    BwOnly,
    TechOnly(TechId),
    BurrowedOnly,
    Unit(UnitId),
    Unknown(u16),
}

pub struct ReadDatReqs {
    pos: *const u16,
    upgrade_level: u8,
}

impl ReadDatReqs {
    pub unsafe fn new(pos: *const u16, upgrade_level: u8) -> ReadDatReqs {
        ReadDatReqs {
            pos,
            upgrade_level,
        }
    }

    // Returns multiple if they are chained with or,
    // nothing only if at end
    pub fn next_dat_requirements(&mut self, out: &mut SmallVec<[DatReq; 4]>) {
        out.clear();
        unsafe {
            loop {
                if *self.pos == 0xffff {
                    // End
                    return;
                }
                if *self.pos == 0xff1f {
                    let end_op = match self.upgrade_level {
                        0 => 0xff1f,
                        1 => 0xff20,
                        _ => 0xff21,
                    };
                    while *self.pos != end_op {
                        self.pos = self.pos.add(1);
                    }
                    self.pos = self.pos.add(1);
                } else {
                    let req = DatReq::read(&mut self.pos);
                    out.push(req);
                    if *self.pos != 0xff01 {
                        // Not or, can return
                        return;
                    } else {
                        self.pos = self.pos.add(1);
                    }
                }
            }
        }
    }
}

#[test]
#[cfg_attr(rustfmt, rustfmt_skip)]
fn read_reqs() {
    fn read(reqs: &[u16], level: u8) -> Vec<Vec<DatReq>> {
        let mut read = unsafe { ReadDatReqs::new(reqs.as_ptr(), level) };
        let mut result = Vec::new();
        loop {
            let mut buf = SmallVec::new();
            read.next_dat_requirements(&mut buf);
            if buf.len() == 0 {
                return result;
            }
            result.push(buf.into_vec());
        }
    }

    use self::DatReq::*;
    let reqs = &[
        0xff05u16,
        0xff02, 0x25,
        0xff16,
        0xff0f, 0x10,
        0xff1f,
        0xff12,
        0x0022,
        0xffff,
        0xff20,
        0xff03, 0x21,
        0xff01,
        0xff03, 0x22,
        0xffff,
        0xff21,
        0xffff,
    ];
    assert_eq!(
        read(reqs, 0),
        vec![
            vec![IsNotLifted],
            vec![CurrentUnitIs(UnitId(0x25))],
            vec![WorkerOnly],
            vec![TechResearched(TechId(0x10))],
            vec![NotLandedBuildingOnly],
            vec![Unit(UnitId(0x22))],
        ]
    );
    assert_eq!(
        read(reqs, 1),
        vec![
            vec![IsNotLifted],
            vec![CurrentUnitIs(UnitId(0x25))],
            vec![WorkerOnly],
            vec![TechResearched(TechId(0x10))],
            vec![HasUnit(UnitId(0x21)), HasUnit(UnitId(0x22))],
        ]
    );
    assert_eq!(
        read(reqs, 2),
        vec![
            vec![IsNotLifted],
            vec![CurrentUnitIs(UnitId(0x25))],
            vec![WorkerOnly],
            vec![TechResearched(TechId(0x10))],
        ]
    );
}
