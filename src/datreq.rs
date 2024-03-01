use smallvec::SmallVec;

use bw_dat::{unit, Game, TechId, Unit, UnitId};

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
            0xff40 => {
                let id = **pos;
                *pos = pos.add(1);
                let count = **pos;
                *pos = pos.add(1);
                UnitCountLessThan(opt_unit_id(id), count)
            }
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
    UnitCountLessThan(UnitId, u16),
    Unit(UnitId),
    Unknown(u16),
}

pub struct ReadDatReqs {
    pos: *const u16,
    current_upgrade_level: u8,
}

impl ReadDatReqs {
    pub unsafe fn new(pos: *const u16, current_upgrade_level: u8) -> ReadDatReqs {
        ReadDatReqs {
            pos,
            current_upgrade_level,
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
                    let end_op = match self.current_upgrade_level {
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

pub unsafe fn check_dat_requirements(
    game: Game,
    reqs: *const u16,
    unit: Unit,
    current_upgrade_level: u8,
) -> bool {
    if unit.is_disabled() || !unit.is_completed() {
        return false;
    }
    let mut dat_reqs: SmallVec<_> = SmallVec::new();
    let mut read = ReadDatReqs::new(reqs, current_upgrade_level);
    let mut hallucinations_allowed = false;
    let player = unit.player();
    loop {
        read.next_dat_requirements(&mut dat_reqs);
        if dat_reqs.is_empty() {
            // End
            if !hallucinations_allowed && unit.is_hallucination() {
                return false;
            }
            return true;
        }
        let pass = dat_reqs.iter().any(|req| {
            match *req {
                DatReq::Disabled => false,
                DatReq::Blank => false,
                DatReq::BwOnly => true, // w/e
                DatReq::IsTransport => unit.is_transport(game),
                DatReq::IsNotBusy => {
                    unit.first_queued_unit().is_none() &&
                        !unit.is_building_addon() &&
                        unit.upgrade_in_progress().is_none() &&
                        unit.tech_in_progress().is_none()
                }
                DatReq::IsNotConstructingAddon => !unit.is_building_addon(),
                DatReq::IsNotConstructingBuilding => !unit.is_constructing_building(),
                DatReq::IsNotTeching => unit.tech_in_progress().is_none(),
                DatReq::IsNotUpgrading => unit.upgrade_in_progress().is_none(),
                DatReq::IsLifted => unit.id().is_building() && !unit.is_landed_building(),
                DatReq::IsNotLifted => unit.is_landed_building(),
                DatReq::HasNoNydusExit => unit.nydus_linked().is_none(),
                DatReq::NotBurrowedOnly => !unit.is_burrowed(),
                DatReq::BurrowedOnly => unit.is_burrowed(),
                DatReq::NotLandedBuildingOnly => !unit.is_landed_building(),
                DatReq::LandedBuildingOnly => unit.is_landed_building(),
                DatReq::CanMoveOnly => {
                    let rclick_action = unit.id().rclick_action();
                    !unit.is_landed_building() &&
                        rclick_action != 0 &&
                        rclick_action != 3 &&
                        (unit.id() != unit::LURKER || !unit.is_burrowed())
                }
                DatReq::CanAttackOnly => {
                    fn can_attack(unit: Unit) -> bool {
                        if unit.id().ground_weapon().is_some() {
                            if unit.id() != unit::LURKER || unit.is_burrowed() {
                                return true;
                            }
                        }
                        if unit.id().air_weapon().is_some() {
                            return true;
                        }
                        match unit.id() {
                            unit::REAVER | unit::WARBRINGER | unit::CARRIER | unit::GANTRITHOR => {
                                return unit.fighter_amount() > 0;
                            }
                            _ => (),
                        }
                        if let Some(subunit) = unit.subunit_linked() {
                            if subunit.id().ground_weapon().is_some() {
                                return true;
                            }
                            if subunit.id().air_weapon().is_some() {
                                return true;
                            }
                        }
                        false
                    }
                    can_attack(unit)
                }
                DatReq::SubunitOnly => unit.id().flags() & 0x10 != 0,
                DatReq::WorkerOnly => unit.id().is_worker(),
                DatReq::FlyingBuildingOnly => unit.id().flags() & 0x20 != 0,
                DatReq::PowerupOnly => unit.id().flags() & 0x800 != 0,
                DatReq::HasSpiderMinesOnly => unit.mine_amount(game) > 0,
                DatReq::CanHoldPositionOnly => true, // The check is dumb anyway
                DatReq::AllowOnHallucinations => {
                    hallucinations_allowed = true;
                    true
                }
                DatReq::TechOnly(tech) => game.tech_researched(player, tech),
                DatReq::TechResearched(tech) => game.tech_researched(player, tech),
                DatReq::HasUnit(unit) => game.unit_count(player, unit) != 0,
                DatReq::Unit(unit) => game.completed_count(player, unit) != 0,
                DatReq::UnitCountLessThan(unit, limit) => {
                    game.unit_count(player, unit) < limit.into()
                }
                DatReq::Unknown(id) => {
                    warn!("Unknown req ty {:x}", id);
                    true
                }
                DatReq::CurrentUnitIs(unit_id) => unit.matches_id(unit_id),
                DatReq::HasHangarSpace => {
                    let amount = unit.fighter_amount()
                        .saturating_add(
                            (0..5).filter(|&i| unit.nth_queued_unit(i).is_some()).count() as u32
                        );
                    amount < unit.hangar_cap(game)
                }
                DatReq::HasNotNukeOnly => !unit.has_nuke(),
                DatReq::HasNoAddon => unit.addon().is_none(),
                DatReq::HasAddonAttached(id) => {
                    unit.addon().map(|x| x.matches_id(id)).unwrap_or(false)
                }
            }
        });
        if !pass {
            return false;
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
