macro_rules! decl_req {
    ($name:ident; $($variant:ident = $val:expr,)*) => {
        pub enum $name {
            $($variant,)*
            End,
            Unknown(u16),
            Unit(u16),
        }

        impl $name {
            pub fn from_raw(raw: u16) -> $name {
                if raw < 0xff00 {
                    $name::Unit(raw)
                } else {
                    match raw {
                        $($val => $name::$variant,)*
                        0xffff => $name::End,
                        x => $name::Unknown(x),
                    }
                }
            }
        }
    }
}

decl_req! {
    UnitReq;

    CurrentUnitIs = 0xff02,
    HasUnit = 0xff03, // Even incomplete
    HasAddonAttached = 0xff04,
    IsNotLifted = 0xff05,
    IsNotBusy = 0xff07,
    IsNotConstructingAddon = 0xff08,
    IsNotTeching = 0xff09,
    IsNotUpgrading = 0xff0a,
    IsNotConstructingBuilding = 0xff0b,
    HasNoAddon = 0xff0c,
    HasHangarSpace = 0xff0e,
    HasNotNukeOnly = 0xff10,
    NotBurrowedOnly = 0xff11,
    Disabled = 0xff22,
    Blank = 0xff23,
    BwOnly = 0xff24,
    TechOnly = 0xff25,
    BurrowedOnly = 0xff26,
}

decl_req! {
    NonUnitReq;

    CurrentUnitIs = 0xff02,
    HasUnit = 0xff03, // Even incomplete
    IsNotLifted = 0xff05,
    IsLifted = 0xff06,
    IsNotBusy = 0xff07,
    IsNotConstructingAddon = 0xff08,
    IsNotTeching = 0xff09,
    IsNotUpgrading = 0xff0a,
    HasNoNydusExit = 0xff0d,
    TechResearched = 0xff0f,
    NotBurrowedOnly = 0xff11,
    NotLandedBuildingOnly = 0xff12,
    LandedBuildingOnly = 0xff13,
    CanMoveOnly = 0xff14,
    CanAttackOnly = 0xff15,
    WorkerOnly = 0xff16,
    FlyingBuildingOnly = 0xff17,
    IsTransport = 0xff18,
    PowerupOnly = 0xff19,
    SubunitOnly = 0xff1a,
    HasSpiderMinesOnly = 0xff1b,
    CanHoldPositionOnly = 0xff1d,
    AllowOnHallucinations = 0xff1e,
    UpgradeLevelJump = 0xff1f,
    Disabled = 0xff22,
    Blank = 0xff23,
    BwOnly = 0xff24,
    BurrowedOnly = 0xff26,
}
