
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct OrderId(pub u8);

pub mod id {
    use super::OrderId;
    pub const DRONE_BUILD: OrderId = OrderId(0x19);
    pub const SCV_BUILD: OrderId = OrderId(0x1e);
    pub const PROBE_BUILD: OrderId = OrderId(0x1f);
    pub const PLACE_ADDON: OrderId = OrderId(0x24);
    pub const BUILD_ADDON: OrderId = OrderId(0x25);
    pub const TRAIN: OrderId = OrderId(0x26);
    pub const BUILD_NYDUS_EXIT: OrderId = OrderId(0x2e);
    pub const UNIT_MORPH: OrderId = OrderId(0x2a);
    pub const BUILDING_MORPH: OrderId = OrderId(0x2b);
    pub const TRAIN_FIGHTER: OrderId = OrderId(0x3f);
    pub const SHIELD_BATTERY: OrderId = OrderId(0x44);
    pub const SPAWNING_LARVA: OrderId = OrderId(0x4e);
    pub const SPREAD_CREEP: OrderId = OrderId(0x66);
    pub const CLOAK: OrderId = OrderId(0x6d);
    pub const DECLOAK: OrderId = OrderId(0x6e);
    pub const CLOAKING_NEARBY_UNITS: OrderId = OrderId(0x83);
    pub const HALLUCINATED: OrderId = OrderId(0x95);
}

impl OrderId {
    pub fn is_secondary(&self) -> bool {
        use self::id::*;
        match *self {
            TRAIN | CLOAKING_NEARBY_UNITS | CLOAK | DECLOAK | BUILD_ADDON | TRAIN_FIGHTER |
                SHIELD_BATTERY | SPAWNING_LARVA | SPREAD_CREEP | HALLUCINATED => true,
            _ => false,
        }
    }
}
