
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct OrderId(pub u8);

pub mod id {
    use super::OrderId;
    pub const DRONE_BUILD: OrderId = OrderId(0x19);
    pub const SCV_BUILD: OrderId = OrderId(0x1e);
    pub const PROBE_BUILD: OrderId = OrderId(0x1f);
    pub const PLACE_ADDON: OrderId = OrderId(0x24);
    pub const UNIT_MORPH: OrderId = OrderId(0x2a);
    pub const BUILDING_MORPH: OrderId = OrderId(0x2b);
}
