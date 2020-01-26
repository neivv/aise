use libc::c_void;

use bw_dat::structs::*;

#[repr(C, packed)]
#[derive(Debug, Clone, Copy)]
pub struct AiScript {
    pub next: *mut AiScript,
    pub prev: *mut AiScript,
    pub pos: u32,
    pub wait: u32,
    pub player: u32,
    pub area: Rect32,
    pub center: Point32,
    pub town: *mut AiTown,
    pub flags: u32,
}

unsafe impl Send for AiScript {}

#[repr(C)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct TownReq {
    pub flags_and_count: u8,
    pub priority: u8,
    pub id: u16,
}

#[repr(C, packed)]
pub struct AiTown {
    pub next: *mut AiTown,                    // 0x0
    pub prev: *mut AiTown,                    // 0x4
    pub free_workers: *mut WorkerAiArray,     // 0x8
    pub workers: *mut WorkerAi,               // 0xc
    pub free_buildings: *mut BuildingAiArray, // 0x10
    pub buildings: *mut BuildingAi,           // 0x14
    pub player: u8,                           // 0x18
    pub inited: u8,                           // 0x19
    pub worker_count: u8,
    pub worker_limit: u8,
    pub resource_area: u8,
    pub resource_units_not_set: u8,
    pub in_battle: u8,
    pub unk1f: u8,
    pub position: Point,
    pub main_building: *mut Unit,
    pub building_scv: *mut Unit,
    pub mineral: *mut Unit,
    pub gas_buildings: [*mut Unit; 0x3],
    pub town_units: [TownReq; 0x64],
}

#[repr(C)]
pub struct AiTownList {
    pub array: *mut AiTownArray,
    pub first: *mut AiTown,
}

#[repr(C, packed)]
pub struct AiTownArray {
    pub towns: [AiTown; 100],
    pub first_free: *mut AiTown,
}

#[repr(C, packed)]
pub struct WorkerAi {
    pub next: *mut WorkerAi,
    pub prev: *mut WorkerAi,
    pub ai_type: u8,
    pub target_resource: u8,
    pub reassign_count: u8,
    pub wait_timer: u8,
    pub last_update_second: u32,
    pub parent: *mut Unit,
    pub town: *mut AiTown,
}

#[repr(C, packed)]
pub struct BuildingAi {
    pub next: *mut BuildingAi,
    pub prev: *mut BuildingAi,
    pub ai_type: u8,
    pub train_queue_types: [u8; 0x5],
    pub padding_e: [u8; 0x2],
    pub parent: *mut Unit, // 0x10
    pub town: *mut AiTown,
    pub train_queue_values: [*mut c_void; 0x5],
}

#[repr(C, packed)]
pub struct WorkerAiArray {
    pub ais: [WorkerAi; 1000],
    pub first_free: *mut WorkerAi,
}

#[repr(C, packed)]
pub struct BuildingAiArray {
    pub ais: [BuildingAi; 1000],
    pub first_free: *mut BuildingAi,
}

#[repr(C, packed)]
pub struct GuardAiList {
    pub array: *mut GuardAiArray,
    pub first: *mut GuardAi,
}

#[repr(C, packed)]
pub struct GuardAiArray {
    pub ais: [GuardAi; 1000],
    pub first_free: *mut GuardAi,
}

#[repr(C, packed)]
pub struct GuardAi {
    pub next: *mut GuardAi,
    pub prev: *mut GuardAi,
    pub ai_type: u8,
    pub times_died: u8,
    pub dca: [u8; 0x2],
    pub parent: *mut Unit,
    pub unit_id: u16,
    pub home: Point,
    pub other_home: Point,
    pub padding1a: [u8; 0x2],
    pub previous_update: u32,
}

#[repr(C, packed)]
pub struct AiRegion {
    pub id: u16,
    pub target_region_id: u16,
    pub player: u8,
    pub state: u8,
    pub unk_val: u8,
    pub unk_count: u8,
    pub flags: u8,
    pub unk: u8,
    pub ground_unit_count: u16,
    pub needed_ground_strength: u16,
    pub needed_air_strength: u16,
    pub local_military_ground_strength: u16,
    pub local_military_air_strength: u16,
    pub all_military_ground_strength: u16,
    pub all_military_air_strength: u16,
    // Are these ordered correctly?
    pub enemy_air_strength: u16,
    pub enemy_ground_strength: u16,
    pub air_target: *mut Unit,
    pub ground_target: *mut Unit,
    pub slowest_military: *mut Unit,
    pub detector: *mut Unit,
    pub military: MilitaryAiList,
}

#[repr(C, packed)]
pub struct MilitaryAiList {
    pub array: *mut MilitaryAiArray,
    pub first: *mut MilitaryAi,
}

#[repr(C, packed)]
pub struct MilitaryAiArray {
    pub ais: [MilitaryAi; 1000],
    pub first_free: *mut MilitaryAi,
}

#[repr(C, packed)]
pub struct MilitaryAi {
    pub next: *mut MilitaryAi,
    pub prev: *mut MilitaryAi,
    pub ai_type: u8,
    pub unk9: u8,
    pub unka: u16,
    pub parent: *mut Unit,
    pub region: *mut AiRegion,
}

#[repr(C, packed)]
pub struct PlayerAiData {
    pub mineral_need: u32,
    pub gas_need: u32,
    pub supply_need: u32,
    pub minerals_available: u32,
    pub gas_available: u32,
    pub supply_available: u32,
    pub requests: [AiSpendingRequest; 0x3f],
    pub request_count: u8,
    pub dc211: [u8; 0x7],
    pub flags: u16,
    pub dc21a: [u8; 0x4],
    pub attack_grouping_region: u16,
    pub train_unit_id: u16,
    pub default_min_strength_for_regions: u8,
    pub defense_priority_base: u8,
    pub previous_building_hit_second: u32,
    pub last_attack_second: u32,
    pub strategic_suicide_mission_cooldown: u8,
    pub spell_cooldown: u8,
    pub attack_failed: u8,
    pub difficulty: u8,
    pub attack_force: [u16; 0x3f],
    pub attack_force_zero_terminator: u16,
    pub ground_vs_ground_build_def: [u16; 0x14],
    pub ground_vs_air_build_def: [u16; 0x14],
    pub air_vs_ground_build_def: [u16; 0x14],
    pub air_vs_air_build_def: [u16; 0x14],
    pub ground_vs_ground_use_def: [u16; 0x14],
    pub ground_vs_air_use_def: [u16; 0x14],
    pub air_vs_ground_use_def: [u16; 0x14],
    pub air_vs_air_use_def: [u16; 0x14],
    pub build_limits: [u8; 0xe4],
    pub free_medic: *mut Unit,
    pub dc4e8: [u8; 0x10],
}

#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct AiSpendingRequest {
    pub priority: u8,
    pub ty: u8,
    pub id: u16,
    pub val: *mut c_void,
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_sizes() {
        use std::mem;
        assert_eq!(mem::size_of::<AiScript>(), 0x34);
        assert_eq!(mem::size_of::<AiRegion>(), 0x34);
        assert_eq!(mem::size_of::<AiTown>(), 0x1cc);
        assert_eq!(mem::size_of::<BuildingAi>(), 0x2c);
        assert_eq!(mem::size_of::<WorkerAi>(), 0x18);
        assert_eq!(mem::size_of::<GuardAi>(), 0x20);
        assert_eq!(mem::size_of::<GuardAiArray>(), 0x7d04);
        assert_eq!(mem::size_of::<MilitaryAi>(), 0x14);
        assert_eq!(mem::size_of::<MilitaryAiArray>(), 0x4e24);
        assert_eq!(mem::size_of::<PlayerAiData>(), 0x4e8);
    }
}
