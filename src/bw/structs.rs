use libc::c_void;

#[repr(C, packed)]
pub struct AiScript {
    pub next: *mut AiScript,
    pub prev: *mut AiScript,
    pub pos: u32,
    pub wait: u32,
    pub player: u32,
    pub area: [u32; 4],
    pub center: [u32; 2],
    pub town: *mut c_void,
    pub flags: u32,
}

#[repr(C, packed)]
pub struct AiRegion {
    pub id: u16,
    pub target_region_id: u16,
    pub dc4: [u8; 0x30],
}

#[repr(C, packed)]
pub struct PlayerAiData {
    pub dc0: [u8; 0x218],
    pub flags: u16,
    pub dc21a: [u8; 0x4],
    pub attack_grouping_region: u16,
    pub dc220: [u8; 0x8],
    pub last_attack_second: u32,
    pub strategic_suicide_mission_cooldown: u8,
    pub spell_cooldown: u8,
    pub dc22e: [u8; 0x2ba],
}

#[repr(C, packed)]
pub struct Game {
    pub dc0: [u8; 0xe4],
    pub map_width_tiles: u16,
    pub map_height_tiles: u16,
    pub dce8: [u8; 0x64],
    pub frame_count: u32,
    pub dc150: [u8; 0x5ba4],
    pub completed_units_count: [[u32; 0xc]; 0xe4],
    pub unit_kills: [[u32; 0xc]; 0xe4],
    pub deaths: [[u32; 0xc]; 0xe4],
    pub dcdd34: [u8; 0x810],
    pub alliances: [[u8; 0xc]; 0xc],
    pub dce5d4: [u8; 0x34],
    pub elapsed_seconds: u32,
    pub dce60c: [u8; 0x564],
    pub locations: [Location; 0xff],
}

#[repr(C, packed)]
#[derive(Debug, Copy, Clone)]
pub struct Location {
    pub area: Rect32,
    pub unk: u16,
    pub flags: u16,
}

pub struct Image;

#[repr(C, packed)]
pub struct Sprite {
    pub prev: *mut Sprite,
    pub next: *mut Sprite,
    pub sprite_id: u16,
    pub player: u8,
    pub selection_index: u8,
    pub visibility_mask: u8,
    pub elevation_level: u8,
    pub flags: u8,
    pub selection_flash_timer: u8,
    pub index: u16,
    pub width: u8,
    pub height: u8,
    pub position: Point,
    pub main_image: *mut Image,
    pub first_image: *mut Image,
    pub last_image: *mut Image,
}

#[repr(C, packed)]
pub struct Order {
    pub prev: *mut Order,
    pub next: *mut Order,
    pub order_id: u8,
    pub padding: u8,
    pub unit_id: u16,
    pub position: Point,
    pub target: *mut Unit,
}

#[repr(C, packed)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rect32 {
    pub left: i32,
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
}

#[repr(C, packed)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rect {
    pub left: i16,
    pub top: i16,
    pub right: i16,
    pub bottom: i16,
}

#[repr(C, packed)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Point {
    pub x: i16,
    pub y: i16,
}

#[repr(C, packed)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Point32 {
    pub x: i32,
    pub y: i32,
}

#[repr(C, packed)]
pub struct Unit {
    pub prev: *mut Unit,
    pub next: *mut Unit,
    pub hitpoints: i32,
    pub sprite: *mut Sprite,
    pub move_target: Point,
    pub move_target_unit: *mut Unit,
    pub next_move_waypoint: Point,
    pub unk_move_waypoint: Point,
    pub flingy_flags: u8,
    pub facing_direction: u8,
    pub flingy_turn_speed: u8,
    pub movement_direction: u8,
    pub flingy_id: u16,
    pub unk_26: u8,
    pub flingy_movement_type: u8,
    pub position: Point,
    pub exact_position: Point32,
    pub flingy_top_speed: u32,
    pub current_speed: i32,
    pub next_speed: i32,
    pub speed: i32,
    pub speed2: i32,
    pub acceleration: u16,
    pub new_direction: u8,
    pub target_direction: u8,
    // Flingy end
    pub player: u8,
    pub order: u8,
    pub order_state: u8,
    pub order_signal: u8,
    pub order_fow_unit: u16,
    pub unused52: u16,
    pub order_timer: u8,
    pub ground_cooldown: u8,
    pub air_cooldown: u8,
    pub spell_cooldown: u8,
    pub order_target_pos: Point,
    pub target: *mut Unit,
    // Entity end
    pub shields: i32,
    pub unit_id: u16,
    pub unused66: u16,
    pub next_player_unit: *mut Unit,
    pub prev_player_unit: *mut Unit,
    pub subunit: *mut Unit,
    pub order_queue_begin: *mut Order,
    pub order_queue_end: *mut Order,
    pub previous_attacker: *mut Unit,
    pub related: *mut Unit,
    pub highlight_order_count: u8,
    pub order_wait: u8,
    pub unk86: u8,
    pub attack_notify_timer: u8,
    pub previous_unit_id: u16,
    pub minimap_draw_counter: u8,
    pub minimap_draw_color: u8,
    pub unused8c: u16,
    pub rank: u8,
    pub kills: u8,
    pub last_attacking_player: u8,
    pub secondary_order_wait: u8,
    pub ai_spell_flags: u8,
    pub order_flags: u8,
    pub buttons: u16,
    pub invisibility_effects: u8,
    pub movement_state: u8,
    pub build_queue: [u16; 5],
    pub energy: u16,
    pub current_build_slot: u8,
    pub minor_unique_index: u8,
    pub secondary_order: u8,
    pub building_overlay_state: u8,
    pub build_hp_gain: u16,
    pub build_shield_gain: u16,
    pub remaining_build_time: u16,
    pub previous_hp: u16,
    pub loaded_units: [u16; 8],
    pub unit_specific: [u8; 16],
    pub unit_specific2: [u8; 12],
    pub flags: u32,
    pub carried_powerup_flags: u8,
    pub wireframe_seed: u8,
    pub secondary_order_state: u8,
    pub move_target_update_timer: u8,
    pub detection_status: u32,
    pub unke8: u16,
    pub unkea: u16,
    pub currently_building: *mut Unit,
    pub next_invisible: *mut Unit,
    pub prev_invisible: *mut Unit,
    pub rally_pylon: [u8; 8],
    pub path: *mut c_void,
    pub path_frame: u8,
    pub pathing_flags: u8,
    pub _unk106: u8,
    pub _unk107: u8,
    pub collision_points: [u16; 0x4],
    pub death_timer: u16,
    pub defensive_matrix_dmg: u16,
    pub matrix_timer: u8,
    pub stim_timer: u8,
    pub ensnare_timer: u8,
    pub lockdown_timer: u8,
    pub irradiate_timer: u8,
    pub stasis_timer: u8,
    pub plague_timer: u8,
    pub is_under_storm: u8,
    pub irradiated_by: *mut Unit,
    pub irradiate_player: u8,
    pub parasited_by_players: u8,
    pub master_spell_timer: u8,
    pub is_blind: u8,
    pub maelstrom_timer: u8,
    pub _unk125: u8,
    pub acid_spore_count: u8,
    pub acid_spore_timers: [u8; 0x9],
    pub _dc130: [u8; 0x20],
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_sizes() {
        use std::mem;
        assert_eq!(mem::size_of::<AiScript>(), 0x34);
        assert_eq!(mem::size_of::<AiRegion>(), 0x34);
        assert_eq!(mem::size_of::<PlayerAiData>(), 0x4e8);
        assert_eq!(mem::size_of::<Game>(), 0xff5c);
        assert_eq!(mem::size_of::<Unit>(), 0x150);
        assert_eq!(mem::size_of::<Sprite>(), 0x24);
    }
}
