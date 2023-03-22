use std::fmt;

use libc::c_void;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(any(feature = "scr-only", target_pointer_width = "64"))]
pub use self::scr::Dialog;
#[cfg(any(feature = "scr-only", target_pointer_width = "64"))]
pub use self::scr::Control;
#[cfg(any(feature = "scr-only", target_pointer_width = "64"))]
pub use self::scr::ControlEvent;

#[repr(C)]
pub struct DatTable {
    pub data: *mut c_void,
    pub entry_size: u32,
    pub entries: u32,
}

#[repr(C)]
pub struct Supplies {
    pub provided: [u32; 0xc],
    pub used: [u32; 0xc],
    pub max: [u32; 0xc],
}

#[repr(C)]
pub struct Game {
    pub minerals: [u32; 0xc],
    pub gas: [u32; 0xc],
    pub dc60: [u8; 0x60],
    pub starting_local_player_id: u32,
    pub starting_player_types: [u8; 0xc],
    pub starting_races: [u8; 0xc],
    pub team_game_main_player: [u8; 4],
    pub screen_pos_x_tiles: u16,
    pub screen_pos_y_tiles: u16,
    pub map_width_tiles: u16,
    pub map_height_tiles: u16,
    pub dce8: [u8; 0x4],
    pub tileset: u16,
    pub bgm_song: u16,
    pub dcf0: u8,
    pub active_net_players: u8,
    pub player_race: u8,
    pub custom_singleplayer: u8,
    pub dcf4: [u8; 0x8],
    pub visions: [u32; 0xc],
    pub player_randomization: [u32; 0x8],
    pub frame_count: u32,
    pub saved_elapsed_seconds: u32,
    pub campaign_mission: u16,
    pub next_scenario: [u8; 0x20],
    pub selected_singleplayer_race: u8,
    pub selected_singleplayer_computer_races: [u8; 0x8],
    pub saved_screen_positions: [[u16; 2]; 3],
    pub unit_availability: [[u8; 0xe4]; 0xc],
    pub dcc3c: [u8; 0x10],
    pub map_path: [u8; 0x104],
    pub map_title: [u8; 0x20],
    pub selection_hotkeys: [[[u32; 0xc]; 0x12]; 0x8],
    pub dc2870: [u8; 0x400],
    pub chat_dialog_recipient: u8,
    pub player_lose_type: u8,
    pub player_was_dropped: [u8; 0x8],
    pub self_alliance_colors: [u8; 0xc],
    pub player_color_palette: [[u8; 0x8]; 0xc],
    pub player_minimap_color: [u8; 0xc],
    pub scores: [[u32; 0xc]; 0x12],
    pub supplies: [Supplies; 0x3],
    pub custom_score: [u32; 0xc],
    pub all_units_count: [[u32; 0xc]; 0xe4],
    pub completed_units_count: [[u32; 0xc]; 0xe4],
    pub unit_kills: [[u32; 0xc]; 0xe4],
    pub deaths: [[u32; 0xc]; 0xe4],
    pub tech_availability_sc: [[u8; 0x18]; 0xc],
    pub tech_level_sc: [[u8; 0x18]; 0xc],
    pub tech_in_progress_legacy: [u8; 0x24],
    pub upgrade_limit_sc: [[u8; 0x2e]; 0xc],
    pub upgrade_level_sc: [[u8; 0x2e]; 0xc],
    pub dce3e8: [u8; 0xd8],
    pub player_forces: [u8; 0x8],
    pub force_flags: [u8; 0x4],
    pub force_names: [[u8; 0x1e]; 0x4],
    pub alliances: [[u8; 0xc]; 0xc],
    pub player_objectives_string: [u32; 0xc],
    pub countdown_timer: u32,
    pub elapsed_seconds: u32,
    pub dce60c: [u8; 0x4],
    pub victory_state: [u8; 0x8],
    pub computers_in_leaderboard: u32,
    pub leaderboard_type: u8,
    pub leaderboard_unit_id: u16,
    pub leaderboard_unk1: u32,
    pub leaderboard_unk2: u32,
    pub pauses_remaining: [u8; 0x8],
    pub start_position: [[u16; 0x2]; 0x8],
    pub locations_legacy: [Location; 0x40],
    pub unkeb50: [u8; 0x20],
    pub locations: [Location; 0xff],
    pub dcff5c: [u8; 0x4],
    pub tech_availability_bw: [[u8; 0x14]; 0xc],
    pub tech_level_bw: [[u8; 0x14]; 0xc],
    pub tech_in_progress: [u8; 0x48],
    pub upgrade_limit_bw: [[u8; 0xf]; 0xc],
    pub upgrade_level_bw: [[u8; 0xf]; 0xc],
    pub upgrade_in_progress: [u8; 0x60],
    pub is_bw: u8,
    pub unk10351: u8,
    pub scr_color_unk: [u8; 0x8],
    pub scr_init_color_rgba: [[u8; 4]; 8],
    pub scr_unk1037a: [u8; 0x10],
    pub scr_player_color_preference: [u8; 0x10],
    pub padding: [u8; 0x7366],
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Location {
    pub area: Rect32,
    pub unk: u16,
    pub flags: u16,
}

pub struct GrpSprite;

#[repr(C)]
pub struct Image {
    pub prev: *mut Image,
    pub next: *mut Image,
    pub image_id: u16,
    pub drawfunc: u8,
    pub direction: u8,
    pub flags: u16,
    pub x_offset: i8,
    pub y_offset: i8,
    pub iscript: Iscript,
    pub frameset: u16,
    pub frame: u16,
    pub map_position: Point,
    pub screen_position: Point,
    pub grp_bounds: Rect,
    pub grp: *mut c_void,
    pub drawfunc_param: *mut c_void,
    pub draw: u32,
    pub step_frame: u32,
    pub parent: *mut Sprite,
}

#[repr(C)]
pub struct Iscript {
    pub header: u16,
    pub pos: u16,
    pub return_pos: u16,
    pub animation: u8,
    pub wait: u8,
}

#[repr(C)]
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
    pub version_specific: SpriteVersions,
}

/// 1.16.1-only sprite if struct size is needed.
/// Other than size the layout will match that of Sprite.
#[repr(C)]
pub struct OldSprite {
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
    pub rest: SpriteLegacy,
}

#[repr(C)]
pub union SpriteVersions {
    pub legacy: SpriteLegacy,
    pub scr: SpriteScr,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SpriteLegacy {
    pub position: Point,
    pub main_image: *mut Image,
    pub first_image: *mut Image,
    pub last_image: *mut Image,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct SpriteScr {
    pub position: [usize; 2],
    pub main_image: *mut Image,
    pub first_image: *mut Image,
    pub last_image: *mut Image,
}

#[repr(C)]
pub struct Order {
    pub prev: *mut Order,
    pub next: *mut Order,
    pub order_id: u8,
    pub padding: u8,
    pub unit_id: u16,
    pub position: Point,
    pub target: *mut Unit,
}

#[repr(C)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rect32 {
    pub left: i32,
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
}

#[repr(C)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rect {
    pub left: i16,
    pub top: i16,
    pub right: i16,
    pub bottom: i16,
}

#[repr(C)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Point {
    pub x: i16,
    pub y: i16,
}

#[repr(C)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Point32 {
    pub x: i32,
    pub y: i32,
}

#[repr(C)]
pub struct LoneSprite {
    pub prev: *mut LoneSprite,
    pub next: *mut LoneSprite,
    pub hitpoints: i32,
    pub sprite: *mut Sprite,
}

#[repr(C)]
pub struct Flingy {
    pub prev: *mut Flingy,
    pub next: *mut Flingy,
    pub hitpoints: i32,
    pub sprite: *mut Sprite,
    pub move_target: PointAndUnit,
    pub next_move_waypoint: Point,
    pub unk_move_waypoint: Point,
    pub flingy_flags: u8,
    pub facing_direction: u8,
    pub turn_speed: u8,
    pub movement_direction: u8,
    pub flingy_id: u16,
    pub unk_26: u8,
    pub movement_type: u8,
    pub position: Point,
    pub exact_position: Point32,
    pub top_speed: u32,
    pub current_speed: i32,
    pub next_speed: i32,
    pub current_speed_x: i32,
    pub current_speed_y: i32,
    pub acceleration: u16,
    pub new_direction: u8,
    pub target_direction: u8,
}

#[repr(C)]
pub struct Unit {
    pub flingy: Flingy,
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
    pub order_target: PointAndUnit,
    // Entity end
    pub shields: i32,
    pub unit_id: u16,
    pub unused66: u16,
    pub prev_player_unit: *mut Unit,
    pub next_player_unit: *mut Unit,
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
    pub unit_specific: UnitSpecific1,
    pub unit_specific2: UnitSpecific2,
    pub flags: u32,
    pub carried_powerup_flags: u8,
    pub wireframe_seed: u8,
    pub secondary_order_state: u8,
    pub move_target_update_timer: u8,
    pub detection_status: u32,
    pub secondary_order_target: PointAndUnit,
    pub next_invisible: *mut Unit,
    pub prev_invisible: *mut Unit,
    pub rally_pylon: RallyPylon,
    pub path: UnitPath,
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
    pub bullet_spread_seed: u16,
    pub scr_carried_unit_high_bits: u16,
    pub ai: *mut c_void,
    pub air_strength: u16,
    pub ground_strength: u16,
    pub position_search_indices: [u32; 4],
    pub repulse_misc: u8,
    pub repulse_direction: u8,
    pub repulse_chunk_x: u8,
    pub repulse_chunk_y: u8,
}

#[repr(C)]
pub struct UnitPath {
    pub path: *mut Path,
    pub path_frame: u8,
    pub pathing_flags: u8,
    pub _unk106: u8,
    pub _unk107: u8,
    pub collision_points: [u16; 0x4],
}

#[repr(C)]
pub struct Path {
    pub header: PathHeader,
    pub start_frame: u32,
    pub dodge_unit_uid: u32,
    pub xy_speed: u32,
    pub flags: u8,
    pub collision_recheck_timer: u8,
    pub direction: u8,
    pub total_region_count: u8,
    pub unk1c: [u8; 4],
    pub values: [u16; 0x30],
}

#[repr(C)]
pub union PathHeader {
    pub allocated: AllocatedPathHeader,
    pub free: FreePathHeader,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct AllocatedPathHeader {
    pub start: Point,
    pub next_pos: Point,
    pub end: Point,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct FreePathHeader {
    pub next_path_index: u32,
    pub self_index: u32,
}

#[repr(C)]
pub struct PointAndUnit {
    pub pos: Point,
    pub unit: *mut Unit,
}

#[repr(C)]
pub union UnitSpecific1 {
    pub carrier: UnitCarrier,
    pub interceptor: UnitInterceptor,
    pub worker: UnitWorker1,
    pub vulture: UnitVulture,
    pub building: UnitBuilding,
}

#[repr(C)]
pub union UnitSpecific2 {
    pub worker: UnitWorker2,
    pub nydus: UnitNydus,
    pub resource: UnitResource,
    pub powerup: UnitPowerup,
    pub nuke_silo: UnitNukeSilo,
    pub pylon: UnitPylonAura,
}

#[repr(C)]
pub union RallyPylon {
    pub rally: UnitRally,
    pub pylon: UnitPylon,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitVulture {
    pub mines: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitCarrier {
    pub first_inside_child: *mut Unit,
    pub first_outside_child: *mut Unit,
    pub in_hangar_count: u8,
    pub out_hangar_count: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitInterceptor {
    pub parent: *mut Unit,
    pub next: *mut Unit,
    pub prev: *mut Unit,
    pub is_outside_hangar: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitBuilding {
    pub addon: *mut Unit,
    pub build_addon_unit_id: u16,
    pub research_time_remaining: u16,
    pub tech: u8,
    pub upgrade: u8,
    pub larva_timer: u8,
    pub is_landing: u8,
    pub creep_spread_cooldown: u8,
    pub next_upgrade_level: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitNukeSilo {
    pub nuke: *mut Unit,
    pub has_nuke: u32,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitPylonAura {
    pub aura: *mut Sprite,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitWorker1 {
    pub powerup: *mut Unit,
    pub harvest_target_pos: Point,
    pub current_harvest_target: *mut Unit,
    pub repair_resource_loss_timer: u16,
    pub is_harvesting: u8,
    pub carried_resource_count: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitWorker2 {
    pub harvest_target: *mut Unit,
    pub previous_awaiting: *mut Unit,
    pub next_awaiting: *mut Unit,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitResource {
    pub amount: u16,
    pub iscript_anim: u8,
    pub has_awaiting_workers: u8,
    pub first_awaiting_worker: *mut Unit,
    pub resource_area: u8,
    pub ai_used_resource: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitNydus {
    pub linked: *mut Unit,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitPowerup {
    pub orig_pos: Point,
    pub worker: *mut Unit,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitRally {
    pub pos: Point,
    pub unit: *mut Unit,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UnitPylon {
    pub prev: *mut Unit,
    pub next: *mut Unit,
}

#[repr(C)]
pub struct Bullet {
    pub flingy: Flingy,
    pub player: u8,
    pub state: u8,
    pub order_state: u8,
    pub order_signal: u8,
    pub order_fow_unit: u16,
    pub unused52: u16,
    pub order_timer: u8,
    pub ground_cooldown: u8,
    pub air_cooldown: u8,
    pub spell_cooldown: u8,
    pub target: PointAndUnit,
    // Entity end
    pub weapon_id: u8,
    pub death_timer: u8,
    pub flags: u8,
    pub bounces_remaining: u8,
    pub parent: *mut Unit,
    pub previous_bounce_target: *mut Unit,
    pub spread_seed: u8,
}

#[repr(C)]
pub struct Pathing {
    pub region_count: u16,
    pub _dc2: [u8; 0x2],
    pub unk_ids: *mut u16,
    pub unk_split_region: *mut SplitRegion,
    pub map_tile_regions: [u16; 0x100 * 0x100],
    pub split_regions: [SplitRegion; 25000],
    pub regions: [Region; 5000],
    pub _dc92bfc: [u8; 0x4e24],
}

#[repr(C)]
pub struct SplitRegion {
    pub minitile_flags: u16,
    pub region_false: u16,
    pub region_true: u16,
}

#[repr(C)]
pub struct Region {
    pub walkability: u16,
    pub group: u16,
    pub tile_count: u16,
    pub ground_neighbours: u8,
    pub all_neighbours: u8,
    pub temp_val: *mut c_void,
    pub neighbour_ids: *mut u16,
    pub center: [u32; 2],
    pub area: Rect,
    pub _dc20: [u8; 0x20],
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Player {
    pub id: u32,
    pub storm_id: u32,
    pub player_type: u8,
    pub race: u8,
    pub team: u8,
    pub name: [u8; 25],
}

#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
#[repr(C, packed)]
pub struct Dialog {
    pub control: Control,
    pub unk36: [u8; 0xc],
    pub first_child: *mut Control,
    pub active: *mut Control,
}

#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
#[repr(C, packed)]
pub struct Control {
    pub next: *mut Control,
    pub area: Rect,
    pub image: [u8; 8],
    pub string: *const u8,
    pub flags: u32,
    pub unk1c: [u8; 4],
    pub id: i16,
    pub ty: u16,
    pub misc_u16: u16,
    pub user_ptr: *mut c_void,
    pub event_handler: Option<unsafe extern "fastcall" fn(*mut Control, *mut ControlEvent) -> u32>,
    pub draw: Option<unsafe extern "fastcall" fn(*mut Control, i32, i32, *const Rect)>,
    pub parent: *mut Dialog,
}

#[cfg(all(not(feature = "scr-only"), target_pointer_width = "32"))]
#[repr(C)]
pub struct ControlEvent {
    pub ext_type: u32,
    pub ext_param: u32,
    pub param: u32,
    pub ty: u16,
    pub x: i16,
    pub y: i16,
}

#[repr(C)]
pub struct Surface {
    pub width: u16,
    pub height: u16,
    pub data: *mut u8,
}

pub mod scr {
    use libc::c_void;
    use super::{Rect, Surface};

    #[repr(C)]
    pub struct ListBox {
        pub control: Control,
        pub label: *mut Control,
        pub entry_strings: *mut *const u8,
        pub entry_flags: *mut u8,
        pub entry_data: *mut usize,
        pub entries: u8,
        pub max_entries: u8,
        pub selected_entry1: u8,
        pub entry_height: u8,
        pub visible_entries: u8,
        pub not_visible_entries: u8,
        pub flags: u8,
        pub unk67: u8,
        pub selected_entry2: u8,
        pub draw_entry: usize,
    }

    #[repr(C)]
    pub struct Dialog {
        pub control: Control,
        pub surface: Surface,
        pub highlighted: *mut Control,
        pub first_child: *mut Control,
        pub active: *mut Control,
    }

    #[repr(C)]
    pub struct Control {
        pub next: *mut Control,
        pub area: Rect,
        pub image: Surface,
        pub string: BwString,
        pub flags: u32,
        pub flags2: u32,
        pub unknown: u16,
        pub id: i16,
        pub ty: u16,
        pub misc_u16: u16,
        pub user_ptr: *mut c_void,
        pub event_handler: Option<unsafe extern "C" fn(*mut Control, *mut ControlEvent) -> u32>,
        pub draw: Option<unsafe extern "C" fn(*mut Control, i32, i32, *const Rect)>,
        pub parent: *mut Dialog,
    }

    #[repr(C)]
    pub struct BwString {
        pub data: *const u8,
        pub length: usize,
        pub capacity: usize,
        pub inline_buffer: [u8; 0x10],
    }

    #[repr(C)]
    pub struct ControlEvent {
        pub ext_type: u32,
        pub ext_ptr: *mut c_void,
        pub ext_param: u32,
        pub param: u32,
        pub ty: u16,
        pub x: i16,
        pub y: i16,
        pub padding: u16,
        pub time: u32,
    }

    #[repr(C)]
    pub struct DrawCommands {
        pub commands: [DrawCommand; 0x2000],
        pub subcommand_buffer: [u8; 0x20000],
        pub subcommand_buffer_bytes_used: u32,
        pub draw_sort_vector: [usize; 3],
        pub unk_pointers: [usize; 6],
        pub draw_command_count: u16,
        pub unk_u32: u32,
    }

    #[repr(C)]
    pub struct DrawCommand {
        pub render_target_id: u32,
        pub is_hd: u32,
        pub texture_ids: [usize; 7],
        pub draw_mode: u32,
        pub shader_id: u32,
        pub vertex_buffer_offset_bytes: usize,
        pub index_buffer_offset_bytes: usize,
        pub allocated_vertex_count: u32,
        pub used_vertex_count: u32,
        pub _unk3c: u16,
        pub blend_mode: u16,
        pub subcommands_pre: DrawSubCommands,
        pub subcommands_post: DrawSubCommands,
        pub shader_constants: [f32; 0x14],
    }

    #[repr(C)]
    pub struct DrawSubCommands {
        pub unk: usize,
        pub first: *mut DrawSubCommand,
    }

    #[repr(C)]
    pub struct DrawSubCommand {
        /// Variable-length data follows this struct depending on id:
        /// 1 = Set scissor, { rect: [f32; 4] }
        ///      x/y/w/h with 0.0 to 1.0 range
        /// 2 = Clear scissor, no data
        /// 3 = Clear render target attachment? {
        ///     render_target_index: u32,
        ///     attachment_index: u32,
        ///     is_hd_render_target: u32,
        ///     More?
        /// }
        /// 4 = Render to texture? {
        ///     render_target: *mut RenderTarget,
        ///     unk: [usize?; 2],
        ///     mask: *mut DrawCommand,
        ///     unk: usize?
        ///     width?: u32,
        ///     height?: u32, (y offset?)
        /// }
        /// 5 = Skip main DrawCommand
        ///     When just needing to modify state with other subcommands?
        /// 6 = Unk texture? { ??? }
        pub id: u32,
        pub next: *mut DrawSubCommand,
    }
}

impl Rect {
    pub fn from_point(point: Point) -> Rect {
        let x = point.x;
        let y = point.y;
        Rect {
            left: x,
            right: x.saturating_add(1),
            top: y,
            bottom: y.saturating_add(1),
        }
    }

    pub fn from_point_radius(point: Point, radius: i16) -> Rect {
        Rect {
            left: point.x.saturating_sub(radius).max(0),
            right: point.x.saturating_add(radius),
            top: point.y.saturating_sub(radius).max(0),
            bottom: point.y.saturating_add(radius),
        }
    }

    pub fn top_left(&self) -> Point {
        Point {
            x: self.left,
            y: self.top,
        }
    }

    /// Note: Inclusive
    pub fn top_right(&self) -> Point {
        Point {
            x: self.right - 1,
            y: self.top,
        }
    }

    /// Note: Inclusive
    pub fn bottom_left(&self) -> Point {
        Point {
            x: self.left,
            y: self.bottom - 1,
        }
    }

    /// Note: Inclusive
    pub fn bottom_right(&self) -> Point {
        Point {
            x: self.right - 1,
            y: self.bottom - 1,
        }
    }

    pub fn overlaps(&self, o: &Rect) -> bool {
        self.left < o.right && self.right > o.left && self.top < o.bottom && self.bottom > o.top
    }

    pub fn contains_point(&self, point: &Point) -> bool {
        point.x >= self.left && point.x < self.right &&
            point.y >= self.top && point.y < self.bottom
    }

    pub fn center(&self) -> Point {
        Point {
            x: self.left + (self.right - self.left) / 2,
            y: self.top + (self.bottom - self.top) / 2,
        }
    }

    pub fn width(&self) -> u16 {
        if self.right < self.left {
            0
        } else {
            (self.right as u16).wrapping_sub(self.left as u16)
        }
    }

    pub fn height(&self) -> u16 {
        if self.bottom < self.top {
            0
        } else {
            (self.bottom as u16).wrapping_sub(self.top as u16)
        }
    }
}

impl Point {
    /// Assuming that `self` and `rect` are in same coordinates, returns
    /// a point that is equicaled to `self` in coordinates relative to the
    /// rect's topleft position.
    ///
    /// If the point is outside rect bounds, it is clamped to be inside rect
    /// (Rect right/bottom are not inside rect bounds).
    pub fn relative_to_rect_clamped(&self, rect: &Rect) -> Point {
        let x = if self.x < rect.left {
            0
        } else if self.x >= rect.right {
            rect.right.saturating_sub(1)
        } else {
            self.x - rect.left
        };
        let y = if self.y < rect.top {
            0
        } else if self.y >= rect.bottom {
            rect.bottom.saturating_sub(1)
        } else {
            self.y - rect.top
        };
        Point {
            x,
            y,
        }
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

impl fmt::Debug for Point32 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[cfg(target_pointer_width = "32")]
    fn size(on32: usize, _on64: usize) -> usize {
        on32
    }

    #[cfg(target_pointer_width = "64")]
    fn size(_on32: usize, on64: usize) -> usize {
        on64
    }

    #[test]
    fn test_sizes() {
        use std::mem;
        assert_eq!(mem::size_of::<Game>(), 0x17700);
        assert_eq!(mem::size_of::<Flingy>(), size(0x4c, 0x68));
        assert_eq!(mem::size_of::<Unit>(), size(0x150, 0x1e8));
        assert_eq!(mem::size_of::<Bullet>(), size(0x70, 0xa8));
        assert_eq!(mem::size_of::<OldSprite>(), size(0x24, 0x40));
        assert_eq!(mem::size_of::<Image>(), size(0x40, 0x58));
        assert_eq!(mem::size_of::<Pathing>(), size(0x97a20, 0xa1670));
        assert_eq!(mem::size_of::<Region>(), size(0x40, 0x48));
        assert_eq!(mem::size_of::<Player>(), 0x24);
        assert_eq!(mem::size_of::<Surface>(), size(0x8, 0x10));
        assert_eq!(mem::size_of::<ControlEvent>(), size(0x14, 0x28));
        assert_eq!(mem::size_of::<Control>(), size(0x36, 0x78));
        assert_eq!(mem::size_of::<Dialog>(), size(0x4a, 0xa0));
        assert_eq!(mem::size_of::<Path>(), 0x80);
        assert_eq!(mem::size_of::<Sprite>(), size(0x28, 0x48));
        assert_eq!(mem::size_of::<scr::ControlEvent>(), size(0x1c, 0x28));
        assert_eq!(mem::size_of::<scr::Control>(), size(0x50, 0x78));
        assert_eq!(mem::size_of::<scr::Dialog>(), size(0x64, 0xa0));
        assert_eq!(mem::size_of::<scr::DrawCommand>(), size(0xa0, 0xd8));
        assert_eq!(mem::size_of::<scr::DrawCommands>(), size(0x160030, 0x1d0058));
        assert_eq!(mem::size_of::<scr::DrawSubCommand>(), size(0x8, 0x10));
        assert_eq!(mem::size_of::<scr::BwString>(), size(0x1c, 0x28));
    }
}
