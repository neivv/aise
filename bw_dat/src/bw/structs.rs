use libc::c_void;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[repr(C, packed)]
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

#[repr(C, packed)]
pub struct Game {
    pub minerals: [u32; 0xc],
    pub gas: [u32; 0xc],
    pub dc60: [u8; 0x84],
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
    pub dc177: [u8; 0x15],
    pub unit_availability: [[u8; 0xe4]; 0xc],
    pub dcc3c: [u8; 0x10],
    pub map_path: [u8; 0x104],
    pub map_title: [u8; 0x20],
    pub selection_hotkeys: [[[u32; 0xc]; 0x12]; 0x8],
    pub dc2870: [u8; 0x400],
    pub chat_dialog_recipient: u8,
    pub player_lose_type: u8,
    pub player_has_left: [u8; 0x8],
    pub self_alliance_colors: [u8; 0xc],
    pub player_color_palette: [[u8; 0x8]; 0xc],
    pub player_minimap_color: [u8; 0xc],
    pub dc2cf2: [u8; 0x2],
    pub scores: [[u32; 0xc]; 0x12],
    pub supplies: [Supplies; 0x3],
    pub custom_score: [u32; 0xc],
    pub all_units_count: [[u32; 0xc]; 0xe4],
    pub completed_units_count: [[u32; 0xc]; 0xe4],
    pub unit_kills: [[u32; 0xc]; 0xe4],
    pub deaths: [[u32; 0xc]; 0xe4],
    pub tech_availability_sc: [[u8; 0x18]; 0xc],
    pub tech_level_sc: [[u8; 0x18]; 0xc],
    pub dcdf74: [u8; 0x24],
    pub upgrade_limit_sc: [[u8; 0x2e]; 0xc],
    pub upgrade_level_sc: [[u8; 0x2e]; 0xc],
    pub dce3e8: [u8; 0xd8],
    pub player_forces: [u8; 0x8],
    pub force_flags: [u8; 0x4],
    pub force_names: [[u8; 0x1e]; 0x4],
    pub alliances: [[u8; 0xc]; 0xc],
    pub dce5d4: [u8; 0x34],
    pub elapsed_seconds: u32,
    pub dce60c: [u8; 0x4],
    pub victory_state: [u8; 0x8],
    pub computers_in_leaderboard: u32,
    pub dce61c: [u8; 0x554],
    pub locations: [Location; 0xff],
    pub dcff5c: [u8; 0x4],
    pub tech_availability_bw: [[u8; 0x14]; 0xc],
    pub tech_level_bw: [[u8; 0x14]; 0xc],
    pub dc10140: [u8; 0x48],
    pub upgrade_limit_bw: [[u8; 0xf]; 0xc],
    pub upgrade_level_bw: [[u8; 0xf]; 0xc],
    pub dc102f0: [u8; 0x60],
    pub is_bw: u8,
    pub dc10351: [u8; 0x9],
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

#[repr(C, packed)]
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
    pub draw: *mut c_void,
    pub step_frame: *mut c_void,
    pub parent: *mut Sprite,
}

#[repr(C, packed)]
pub struct Iscript {
    pub header: u16,
    pub pos: u16,
    pub return_pos: u16,
    pub animation: u8,
    pub wait: u8,
}

#[repr(C, packed)]
pub struct Sprite {
    pub prev: *mut Sprite,
    pub next: *mut Sprite,
    pub(crate) sprite_id: u16,
    pub(crate) player: u8,
    pub(crate) selection_index: u8,
    pub(crate) visibility_mask: u8,
    pub(crate) elevation_level: u8,
    pub(crate) flags: u8,
    pub(crate) selection_flash_timer: u8,
    pub(crate) index: u16,
    pub(crate) width: u8,
    pub(crate) height: u8,
    pub(crate) position: Point,
    pub(crate) main_image: *mut Image,
    pub(crate) first_image: *mut Image,
    pub(crate) last_image: *mut Image,
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
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Point {
    pub x: i16,
    pub y: i16,
}

#[repr(C)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Point32 {
    pub x: i32,
    pub y: i32,
}

#[repr(C, packed)]
pub struct LoneSprite {
    pub prev: *mut LoneSprite,
    pub next: *mut LoneSprite,
    pub hitpoints: i32,
    pub sprite: *mut Sprite,
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
    pub bullet_spread_seed: u16,
    pub scr_carried_unit_high_bits: u16,
    pub ai: *mut c_void,
    pub _dc138: [u8; 0x18],
}

#[repr(C, packed)]
pub struct Bullet {
    pub prev: *mut Bullet,
    pub next: *mut Bullet,
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
    pub state: u8,
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
    pub weapon_id: u8,
    pub death_timer: u8,
    pub flags: u8,
    pub bounces_remaining: u8,
    pub parent: *mut Unit,
    pub previous_bounce_target: *mut Unit,
    pub spread_seed: u8,
    pub padding: [u8; 0x3],
}

#[repr(C, packed)]
pub struct Pathing {
    pub region_count: u16,
    pub _dc2: [u8; 0xa],
    pub map_tile_regions: [u16; 0x100 * 0x100],
    pub split_regions: [SplitRegion; 25000],
    pub regions: [Region; 5000],
    pub _dc92bfc: [u8; 0x4e24],
}

#[repr(C, packed)]
pub struct SplitRegion {
    pub minitile_flags: u16,
    pub region_false: u16,
    pub region_true: u16,
}

#[repr(C, packed)]
pub struct Region {
    pub unk: u16,
    pub group: u16,
    pub _dc4: [u8; 0x14],
    pub area: Rect,
    pub _dc20: [u8; 0x20],
}

#[repr(C, packed)]
pub struct Player {
    pub id: u32,
    pub storm_id: u32,
    pub ty: u8,
    pub race: u8,
    pub team: u8,
    pub name: [u8; 25],
}

#[repr(C, packed)]
pub struct Dialog {
    pub control: Control,
    pub unk36: [u8; 0xc],
    pub first_child: *mut Control,
    pub active: *mut Control,
}

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

#[repr(C, packed)]
pub struct ControlEvent {
    pub ext_type: u32,
    pub ext_param: u32,
    pub param: u32,
    pub ty: u16,
    pub x: i16,
    pub y: i16,
}

pub mod scr {
    use libc::c_void;
    use super::{Image, Rect};

    #[repr(C, packed)]
    pub struct Dialog {
        pub control: Control,
        pub unk50: [u8; 0xc],
        pub first_child: *mut Control,
        pub active: *mut Control,
    }

    #[repr(C, packed)]
    pub struct Control {
        pub next: *mut Control,
        pub area: Rect,
        pub image: [u8; 8],
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

    #[repr(C, packed)]
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

    #[repr(C, packed)]
    pub struct DrawCommands {
        pub commands: [DrawCommand; 0x2000],
        pub _dc140000: [u8; 0x20028],
        pub draw_command_count: u16,
    }

    #[repr(C)]
    pub struct DrawCommand {
        pub draw_target_index: u32,
        pub is_hd: u32,
        pub texture_ids: [u32; 7],
        pub draw_mode: u32,
        pub shader_id: u32,
        pub vertex_buffer_index: u32,
        pub _unk30: u32,
        pub allocated_vertex_count: u32,
        pub used_vertex_count: u32,
        pub _unk3c: u16,
        pub blend_mode: u16,
        pub _unk40: [u8; 0x10],
        pub uniforms: [f32; 0x14],
    }

    #[repr(C, packed)]
    pub struct Sprite {
        pub prev: *mut Sprite,
        pub next: *mut Sprite,
        pub(crate) sprite_id: u16,
        pub(crate) player: u8,
        pub(crate) selection_index: u8,
        pub(crate) visibility_mask: u8,
        pub(crate) elevation_level: u8,
        pub(crate) flags: u8,
        pub(crate) selection_flash_timer: u8,
        pub(crate) index: u16,
        pub(crate) width: u8,
        pub(crate) height: u8,
        pub(crate) pos_x: u32,
        pub(crate) pos_y: u32,
        pub(crate) main_image: *mut Image,
        pub(crate) first_image: *mut Image,
        pub(crate) last_image: *mut Image,
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

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_sizes() {
        use std::mem;
        assert_eq!(mem::size_of::<Game>(), 0x17700);
        assert_eq!(mem::size_of::<Unit>(), 0x150);
        assert_eq!(mem::size_of::<Bullet>(), 0x70);
        assert_eq!(mem::size_of::<Sprite>(), 0x24);
        assert_eq!(mem::size_of::<Image>(), 0x40);
        assert_eq!(mem::size_of::<Pathing>(), 0x97a20);
        assert_eq!(mem::size_of::<Region>(), 0x40);
        assert_eq!(mem::size_of::<Player>(), 0x24);
        assert_eq!(mem::size_of::<ControlEvent>(), 0x12);
        assert_eq!(mem::size_of::<Control>(), 0x36);
        assert_eq!(mem::size_of::<Dialog>(), 0x4a);
        assert_eq!(mem::size_of::<scr::ControlEvent>(), 0x1c);
        assert_eq!(mem::size_of::<scr::Control>(), 0x50);
        assert_eq!(mem::size_of::<scr::Dialog>(), 0x64);
        assert_eq!(mem::size_of::<scr::DrawCommand>(), 0xa0);
        assert_eq!(mem::size_of::<scr::BwString>(), 0x1c);
        assert_eq!(mem::size_of::<scr::Sprite>(), 0x28);
    }
}
