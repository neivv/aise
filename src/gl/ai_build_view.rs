use std::mem;

use cgmath::conv::array4x4;
use cgmath::{vec4, Matrix4};
use glium::backend::Facade;
use glium::{uniform, Surface};

use bw_dat::{unit, Game, Unit, UnitId};

use crate::aiscript::Town;
use crate::bw;
use crate::unit_search::UnitSearch;

use super::draw_shapes::{self, DrawShapes};
use super::gl_common::GlCommon;
use super::support::UiList;
use super::ui::{Page, Ui};
use super::{screen_rect, unit_name, Program, UiInput};

pub struct AiBuildView {
    towns: UiList<*mut bw::AiTown>,
    build_info: Option<TownBuildInfo>,
    unit_id: UnitId,
    squares: DrawShapes,
    program: Program,
}

impl AiBuildView {
    pub fn new<F: Facade>(facade: &F) -> AiBuildView {
        let program = compile_program!(facade, "passthrough_color.vert", "masked.frag");
        AiBuildView {
            towns: UiList::new(),
            build_info: None,
            unit_id: UnitId(0),
            squares: DrawShapes::new(),
            program,
        }
    }

    pub fn draw_page(&mut self, page: &mut Page) {
        page.clear();
        self.build_info = None;
        self.towns.update();
        let town = match self.towns.current() {
            Some(&s) => s,
            None => return,
        };
        if !self.unit_id.is_building() {
            page.push(format!("{} is not a building", unit_name(self.unit_id)));
            return;
        }
        page.push(format!("Placement for {}", unit_name(self.unit_id)));

        let game = bw::game();
        let town_array = bw::town_array();
        unsafe {
            let town_id = (town as usize - town_array as usize) / mem::size_of::<bw::AiTown>();
            page.push(format!(
                "Town #{}/{} id {:x}: {:p} player {:x} pos ({}, {})",
                self.towns.current_pos() + 1,
                self.towns.len(),
                town_id,
                town,
                (*town).player,
                (*town).position.x,
                (*town).position.y,
            ));
        }
        let tile_flags = MapTileFlags::new(game);
        let search = unsafe { UnitSearch::from_bw() };
        let mut build_info =
            TownBuildInfo::new(game, &search, &tile_flags, Town(town), self.unit_id, 0x40);
        if self.unit_id.require_psi() {
            build_info.add_unpowered_tiles();
        }
        self.build_info = Some(build_info);
    }

    pub fn input(&mut self, value: char, ui: &mut Ui) -> UiInput {
        match value {
            'q' => {
                self.towns.page_back(1);
                UiInput::Handled
            }
            'w' => {
                self.towns.page_forward(1);
                UiInput::Handled
            }
            'a' => {
                ui.request_input('a', "Unit id (dec)");
                UiInput::Handled
            }
            's' => {
                ui.request_input('s', "Unit id (hex)");
                UiInput::Handled
            }
            _ => UiInput::NotHandled,
        }
    }

    // Returns the (error) message that gets shown on the ui afterwards, if any
    pub fn input_end(&mut self, value: char, message: &str) -> Option<String> {
        match value {
            'a' | 's' => {
                let base = match value {
                    'a' => 10,
                    's' | _ => 16,
                };

                let unit_id = u16::from_str_radix(message, base)
                    .ok()
                    .filter(|&x| x < bw_dat::unit::NONE.0);
                if let Some(unit_id) = unit_id {
                    self.unit_id = UnitId(unit_id);
                    None
                } else {
                    let message = format!("'{}' is not a valid unit id", message);
                    Some(message)
                }
            }
            _ => None,
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    pub fn draw_overlay<F: Facade, S: Surface>(
        &mut self,
        facade: &F,
        surface: &mut S,
        gl_common: &mut GlCommon,
    ) {
        let build_info = match self.build_info {
            Some(ref s) => s,
            None => return,
        };
        self.squares.clear();
        let screen_area = screen_rect();
        let area = rect_intersection(&screen_area, &build_info.area());
        if area.left >= area.right || area.top >= area.bottom {
            return;
        }
        let round32up = |val: u16| match val {
            0 => 0,
            val => ((val - 1) | 31) + 1,
        };
        let top_tile = area.top as u16 / 32;
        let bottom_tile = round32up(area.bottom as u16) / 32;
        let height = bottom_tile - top_tile;
        let left_tile = area.left as u16 / 32;
        let right_tile = round32up(area.right as u16) / 32;
        let width = right_tile - left_tile;
        for y_tile in top_tile..(top_tile + height) {
            let tiles = build_info.iter_slice(left_tile, y_tile, width);
            for (x_tile, status) in (left_tile..).zip(tiles) {
                if status.intersects(BuildStatus::NOT_OK) {
                    let rect = draw_shapes::Rect {
                        left: x_tile as f32 * 32.0,
                        top: y_tile as f32 * 32.0,
                        right: x_tile as f32 * 32.0 + 32.0,
                        bottom: y_tile as f32 * 32.0 + 32.0,
                    };
                    let creep_flags = BuildStatus::NEED_CREEP |
                        BuildStatus::HAS_CREEP |
                        BuildStatus::CREEP_DISAPPEARING;
                    let color = if status.intersects(BuildStatus::UNBUILDABLE) {
                        (1.0, 0.0, 0.0, 0.4)
                    } else if status.intersects(BuildStatus::BUILDING_EXISTS) {
                        (0.8, 0.1, 0.0, 0.4)
                    } else if status.intersects(creep_flags) {
                        (0.8, 0.0, 0.4, 0.4)
                    } else if status.intersects(BuildStatus::NEED_POWER) {
                        (0.6, 0.2, 0.4, 0.2)
                    } else if status.intersects(BuildStatus::MINERAL_LINE) {
                        (0.0, 0.2, 0.5, 0.3)
                    } else {
                        (1.0, 0.0, 1.0, 0.4)
                    };
                    self.squares.fill_rect(&rect, color);
                }
            }
        }

        let squares = self.squares.finish(facade);

        // From map coords to screen-relative
        let screen_shift = Matrix4::from_cols(
            vec4(1.0,       0.0,        0.0,    0.0),
            vec4(0.0,       1.0,        0.0,    0.0),
            vec4(0.0,       0.0,        1.0,    0.0),
            vec4(0.0 - screen_area.left as f32, 0.0 - screen_area.top as f32,   0.0,    1.0),
        );
        // From 640x480 coords to -1 .. 1
        let reso_w = 640.0f32;
        let reso_h = 480.0f32;
        let pos_transform = Matrix4::from_cols(
            vec4(2.0 / reso_w,  0.0,            0.0,    0.0),
            vec4(0.0,           -2.0 / reso_h,  0.0,    0.0),
            vec4(0.0,           0.0,            1.0,    0.0),
            vec4(-1.0,          1.0,            0.0,    1.0),
        );
        let uniforms = uniform! {
            transform: array4x4(pos_transform * screen_shift),
            mask: gl_common.game_screen_mask_texture(facade),
        };
        let params = glium::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            ..Default::default()
        };
        surface.draw(
            &squares.vertices,
            &squares.indices,
            self.program.glium_program(facade),
            &uniforms,
            &params,
        ).unwrap();
    }
}

fn rect_intersection(a: &bw::Rect, b: &bw::Rect) -> bw::Rect {
    bw::Rect {
        left: a.left.max(b.left),
        top: a.top.max(b.top),
        right: a.right.min(b.right),
        bottom: a.bottom.min(b.bottom),
    }
}

struct TownBuildInfo {
    topleft_tile: (u16, u16),
    width: u16,
    height: u16,
    tiles: Vec<BuildStatus>,
    unit_id: UnitId,
}

bitflags::bitflags! {
    struct BuildStatus: u16 {
        // HAS_POWER is ok
        const NOT_OK = 0x7fff;
        const UNBUILDABLE = 0x1;
        const BUILDING_EXISTS = 0x2;
        const OTHER_UNIT = 0x4;
        const NEAR_PRODUCTION = 0x8;
        const MINERAL_LINE = 0x10;
        const BLOCKS_ADDON = 0x20;
        // Debug display only, HAS_POWER is the meaningful one
        const NEED_POWER = 0x40;
        const NEED_CREEP = 0x80;
        const HAS_CREEP = 0x100;
        const RESOURCES_BLOCKING = 0x200;
        const CREEP_DISAPPEARING = 0x400;
        // Inversely a required flag due to pylons only checking only building's center
        // point (rounded to a tile) for whether there is power.
        // This flag is is set if a building with its top left tile placed here has power.
        const HAS_POWER = 0x8000;
    }
}

impl TownBuildInfo {
    pub fn new(
        game: Game,
        search: &UnitSearch,
        tile_flags: &MapTileFlags,
        town: Town,
        unit_id: UnitId,
        radius_tiles: u16,
    ) -> TownBuildInfo {
        let player = unsafe { (*town.0).player };
        let center_x = unsafe { (*town.0).position.x / 32 }.max(0) as u16;
        let center_y = unsafe { (*town.0).position.y / 32 }.max(0) as u16;
        let left = center_x.saturating_sub(radius_tiles);
        let top = center_y.saturating_sub(radius_tiles);
        let right = center_x
            .saturating_add(radius_tiles)
            .min(game.map_width_tiles());
        let bottom = center_y
            .saturating_add(radius_tiles)
            .min(game.map_height_tiles());
        let width = right - left;
        let height = bottom - top;
        let tiles = vec![BuildStatus::empty(); (width as usize) * (height as usize)];
        let mut info = TownBuildInfo {
            topleft_tile: (left, top),
            width,
            height,
            tiles,
            unit_id,
        };
        info.check_tile_flags(tile_flags);
        if let Some(main_building) = unsafe { Unit::from_ptr((*town.0).main_building) } {
            info.add_mineral_line(search, main_building);
        }
        if unit_id.require_psi() {
            info.add_powered_tiles(search, player);
        }
        info
    }

    fn add_powered_tiles(&mut self, search: &UnitSearch, player: u8) {
        assert!(PYLON_X_RAD & 0x1f == 0);
        assert!(PYLON_Y_RAD & 0x1f == 0);
        let placement = self.unit_id.placement();
        let placement_box_center_x = placement.width / 2;
        let placement_box_center_y = placement.height / 2;
        let ctx = PylonPowerContext {
            tile_offset_x: placement_box_center_x / 32,
            tile_offset_y: placement_box_center_y / 32,
            center_offset_in_tile_x: placement_box_center_x & 0x1f,
            center_offset_in_tile_y: placement_box_center_y & 0x1f,
        };

        let mut area = self.area();
        area.left = area.left.saturating_sub(PYLON_X_RAD).max(0);
        area.right = area.right.saturating_add(PYLON_X_RAD);
        area.top = area.top.saturating_sub(PYLON_Y_RAD).max(0);
        area.bottom = area.bottom.saturating_add(PYLON_Y_RAD);
        let pylons = search
            .search_iter(&area)
            .filter(|x| x.id() == unit::PYLON && x.player() == player && x.is_completed());
        for pylon in pylons {
            ctx.add_pylon(pylon.position(), self);
        }
    }

    fn add_unpowered_tiles(&mut self) {
        let placement = self.unit_id.placement();
        let width_tiles = (placement.width / 32).max(1);
        let height_tiles = (placement.height / 32).max(1);
        // Since the powered tiles only mark the topleft, and only that is actually relevant,
        // the "unpowered" means "always unpowered, regardless of topleft position".
        // A tile is always unpowered if there is no powered topleft tile anywhere in range
        // (tile_x - width_tiles + 1)..=(tile_x), ((same for y))
        for tile_y in (self.topleft_tile.1..).take(self.height as usize) {
            for tile_x in (self.topleft_tile.0..).take(self.width as usize) {
                let powered = ((tile_y.saturating_sub(height_tiles - 1))..=(tile_y)).any(|y| {
                    ((tile_x.saturating_sub(width_tiles - 1))..=(tile_x)).any(|x| {
                        if let Some(tile) = self.get_tile_mut(x, y) {
                            tile.intersects(BuildStatus::HAS_POWER)
                        } else {
                            false
                        }
                    })
                });
                if !powered {
                    if let Some(tile) = self.get_tile_mut(tile_x, tile_y) {
                        *tile |= BuildStatus::NEED_POWER;
                    }
                }
            }
        }
    }

    /// Returns the area which build info has been calculated for (Rect unit is a pixel)
    pub fn area(&self) -> bw::Rect {
        bw::Rect {
            left: (self.topleft_tile.0 * 32) as i16,
            top: (self.topleft_tile.1 * 32) as i16,
            right: ((self.topleft_tile.0 + self.width) * 32) as i16,
            bottom: ((self.topleft_tile.1 + self.height) * 32) as i16,
        }
    }

    fn check_tile_flags(&mut self, tile_flags: &MapTileFlags) {
        let left = self.topleft_tile.0;
        let top = self.topleft_tile.1;
        let mut out_pos = 0;

        let zerg = self.unit_id.races().intersects(bw_dat::RaceFlags::ZERG);
        let require_creep = self.unit_id.require_creep();
        let forbid_creep = !require_creep && !zerg;

        for y in top..(top + self.height) {
            let out = &mut self.tiles[out_pos..out_pos + self.width as usize];
            out_pos += self.width as usize;
            let flag_iter = tile_flags.iter_slice((left, y), self.width);
            for (out, tile_flags) in out.iter_mut().zip(flag_iter) {
                if tile_flags & 0x0080_0000 != 0 {
                    *out |= BuildStatus::UNBUILDABLE;
                }
                if tile_flags & 0x0800_0000 != 0 {
                    *out |= BuildStatus::BUILDING_EXISTS;
                }
                if tile_flags & 0x1000_0000 != 0 {
                    // Creep disappearing
                    if forbid_creep || require_creep {
                        *out |= BuildStatus::CREEP_DISAPPEARING;
                    }
                }
                let creep_tile = tile_flags & 0x0040_0000 != 0;
                if !creep_tile && require_creep {
                    *out |= BuildStatus::NEED_CREEP;
                }
                if creep_tile && forbid_creep {
                    *out |= BuildStatus::HAS_CREEP;
                }
            }
        }
    }

    fn get_tile_mut(&mut self, x_tile: u16, y_tile: u16) -> Option<&mut BuildStatus> {
        let relative_x = x_tile.checked_sub(self.topleft_tile.0)?;
        let relative_y = y_tile.checked_sub(self.topleft_tile.1)?;
        if relative_x >= self.width || relative_y >= self.height {
            return None;
        }
        Some(&mut self.tiles[relative_x as usize + relative_y as usize * self.width as usize])
    }

    fn add_mineral_line(&mut self, search: &UnitSearch, main_building: Unit) {
        struct MarkAsMineralLine<'a> {
            build_info: &'a mut TownBuildInfo,
        }

        impl<'a> LineDraw for MarkAsMineralLine<'a> {
            const TILE_SIZE: u32 = 32;
            fn on_tile(&mut self, x_tile: u32, y_tile: u32) {
                if let Some(tile) = self.build_info.get_tile_mut(x_tile as u16, y_tile as u16) {
                    *tile |= BuildStatus::MINERAL_LINE;
                }
            }
        }

        let region_group = unit_region_group(main_building);
        let center = main_building.position();
        let area = bw::Rect::from_point_radius(center, 32 * 12);
        for unit in search.search_iter(&area) {
            if !unit.id().is_resource_container() {
                continue;
            }
            // Check region groups so that we won't bother blocking things
            // that are behind cliffs etc
            if unit_region_group(unit) != region_group {
                continue;
            }
            let direction = point_direction(center, unit.position());
            let crect = unit.collision_rect();
            let points = match direction.wrapping_add(16) / 32 {
                // Up
                0 => [crect.bottom_left(), crect.bottom_right()],
                1 => [crect.top_left(), crect.bottom_right()],
                2 => [crect.top_left(), crect.bottom_left()],
                3 => [crect.top_right(), crect.bottom_left()],
                4 => [crect.top_right(), crect.top_left()],
                5 => [crect.bottom_right(), crect.top_left()],
                6 => [crect.bottom_right(), crect.top_right()],
                7 | _ => [crect.bottom_left(), crect.top_right()],
            };
            for point in &points {
                let mut line_draw = MarkAsMineralLine {
                    build_info: self,
                };
                let first = (point.x as u32, point.y as u32);
                let second = (center.x as u32, center.y as u32);
                draw_line(&mut line_draw, first, second);
            }
        }
    }

    /// Tile coordinates are to entire map, but panics if they aren't in the calculated area
    pub fn iter_slice<'a>(
        &'a self,
        x_tile: u16,
        y_tile: u16,
        length: u16,
    ) -> impl Iterator<Item = BuildStatus> + 'a {
        assert!(x_tile >= self.topleft_tile.0);
        assert!(x_tile + length <= self.topleft_tile.0 + self.width);
        assert!(y_tile >= self.topleft_tile.1);
        assert!(y_tile < self.topleft_tile.1 + self.height);

        let rel_y = y_tile - self.topleft_tile.1;
        let rel_x = x_tile - self.topleft_tile.0;
        let start = rel_x as usize + rel_y as usize * self.width as usize;

        self.tiles.iter().cloned().skip(start).take(length as usize)
    }
}

/// Returns 256-unit circle, 0 = up, 64 = right, 128 = down
fn point_direction(from: bw::Point, to: bw::Point) -> u8 {
    use std::f32::consts::PI;
    let x = to.x as f32 - from.x as f32;
    let y = from.y as f32 - to.y as f32; // Math y axis goes up
    let rad = y.atan2(x);
    ((rad * 128.0 / PI) * -1.0 + 128.0 + 192.0) as u16 as u8
}

#[test]
fn test_point_direction() {
    let p = |x: i16, y: i16| bw::Point {
        x,
        y,
    };
    assert_eq!(point_direction(p(50, 50), p(100, 50)), 64);
    assert_eq!(point_direction(p(100, 50), p(50, 50)), 192);
    assert_eq!(point_direction(p(100, 50), p(100, 0)), 0);
    assert_eq!(point_direction(p(50, 50), p(100, 100)), 96);
}

fn unit_region_group(unit: Unit) -> u16 {
    unsafe {
        let region = bw::get_region(unit.position()).unwrap_or(0);
        let pathing = bw::pathing();
        (*pathing).regions[region as usize].group
    }
}

// View to tile-specific flags with bounds checking
struct MapTileFlags {
    width: u16,
    height: u16,
    pointer: *mut u32,
}

impl MapTileFlags {
    fn new(game: Game) -> MapTileFlags {
        MapTileFlags {
            pointer: unsafe { *bw::tile_flags },
            width: game.map_width_tiles(),
            height: game.map_height_tiles(),
        }
    }

    /// Iterates through an horizontal slice of flags.
    /// Panics on out-of-bounds.
    fn iter_slice(&self, pos: (u16, u16), length: u16) -> impl Iterator<Item = u32> {
        assert!(pos.0 + length <= self.width);
        assert!(pos.1 < self.height);
        unsafe {
            let start = self
                .pointer
                .add(self.width as usize * pos.1 as usize + pos.0 as usize);
            (0..length).map(move |x| *start.add(x as usize))
        }
    }
}

trait LineDraw {
    const TILE_SIZE: u32;
    /// Units are tiles, even though draw_line uses pixels
    fn on_tile(&mut self, x: u32, y: u32);
}

fn draw_line<L: LineDraw>(cb: &mut L, a: (u32, u32), b: (u32, u32)) {
    let width = a.0.max(b.0) - a.0.min(b.0) + 1;
    let height = a.1.max(b.1) - a.1.min(b.1) + 1;
    if height < width {
        // Draw from left to right
        let (start, end) = match a.0 < b.0 {
            true => (a, b),
            false => (b, a),
        };
        let (mut x, mut y) = start;
        // Handle first part outside loop so that x_add can be precalculated.
        let mut next_y;
        let mut next_x;
        let go_up = end.1 < start.1;
        if go_up {
            next_y = (y / L::TILE_SIZE * L::TILE_SIZE).saturating_sub(1);
            next_x = x + (y - next_y) * width / height;
        } else {
            next_y = (y / L::TILE_SIZE * L::TILE_SIZE) + L::TILE_SIZE;
            next_x = x + (next_y - y) * width / height;
        }
        if next_x > end.0 {
            next_x = end.0;
        }
        for x_tile in (x / L::TILE_SIZE)..=(next_x / L::TILE_SIZE) {
            cb.on_tile(x_tile, y / L::TILE_SIZE);
        }
        x = next_x;
        y = next_y;
        let x_add = L::TILE_SIZE * width / height;
        while x < end.0 {
            if go_up {
                next_y = y.saturating_sub(L::TILE_SIZE);
                if next_y < end.1 {
                    next_y = end.1;
                    if next_x / L::TILE_SIZE == end.0 / L::TILE_SIZE {
                        break;
                    }
                }
            } else {
                next_y = y.saturating_add(L::TILE_SIZE);
                if next_y > end.1 {
                    next_y = end.1;
                    if next_x / L::TILE_SIZE == end.0 / L::TILE_SIZE {
                        break;
                    }
                }
            }
            next_x = x + x_add;
            if next_x > end.0 {
                next_x = end.0;
            }
            for x_tile in (x / L::TILE_SIZE)..=(next_x / L::TILE_SIZE) {
                cb.on_tile(x_tile, y / L::TILE_SIZE);
            }
            x = next_x;
            y = next_y;
        }
    } else {
        // Draw from top to bottom
        let (start, end) = match a.1 < b.1 {
            true => (a, b),
            false => (b, a),
        };
        let (mut x, mut y) = start;
        // Handle first part outside loop so that x_add can be precalculated.
        let mut next_y;
        let mut next_x;
        let go_left = end.0 < start.0;
        if go_left {
            next_x = (x / L::TILE_SIZE * L::TILE_SIZE).saturating_sub(1);
            next_y = y + (x - next_x) * height / width;
        } else {
            next_x = (x / L::TILE_SIZE * L::TILE_SIZE) + L::TILE_SIZE;
            next_y = y + (next_x - x) * height / width;
        }
        if next_y > end.1 {
            next_y = end.1;
        }
        for y_tile in (y / L::TILE_SIZE)..=(next_y / L::TILE_SIZE) {
            cb.on_tile(x / L::TILE_SIZE, y_tile);
        }
        x = next_x;
        y = next_y;
        let y_add = L::TILE_SIZE * height / width;
        while y < end.1 {
            if go_left {
                next_x = x.saturating_sub(L::TILE_SIZE);
                if next_x < end.0 {
                    next_x = end.0;
                    if next_y / L::TILE_SIZE == end.1 / L::TILE_SIZE {
                        break;
                    }
                }
            } else {
                next_x = x.saturating_add(L::TILE_SIZE);
                if next_x > end.0 {
                    next_x = end.0;
                    if next_y / L::TILE_SIZE == end.1 / L::TILE_SIZE {
                        break;
                    }
                }
            }
            next_y = y + y_add;
            if next_y > end.1 {
                next_y = end.1;
            }
            for y_tile in (y / L::TILE_SIZE)..=(next_y / L::TILE_SIZE) {
                cb.on_tile(x / L::TILE_SIZE, y_tile);
            }
            x = next_x;
            y = next_y;
        }
    }
}

// Technically these should be parsed from a grp but who changes that ever..
// Also the algorithm below breaks if these are not divisible by 32
const PYLON_X_RAD: i16 = 256;
const PYLON_Y_RAD: i16 = 160;

// 16 * 12 tiles, not that x/y are radius so they represent only 8/6 tile distances
#[cfg_attr(rustfmt, rustfmt_skip)]
static PYLON_MASK: &[u8] = &[
    0, 0, 0, 0,  0, 1, 1, 1,  1, 1, 1, 0,  0, 0, 0, 0,
    0, 0, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 0, 0,
    0, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 0,
    1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,
    1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,
    1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,
    1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,
    0, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 0,
    0, 0, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 0, 0,
    0, 0, 0, 0,  0, 1, 1, 1,  1, 1, 1, 0,  0, 0, 0, 0,
];

struct PylonPowerContext {
    // How many full tiles the unit's center will be from topleft tile
    tile_offset_x: u16,
    tile_offset_y: u16,
    // How many pixels the unit's center will be from center tile's topleft corner
    center_offset_in_tile_x: u16,
    center_offset_in_tile_y: u16,
}

impl PylonPowerContext {
    fn add_pylon(&self, unit_pos: bw::Point, info: &mut TownBuildInfo) {
        // This area contains units which would be at most powered by the pylon.
        // -1 because bw checks that abs(pylon_x - unit_x) < PYLON_X_RAD
        let area = bw::Rect {
            left: unit_pos.x.saturating_sub(PYLON_X_RAD - 1).max(0),
            top: unit_pos.y.saturating_sub(PYLON_Y_RAD - 1).max(0),
            right: unit_pos.x.saturating_add(PYLON_X_RAD),
            bottom: unit_pos.y.saturating_add(PYLON_Y_RAD),
        };
        // Since the unit's center will always be N pixels from tile's topleft position,
        // start from the next possible center pixel from the topleft corner of area.
        // (This is since the area's topleft pos may be in a different tile than next
        // possible center pixel)
        // It'll move by 31 pixels at most, but because of the -1 left/top above,
        // a 31-pixel move means that the top row/column of the pylon mask gets skipped over.
        let x_move = (self.center_offset_in_tile_x + 32 - (area.left as u16 & 0x1f)) & 0x1f;
        let y_move = (self.center_offset_in_tile_y + 32 - (area.top as u16 & 0x1f)) & 0x1f;
        let x_start = area.left + x_move as i16;
        let y_start = area.top + y_move as i16;
        // + 31 here and below is to round up when dividing
        let mask_left_skip = PYLON_X_RAD / 32 - (unit_pos.x - x_start + 31) / 32;
        let mask_top_skip = PYLON_Y_RAD / 32 - (unit_pos.y - y_start + 31) / 32;
        // Now just check which tiles are powered. The topleft tile gets marked, so subtract
        // placement box tile offset from a powered tile x/y to get the topleft tile.
        let mask_lines = PYLON_MASK
            .chunks_exact(PYLON_X_RAD as usize / 32 * 2)
            .skip(mask_top_skip as usize)
            .enumerate()
            .take((area.bottom - y_start + 31) as usize / 32);
        for (y_tile, mask_line) in mask_lines {
            let y_tile = y_tile as u16;
            let mask_values = mask_line
                .iter()
                .skip(mask_left_skip as usize)
                .enumerate()
                .take((area.right - x_start + 31) as usize / 32);
            for (x_tile, &mask_value) in mask_values {
                let x_tile = x_tile as u16;
                if mask_value != 0 {
                    let x_tile = (x_start as u16 / 32 + x_tile).checked_sub(self.tile_offset_x);
                    let y_tile = (y_start as u16 / 32 + y_tile).checked_sub(self.tile_offset_y);
                    if let (Some(x_tile), Some(y_tile)) = (x_tile, y_tile) {
                        if let Some(status) = info.get_tile_mut(x_tile, y_tile) {
                            *status |= BuildStatus::HAS_POWER;
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestDrawLine {
        missing_tiles: Vec<(u32, u32)>,
    }

    impl LineDraw for TestDrawLine {
        const TILE_SIZE: u32 = 32;

        fn on_tile(&mut self, x: u32, y: u32) {
            let pos = match self
                .missing_tiles
                .iter()
                .position(|val| val.0 == x && val.1 == y)
            {
                Some(s) => s,
                None => {
                    panic!(
                        "Drew line at {}, {}, was expecting one of {:#?}",
                        x, y, self.missing_tiles,
                    );
                }
            };
            self.missing_tiles.swap_remove(pos);
        }
    }

    fn test_line(a: (u32, u32), b: (u32, u32), tiles: Vec<(u32, u32)>) {
        let mut test = TestDrawLine {
            missing_tiles: tiles,
        };
        draw_line(&mut test, a, b);
        assert!(
            test.missing_tiles.is_empty(),
            "Left tiles {:#?}",
            test.missing_tiles
        );
    }

    #[test]
    fn line_draw_dot() {
        test_line((0, 0), (0, 0), vec![(0, 0)]);
    }

    #[test]
    fn line_draw_within_tile() {
        test_line((9, 2), (0, 0), vec![(0, 0)]);
    }

    #[test]
    fn line_draw_horizontal_only() {
        test_line((9, 2), (80, 20), vec![(0, 0), (1, 0), (2, 0)]);
    }

    #[test]
    fn line_draw_diag_horizontal1() {
        test_line((9, 35), (80, 20), vec![(0, 0), (1, 0), (2, 0), (0, 1)]);
    }

    #[test]
    fn line_draw_diag_horizontal2() {
        test_line(
            (9, 95),
            (180, 20),
            vec![
                (0, 2),
                (1, 2),
                (2, 2),
                (2, 1),
                (3, 1),
                (4, 1),
                (4, 0),
                (5, 0),
            ],
        );
    }

    #[test]
    fn line_draw_diag_horizontal3() {
        test_line(
            (9, 96),
            (180, 20),
            vec![
                (0, 3),
                (0, 2),
                (1, 2),
                (2, 2),
                (2, 1),
                (3, 1),
                (4, 1),
                (4, 0),
                (5, 0),
            ],
        );
    }

    #[test]
    fn line_draw_vertical_only() {
        test_line((9, 2), (20, 80), vec![(0, 0), (0, 1), (0, 2)]);
    }

    #[test]
    fn line_draw_diag_vertical1() {
        test_line(
            (9, 35),
            (80, 220),
            vec![
                (0, 1),
                (0, 2),
                (1, 2),
                (1, 3),
                (1, 4),
                (1, 5),
                (2, 5),
                (2, 6),
            ],
        );
    }

    #[test]
    fn line_top() {
        test_line((0, 0), (64, 0), vec![(0, 0), (1, 0), (2, 0)]);
    }

    #[test]
    fn line_left() {
        test_line((0, 0), (0, 64), vec![(0, 0), (0, 1), (0, 2)]);
    }

    #[test]
    fn line_misc_1() {
        test_line(
            (0x1580, 0x1710),
            (0x1540, 0x17a0),
            vec![
                (172, 184),
                (171, 184),
                (171, 185),
                (171, 186),
                (170, 186),
                (170, 187),
                (170, 188),
                (170, 189),
            ],
        );
    }

    #[test]
    fn line_misc_2() {
        test_line(
            (0x1580, 0x1710),
            (0x1540, 0x17bf),
            vec![
                (172, 184),
                (171, 184),
                (171, 185),
                (171, 186),
                (171, 187),
                (170, 187),
                (170, 188),
                (170, 189),
            ],
        );
    }

    fn check_pylon(ctx: &PylonPowerContext, expected: &[u8], x: i16, y: i16) {
        let rad_x = 10;
        let rad_y = 6;

        let left = (x as u16 / 32).saturating_sub(rad_x);
        let top = (y as u16 / 32).saturating_sub(rad_y);
        let right = (x as u16 / 32) + rad_x;
        let bottom = (y as u16 / 32) + rad_y;
        let width = right - left;
        let height = bottom - top;
        let mut info = TownBuildInfo {
            topleft_tile: (left, top),
            width,
            height,
            tiles: vec![BuildStatus::empty(); width as usize * height as usize],
            unit_id: unit::NONE,
        };
        ctx.add_pylon(
            bw::Point {
                x,
                y,
            },
            &mut info,
        );
        let result = info
            .tiles
            .iter()
            .map(|&x| match x != BuildStatus::empty() {
                true => 1,
                false => 0,
            })
            .collect::<Vec<u8>>();
        assert_eq!(result.len(), expected.len());

        if result != expected {
            use std::fmt::Write;

            let format_array = |msg: &mut String, arr: &[u8]| {
                let lines = arr.chunks_exact(width as usize);
                for (i, line) in lines.enumerate() {
                    for subline in line.chunks(5) {
                        for c in subline {
                            write!(msg, "{}, ", c).unwrap();
                        }
                        write!(msg, " ").unwrap();
                    }
                    writeln!(msg).unwrap();
                    if i as u16 == height - rad_y - 1 {
                        writeln!(msg).unwrap();
                    }
                }
            };
            let mut msg = String::new();
            writeln!(msg, "\nPYLON CHECK FAILED:").unwrap();
            writeln!(msg, "EXPECTED:").unwrap();
            format_array(&mut msg, expected);
            writeln!(msg).unwrap();
            writeln!(msg, "GOT:").unwrap();
            format_array(&mut msg, &result);
            panic!(msg);
        }
    }

    #[test]
    fn pylon_power_gateway() {
        let ctx = PylonPowerContext {
            tile_offset_x: 2,
            tile_offset_y: 1,
            center_offset_in_tile_x: 0,
            center_offset_in_tile_y: 16,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
            0, 0, 0, 0, 0,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,

            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 1024, 1024);
    }

    #[test]
    fn pylon_power_cybernetics_core() {
        let ctx = PylonPowerContext {
            tile_offset_x: 1,
            tile_offset_y: 1,
            center_offset_in_tile_x: 16,
            center_offset_in_tile_y: 0,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,

            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 0, 0, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 1, 1, 1, 1,  1, 1, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 1024, 1024);
    }

    #[test]
    fn pylon_power_cannon() {
        let ctx = PylonPowerContext {
            tile_offset_x: 1,
            tile_offset_y: 1,
            center_offset_in_tile_x: 0,
            center_offset_in_tile_y: 0,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,

            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 0, 0, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 1, 1, 1, 1,  1, 1, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 1024, 1024);
    }

    #[test]
    fn pylon_topleft() {
        // (gateway)
        let ctx = PylonPowerContext {
            tile_offset_x: 2,
            tile_offset_y: 1,
            center_offset_in_tile_x: 0,
            center_offset_in_tile_y: 16,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
             1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
             1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
             1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,

             1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
             1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
             1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
             1, 1, 1,  1, 0, 0, 0, 0,  0, 0, 0, 0, 0,
             0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
             0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 96, 96);
    }

    #[test]
    fn pylon_horizontal_misalign() {
        // (gateway)
        let ctx = PylonPowerContext {
            tile_offset_x: 2,
            tile_offset_y: 1,
            center_offset_in_tile_x: 0,
            center_offset_in_tile_y: 16,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
           0, 0, 0, 0, 0,  0, 1, 1, 1, 1,  1, 1, 0, 0, 0,  0, 0, 0, 0, 0,
           0, 0, 0, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
           0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
           0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
           0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
           0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,

           0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 0, 0, 0,
           0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
           0, 0, 0, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
           0, 0, 0, 0, 0,  0, 1, 1, 1, 1,  1, 1, 0, 0, 0,  0, 0, 0, 0, 0,
           0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
           0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 1023, 1024);
    }

    #[test]
    fn pylon_vertical_misalign1() {
        let ctx = PylonPowerContext {
            tile_offset_x: 2,
            tile_offset_y: 1,
            center_offset_in_tile_x: 0,
            center_offset_in_tile_y: 16,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,

            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 1024, 1024 - 16);
    }

    #[test]
    fn pylon_vertical_misalign2() {
        let ctx = PylonPowerContext {
            tile_offset_x: 2,
            tile_offset_y: 1,
            center_offset_in_tile_x: 0,
            center_offset_in_tile_y: 16,
        };
        #[cfg_attr(rustfmt, rustfmt_skip)]
        let result = &[
            0, 0, 0, 0, 0,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,

            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,
            0, 1, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,
            0, 0, 1, 1, 1,  1, 1, 1, 1, 1,  1, 1, 1, 1, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  1, 1, 1, 1, 1,  1, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,
        ];
        check_pylon(&ctx, result, 1024, 1024 - 17);
    }
}
