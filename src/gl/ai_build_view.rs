use std::mem;

use cgmath::conv::array4x4;
use cgmath::{vec4, Matrix4};
use glium::backend::Facade;
use glium::{uniform, Surface};

use bw_dat::UnitId;

use crate::aiscript::Town;
use crate::bw;
use crate::game::Game;
use crate::unit::Unit;
use crate::unit_search::UnitSearch;

use super::draw_shapes::{self, DrawShapes};
use super::gl_common::GlCommon;
use super::support::UiList;
use super::ui::{Page, Ui};
use super::{bw_ext, screen_rect, unit_name, Program, UiInput};

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

        let game = Game::get();
        let town_array = bw::town_array_start();
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
        self.build_info = Some(TownBuildInfo::new(
            game,
            &search,
            &tile_flags,
            Town(town),
            self.unit_id,
            0x40,
        ));
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
                if status != BuildStatus::OK {
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
}

bitflags! {
    #[derive(Deserialize, Serialize)]
    struct BuildStatus: u16 {
        const OK = 0x0;
        const UNBUILDABLE = 0x1;
        const BUILDING_EXISTS = 0x2;
        const OTHER_UNIT = 0x4;
        const NEAR_PRODUCTION = 0x8;
        const MINERAL_LINE = 0x10;
        const BLOCKS_ADDON = 0x20;
        const NEED_POWER = 0x40;
        const NEED_CREEP = 0x80;
        const HAS_CREEP = 0x100;
        const RESOURCES_BLOCKING = 0x200;
        const CREEP_DISAPPEARING = 0x400;
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
        let tiles = vec![BuildStatus::OK; (width as usize) * (height as usize)];
        let mut info = TownBuildInfo {
            topleft_tile: (left, top),
            width,
            height,
            tiles,
        };
        info.check_tile_flags(tile_flags, unit_id);
        if let Some(main_building) = Unit::from_ptr(unsafe { (*town.0).main_building }) {
            info.add_mineral_line(search, main_building);
        }
        info
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

    fn check_tile_flags(&mut self, tile_flags: &MapTileFlags, unit_id: UnitId) {
        let left = self.topleft_tile.0;
        let top = self.topleft_tile.1;
        let mut out_pos = 0;

        let zerg = unit_id.races().intersects(bw_dat::RaceFlags::ZERG);
        let require_creep = unit_id.require_creep();
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
    println!("atan({}/{}) = {}", y, x, rad);
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

impl bw::Rect {
    fn from_point_radius(point: bw::Point, radius: i16) -> bw::Rect {
        bw::Rect {
            left: point.x.saturating_sub(radius).max(0),
            right: point.x.saturating_add(radius),
            top: point.y.saturating_sub(radius).max(0),
            bottom: point.y.saturating_add(radius),
        }
    }

    fn top_left(&self) -> bw::Point {
        bw::Point {
            x: self.left,
            y: self.top,
        }
    }

    /// Note: Inclusive
    fn top_right(&self) -> bw::Point {
        bw::Point {
            x: self.right - 1,
            y: self.top,
        }
    }

    /// Note: Inclusive
    fn bottom_left(&self) -> bw::Point {
        bw::Point {
            x: self.left,
            y: self.bottom - 1,
        }
    }

    /// Note: Inclusive
    fn bottom_right(&self) -> bw::Point {
        bw::Point {
            x: self.right - 1,
            y: self.bottom - 1,
        }
    }
}

fn unit_region_group(unit: Unit) -> u16 {
    unsafe {
        let region = bw::get_region(unit.position()).unwrap_or(0);
        let pathing = *bw_ext::pathing;
        (*pathing).regions[region as usize].group
    }
}

impl Game {
    pub fn map_width_tiles(self) -> u16 {
        unsafe { (*self.0).map_width_tiles }
    }

    pub fn map_height_tiles(self) -> u16 {
        unsafe { (*self.0).map_height_tiles }
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
}
