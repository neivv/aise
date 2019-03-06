use std::mem;

use cgmath::conv::array4x4;
use cgmath::{vec4, Matrix4};
use glium::backend::Facade;
use glium::{uniform, Surface};

use bw_dat::UnitId;

use crate::aiscript::Town;
use crate::bw;
use crate::game::Game;

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
        self.build_info = Some(TownBuildInfo::new(
            game,
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
