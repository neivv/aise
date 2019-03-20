use std::borrow::Cow;
use std::rc::Rc;

use font_kit::canvas::{self, Canvas, RasterizationOptions};
use font_kit::font::Font;
use font_kit::hinting::HintingOptions;
use fxhash::FxHashMap;
use glium::backend::Facade;
use glium::texture::{ClientFormat, RawImage2d, Texture2d};

// 0,0 is at top left of atlas texture, so it has to be converted.
#[derive(Copy, Clone)]
pub struct BufferCoord {
    pub x: u16,
    pub y: u16,
    pub width_px: u8,
    pub height_px: u8,
    pub offset_x: i8,
    pub offset_y: i8,
}

struct BufferCoordMap {
    // FIXME: Should store glyph ids so chars with equal glyphs don't waste memory
    ascii: Vec<Option<BufferCoord>>,
    other: FxHashMap<char, BufferCoord>,
}

impl BufferCoordMap {
    fn get(&self, c: char) -> Option<BufferCoord> {
        if c.is_ascii() {
            self.ascii[c as u8 as usize]
        } else {
            self.other.get(&c).cloned()
        }
    }

    fn insert(&mut self, c: char, coord: BufferCoord) {
        if c.is_ascii() {
            self.ascii[c as u8 as usize] = Some(coord);
        } else {
            self.other.insert(c, coord);
        }
    }
}

struct State {
    texture_data: Vec<u8>,
    placed_chars: BufferCoordMap,
    insert_pos: (u32, u32),
    next_line_y: u32,
    width: u32,
    height: u32,
    dirty: bool,
    point_size: f32,
}

pub struct Atlas {
    fonts: Rc<Vec<Font>>,
    state: State,
    texture: Texture2d,
}

impl Atlas {
    pub fn new<F: Facade>(facade: &F, fonts: Rc<Vec<Font>>, point_size: f32) -> Atlas {
        let width = 256;
        let height = 256;
        Atlas {
            fonts,
            texture: Texture2d::empty(facade, width, height).unwrap(),
            state: State {
                texture_data: vec![0; width as usize * height as usize],
                next_line_y: 0,
                insert_pos: (0, 0),
                width,
                height,
                placed_chars: BufferCoordMap {
                    ascii: vec![None; 128],
                    other: Default::default(),
                },
                dirty: false,
                point_size,
            },
        }
    }

    pub fn place_char(&mut self, c: char) -> BufferCoord {
        if let Some(coord) = self.state.placed_chars.get(c) {
            return coord;
        }
        for font in self.fonts.iter() {
            if let Some(coord) = self.state.place_char(font, c) {
                return coord;
            }
        }
        if let Some(font) = self.fonts.get(0) {
            self.state.place_glyph(font, c, 0)
        } else {
            let coord = BufferCoord {
                x: 0,
                y: 0,
                width_px: 0,
                height_px: 0,
                offset_x: 0,
                offset_y: 0,
            };
            self.state.placed_chars.insert(c, coord);
            coord
        }
    }

    pub fn texture<F: Facade>(&mut self, facade: &F) -> &Texture2d {
        if self.state.dirty {
            let data = RawImage2d {
                data: Cow::Borrowed(&self.state.texture_data),
                width: self.state.width,
                height: self.state.height,
                format: ClientFormat::U8,
            };
            self.texture = Texture2d::new(facade, data).unwrap();
            self.state.dirty = false;
        }
        &self.texture
    }

    // Bad, code should not assume monospaced (But could fast case that)
    pub fn advance_width(&self) -> u32 {
        self.fonts
            .get(0)
            .and_then(|font| {
                let metrics = font.metrics();
                font.advance(0).ok().map(|vec| {
                    (self.state.point_size * vec.x / metrics.units_per_em as f32).ceil() as u32
                })
            })
            .unwrap_or(1)
    }

    pub fn string_width(&self, string: &str) -> u32 {
        let advance_width = self.advance_width();
        string.chars().count() as u32 * advance_width
    }
}

impl State {
    pub fn place_char(&mut self, font: &Font, c: char) -> Option<BufferCoord> {
        let glyph = match font.glyph_for_char(c) {
            Some(0) | None => {
                debug!("No glyph for '{}' in {}", c, font.full_name());
                return None;
            }
            Some(s) => s,
        };
        Some(self.place_glyph(font, c, glyph))
    }

    pub fn place_glyph(&mut self, font: &Font, c: char, glyph: u32) -> BufferCoord {
        let bounds = font.raster_bounds(
            glyph,
            self.point_size,
            &euclid::Point2D::new(0.0, 0.0),
            HintingOptions::None,
            RasterizationOptions::Bilevel,
        );
        let bounds = bounds.unwrap_or_else(|_| {
            euclid::Rect::new(euclid::Point2D::new(0, 0), euclid::Size2D::new(1, 1))
        });

        let glyph_width = (bounds.max_x() - bounds.min_x()) as u32;
        let glyph_height = (bounds.max_y() - bounds.min_y()) as u32;
        if glyph_width == 0 || glyph_height == 0 {
            let coord = BufferCoord {
                x: 0,
                y: 0,
                width_px: 0,
                height_px: 0,
                offset_x: 0,
                offset_y: 0,
            };
            self.placed_chars.insert(c, coord);
            return coord;
        }
        assert!(glyph_width < self.width);
        let pos = if self.insert_pos.0 + glyph_width <= self.width {
            // Append to the current line
            let pos = self.insert_pos;
            self.insert_pos.0 += glyph_width;
            self.next_line_y = self.next_line_y.max(pos.1 + glyph_height);
            pos
        } else {
            // Start a new line
            self.insert_pos = (glyph_width, self.next_line_y);
            self.next_line_y += glyph_height;
            (0, self.next_line_y)
        };
        if pos.1 + glyph_height > self.height {
            // Double height
            self.height *= 2;
            let new_len = self.texture_data.len() * 2;
            self.texture_data.resize(new_len, 0);
            debug!("Resized font atlas to {} x {}", self.width, self.height);
            assert!(pos.1 + glyph_height <= self.height);
        }
        debug!("Bounds for '{}': {:?}, rendering to {:?}", c, bounds, pos);
        let mut canvas = Canvas::new(
            &euclid::Size2D::new(glyph_width, glyph_height),
            canvas::Format::A8,
        );
        let result = font.rasterize_glyph(
            &mut canvas,
            glyph,
            self.point_size,
            &euclid::Point2D::new(0.0, 0.0),
            HintingOptions::None,
            RasterizationOptions::Bilevel,
        );
        for y in 0..glyph_height {
            let canvas_pos = canvas.stride * y as usize;
            let out_pos = ((pos.1 + y) * self.width + pos.0) as usize;
            let slice = &canvas.pixels[canvas_pos..canvas_pos + glyph_width as usize];
            let out_slice = &mut self.texture_data[out_pos..out_pos + glyph_width as usize];
            out_slice.copy_from_slice(slice);
        }
        if let Err(e) = result {
            error!("Error rasterizing '{}': {}", c, e);
        }
        let coord = BufferCoord {
            x: pos.0 as u16,
            y: pos.1 as u16,
            width_px: glyph_width as u8,
            height_px: glyph_height as u8,
            offset_x: 0,
            offset_y: 0 - bounds.min_y() as i8,
        };
        self.placed_chars.insert(c, coord);
        self.dirty = true;
        coord
    }
}
