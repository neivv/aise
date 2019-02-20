use std::path::PathBuf;
use std::rc::Rc;

use cgmath::conv::{array3x3, array4x4};
use cgmath::{vec3, vec4, Matrix3, Matrix4};
use font_kit::font::Font;
use glium::backend::Facade;
use glium::index::PrimitiveType;
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};
use glium::{implement_vertex, uniform, IndexBuffer, Surface, VertexBuffer};

use super::text::Atlas;
use super::{vertex2d, Program, UiInput, Vertex2d};

struct Rect {
    left: f32,
    top: f32,
    right: f32,
    bottom: f32,
}

pub struct Ui {
    shown: bool,
    area: Rect,
    bg_program: Program,
    bg_vertices: VertexBuffer<Vertex2d>,
    pages: Vec<Page>,
    active_page: usize,
    font_data: Option<FontData>,
    input_buffer: String,
}

// Since cannot borrow the entire State as Ui is also part of it
pub struct InputBorrow<'a> {
    pub ai_scripts: &'a mut super::ai_scripts::AiScripts,
}

pub struct Page {
    name: &'static str,
    title: String,
    scroll: Scroll,
    current_indent: u32,
    lines: Vec<String>,
}

impl Page {
    pub fn clear(&mut self) {
        self.lines.clear();
    }

    pub fn push<S: Into<String>>(&mut self, val: S) {
        self.lines.push(format!(
            "{}{}",
            " ".repeat(self.current_indent as usize),
            val.into()
        ));
    }

    pub fn indent<'a>(&'a mut self, amt: u32) -> Indent<'a> {
        let old = self.current_indent;
        self.current_indent += amt;
        Indent(self, old)
    }
}

pub struct Indent<'a>(&'a mut Page, u32);

impl<'a> Indent<'a> {
    pub fn page(&mut self) -> &mut Page {
        self.0
    }
}

impl<'a> Drop for Indent<'a> {
    fn drop(&mut self) {
        self.0.current_indent = self.1;
    }
}

#[derive(Default)]
struct Scroll {
    line: u32,
    // If scroll offset is in middle of a linewrapped line, this is > 0
    subline: u32,
}

fn font_cache_path() -> PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("_ui_font_cache.bin")
}

fn cached_fonts() -> Vec<(String, PathBuf, u32)> {
    (|| {
        let mut file = std::fs::File::open(font_cache_path()).ok()?;
        bincode::deserialize_from(&mut file).ok()
    })()
    .unwrap_or_else(|| Vec::new())
}

fn cache_fonts(fonts: &[(String, PathBuf, u32)]) {
    (|| {
        let mut file = std::fs::File::create(font_cache_path()).ok()?;
        bincode::serialize_into(&mut file, fonts).ok()?;
        Some(())
    })();
}

fn default_fonts() -> Vec<Font> {
    use font_kit::family_name::FamilyName;

    debug!("Loading fonts");
    let mut font_cache = cached_fonts();
    let mut font_cache_changed = false;
    let source = font_kit::source::SystemSource::new();
    let mut fonts = Vec::new();
    let names = [
        "Lucida Console",
        "Lucida Sans Unicode",
        //"Arial Unicode MS", // Broken since rendering assumes monospace
    ];
    for &name in names.iter() {
        let handle = if let Some(pos) = font_cache.iter().position(|x| x.0 == name) {
            let &(_, ref path, index) = &font_cache[pos];
            font_kit::handle::Handle::from_path(path.clone(), index)
        } else {
            let result =
                source.select_best_match(&[FamilyName::Title(name.into())], &Default::default());
            match result {
                Ok(o) => {
                    match o {
                        font_kit::handle::Handle::Path {
                            ref path,
                            font_index,
                        } => {
                            font_cache.push((name.into(), path.clone(), font_index));
                            font_cache_changed = true;
                        }
                        _ => (),
                    }
                    o
                }
                Err(_) => continue,
            }
        };
        match handle.load() {
            Ok(o) => {
                fonts.push(o);
            }
            Err(e) => {
                warn!("Couldn't load a font: {}", e);
            }
        }
    }
    if fonts.is_empty() {
        let result = source.select_best_match(&[FamilyName::Monospace], &Default::default());
        let handle = match result {
            Ok(o) => o,
            Err(_) => return vec![],
        };
        match handle.load() {
            Ok(o) => vec![o],
            Err(e) => {
                warn!("Couldn't load a font: {}", e);
                vec![]
            }
        }
    } else {
        if font_cache_changed {
            cache_fonts(&font_cache);
        }
        fonts
    }
}

fn load_font<F: Facade>(facade: &F) -> Option<FontData> {
    let fonts = default_fonts();
    if fonts.is_empty() {
        return None;
    }
    let fonts = Rc::new(fonts);
    for font in fonts.iter() {
        debug!("Loaded font {}", font.full_name());
    }
    let atlas = Atlas::new(facade, fonts, 11.0);
    let program = compile_program!(facade, "font.vert", "font.frag");
    Some(FontData {
        atlas,
        vertices: VertexBuffer::empty(facade, 0).unwrap(),
        indices: IndexBuffer::empty(facade, PrimitiveType::TrianglesList, 0).unwrap(),
        program,
    })
}

struct FontData {
    atlas: Atlas,
    vertices: VertexBuffer<TexCoordVertex>,
    indices: IndexBuffer<u32>,
    program: Program,
}

impl Ui {
    pub fn new<F: Facade>(facade: &F) -> Ui {
        let bg_program = compile_program!(facade, "passthrough.vert", "ui_background.frag");
        let area = Rect {
            left: 0.01 / (4.0 / 3.0),
            top: 0.01,
            right: 0.75,
            bottom: 0.99,
        };
        let square = vec![
            vertex2d(-1.0, 1.0),
            vertex2d(1.0, 1.0),
            vertex2d(-1.0, -1.0),
            vertex2d(1.0, 1.0),
            vertex2d(1.0, -1.0),
            vertex2d(-1.0, -1.0),
        ];

        let bg_vertices = VertexBuffer::new(facade, &square).unwrap();
        let font_data = load_font(facade);
        Ui {
            shown: false,
            area,
            bg_program,
            bg_vertices,
            pages: Vec::new(),
            active_page: 0,
            font_data,
            input_buffer: String::new(),
        }
    }

    pub fn current_page(&self) -> &'static str {
        self.pages
            .get(self.active_page)
            .map(|x| x.name)
            .unwrap_or("")
    }

    pub fn page(&mut self, name: &'static str) -> &mut Page {
        match self.pages.iter().position(|x| x.name == name) {
            Some(pos) => &mut self.pages[pos],
            None => {
                self.pages.push(Page {
                    name,
                    title: name.into(),
                    scroll: Default::default(),
                    lines: Vec::new(),
                    current_indent: 0,
                });
                self.pages.last_mut().unwrap()
            }
        }
    }

    pub fn toggle_shown(&mut self) {
        self.shown = !self.shown;
        self.input_buffer.clear();
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    pub fn draw<F: Facade, S: Surface>(&mut self, facade: &F, surface: &mut S) {
        if !self.shown {
            return;
        }
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);
        let scale_x = self.area.right - self.area.left;
        let scale_y = self.area.bottom - self.area.top;
        let shift_x = self.area.left + self.area.right - 1.0;
        let shift_y = -(self.area.top + self.area.bottom - 1.0);
        let transform = Matrix4::from_cols(
            vec4(scale_x,   0.0,        0.0,    0.0),
            vec4(0.0,       scale_y,    0.0,    0.0),
            vec4(0.0,       0.0,        1.0,    0.0),
            vec4(shift_x,   shift_y,    0.0,    1.0),
        );
        let uniforms = uniform! {
            transform: array4x4(transform),
        };
        let params = glium::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            ..Default::default()
        };
        surface
            .draw(
                &self.bg_vertices,
                &indices,
                self.bg_program.glium_program(facade),
                &uniforms,
                &params,
            )
            .unwrap();

        let page = match self.pages.get(self.active_page) {
            Some(s) => s,
            None => return,
        };
        let font_data = match self.font_data {
            Some(ref mut s) => s,
            None => return,
        };
        let mut text_draw = TextDrawContext::new(font_data);
        let left = self.area.left * 640.0;
        let top = self.area.top * 480.0;
        let right = self.area.right * 640.0;
        let bottom = self.area.bottom * 480.0;
        text_draw.line((left + 10.0, top + 20.0), &page.title);

        let x = left + 10.0;
        let mut y = top + 50.0;
        let mut line_end_pos = page.scroll.line;
        let mut skip_sublines = page.scroll.subline;
        let width = right - 10.0 - x;
        let page_bottom = bottom - 20.0;
        let line_iter = page.lines.iter().skip(page.scroll.line as usize);
        'outer: for line in line_iter {
            let mut remaining = &line[..];
            let mut part_num = 0;
            while !remaining.is_empty() {
                let width = match part_num {
                    0 => width,
                    _ => width - 14.0,
                };
                let x = match part_num {
                    0 => x,
                    _ => x + 14.0,
                };
                let (part, rest) = split_line(remaining, width, &text_draw.font_data.atlas);
                part_num += 1;
                remaining = rest;

                if skip_sublines > 0 {
                    skip_sublines -= 1;
                    continue;
                }
                if y > page_bottom {
                    break 'outer;
                }
                text_draw.line((x, y), part);
                y += 10.0;
            }
            skip_sublines = 0;
            line_end_pos += 1;
        }
        let scroll_pos_str = format!("{}/{}", line_end_pos, page.lines.len());
        text_draw.line((right - 60.0, top + 20.0), &scroll_pos_str);

        text_draw.line((x, bottom - 5.0), &self.input_buffer);

        text_draw.render(facade, surface);
    }

    pub fn input_key(&mut self, key: i32) -> UiInput {
        use winapi::um::winuser::VK_BACK;
        if !self.shown {
            return UiInput::NotHandled;
        }
        match key {
            VK_BACK => {
                self.input_buffer.pop();
                UiInput::Handled
            }
            _ => UiInput::NotHandled,
        }
    }

    pub fn input_char(&mut self, value: char, state: &mut InputBorrow) -> UiInput {
        if !self.shown {
            return UiInput::NotHandled;
        }
        if self.page_specific_input(value, state) == UiInput::Handled {
            return UiInput::Handled;
        }
        match value {
            '0'...'9' => self.input_buffer.push(value),
            _ => {
                self.input_buffer.clear();
            }
        }
        UiInput::Handled
    }

    fn page_specific_input(&mut self, value: char, state: &mut InputBorrow) -> UiInput {
        match self.current_page() {
            "ai_scripts" => state.ai_scripts.input(value),
            _ => UiInput::NotHandled,
        }
    }
}

fn split_line<'a>(text: &'a str, width: f32, font_data: &Atlas) -> (&'a str, &'a str) {
    let mut rest = text;
    let mut width_remaining = width as u32;
    let mut len = 0;
    while !rest.is_empty() {
        let next_nonspace = match rest.char_indices().find(|x| !x.1.is_whitespace()) {
            Some(s) => s.0,
            None => break,
        };
        let next_space = (&rest[next_nonspace..])
            .char_indices()
            .find(|x| x.1.is_whitespace())
            .map(|x| next_nonspace + x.0)
            .unwrap_or(rest.len());
        let next_part = &rest[..next_space];
        let next_part_width = font_data.string_width(next_part);
        if next_part_width > width_remaining {
            // New line
            if len == 0 {
                // Can't word break, just take as much as possible
                let mut next_part = next_part;
                while !next_part.is_empty() {
                    let char_size = next_part
                        .char_indices()
                        .nth(1)
                        .map(|x| x.0)
                        .unwrap_or(next_part.len());
                    let next_char_width = font_data.string_width(&next_part[..char_size]);
                    if next_char_width > width_remaining {
                        break;
                    }
                    width_remaining -= next_char_width;
                    next_part = &next_part[char_size..];
                    len += char_size;
                }
                if len == 0 {
                    // Not even a single char could fit??
                    return ("", "");
                }
                rest = &rest[len..];
            }
            // Skip any spaces on line break
            if let Some(next_nonspace) = rest.char_indices().find(|x| !x.1.is_whitespace()) {
                rest = &rest[next_nonspace.0..];
            }
            return (&text[..len], rest);
        } else {
            width_remaining -= next_part_width;
            len += next_space;
            rest = &rest[next_space..];
        }
    }
    (text, "")
}

struct TextDrawContext<'a> {
    vertices: Vec<TexCoordVertex>,
    indices: Vec<u32>,
    font_data: &'a mut FontData,
}

impl<'a> TextDrawContext<'a> {
    fn new(font_data: &'a mut FontData) -> TextDrawContext<'a> {
        TextDrawContext {
            vertices: Vec::new(),
            indices: Vec::new(),
            font_data,
        }
    }

    // Pos is 640x480 coords, bottom left of the text string
    fn line(&mut self, pos: (f32, f32), text: &str) {
        let mut x = pos.0;
        let y = pos.1;
        let char_width = self.font_data.atlas.advance_width();
        for c in text.chars() {
            let coords = self.font_data.atlas.place_char(c);
            if coords.width_px != 0 && coords.height_px != 0 {
                let glyph_x = x + coords.offset_x as f32;
                let glyph_y = y + coords.offset_y as f32 - coords.height_px as f32;
                let glyph_r = glyph_x + coords.width_px as f32;
                let glyph_b = y + coords.offset_y as f32;
                let i = self.vertices.len() as u32;
                self.vertices.push(TexCoordVertex {
                    position: [glyph_x, glyph_y],
                    tex_coord: [coords.x as f32, coords.y as f32],
                });
                self.vertices.push(TexCoordVertex {
                    position: [glyph_r, glyph_y],
                    tex_coord: [coords.x as f32 + coords.width_px as f32, coords.y as f32],
                });
                self.vertices.push(TexCoordVertex {
                    position: [glyph_x, glyph_b],
                    tex_coord: [coords.x as f32, coords.y as f32 + coords.height_px as f32],
                });
                self.vertices.push(TexCoordVertex {
                    position: [glyph_r, glyph_b],
                    tex_coord: [
                        coords.x as f32 + coords.width_px as f32,
                        coords.y as f32 + coords.height_px as f32,
                    ],
                });
                self.indices
                    .extend([i, i + 1, i + 2, i + 1, i + 3, i + 2].iter().cloned());
            }
            x += char_width as f32;
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn render<F: Facade, S: Surface>(self, facade: &F, surface: &mut S) {
        self.font_data.vertices = VertexBuffer::new(facade, &self.vertices).unwrap();
        self.font_data.indices =
            IndexBuffer::new(facade, PrimitiveType::TrianglesList, &self.indices).unwrap();

        let texture = self.font_data.atlas.texture(facade);

        // From 640x480 coords to -1 .. 1
        let reso_w = 640.0f32;
        let reso_h = 480.0f32;
        let pos_transform = Matrix4::from_cols(
            vec4(2.0 / reso_w,  0.0,            0.0,    0.0),
            vec4(0.0,           -2.0 / reso_h,  0.0,    0.0),
            vec4(0.0,           0.0,            1.0,    0.0),
            vec4(-1.0,          1.0,            0.0,    1.0),
        );
        let tex_transform = Matrix3::from_cols(
            vec3(1.0 / texture.width() as f32,  0.0,                            0.0),
            vec3(0.0,                           1.0 / texture.height() as f32,  0.0),
            vec3(0.0,                           0.0,                            1.0),
        );
        let uniforms = uniform! {
            pos_transform: array4x4(pos_transform),
            tex_transform: array3x3(tex_transform),
            glyphs: texture.sampled()
                .magnify_filter(MagnifySamplerFilter::Nearest)
                .minify_filter(MinifySamplerFilter::Nearest),
        };
        let params = glium::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            ..Default::default()
        };
        surface
            .draw(
                &self.font_data.vertices,
                &self.font_data.indices,
                self.font_data.program.glium_program(facade),
                &uniforms,
                &params,
            )
            .unwrap();
    }
}

#[derive(Copy, Clone, Debug)]
struct TexCoordVertex {
    position: [f32; 2],
    tex_coord: [f32; 2],
}

implement_vertex!(TexCoordVertex, position, tex_coord);
