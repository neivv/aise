use std::borrow::Cow;

use cgmath::conv::array4x4;
use cgmath::{Matrix4, SquareMatrix};
use glium::backend::Facade;
use glium::texture::{
    ClientFormat, MipmapsOption, RawImage1d, RawImage2d, SrgbFormat, SrgbTexture1d,
    UncompressedUintFormat, UnsignedTexture2d,
};
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};
use glium::{uniform, Surface, VertexBuffer};

use super::{vertex2d, Program, Vertex2d};

// Renders the 640x480 indexed image, so that we can render on top of it.
pub struct BwRender {
    software_framebuf: Vec<u8>,
    framebuf_dirty: bool,
    image: UnsignedTexture2d,
    palette: Vec<u32>,
    palette_texture: SrgbTexture1d,
    vertices: VertexBuffer<Vertex2d>,
    program: Program,
}

impl BwRender {
    pub fn new<F: Facade>(facade: &F) -> BwRender {
        let program = compile_program!(facade, "passthrough.vert", "bw_render.frag");
        let image = UnsignedTexture2d::empty(facade, 640, 480).unwrap();
        let palette_texture = SrgbTexture1d::empty(facade, 256).unwrap();

        let square = vec![
            vertex2d(-1.0, 1.0),
            vertex2d(1.0, 1.0),
            vertex2d(-1.0, -1.0),
            vertex2d(1.0, 1.0),
            vertex2d(1.0, -1.0),
            vertex2d(-1.0, -1.0),
        ];

        let vertices = VertexBuffer::new(facade, &square).unwrap();
        BwRender {
            software_framebuf: vec![0; 640 * 480],
            framebuf_dirty: false,
            image,
            palette: vec![0; 256],
            palette_texture,
            vertices,
            program,
        }
    }

    pub fn update_palette<F: Facade>(&mut self, facade: &F, start: u32, palette: &[u32]) {
        {
            let iter = self
                .palette
                .iter_mut()
                .skip(start as usize)
                .zip(palette.iter());
            for (out, &new) in iter {
                *out = new;
            }
        }
        let image = RawImage1d {
            data: Cow::Borrowed(&self.palette),
            width: 256,
            format: ClientFormat::U8U8U8U8,
        };
        self.palette_texture = SrgbTexture1d::with_format(
            facade,
            image,
            SrgbFormat::U8U8U8U8,
            MipmapsOption::NoMipmap,
        )
        .unwrap();
    }

    pub fn draw<F: Facade, S: Surface>(&mut self, facade: &F, surface: &mut S) {
        if self.framebuf_dirty {
            let image = RawImage2d {
                data: Cow::Borrowed(&self.software_framebuf),
                width: 640,
                height: 480,
                format: ClientFormat::U8,
            };
            self.image = UnsignedTexture2d::with_format(
                facade,
                image,
                UncompressedUintFormat::U8,
                MipmapsOption::NoMipmap,
            )
            .unwrap();
        }
        let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);
        let uniforms = uniform! {
            palette: self.palette_texture.sampled()
                .magnify_filter(MagnifySamplerFilter::Nearest)
                .minify_filter(MinifySamplerFilter::Nearest),
            image: self.image.sampled()
                .magnify_filter(MagnifySamplerFilter::Nearest)
                .minify_filter(MinifySamplerFilter::Nearest),
            transform: array4x4(Matrix4::<f32>::identity()),
        };
        surface
            .draw(
                &self.vertices,
                &indices,
                self.program.glium_program(facade),
                &uniforms,
                &Default::default(),
            )
            .unwrap();
    }

    pub fn get_framebuf(&mut self) -> &mut [u8] {
        &mut self.software_framebuf
    }

    pub fn framebuf_updated(&mut self) {
        self.framebuf_dirty = true;
    }
}
