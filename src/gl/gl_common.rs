use std::borrow::Cow;

use glium::backend::Facade;
use glium::texture::{ClientFormat, RawImage2d, Texture2d};
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter, Sampler};

use super::bw_ext;

pub struct GlCommon {
    game_screen_mask: Option<Texture2d>,
}

impl GlCommon {
    pub fn new() -> GlCommon {
        GlCommon {
            game_screen_mask: None,
        }
    }

    pub fn game_init(&mut self) {
        self.game_screen_mask = None;
    }

    pub fn game_screen_mask_texture<F: Facade>(&mut self, facade: &F) -> Sampler<Texture2d> {
        let texture = self.game_screen_mask.get_or_insert_with(|| {
            let mut buf = vec![255u8; 640 * 480];
            for y in 288..480 {
                for x in 0..640 {
                    unsafe {
                        if bw_ext::is_outside_game_screen(x as u32, y as u32) != 0 {
                            buf[x + y * 640] = 0;
                        }
                    }
                }
            }
            let data = RawImage2d {
                data: Cow::Borrowed(&buf),
                width: 640,
                height: 480,
                format: ClientFormat::U8,
            };
            Texture2d::new(facade, data).unwrap()
        });
        texture
            .sampled()
            .magnify_filter(MagnifySamplerFilter::Nearest)
            .minify_filter(MinifySamplerFilter::Nearest)
    }
}
