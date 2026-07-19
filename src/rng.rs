use std::ops::Range;

use rand::distr::{Distribution, Uniform};
use rand::SeedableRng;
use rand_xoshiro::Xoshiro128PlusPlus;
use serde::{Deserialize, Serialize};

use crate::bw;

// Option as this should be tied to bw's seed, but I'm not sure if the seed is set
// at game init.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Rng(Option<Xoshiro128PlusPlus>);

impl Rng {
    pub const fn new() -> Rng {
        Rng(None)
    }

    pub fn synced_rand(&mut self, range: Range<u32>) -> u32 {
        let rng = self.0.get_or_insert_with(|| {
            let mut buf = [0x42; 16];
            buf[0..4].copy_from_slice(&bw::rng_seed().to_le_bytes());
            Xoshiro128PlusPlus::from_seed(buf)
        });
        match Uniform::new(range.start, range.end) {
            Ok(s) => s.sample(rng),
            Err(_) => {
                debug_assert!(false, "Invalid range to synced_rand");
                range.start
            }
        }
    }
}
