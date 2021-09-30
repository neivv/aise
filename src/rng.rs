use std::ops::Range;

use byteorder::{WriteBytesExt, LE};
use rand::distributions::{Distribution, Uniform};
use rand::SeedableRng;
use rand_xorshift::XorShiftRng;
use serde::{Deserialize, Serialize};

use crate::bw;

// Option as this should be tied to bw's seed, but I'm not sure if the seed is set
// at game init.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Rng(Option<XorShiftRng>);

impl Rng {
    pub const fn new() -> Rng {
        Rng(None)
    }

    pub fn synced_rand(&mut self, range: Range<u32>) -> u32 {
        let rng = self.0.get_or_insert_with(|| {
            let mut buf = [0x42; 16];
            (&mut buf[..]).write_u32::<LE>(bw::rng_seed()).unwrap();
            XorShiftRng::from_seed(buf)
        });
        Uniform::from(range).sample(rng)
    }
}
