use std::ops::Range;
use std::sync::{Mutex, MutexGuard};

use byteorder::{WriteBytesExt, LE};
use rand::SeedableRng;
use rand::distributions::{Distribution, Uniform};
use rand::prng::XorShiftRng;

use bw;

lazy_static! {
    static ref SYNCED_RNG: Mutex<Option<Rng>> = Mutex::new(None);
}

fn synced_rng() -> MutexGuard<'static, Option<Rng>> {
    SYNCED_RNG.lock().unwrap()
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Rng(XorShiftRng);

pub fn init_rng() {
    *synced_rng() = None;
}

pub fn save_rng() -> Option<Rng> {
    synced_rng().clone()
}

pub fn load_rng(rng: Option<Rng>) {
    *synced_rng() = rng;
}

pub fn synced_rand(range: Range<u32>) -> u32 {
    let mut rng = synced_rng();
    let rng = rng.get_or_insert_with(|| {
        let mut buf = [0x42; 16];
        (&mut buf[..]).write_u32::<LE>(bw::rng_seed()).unwrap();
        Rng(XorShiftRng::from_seed(buf))
    });
    Uniform::from(range).sample(&mut rng.0)
}
