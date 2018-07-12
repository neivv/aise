use std::slice;
use std::sync::{Mutex, MutexGuard};

use bincode;

use aiscript::{self, AttackTimeoutState, MaxWorkers, Town, CallStacks};
use bw;
use idle_orders::IdleOrders;
use rng::Rng;
use unit;

lazy_static! {
    static ref GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Globals {
    pub attack_timeouts: [AttackTimeoutState; 8],
    pub idle_orders: IdleOrders,
    pub max_workers: Vec<MaxWorkers>,
    pub under_attack_mode: [Option<bool>; 8],
    pub wait_for_resources: [bool; 8],
    // For tracking deleted towns.
    // If the tracking is updated after step_objects, it shouldn't be possible for a town
    // to be deleted and recreated in the same frame. (As recreation happens in scripts,
    // and deletion happens on last unit dying) Better solutions won't obviously hurt though.
    pub towns: Vec<Town>,
    pub call_stacks: CallStacks,
    pub rng: Rng,
}

impl Globals {
    fn new() -> Globals {
        Globals {
            attack_timeouts: [AttackTimeoutState::new(); 8],
            idle_orders: Default::default(),
            max_workers: Vec::new(),
            under_attack_mode: [None; 8],
            wait_for_resources: [true; 8],
            towns: Vec::new(),
            call_stacks: Default::default(),
            rng: Default::default(),
        }
    }

    // Should only be called on hook start to prevent deadlocks.
    // Should also have something to detect/typesystem level block misuse :l
    pub fn get() -> MutexGuard<'static, Globals> {
        GLOBALS.lock().unwrap()
    }
}

pub unsafe extern fn init_game() {
    *GLOBALS.lock().unwrap() = Globals::new();
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    let mut globals = GLOBALS.lock().unwrap();
    unit::init_save_mapping();
    defer!(unit::clear_save_mapping());
    aiscript::init_save_mapping(&mut globals);
    defer!(aiscript::clear_save_mapping());
    match bincode::serialize(&*globals) {
        Ok(o) => {
            set_data(o.as_ptr(), o.len());
        }
        Err(e) => {
            error!("Couldn't save game: {}", e);
            bw::print_text(format!("(Aise) Couldn't save game: {}", e));
        }
    }
}

pub unsafe extern fn load(ptr: *const u8, len: usize) -> u32 {
    unit::init_load_mapping();
    defer!(unit::clear_load_mapping());
    aiscript::init_load_mapping();
    defer!(aiscript::clear_load_mapping());

    let slice = slice::from_raw_parts(ptr, len);
    let data: Globals = match bincode::deserialize(slice) {
        Ok(o) => o,
        Err(e) => {
            error!("Couldn't load game: {}", e);
            return 0
        }
    };
    *GLOBALS.lock().unwrap() = data;
    1
}
