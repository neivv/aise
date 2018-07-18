use std::ptr::null_mut;
use std::slice;
use std::sync::{Mutex, MutexGuard};

use bincode;

use aiscript::{self, AttackTimeoutState, MaxWorkers, Town};
use block_alloc::BlockAllocSet;
use bw;
use idle_orders::IdleOrders;
use rng::Rng;
use unit;

lazy_static! {
    static ref GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
    static ref SAVE_STATE: Mutex<Option<SaveState>> = Mutex::new(None);
}

pub struct SaveState {
    pub first_ai_script: SendPtr<bw::AiScript>,
}

pub struct SendPtr<T>(pub *mut T);
unsafe impl<T> Send for SendPtr<T> {}

#[derive(Serialize, Deserialize)]
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
    pub rng: Rng,
    #[serde(serialize_with = "aiscript::serialize_scripts")]
    #[serde(deserialize_with = "aiscript::deserialize_scripts")]
    pub ai_scripts: BlockAllocSet<aiscript::Script>,
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
            rng: Default::default(),
            ai_scripts: BlockAllocSet::new(),
        }
    }

    // Should only be called on hook start to prevent deadlocks.
    // Should also have something to detect/typesystem level block misuse :l
    pub fn get() -> MutexGuard<'static, Globals> {
        GLOBALS.lock().unwrap()
    }
}

pub fn save_state() -> MutexGuard<'static, Option<SaveState>> {
    SAVE_STATE.lock().unwrap()
}

pub unsafe extern fn init_game() {
    *GLOBALS.lock().unwrap() = Globals::new();
}

pub unsafe extern fn wrap_save(
    data: *const u8,
    len: u32,
    _player: u32,
    _unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    trace!("Saving..");
    let mut globals = GLOBALS.lock().unwrap();
    aiscript::claim_bw_allocated_scripts(&mut globals);

    let first_ai_script = bw::first_ai_script();
    bw::set_first_ai_script(null_mut());
    defer!({
        bw::set_first_ai_script(first_ai_script);
    });
    *SAVE_STATE.lock().unwrap() = Some(SaveState {
        first_ai_script: SendPtr(first_ai_script),
    });
    drop(globals);

    orig(data, len);
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    let globals = GLOBALS.lock().unwrap();
    unit::init_save_mapping();
    aiscript::init_save_mapping();
    defer!(aiscript::clear_save_mapping());
    defer!(unit::clear_save_mapping());
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