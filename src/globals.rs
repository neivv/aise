use std::ffi::CString;
use std::ptr::null_mut;
use std::slice;
use std::sync::{Mutex, MutexGuard};

use bincode;

use bw_dat::UnitId;

use ai::GuardState;
use aiscript::{
    self, AiMode, AttackTimeoutState, MaxWorkers, PlayerMatch, Town, TownId, UnitMatch,
};
use block_alloc::BlockAllocSet;
use bw;
use idle_orders::IdleOrders;
use rng::Rng;
use swap_retain::SwapRetain;
use unit::{self, Unit};

lazy_static! {
    static ref GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
    static ref SAVE_STATE: Mutex<Option<SaveState>> = Mutex::new(None);
}

pub struct SaveState {
    pub first_ai_script: SendPtr<bw::AiScript>,
}

pub struct SendPtr<T>(pub *mut T);
unsafe impl<T> Send for SendPtr<T> {}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct KCPos {
    player1: u8,
    player2: u8,
    unit: u16,
}

impl KCPos {
    pub fn new(player1: u8, player2: u8, unit: u16) -> KCPos {
        KCPos {
            player1,
            player2,
            unit,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct KillCount {
    pos: KCPos,
    value: u32,
}

impl KillCount {
    pub fn new(pos: KCPos, value: u32) -> KillCount {
        KillCount {
            pos,
            value,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub struct RevealState {
    pub pos: bw::Rect,
    pub time: u16,
    pub reveal_type: RevealType,
    pub players: PlayerMatch,
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub enum RevealType {
    RevealFog,
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub struct BaseLayout {
    pub pos: bw::Rect,
    pub player: u8,
    pub unit_id: UnitId,
    pub amount: u8,
    pub town_id: u8,
    pub town: Town,
    pub priority: u8,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BaseLayouts {
    pub layouts: Vec<BaseLayout>,
}

impl BaseLayouts {
    pub fn try_add(&mut self, value: BaseLayout) {
        if !self.layouts.iter().any(|i| i == &value) {
            self.layouts.push(value)
        }
        self.layouts.sort_by_key(|x| !0 - x.priority);
    }

    pub fn try_remove(&mut self, value: &BaseLayout) {
        if let Some(pos) = self.layouts.iter().position(|i| i == value) {
            self.layouts.remove(pos);
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct KillsTable {
    pub kills: Vec<KillCount>,
}

impl KillsTable {
    pub fn try_add(&mut self, kt_pos: KCPos, amount: u32) {
        match self.kills.iter_mut().position(|i| i.pos == kt_pos) {
            Some(pos) => {
                let i = &mut self.kills[pos];
                i.value = i.value.saturating_add(amount);
            }
            None => {
                self.kills.push(KillCount::new(kt_pos, amount));
            }
        }
    }

    pub fn try_set(&mut self, kt_pos: KCPos, amount: u32) {
        match self.kills.iter_mut().position(|i| i.pos == kt_pos) {
            Some(pos) => {
                let i = &mut self.kills[pos];
                i.value = amount;
            }
            None => {
                self.kills.push(KillCount::new(kt_pos, amount));
            }
        }
    }

    pub fn count_kills(&mut self, p1: u8, p2: u8, uid: u16) -> u32 {
        let mut count: u32 = 0;
        for i in &self.kills {
            if i.pos.player1 == p1 && i.pos.player2 == p2 {
                if uid == i.pos.unit {
                    count += i.value;
                    break;
                }
            }
        }
        count
    }

    pub fn try_sub(&mut self, kt_pos: KCPos, amount: u32) {
        match self.kills.iter_mut().position(|i| i.pos == kt_pos) {
            Some(pos) => {
                if amount < self.kills[pos].value {
                    self.kills[pos].value -= amount;
                } else {
                    self.kills.swap_remove(pos);
                }
            }
            None => (),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub struct BunkerState {
    pub pos: bw::Rect,
    pub unit_id: UnitMatch,
    pub bunker_id: UnitMatch,
    pub quantity: u8,
    pub player: u8,
    pub bunker_quantity: u8,
    pub priority: u8,
    pub single_bunker_states: Vec<SingleBunkerState>,
}

impl BunkerState {
    pub fn count_associated_units(&mut self, bunker: Unit) -> Option<u8> {
        self.single_bunker_states
            .iter()
            .find(|link| link.bunker == bunker)
            .map(|link| link.associated_units.len() as u8)
    }

    pub fn in_list(&self, unit: Unit) -> bool {
        self.single_bunker_states
            .iter()
            .any(|state| state.associated_units.iter().any(|&x| x == unit))
    }

    pub fn add_targeter(&mut self, targeter: Unit, bunker: Unit) {
        match self
            .single_bunker_states
            .iter_mut()
            .position(|x| x.bunker == bunker)
        {
            Some(s) => self.single_bunker_states[s].associated_units.push(targeter),
            None => {
                self.single_bunker_states.push(SingleBunkerState {
                    associated_units: vec![targeter],
                    bunker,
                });
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub struct SingleBunkerState {
    pub associated_units: Vec<Unit>,
    pub bunker: Unit,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct RenameStatus {
    pub area: bw::Rect,
    pub unit_id: UnitId,
    pub name: CString,
    pub players: PlayerMatch,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RenameUnitState {
    pub states: Vec<RenameStatus>,
}

impl RenameUnitState {
    pub fn try_add(&mut self, value: RenameStatus) {
        if !self.states.iter().any(|i| i == &value) {
            self.states.push(value)
        }
        self.states.sort_by_key(|x| {
            (x.area.right.saturating_sub(x.area.left)) as u32 *
                (x.area.bottom.saturating_sub(x.area.top)) as u32
        });
    }

    pub fn try_remove(&mut self, value: &RenameStatus) {
        if let Some(pos) = self.states.iter().position(|i| i == value) {
            self.states.remove(pos);
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
pub struct BankKey {
    pub label: String,
    pub category: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BankValue {
    pub key: BankKey,
    pub value: u32,
}

impl BankValue {
    pub fn new(key: BankKey, value: u32) -> BankValue {
        BankValue {
            key,
            value,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Bank {
    pub bank_data: Vec<BankValue>,
}

impl Bank {
    pub fn reset(&mut self) {
        self.bank_data.clear();
    }

    pub fn get(&self, key: &BankKey) -> u32 {
        match self.bank_data.iter().position(|i| i.key == *key) {
            Some(pos) => self.bank_data[pos].value as u32,
            None => 0,
        }
    }

    pub fn update<F: FnOnce(u32) -> u32>(&mut self, key: BankKey, update_fn: F) {
        match self.bank_data.iter_mut().position(|i| i.key == key) {
            Some(pos) => {
                let new = update_fn(self.bank_data[pos].value);
                if new == 0 {
                    self.bank_data.swap_remove(pos);
                } else {
                    self.bank_data[pos].value = new;
                }
            }
            None => {
                let new = update_fn(0);
                if new != 0 {
                    self.bank_data.push(BankValue::new(key, new));
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BunkerCondition {
    pub bunker_states: Vec<BunkerState>,
}

impl BunkerCondition {
    pub fn add(&mut self, bunker_state: BunkerState) {
        self.bunker_states.push(bunker_state);
        self.bunker_states.sort_by_key(|x| !0 - x.priority);
    }

    pub fn in_list(&self, unit: Unit) -> bool {
        self.bunker_states.iter().any(|bs| bs.in_list(unit))
    }

    pub fn unit_removed(&mut self, unit: Unit) {
        for condition in &mut self.bunker_states {
            condition
                .single_bunker_states
                .swap_retain(|x| x.bunker.0 != unit.0);
            for link in &mut condition.single_bunker_states {
                link.associated_units.swap_retain(|x| x.0 != unit.0);
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Globals {
    pub attack_timeouts: [AttackTimeoutState; 8],
    pub idle_orders: IdleOrders,
    pub kills_table: KillsTable,
    pub base_layouts: BaseLayouts,
    pub max_workers: Vec<MaxWorkers>,
    pub town_ids: Vec<TownId>,
    pub bunker_states: BunkerCondition,
    pub renamed_units: RenameUnitState,
    pub guards: GuardState,
    pub bank: Bank,
    pub reveal_states: Vec<RevealState>,
    pub under_attack_mode: [Option<bool>; 8],
    pub ai_mode: [AiMode; 8],
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
            kills_table: Default::default(),
            base_layouts: Default::default(),
            max_workers: Vec::new(),
            town_ids: Vec::new(),
            reveal_states: Vec::new(),
            bunker_states: Default::default(),
            renamed_units: Default::default(),
            guards: GuardState::new(),
            bank: Default::default(),
            under_attack_mode: [None; 8],
            ai_mode: [Default::default(); 8],
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
            return 0;
        }
    };
    *GLOBALS.lock().unwrap() = data;
    1
}
