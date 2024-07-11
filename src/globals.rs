use std::ffi::CString;
use std::ptr::null_mut;
use std::slice;

use hashbrown::HashMap;
use rustc_hash::{FxBuildHasher};
use scopeguard::defer;
use serde::{Deserialize, Serialize};

use bw_dat::{Unit, UnitId};

use crate::ai::GuardState;
use crate::aiscript::{
    self, AiMode, AttackTimeoutState, MaxWorkers, PlayerMatch, Town, TownId, UnitMatch,
    GlobalAiMode,
};
use crate::block_alloc::BlockAllocSet;
use crate::bw;
use crate::idle_orders::IdleOrders;
use crate::pathing::RegionNeighbourSearch;
use crate::recurse_checked_mutex::{Mutex, MutexGuard};
use crate::rng::Rng;
use crate::swap_retain::SwapRetain;
use crate::unit::{SerializableUnit};

static GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
static SAVE_STATE: Mutex<Option<SaveState>> = Mutex::new(None);

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

#[derive(Debug, Serialize, Deserialize, Copy, Clone, Eq, PartialEq)]
pub enum RevealType {
    RevealFog,
    RevealFull,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaseLayouts {
    pub layouts: Vec<BaseLayout>,
}

impl BaseLayouts {
    const fn new() -> BaseLayouts {
        BaseLayouts {
            layouts: Vec::new(),
        }
    }

    pub fn try_add(&mut self, value: BaseLayout) {
        if !self.layouts.iter().any(|i| i == &value) {
            let insert_pos = self
                .layouts
                .binary_search_by(|a| value.priority.cmp(&a.priority))
                .unwrap_or_else(|e| e);
            self.layouts.insert(insert_pos, value)
        }
    }

    pub fn try_remove(&mut self, value: &BaseLayout) {
        if let Some(pos) = self.layouts.iter().position(|i| i == value) {
            self.layouts.remove(pos);
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Eq, PartialEq)]
pub struct BuildMax {
    pub town: Option<Town>,
    pub quantity: u16,
    pub unit_id: UnitId,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Eq, PartialEq)]
pub struct UnitQueue {
    pub player: u8,
    pub current_quantity: u8,
    pub unit_id: UnitId,
    pub factory_id: UnitId,
    pub town: Option<Town>,
    pub pos: bw::Rect,
    pub priority: u8,
}

impl UnitQueue {
    pub unsafe fn can_train(&mut self, unit: Unit) -> bool {
        use bw_dat::order::{ARCHON_WARP, DARK_ARCHON_MELD, TRAIN, UNIT_MORPH};
        if unit.id() != self.factory_id || unit.player() != self.player {
            return false;
        }
        let is_train_order = match unit.order() {
            UNIT_MORPH | ARCHON_WARP | DARK_ARCHON_MELD => true,
            _ => unit.secondary_order() == TRAIN,
        };
        if is_train_order {
            return false;
        }

        if unit.is_air() && unit.id().is_building() {
            return false;
        }
        use bw_dat::unit::*;
        if !unit.id().is_building() && unit.id() != LARVA {
            return true;
        }

        match &mut self.town {
            None => return true,
            Some(s) => return s.has_building(unit),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Queues {
    pub queue: Vec<UnitQueue>,
}

impl Queues {
    const fn new() -> Queues {
        Queues {
            queue: Vec::new(),
        }
    }

    pub fn add(&mut self, value: UnitQueue) {
        if !self.queue.iter().any(|i| i == &value) {
            let insert_pos = self
                .queue
                .binary_search_by(|a| value.priority.cmp(&a.priority))
                .unwrap_or_else(|e| e);
            self.queue.insert(insert_pos, value)
        }
    }
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildMaxSet {
    pub buildmax: Vec<BuildMax>,
}
impl BuildMaxSet {
    const fn new() -> BuildMaxSet {
        BuildMaxSet {
            buildmax: Vec::new(),
        }
    }

    pub fn add(&mut self, value: BuildMax) {
        if !self.buildmax.iter().any(|i| i == &value) {
            let insert_pos = self
                .buildmax
                .binary_search_by(|a| value.unit_id.0.cmp(&a.unit_id.0))
                .unwrap_or_else(|e| e);
            self.buildmax.insert(insert_pos, value)
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Eq, PartialEq)]
pub struct LiftLandState {
    pub unit: SerializableUnit,
    pub stage: LiftLandStage,
    pub is_returning: bool,
    pub target: bw::Point,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Eq, PartialEq)]
pub struct LiftLandBuilding {
    pub player: u8,
    pub unit_id: UnitId,
    pub src: bw::Rect,
    pub tgt: bw::Rect,
    pub town_src: Town,
    pub town_tgt: Town,
    pub return_hp_percent: u8,
    pub state: Option<LiftLandState>,
}

#[cfg(target_pointer_width = "32")]
impl LiftLandBuilding {
    pub fn stage(&mut self) -> LiftLandStage {
        match self.state {
            Some(s) => s.stage,
            None => LiftLandStage::LiftOff_Start,
        }
    }

    pub fn init_state(&mut self, unit: Unit) {
        self.state = Some(LiftLandState {
            unit: SerializableUnit(unit),
            stage: LiftLandStage::LiftOff_Start,
            is_returning: false,
            target: bw::Point {
                x: 0,
                y: 0,
            },
        });
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Eq, PartialEq)]
#[allow(non_camel_case_types)]
pub enum LiftLandStage {
    LiftOff_Start,
    LiftOff_End,
    Fly,
    FindLocation,
    Land,
    End,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiftLand {
    pub structures: Vec<LiftLandBuilding>,
}

impl LiftLand {
    const fn new() -> LiftLand {
        LiftLand {
            structures: Vec::new(),
        }
    }

    pub fn add(&mut self, value: LiftLandBuilding, amount: u8) {
        for _ in 0..amount {
            self.structures.push(value);
        }
    }

    pub fn unit_removed(&mut self, unit: Unit) {
        for liftland in &mut self.structures {
            let state = liftland.state;
            if let Some(state) = state {
                if state.unit.0 == unit {
                    liftland.state = None;
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KillsTable {
    pub kills: Vec<KillCount>,
}

impl KillsTable {
    const fn new() -> KillsTable {
        KillsTable {
            kills: Vec::new(),
        }
    }

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
pub struct BunkerDecl {
    pub pos: bw::Rect,
    pub unit_id: UnitMatch,
    pub bunker_id: UnitMatch,
    pub quantity: u8,
    pub player: u8,
    pub bunker_quantity: u8,
    pub priority: u8,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct BunkerState {
    single_bunker_states: Vec<SingleBunkerState>,
}

impl BunkerState {
    pub fn add_targeter(&mut self, targeter: Unit, bunker: Unit) {
        let targeter = SerializableUnit(targeter);
        match self
            .single_bunker_states
            .iter_mut()
            .position(|x| x.bunker.0 == bunker)
        {
            Some(s) => self.single_bunker_states[s].associated_units.push(targeter),
            None => {
                self.single_bunker_states.push(SingleBunkerState {
                    associated_units: vec![targeter],
                    bunker: SerializableUnit(bunker),
                });
            }
        }
    }

    fn unit_removed(&mut self, unit: Unit) {
        self.single_bunker_states
            .swap_retain(|x| x.bunker.0 != unit);
        for link in &mut self.single_bunker_states {
            link.associated_units.swap_retain(|x| x.0 != unit);
        }
    }

    pub fn count_associated_units(&self, bunker: Unit) -> Option<u8> {
        self.single_bunker_states
            .iter()
            .find(|link| link.bunker.0 == bunker)
            .map(|link| link.associated_units.len() as u8)
    }

    pub fn in_list(&self, unit: Unit) -> bool {
        self.single_bunker_states
            .iter()
            .any(|state| state.associated_units.iter().any(|&x| x.0 == unit))
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Eq, PartialEq)]
pub struct SingleBunkerState {
    pub associated_units: Vec<SerializableUnit>,
    pub bunker: SerializableUnit,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct RenameStatus {
    pub area: bw::Rect,
    pub unit_id: UnitId,
    pub name: CString,
    pub players: PlayerMatch,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenameUnitState {
    pub states: Vec<RenameStatus>,
}

impl RenameUnitState {
    const fn new() -> RenameUnitState {
        RenameUnitState {
            states: Vec::new(),
        }
    }

    pub fn try_add(&mut self, value: RenameStatus) {
        fn area(x: &bw::Rect) -> u32 {
            (x.right.saturating_sub(x.left) as u32) * (x.bottom.saturating_sub(x.top) as u32)
        }

        if !self.states.iter().any(|i| i == &value) {
            let area_size = area(&value.area);
            let insert_pos = self
                .states
                .binary_search_by_key(&area_size, |x| area(&x.area))
                .unwrap_or_else(|e| e);

            self.states.insert(insert_pos, value)
        }
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bank {
    pub bank_data: Vec<BankValue>,
}

impl Bank {
    const fn new() -> Bank {
        Bank {
            bank_data: Vec::new(),
        }
    }

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BunkerCondition {
    pub bunker_states: Vec<(BunkerDecl, BunkerState)>,
}

impl BunkerCondition {
    const fn new() -> BunkerCondition {
        BunkerCondition {
            bunker_states: Vec::new(),
        }
    }

    pub fn add(&mut self, decl: BunkerDecl) {
        let insert_pos = self
            .bunker_states
            .binary_search_by(|a| decl.priority.cmp(&a.0.priority))
            .unwrap_or_else(|e| e);
        self.bunker_states
            .insert(insert_pos, (decl, BunkerState::default()));
    }

    pub fn in_list(&self, unit: Unit) -> bool {
        self.bunker_states
            .iter()
            .any(|(_, state)| state.in_list(unit))
    }

    pub fn unit_removed(&mut self, unit: Unit) {
        for (_, state) in &mut self.bunker_states {
            state.unit_removed(unit);
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct ReplaceValue {
    pub first: UnitId,
    pub second: UnitId,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitReplace {
    pub unit_data: Vec<ReplaceValue>,
}

impl UnitReplace {
    const fn new() -> UnitReplace {
        UnitReplace {
            unit_data: Vec::new(),
        }
    }

    pub fn replace_check(&self, unit_id: UnitId) -> UnitId {
        for u in &self.unit_data {
            if u.first == unit_id {
                return u.second;
            }
        }
        return unit_id;
    }

    pub fn add(&mut self, first: UnitId, second: UnitId) {
        let replace_value = ReplaceValue {
            first: first,
            second: second,
        };
        self.unit_data.push(replace_value);
    }
}

/// Cycles through region ids, so each player do one heavy region-specific check per frame
/// to distribute load.
#[derive(Serialize, Deserialize)]
pub struct RegionIdCycle {
    pos: [u16; 8],
    region_count: u16,
}

impl RegionIdCycle {
    const fn new() -> RegionIdCycle {
        RegionIdCycle {
            pos: [0; 8],
            region_count: 0,
        }
    }

    pub fn is_inited(&self) -> bool {
        self.region_count != 0
    }

    pub fn init(&mut self, region_count: u16) {
        self.pos = [0; 8];
        self.region_count = region_count;
    }

    /// Returns player id and region id iter for the player.
    /// If the region id iter is not fully consumed, it'll continue next time from where
    /// it left off. Otherwise it'll reset to 0.
    pub fn cycle_all<'a>(&'a mut self) -> impl Iterator<Item = (u8, RegionCycleIter<'a>)> {
        let limit = self.region_count;
        self.pos.iter_mut().enumerate().map(move |(player, pos)| {
            let iter = RegionCycleIter {
                pos,
                limit,
            };
            (player as u8, iter)
        })
    }
}

pub struct RegionCycleIter<'a> {
    pos: &'a mut u16,
    limit: u16,
}

impl<'a> Iterator for RegionCycleIter<'a> {
    type Item = u16;
    fn next(&mut self) -> Option<Self::Item> {
        if *self.pos >= self.limit {
            *self.pos = 0;
            None
        } else {
            let result = *self.pos;
            *self.pos += 1;
            Some(result)
        }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Globals {
    pub attack_timeouts: [AttackTimeoutState; 8],
    pub idle_orders: IdleOrders,
    pub kills_table: KillsTable,
    pub base_layouts: BaseLayouts,
    pub lift_lands: LiftLand,
    pub queues: Queues,
    pub build_max: BuildMaxSet,
    pub max_workers: Vec<MaxWorkers>,
    pub town_ids: Vec<TownId>,
    pub bunker_states: BunkerCondition,
    pub renamed_units: RenameUnitState,
    pub guards: GuardState,
    pub bank: Bank,
    pub unit_replace: UnitReplace,
    pub reveal_states: Vec<RevealState>,
    pub under_attack_mode: [Option<bool>; 8],
    pub ai_mode: [AiMode; 8],
    pub global_ai_mode: GlobalAiMode,
    pub region_safety_pos: RegionIdCycle,
    pub build_at: HashMap<Town, Vec<BuildAt>, FxBuildHasher>,
    // For tracking deleted towns.
    // If the tracking is updated after step_objects, it shouldn't be possible for a town
    // to be deleted and recreated in the same frame. (As recreation happens in scripts,
    // and deletion happens on last unit dying) Better solutions won't obviously hurt though.
    pub towns: Vec<Town>,
    pub rng: Rng,
    #[serde(serialize_with = "aiscript::serialize_scripts")]
    #[serde(deserialize_with = "aiscript::deserialize_scripts")]
    pub ai_scripts: BlockAllocSet<aiscript::Script>,
    #[serde(skip)]
    pub region_search: RegionNeighbourSearch,
}

#[derive(Serialize, Deserialize)]
pub struct BuildAt {
    pub unit_match: UnitMatch,
    pub flags: u32,
    pub position: bw::Point,
}

impl Globals {
    const fn new() -> Globals {
        Globals {
            attack_timeouts: [AttackTimeoutState::new(); 8],
            idle_orders: IdleOrders::new(),
            kills_table: KillsTable::new(),
            base_layouts: BaseLayouts::new(),
            lift_lands: LiftLand::new(),
            queues: Queues::new(),
            build_max: BuildMaxSet::new(),
            max_workers: Vec::new(),
            town_ids: Vec::new(),
            reveal_states: Vec::new(),
            bunker_states: BunkerCondition::new(),
            renamed_units: RenameUnitState::new(),
            guards: GuardState::new(),
            bank: Bank::new(),
            unit_replace: UnitReplace::new(),
            under_attack_mode: [None; 8],
            ai_mode: [AiMode::default(); 8],
            global_ai_mode: GlobalAiMode::default(),
            region_safety_pos: RegionIdCycle::new(),
            build_at: HashMap::with_hasher(FxBuildHasher),
            towns: Vec::new(),
            rng: Rng::new(),
            ai_scripts: BlockAllocSet::new(),
            region_search: RegionNeighbourSearch::new(),
        }
    }

    // Should only be called on hook start to prevent deadlocks.
    // Inline never since it keeps getting inlined and lazy_static init code is fat ;_;
    #[inline(never)]
    pub fn get(caller: &'static str) -> MutexGuard<'static, Globals> {
        GLOBALS.lock(caller)
    }

    pub fn unit_removed(&mut self, unit: Unit) {
        self.idle_orders.unit_removed(unit);
        self.bunker_states.unit_removed(unit);
        self.lift_lands.unit_removed(unit);
    }
}

pub fn save_state(caller: &'static str) -> MutexGuard<'static, Option<SaveState>> {
    SAVE_STATE.lock(caller)
}

pub unsafe extern fn init_game() {
    aiscript::invalidate_cached_unit_search();
    bw::reset_ai_scripts();
    *Globals::get("init") = Globals::new();
}

pub unsafe extern fn wrap_save(
    data: *const u8,
    len: u32,
    _player: u32,
    _unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    trace!("Saving..");
    let mut globals = Globals::get("before save");
    aiscript::claim_bw_allocated_scripts(&mut globals);

    let first_ai_script = bw::first_ai_script();
    bw::set_first_ai_script(null_mut());
    defer!({
        bw::set_first_ai_script(first_ai_script);
    });
    *save_state("init save state") = Some(SaveState {
        first_ai_script: SendPtr(first_ai_script),
    });
    drop(globals);

    orig(data, len);
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    let globals = Globals::get("save");
    aiscript::init_save_mapping();
    defer!(aiscript::clear_save_mapping());
    match bincode::serialize(&*globals) {
        Ok(o) => {
            set_data(o.as_ptr(), o.len());
        }
        Err(e) => {
            error!("Couldn't save game: {}", e);
            bw_print!("(Aise) Couldn't save game: {}", e);
        }
    }
}

pub unsafe extern fn load(ptr: *const u8, len: usize) -> u32 {
    aiscript::invalidate_cached_unit_search();
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
    *Globals::get("load") = data;
    1
}
