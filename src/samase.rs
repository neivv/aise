use std::mem;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};

use libc::c_void;
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use samase_plugin::{FuncId, PluginApi, VarId};

use crate::bw;
use crate::order::OrderId;
use crate::unit::UnitId;
use crate::windows;

struct GlobalFunc<T: Copy>(AtomicUsize, std::marker::PhantomData<T>);

impl<T: Copy> GlobalFunc<T> {
    const fn new() -> GlobalFunc<T> {
        GlobalFunc(AtomicUsize::new(0), std::marker::PhantomData)
    }

    fn set(&self, val: T) {
        unsafe {
            self.0.store(mem::transmute_copy(&val), Ordering::Relaxed);
        }
    }

    fn get(&self) -> T {
        unsafe {
            let value = self.0.load(Ordering::Relaxed);
            debug_assert!(value != 0);
            mem::transmute_copy(&value)
        }
    }

    fn get_opt(&self) -> Option<T> {
        unsafe {
            mem::transmute_copy::<usize, Option<T>>(&self.0.load(Ordering::Relaxed))
        }
    }

    fn try_init(&self, val: Option<*mut c_void>) -> bool {
        let val = match val {
            Some(s) => s,
            None => return false,
        };
        unsafe {
            let value: usize = mem::transmute(val);
            self.0.store(value, Ordering::Relaxed);
        }
        true
    }

    fn init(&self, val: Option<*mut c_void>, desc: &str) {
        if !self.try_init(val) {
            fatal(&format!("Can't get {}", desc));
        }
    }
}

fn fatal(text: &str) -> ! {
    let msg = format!("This StarCraft version is not supported :(\n({})", text);
    windows::message_box("Aiscript extension plugin", &msg);
    unsafe {
        TerminateProcess(GetCurrentProcess(), 0x4230daef);
    }
    unreachable!();
}

const GLOBALS: &[VarId] = &[
    VarId::Game,
    VarId::FirstActiveUnit,
    VarId::FirstHiddenUnit,
    VarId::Players,
    VarId::MapTileFlags,
    VarId::Pathing,
    VarId::AiRegions,
    VarId::PlayerAi,
    VarId::ActiveAiTowns,
    // Writable
    VarId::FirstAiScript,
    VarId::FirstFreeAiScript,
    // Optional
    VarId::RngSeed,
    // Writable + Optional
];

const fn global_idx(var: VarId) -> usize {
    let mut i = 0;
    loop {
        if GLOBALS[i] as u16 == var as u16 {
            return i;
        }
        i += 1;
    }
}

const FIRST_WRITABLE_GLOBAL: usize = global_idx(VarId::FirstAiScript);
const FIRST_OPTIONAL_GLOBAL: usize = global_idx(VarId::RngSeed);

const ZERO_U8: AtomicU8 = AtomicU8::new(0);
static OPT_GLOBALS: [AtomicU8; GLOBALS.len() - FIRST_OPTIONAL_GLOBAL] = [
    ZERO_U8; GLOBALS.len() - FIRST_OPTIONAL_GLOBAL
];

unsafe fn init_globals(api: *const samase_plugin::PluginApi) {
    let mut result = [0u8; GLOBALS.len()];
    ((*api).load_vars)(GLOBALS.as_ptr() as *const u16, result.as_mut_ptr(), GLOBALS.len());
    let mut i = 0;
    for (last, needed) in [(FIRST_WRITABLE_GLOBAL, 2), (FIRST_OPTIONAL_GLOBAL, 3)] {
        while i < last {
            if result[i] < needed {
                if result[i] == 1 {
                    fatal(
                        &format!("Newer samase is required (Failed to get variable {:?})", GLOBALS[i])
                    );
                } else {
                    fatal(&format!("Failed to get variable {:?}", GLOBALS[i]));
                }
            }
            i += 1;
        }
    }
    while i < GLOBALS.len() {
        OPT_GLOBALS[i - FIRST_OPTIONAL_GLOBAL].store(result[i], Ordering::Relaxed);
        i += 1;
    }
}

static READ_VARS:
    GlobalFunc<unsafe extern fn(*const u16, *mut usize, usize)> = GlobalFunc::new();
fn read_var(var: VarId) -> usize {
    unsafe {
        let var = var as u16;
        let mut out = 0usize;
        READ_VARS.get()(&var, &mut out, 1);
        out
    }
}

static WRITE_VARS:
    GlobalFunc<unsafe extern fn(*const u16, *const usize, usize)> = GlobalFunc::new();
fn write_var(var: VarId, value: usize) {
    unsafe {
        let var = var as u16;
        WRITE_VARS.get()(&var, &value, 1);
    }
}

/// Returns result from samase api (0/1 = bad, 2 = read-only, 3 = read/write)
macro_rules! opt_global {
    ($id:expr) => {{
        const IDX: usize = global_idx($id) - FIRST_OPTIONAL_GLOBAL;
        OPT_GLOBALS[IDX].load(Ordering::Relaxed)
    }}
}

static CRASH_WITH_MESSAGE: GlobalFunc<unsafe extern fn(*const u8) -> !> = GlobalFunc::new();
pub fn crash_with_message(msg: &str) -> ! {
    let msg = format!("{}\0", msg);
    unsafe { CRASH_WITH_MESSAGE.get()(msg.as_bytes().as_ptr()) }
}

pub fn game() -> *mut bw::Game {
    read_var(VarId::Game) as _
}

pub fn ai_regions(player: u32) -> *mut bw::AiRegion {
    assert!(player < 8);
    unsafe {
        *(read_var(VarId::AiRegions) as *mut *mut bw::AiRegion).add(player as usize)
    }
}

pub fn player_ai(player: u32) -> *mut bw::PlayerAiData {
    assert!(player < 8);
    unsafe {
        (read_var(VarId::PlayerAi) as *mut bw::PlayerAiData).add(player as usize)
    }
}

pub fn first_active_unit() -> *mut bw::Unit {
    read_var(VarId::FirstActiveUnit) as _
}

pub fn first_hidden_unit() -> *mut bw::Unit {
    read_var(VarId::FirstHiddenUnit) as _
}

pub fn first_ai_script() -> *mut bw::AiScript {
    read_var(VarId::FirstAiScript) as _
}

pub fn set_first_ai_script(value: *mut bw::AiScript) {
    write_var(VarId::FirstAiScript, value as usize);
}

pub fn first_free_ai_script() -> *mut bw::AiScript {
    read_var(VarId::FirstFreeAiScript) as _
}

pub fn set_first_free_ai_script(value: *mut bw::AiScript) {
    write_var(VarId::FirstFreeAiScript, value as usize);
}

pub fn guard_ais() -> *mut bw::AiListHead<1000, bw::GuardAi> {
    read_var(VarId::FirstGuardAi) as _
}

pub fn pathing() -> *mut bw::Pathing {
    read_var(VarId::Pathing) as _
}

pub fn active_towns() -> *mut bw::AiListHead<100, bw::AiTown> {
    read_var(VarId::ActiveAiTowns) as _
}

static GET_REGION: GlobalFunc<extern fn(u32, u32) -> u32> = GlobalFunc::new();
pub fn get_region(x: u32, y: u32) -> u32 {
    GET_REGION.get()(x, y)
}

static DAT_REQUIREMENTS: GlobalFunc<extern fn(u32, u32) -> *const u16> = GlobalFunc::new();
pub fn requirements(ty: u32, id: u32) -> *const u16 {
    DAT_REQUIREMENTS.get()(ty, id)
}

static CHANGE_AI_REGION_STATE: GlobalFunc<extern fn(*mut bw::AiRegion, u32)> = GlobalFunc::new();
pub fn change_ai_region_state(region: *mut bw::AiRegion, state: u32) {
    CHANGE_AI_REGION_STATE.get()(region, state)
}

pub fn players() -> *mut bw::Player {
    read_var(VarId::Players) as _
}

pub fn map_tile_flags() -> *mut u32 {
    read_var(VarId::MapTileFlags) as _
}

static UNIT_BASE_STRENGTH: GlobalFunc<extern fn(*mut *mut u32)> = GlobalFunc::new();
pub fn unit_base_strength() -> (*mut u32, *mut u32) {
    let mut out = [null_mut(); 2];
    (UNIT_BASE_STRENGTH.get())(out.as_mut_ptr());
    (out[0], out[1])
}

static ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut c_void, u32, u32, u32, *mut c_void, u32),
> = GlobalFunc::new();

pub fn issue_order(
    unit: *mut bw::Unit,
    order: OrderId,
    x: u32,
    y: u32,
    target: *mut bw::Unit,
    fow_unit: UnitId,
) {
    assert!(x < 0x10000);
    assert!(y < 0x10000);
    assert!(unit != null_mut());
    unsafe { ISSUE_ORDER.get()(
        unit as *mut c_void,
        order.0 as u32,
        x,
        y,
        target as *mut c_void,
        fow_unit.0 as u32,
    ) }
}

static PRINT_TEXT: GlobalFunc<unsafe extern fn(*const u8)> = GlobalFunc::new();
// Too common to be inlined. Would be better if PRINT_TEXT were changed to always be valid
// (But C ABI is still worse for binsize)
#[inline(never)]
pub fn print_text(msg: *const u8) {
    if let Some(print) = PRINT_TEXT.get_opt() {
        unsafe {
            print(msg);
        }
    }
}

pub fn rng_seed() -> Option<u32> {
    if opt_global!(VarId::RngSeed) >= 2 {
        Some(read_var(VarId::RngSeed) as u32)
    } else {
        None
    }
}

static READ_FILE: GlobalFunc<unsafe extern fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc::new();
pub fn read_file(name: &str) -> Option<(*mut u8, usize)> {
    // Uh, should work fine
    let cstring = format!("{}\0", name);
    let mut size = 0usize;
    let result = unsafe { READ_FILE.get()(cstring.as_ptr(), &mut size) };
    if result == null_mut() {
        None
    } else {
        Some((result, size))
    }
}

static FREE_MEMORY: GlobalFunc<unsafe extern fn(*mut u8)> = GlobalFunc::new();
pub unsafe fn free_memory(ptr: *mut u8) {
    unsafe { FREE_MEMORY.get()(ptr) }
}

static UNIT_ARRAY_LEN:
GlobalFunc<unsafe extern fn(*mut *mut c_void, *mut usize)> = GlobalFunc::new();
pub unsafe fn unit_array() -> (*mut bw::Unit, usize) {
    let mut size = 0usize;
    let mut ptr = null_mut();
    unsafe {
        UNIT_ARRAY_LEN.get()(&mut ptr, &mut size);
    }
    (ptr as *mut bw::Unit, size)
}

unsafe fn aiscript_opcode(
    api: *const PluginApi,
    opcode: u32,
    hook: unsafe extern fn(*mut bw::AiScript),
) {
    let ok = ((*api).hook_aiscript_opcode)(opcode, mem::transmute(hook));
    if ok == 0 {
        fatal("Unable to hook aiscript opcodes");
    }
}

#[no_mangle]
pub unsafe extern fn samase_plugin_init(api: *const PluginApi) {
    bw_dat::set_is_scr(crate::is_scr());
    let required_version = 42;
    if (*api).version < required_version {
        fatal(&format!(
            "Newer samase is required. (Plugin API version {}, this plugin requires version {})",
            (*api).version,
            required_version,
        ));
    }

    let mut ext_arrays = null_mut();
    let ext_arrays_len = ((*api).extended_arrays)(&mut ext_arrays);
    bw_dat::set_extended_arrays(ext_arrays as *mut _, ext_arrays_len);

    CRASH_WITH_MESSAGE.set((*api).crash_with_message);
    READ_VARS.set((*api).read_vars);
    WRITE_VARS.set((*api).write_vars);
    init_globals(api);

    aiscript_opcode(api, 0x00, crate::aiscript::goto);
    aiscript_opcode(api, 0x09, crate::aiscript::wait_build);
    aiscript_opcode(api, 0x0a, crate::aiscript::wait_buildstart);
    aiscript_opcode(api, 0x40, crate::aiscript::call);
    aiscript_opcode(api, 0x41, crate::aiscript::ret);
    aiscript_opcode(api, 0x44, crate::aiscript::panic_opcode);
    aiscript_opcode(api, 0x46, crate::aiscript::do_morph);
    aiscript_opcode(api, 0x4c, crate::aiscript::train);
    aiscript_opcode(api, 0x71, crate::aiscript::attack_to);
    aiscript_opcode(api, 0x72, crate::aiscript::attack_timeout);
    aiscript_opcode(api, 0x73, crate::aiscript::issue_order);
    aiscript_opcode(api, 0x74, crate::aiscript::deaths);
    aiscript_opcode(api, 0x75, crate::idle_orders::idle_orders);
    aiscript_opcode(api, 0x76, crate::aiscript::if_attacking);
    aiscript_opcode(api, 0x77, crate::aiscript::unstart_campaign);
    aiscript_opcode(api, 0x78, crate::aiscript::max_workers);
    aiscript_opcode(api, 0x79, crate::aiscript::under_attack);
    aiscript_opcode(api, 0x7a, crate::aiscript::aicontrol);
    aiscript_opcode(api, 0x7b, crate::aiscript::bring_jump);
    aiscript_opcode(api, 0x7c, crate::aiscript::create_script);
    aiscript_opcode(api, 0x7d, crate::aiscript::player_jump);
    aiscript_opcode(api, 0x7e, crate::aiscript::kills_command);
    aiscript_opcode(api, 0x7f, crate::aiscript::wait_rand);
    aiscript_opcode(api, 0x80, crate::aiscript::upgrade_jump);
    aiscript_opcode(api, 0x81, crate::aiscript::tech_jump);
    aiscript_opcode(api, 0x82, crate::aiscript::random_call);
    aiscript_opcode(api, 0x83, crate::aiscript::attack_rand);
    aiscript_opcode(api, 0x84, crate::aiscript::supply);
    aiscript_opcode(api, 0x85, crate::aiscript::time_command);
    aiscript_opcode(api, 0x86, crate::aiscript::resources_command);
    aiscript_opcode(api, 0x87, crate::aiscript::set_town_id);
    aiscript_opcode(api, 0x88, crate::aiscript::remove_build);
    aiscript_opcode(api, 0x89, crate::aiscript::guard_command);
    aiscript_opcode(api, 0x8a, crate::aiscript::base_layout_old);
    aiscript_opcode(api, 0x8b, crate::aiscript::print_command);
    aiscript_opcode(api, 0x8c, crate::aiscript::attacking);
    aiscript_opcode(api, 0x8d, crate::aiscript::base_layout);
    aiscript_opcode(api, 0x8e, crate::aiscript::unit_avail);
    aiscript_opcode(api, 0x8f, crate::aiscript::load_bunkers);
    aiscript_opcode(api, 0x90, crate::aiscript::ping);
    aiscript_opcode(api, 0x91, crate::aiscript::reveal_area);
    aiscript_opcode(api, 0x92, crate::aiscript::tech_avail);
    aiscript_opcode(api, 0x93, crate::aiscript::remove_creep);
    aiscript_opcode(api, 0x94, crate::aiscript::save_bank);
    aiscript_opcode(api, 0x95, crate::aiscript::load_bank);
    aiscript_opcode(api, 0x96, crate::aiscript::bank_data_old);
    aiscript_opcode(api, 0x97, crate::aiscript::unit_name);
    aiscript_opcode(api, 0x98, crate::aiscript::bank_data);
    aiscript_opcode(api, 0x99, crate::aiscript::lift_land);
    aiscript_opcode(api, 0x9a, crate::aiscript::queue);
    aiscript_opcode(api, 0x9b, crate::aiscript::aise_debug);
    aiscript_opcode(api, 0x9c, crate::aiscript::replace_requests);
    aiscript_opcode(api, 0x9d, crate::aiscript::defense_command);
    aiscript_opcode(api, 0x9e, crate::aiscript::max_build);
    aiscript_opcode(api, 0x9f, crate::aiscript::attack_to_deaths);
    aiscript_opcode(api, 0xa0, crate::aiscript::bw_kills);
    aiscript_opcode(api, 0xa1, crate::aiscript::build_at);
    aiscript_opcode(api, 0xa2, crate::aiscript::debug_name);

    GET_REGION.init(
        ((*api).get_region)().map(|x| mem::transmute(x)),
        "get_region",
    );
    DAT_REQUIREMENTS.init(
        ((*api).dat_requirements)().map(|x| mem::transmute(x)),
        "dat_requirements",
    );
    UNIT_BASE_STRENGTH.init(((*api).unit_base_strength)().map(|x| mem::transmute(x)), "strength");
    match ((*api).issue_order)() {
        None => ((*api).warn_unsupported_feature)(b"Ai script issue_order\0".as_ptr()),
        Some(s) => {
            ISSUE_ORDER.set(s);
        }
    }
    let read_file = ((*api).read_file)();
    READ_FILE.set(read_file);
    FREE_MEMORY.set((*api).free_memory);
    CHANGE_AI_REGION_STATE.init(
        ((*api).change_ai_region_state)().map(|x| mem::transmute(x)),
        "change_ai_region_state",
    );
    if !crate::feature_disabled("everything_else") {
        let result = ((*api).hook_step_objects)(crate::frame_hook, 0);
        if result == 0 {
            fatal("Couldn't hook step_objects");
        }
        let result = ((*api).hook_step_objects)(crate::frame_hook_after, 1);
        if result == 0 {
            fatal("Couldn't hook step_objects");
        }
        let result = ((*api).hook_step_order)(crate::step_order_hook);
        if result == 0 {
            fatal("Couldn't hook step_order");
        }
        let result = ((*api).hook_step_order_hidden)(crate::step_order_hidden_hook);
        if result == 0 {
            fatal("Couldn't hook soi");
        }
        // Not critical hook
        ((*api).hook_ai_step_region)(crate::ai::step_region_hook);
        ((*api).hook_ai_focus_disabled)(crate::ai::focus_disabled_hook);
        ((*api).hook_ai_focus_air)(crate::ai::focus_air_hook);
    }

    ((*api).hook_func)(
        FuncId::AiPickBestPlacementPosition as u16,
        crate::placement::placement_position_hook as _,
    );
    ((*api).hook_func)(
        FuncId::AiPlacementFlags as u16,
        crate::placement::placement_flags_hook as _,
    );

    let mut dat_len = 0usize;
    let units_dat = ((*api).extended_dat)(0).expect("units.dat")(&mut dat_len);
    bw_dat::init_units(units_dat as *const _, dat_len);
    let weapons_dat = ((*api).extended_dat)(1).expect("weapons.dat")(&mut dat_len);
    bw_dat::init_weapons(weapons_dat as *const _, dat_len);
    let upgrades_dat = ((*api).extended_dat)(3).expect("upgrades.dat")(&mut dat_len);
    bw_dat::init_upgrades(upgrades_dat as *const _, dat_len);
    let techdata_dat = ((*api).extended_dat)(4).expect("techdata.dat")(&mut dat_len);
    bw_dat::init_techdata(techdata_dat as *const _, dat_len);
    let orders_dat = ((*api).extended_dat)(7).expect("orders.dat")(&mut dat_len);
    bw_dat::init_orders(orders_dat as *const _, dat_len);

    UNIT_ARRAY_LEN.set(((*api).unit_array_len)().expect("unit_array_len"));
    PRINT_TEXT.set(((*api).print_text)().expect("print_text"));
    let mut result = ((*api).extend_save)(
        "aise\0".as_ptr(),
        Some(crate::globals::save),
        Some(crate::globals::load),
        crate::globals::init_game,
    );
    if result != 0 {
        result = ((*api).hook_ingame_command)(6, crate::globals::wrap_save, None);
        if !crate::is_scr() {
            // Hackfix for mtl
            let _ = ((*api).hook_ingame_command)(
                0xcc,
                nop_command_hook,
                Some(mtl_rally_command_length),
            );
        }
    }
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }
    crate::debug_ui::init(api);
    crate::init();
}

pub unsafe extern fn nop_command_hook(
    data: *const u8,
    len: u32,
    _player: u32,
    _unique_player: u32,
    orig: unsafe extern fn(*const u8, u32),
) {
    orig(data, len)
}

pub unsafe extern fn mtl_rally_command_length(
    data: *const u8,
    max_len: u32,
) -> u32 {
    if max_len < 2 {
        return !0;
    }
    match *data.add(1) {
        0 => 0xa,
        _ => !0,
    }
}
