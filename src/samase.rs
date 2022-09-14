use std::mem;
use std::ptr::null_mut;

use libc::c_void;
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use samase_plugin::PluginApi;

use crate::bw;
use crate::order::OrderId;
use crate::unit::UnitId;
use crate::windows;

struct GlobalFunc<T: Copy>(Option<T>);

impl<T: Copy> GlobalFunc<T> {
    fn get(&self) -> T {
        self.0.unwrap()
    }

    fn try_init(&mut self, val: Option<*mut c_void>) -> bool {
        let val = match val {
            Some(s) => s,
            None => return false,
        };
        unsafe {
            assert_eq!(mem::size_of::<T>(), mem::size_of::<*mut c_void>());
            let mut typecast_hack: mem::MaybeUninit<T> = mem::MaybeUninit::uninit();
            *(typecast_hack.as_mut_ptr() as *mut *mut c_void) = val;
            self.0 = Some(typecast_hack.assume_init());
        }
        true
    }

    fn init(&mut self, val: Option<*mut c_void>, desc: &str) {
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

static mut CRASH_WITH_MESSAGE: GlobalFunc<unsafe extern fn(*const u8) -> !> = GlobalFunc(None);
pub fn crash_with_message(msg: &str) -> ! {
    let msg = format!("{}\0", msg);
    unsafe { CRASH_WITH_MESSAGE.get()(msg.as_bytes().as_ptr()) }
}

static mut GAME: GlobalFunc<extern fn() -> *mut bw::Game> = GlobalFunc(None);
pub fn game() -> *mut bw::Game {
    unsafe { GAME.get()() }
}

static mut AI_REGIONS: GlobalFunc<extern fn() -> *mut *mut bw::AiRegion> = GlobalFunc(None);
pub fn ai_regions(player: u32) -> *mut bw::AiRegion {
    assert!(player < 8);
    unsafe { *(AI_REGIONS.get()()).offset(player as isize) }
}

static mut PLAYER_AI: GlobalFunc<extern fn() -> *mut bw::PlayerAiData> = GlobalFunc(None);
pub fn player_ai(player: u32) -> *mut bw::PlayerAiData {
    assert!(player < 8);
    unsafe { (PLAYER_AI.get()()).offset(player as isize) }
}

static mut FIRST_ACTIVE_UNIT: GlobalFunc<extern fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_active_unit() -> *mut bw::Unit {
    unsafe { FIRST_ACTIVE_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut FIRST_HIDDEN_UNIT: GlobalFunc<extern fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_hidden_unit() -> *mut bw::Unit {
    unsafe { FIRST_HIDDEN_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut FIRST_AI_SCRIPT: GlobalFunc<extern fn() -> *mut bw::AiScript> = GlobalFunc(None);
pub fn first_ai_script() -> *mut bw::AiScript {
    unsafe { FIRST_AI_SCRIPT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut SET_FIRST_AI_SCRIPT: GlobalFunc<extern fn(*mut bw::AiScript)> = GlobalFunc(None);
pub fn set_first_ai_script(value: *mut bw::AiScript) {
    unsafe {
        SET_FIRST_AI_SCRIPT.0.map(|x| x(value));
    }
}

static mut FIRST_FREE_AI_SCRIPT: GlobalFunc<extern fn() -> *mut bw::AiScript> = GlobalFunc(None);
pub fn first_free_ai_script() -> *mut bw::AiScript {
    unsafe { FIRST_FREE_AI_SCRIPT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut SET_FIRST_FREE_AI_SCRIPT: GlobalFunc<extern fn(*mut bw::AiScript)> = GlobalFunc(None);
pub fn set_first_free_ai_script(value: *mut bw::AiScript) {
    unsafe {
        SET_FIRST_FREE_AI_SCRIPT.0.map(|x| x(value));
    }
}

static mut GUARD_AIS: GlobalFunc<extern fn() -> *mut bw::GuardAiList> = GlobalFunc(None);
pub fn guard_ais() -> *mut bw::GuardAiList {
    unsafe { GUARD_AIS.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut PATHING: GlobalFunc<extern fn() -> *mut bw::Pathing> = GlobalFunc(None);
pub fn pathing() -> *mut bw::Pathing {
    unsafe { PATHING.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut PLAYER_AI_TOWNS: GlobalFunc<extern fn() -> *mut bw::AiTownList> = GlobalFunc(None);
pub fn active_towns() -> *mut bw::AiTownList {
    unsafe { PLAYER_AI_TOWNS.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut GET_REGION: GlobalFunc<extern fn(u32, u32) -> u32> = GlobalFunc(None);
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static mut DAT_REQUIREMENTS: GlobalFunc<extern fn(u32, u32) -> *const u16> = GlobalFunc(None);
pub fn requirements(ty: u32, id: u32) -> *const u16 {
    unsafe { DAT_REQUIREMENTS.get()(ty, id) }
}

static mut CHANGE_AI_REGION_STATE: GlobalFunc<extern fn(*mut bw::AiRegion, u32)> = GlobalFunc(None);
pub fn change_ai_region_state(region: *mut bw::AiRegion, state: u32) {
    unsafe { CHANGE_AI_REGION_STATE.get()(region, state) }
}

static mut PLAYERS: GlobalFunc<extern fn() -> *mut bw::Player> = GlobalFunc(None);
pub fn players() -> *mut bw::Player {
    unsafe { PLAYERS.get()() }
}

static mut MAP_TILE_FLAGS: GlobalFunc<extern fn() -> *mut u32> = GlobalFunc(None);
pub fn map_tile_flags() -> *mut u32 {
    unsafe { MAP_TILE_FLAGS.get()() }
}

static mut ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32),
> = GlobalFunc(None);

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
    unsafe { ISSUE_ORDER.get()(unit, order.0 as u32, x, y, target, fow_unit.0 as u32) }
}

static mut PRINT_TEXT: GlobalFunc<extern fn(*const u8)> = GlobalFunc(None);
// Too common to be inlined. Would be better if PRINT_TEXT were changed to always be valid
// (But C ABI is still worse for binsize)
#[inline(never)]
pub fn print_text(msg: *const u8) {
    unsafe {
        if let Some(print) = PRINT_TEXT.0 {
            print(msg);
        }
    }
}

static mut RNG_SEED: GlobalFunc<extern fn() -> u32> = GlobalFunc(None);
pub fn rng_seed() -> Option<u32> {
    unsafe {
        if let Some(rng) = RNG_SEED.0 {
            Some(rng())
        } else {
            None
        }
    }
}

static mut READ_FILE: GlobalFunc<extern fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc(None);
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

static mut UNIT_ARRAY_LEN: GlobalFunc<extern fn(*mut *mut bw::Unit, *mut usize)> = GlobalFunc(None);
pub unsafe fn unit_array() -> (*mut bw::Unit, usize) {
    let mut size = 0usize;
    let mut ptr = null_mut();
    UNIT_ARRAY_LEN.get()(&mut ptr, &mut size);
    (ptr, size)
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
    let required_version = 36;
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

    CRASH_WITH_MESSAGE.0 = Some((*api).crash_with_message);

    aiscript_opcode(api, 0x00, crate::aiscript::goto);
    aiscript_opcode(api, 0x09, crate::aiscript::wait_build);
    aiscript_opcode(api, 0x0a, crate::aiscript::wait_buildstart);
    aiscript_opcode(api, 0x0c, crate::aiscript::attack_add);
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

    GAME.init(((*api).game)().map(|x| mem::transmute(x)), "Game object");
    AI_REGIONS.init(
        ((*api).ai_regions)().map(|x| mem::transmute(x)),
        "AI regions",
    );
    PLAYER_AI.init(((*api).player_ai)().map(|x| mem::transmute(x)), "Player AI");
    GET_REGION.init(
        ((*api).get_region)().map(|x| mem::transmute(x)),
        "get_region",
    );
    DAT_REQUIREMENTS.init(
        ((*api).dat_requirements)().map(|x| mem::transmute(x)),
        "dat_requirements",
    );
    FIRST_ACTIVE_UNIT.init(
        ((*api).first_active_unit)().map(|x| mem::transmute(x)),
        "first active unit",
    );
    FIRST_HIDDEN_UNIT.init(
        ((*api).first_hidden_unit)().map(|x| mem::transmute(x)),
        "first hidden unit",
    );
    FIRST_AI_SCRIPT.init(
        ((*api).first_ai_script)().map(|x| mem::transmute(x)),
        "first_ai_script",
    );
    SET_FIRST_AI_SCRIPT.init(
        ((*api).set_first_ai_script)().map(|x| mem::transmute(x)),
        "set_first_ai_script",
    );
    FIRST_FREE_AI_SCRIPT.init(
        ((*api).first_free_ai_script)().map(|x| mem::transmute(x)),
        "first_free_ai_script",
    );
    SET_FIRST_FREE_AI_SCRIPT.init(
        ((*api).set_first_free_ai_script)().map(|x| mem::transmute(x)),
        "set_first_free_ai_script",
    );
    GUARD_AIS.init(
        ((*api).first_guard_ai)().map(|x| mem::transmute(x)),
        "guard ais",
    );
    PATHING.init(((*api).pathing)().map(|x| mem::transmute(x)), "pathing");
    PLAYERS.init(((*api).players)().map(|x| mem::transmute(x)), "players");
    MAP_TILE_FLAGS.init(
        ((*api).map_tile_flags)().map(|x| mem::transmute(x)),
        "map_tile_flags",
    );
    match ((*api).issue_order)() {
        None => ((*api).warn_unsupported_feature)(b"Ai script issue_order\0".as_ptr()),
        Some(s) => ISSUE_ORDER.0 = Some(mem::transmute(s)),
    }
    let read_file = ((*api).read_file)();
    READ_FILE.0 = Some(mem::transmute(read_file));
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

    UNIT_ARRAY_LEN.0 = Some(mem::transmute(((*api).unit_array_len)()));
    PRINT_TEXT.0 = Some(mem::transmute(((*api).print_text)()));
    RNG_SEED.0 = Some(mem::transmute(((*api).rng_seed)()));
    let mut result = ((*api).extend_save)(
        "aise\0".as_ptr(),
        Some(crate::globals::save),
        Some(crate::globals::load),
        crate::globals::init_game,
    );
    if result != 0 {
        result = ((*api).hook_ingame_command)(6, crate::globals::wrap_save, None);
    }
    if result != 0 {
        let ptr = ((*api).player_ai_towns)();
        if ptr.is_none() {
            result = 0;
        } else {
            PLAYER_AI_TOWNS.0 = Some(mem::transmute(ptr));
        }
    }
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }
    crate::init();
}
