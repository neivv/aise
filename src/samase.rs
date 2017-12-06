use std::mem;
use std::ptr::null_mut;

use kernel32;
use libc::c_void;

use bw;
use order::OrderId;
use unit::UnitId;
use windows;

#[repr(C, packed)]
pub struct PluginApi {
    version: u16,
    padding: u16,
    free_memory: unsafe extern fn(*mut u8),
    write_exe_memory: unsafe extern fn(usize, *const u8, usize) -> u32,
    warn_unsupported_feature: unsafe extern fn(*const u8),
    read_file: unsafe extern fn() -> unsafe extern fn(*const u8, *mut usize) -> *mut u8,
    game: unsafe extern fn() -> Option<unsafe extern fn() -> *mut c_void>,
    rng_seed: unsafe extern fn() -> Option<unsafe extern fn() -> u32>,
    hook_step_objects: unsafe extern fn(unsafe extern fn()) -> u32,
    hook_aiscript_opcode: unsafe extern fn(u32, unsafe extern fn(*mut bw::AiScript)) -> u32,
    ai_regions: unsafe extern fn() -> Option<unsafe extern fn() -> *mut c_void>,
    player_ai: unsafe extern fn() -> Option<unsafe extern fn() -> *mut c_void>,
    get_region: unsafe extern fn() -> Option<unsafe extern fn(u32, u32) -> u32>,
    change_ai_region_state: unsafe extern fn() -> Option<unsafe extern fn(*mut c_void, u32)>,
    first_active_unit: unsafe extern fn() -> Option<unsafe extern fn() -> *mut c_void>,
    first_hidden_unit: unsafe extern fn() -> Option<unsafe extern fn() -> *mut c_void>,
    // self, order, x, y, target, fow_unit
    issue_order: unsafe extern fn() ->
        Option<unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32)>,
}

struct GlobalFunc<T: Copy>(Option<T>);

impl<T: Copy> GlobalFunc<T> {
    fn get(&self) -> T {
        self.0.unwrap()
    }

    fn init(&mut self, val: Option<*mut c_void>, desc: &str) {
        let val = val.unwrap_or_else(|| fatal(&format!("Can't get {}", desc)));
        unsafe {
            assert_eq!(mem::size_of::<T>(), 4);
            let mut typecast_hack: T = mem::uninitialized();
            *(&mut typecast_hack as *mut T as *mut *mut c_void) = val;
            self.0 = Some(typecast_hack);
        }
    }
}

fn fatal(text: &str) -> ! {
    let msg = format!("This StarCraft version is not supported :(\n({})", text);
    windows::message_box("Aiscript extension plugin", &msg);
    unsafe { kernel32::TerminateProcess(kernel32::GetCurrentProcess(), 0x4230daef); }
    unreachable!();
}

static mut GAME: GlobalFunc<fn() -> *mut bw::Game> = GlobalFunc(None);
pub fn game() -> *mut bw::Game {
    unsafe { GAME.get()() }
}

static mut AI_REGIONS: GlobalFunc<fn() -> *mut *mut bw::AiRegion> = GlobalFunc(None);
pub fn ai_regions(player: u32) -> *mut bw::AiRegion {
    assert!(player < 8);
    unsafe { *(AI_REGIONS.get()()).offset(player as isize) }
}

static mut PLAYER_AI: GlobalFunc<fn() -> *mut bw::PlayerAiData> = GlobalFunc(None);
pub fn player_ai(player: u32) -> *mut bw::PlayerAiData {
    assert!(player < 8);
    unsafe { (PLAYER_AI.get()()).offset(player as isize) }
}

static mut FIRST_ACTIVE_UNIT: GlobalFunc<fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_active_unit() -> *mut bw::Unit {
    unsafe { FIRST_ACTIVE_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut GET_REGION: GlobalFunc<fn(u32, u32) -> u32> = GlobalFunc(None);
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static mut CHANGE_AI_REGION_STATE: GlobalFunc<fn(*mut bw::AiRegion, u32)> = GlobalFunc(None);
pub fn change_ai_region_state(region: *mut bw::AiRegion, state: u32) {
    unsafe { CHANGE_AI_REGION_STATE.get()(region, state) }
}

static mut ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32)
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

static mut READ_FILE: GlobalFunc<fn(*const u8) -> *mut u8> = GlobalFunc(None);
pub fn read_file(name: &str) -> Option<*mut u8> {
    // Uh, should work fine
    let cstring = format!("{}\0", name);
    let result = unsafe { READ_FILE.get()(cstring.as_ptr()) };
    if result == null_mut() {
        None
    } else {
        Some(result)
    }
}

#[no_mangle]
pub unsafe extern fn samase_plugin_init(api: *const PluginApi) {
    let ok = ((*api).hook_aiscript_opcode)(0x71, ::aiscript::attack_to);
    if ok == 0 {
        fatal("Unable to hook aiscript opcodes");
    }
    let ok = ((*api).hook_aiscript_opcode)(0x72, ::aiscript::attack_timeout);
    if ok == 0 {
        fatal("Unable to hook aiscript opcodes");
    }
    let ok = ((*api).hook_aiscript_opcode)(0x73, ::aiscript::issue_order);
    if ok == 0 {
        fatal("Unable to hook aiscript opcodes");
    }
    let ok = ((*api).hook_aiscript_opcode)(0x74, ::aiscript::if_deaths);
    if ok == 0 {
        fatal("Unable to hook aiscript opcodes");
    }
    GAME.init(((*api).game)().map(|x| mem::transmute(x)), "Game object");
    AI_REGIONS.init(((*api).ai_regions)().map(|x| mem::transmute(x)), "AI regions");
    PLAYER_AI.init(((*api).player_ai)().map(|x| mem::transmute(x)), "Player AI");
    GET_REGION.init(((*api).get_region)().map(|x| mem::transmute(x)), "get_region");
    match ((*api).first_active_unit)() {
        None => ((*api).warn_unsupported_feature)(b"Ai script issue_order\0".as_ptr()),
        Some(s) => FIRST_ACTIVE_UNIT.0 = Some(mem::transmute(s)),
    }
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
    ::init(true);
}
