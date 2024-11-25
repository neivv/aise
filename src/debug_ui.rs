use std::borrow::Cow;
use std::fmt::Write;
use std::mem::{self};
use std::ptr::null_mut;
use std::sync::atomic::Ordering;

use bw_dat::{UnitId};
use libc::c_void;
use samase_plugin::{
    DebugUiDraw, DebugUiDrawHelper, DebugUiColor, DebugUiLog, FfiStr, PluginApi, ComplexLineParam,
};
use parking_lot::Mutex;

use crate::aiscript::{Script, ScriptData};
use crate::ai_spending::{DatReqSatisfyError, RequestSatisfyError};
use crate::bw;
use crate::globals::{Globals};
use crate::list::ListIter;

struct DebugUi {
    /// May be invalid, need to walk through active_ai_scripts to check for validity.
    /// If it gets reused by another script then oh well.
    current_script: *mut bw::AiScript,
    call_stack_frame: u32,
    script_player_filter: Option<u8>,
    request_player_filter: Option<u8>,
}

struct DebugUiLogs {
    add_data: unsafe extern fn(
        *mut DebugUiLog, *const FfiStr, *const ComplexLineParam, usize, *mut c_void
    ),
    /// One per player
    requests: [*mut DebugUiLog; 8],
}

unsafe impl Send for DebugUi {}
unsafe impl Sync for DebugUi {}
unsafe impl Send for DebugUiLogs {}
unsafe impl Sync for DebugUiLogs {}

static mut DEBUG_UI_LOGS: DebugUiLogs = DebugUiLogs {
    add_data: log_add_data_nop,
    requests: [null_mut(); 8],
};

unsafe extern fn log_add_data_nop(
    _: *mut DebugUiLog,
    _: *const FfiStr,
    _: *const ComplexLineParam,
    _: usize,
    _: *mut c_void,
) {
}

static DEBUG_UI: Mutex<DebugUi> = Mutex::new(DebugUi {
    current_script: null_mut(),
    call_stack_frame: 0,
    script_player_filter: None,
    request_player_filter: None,
});

pub unsafe fn init(api: *const PluginApi) {
    let ok = ((*api).debug_ui_add_tab)(
        &FfiStr::from_str("aise"),
        &FfiStr::from_str("Scripts"),
        debug_tab_scripts,
        null_mut(),
    );
    if ok == 0 {
        return;
    }
    let ok = ((*api).debug_ui_add_tab)(
        &FfiStr::from_str("aise"),
        &FfiStr::from_str("Requests"),
        debug_tab_requests,
        null_mut(),
    );
    if ok == 0 {
        return;
    }
    let logs = logs();
    (*logs).add_data = (*api).debug_log_add_data;
    for i in 0..8 {
        (*logs).requests[i] = ((*api).debug_ui_add_log)();
    }
    crate::DEBUG_UI_ACTIVE.store(true, Ordering::Relaxed);
}

unsafe fn logs() -> *mut DebugUiLogs {
    &raw mut DEBUG_UI_LOGS
}

static OPCODE_NAMES: &[&str] = &[
    "goto",                 // 0x00
    "notowns_jump",         // 0x01
    "wait",                 // 0x02
    "start_town",           // 0x03
    "start_areatown",       // 0x04
    "expand",               // 0x05
    "build",                // 0x06
    "upgrade",              // 0x07
    "tech",                 // 0x08
    "wait_build",           // 0x09
    "wait_buildstart",      // 0x0A
    "attack_clear",         // 0x0B
    "attack_add",           // 0x0C
    "attack_prepare",       // 0x0D
    "attack_do",            // 0x0E
    "wait_secure",          // 0x0F
    "capt_expand",          // 0x10
    "build_bunkers",        // 0x11
    "wait_bunkers",         // 0x12
    "defensebuild_gg",      // 0x13
    "defensebuild_ag",      // 0x14
    "defensebuild_ga",      // 0x15
    "defensebuild_aa",      // 0x16
    "defenseuse_gg",        // 0x17
    "defenseuse_ag",        // 0x18
    "defenseuse_ga",        // 0x19
    "defenseuse_aa",        // 0x1A
    "defenseclear_gg",      // 0x1B
    "defenseclear_ag",      // 0x1C
    "defenseclear_ga",      // 0x1D
    "defenseclear_aa",      // 0x1E
    "send_suicide",         // 0x1F
    "player_enemy",         // 0x20
    "player_ally",          // 0x21
    "default_min",          // 0x22
    "defaultbuild_off",     // 0x23
    "stop",                 // 0x24
    "switch_rescue",        // 0x25
    "move_dt",              // 0x26
    "debug",                // 0x27
    "fatal_error",          // 0x28
    "enter_bunker",         // 0x29
    "value_area",           // 0x2A
    "transports_off",       // 0x2B
    "check_transports",     // 0x2C
    "nuke_rate",            // 0x2D
    "max_force",            // 0x2E
    "clear_combatdata",     // 0x2F
    "random_jump",          // 0x30
    "time_jump",            // 0x31
    "farms_notiming",       // 0x32
    "farms_timing",         // 0x33
    "build_turrets",        // 0x34
    "wait_turrets",         // 0x35
    "default_build",        // 0x36
    "harass_factor",        // 0x37
    "start_campaign",       // 0x38
    "race_jump",            // 0x39
    "region_size",          // 0x3A
    "get_oldpeons",         // 0x3B
    "groundmap_jump",       // 0x3C
    "place_guard",          // 0x3D
    "wait_force",           // 0x3E
    "guard_resources",      // 0x3F
    "call",                 // 0x40
    "return",               // 0x41
    "eval_harass",          // 0x42
    "creep",                // 0x43
    "panic",                // 0x44
    "player_need",          // 0x45
    "do_morph",             // 0x46
    "wait_upgrades",        // 0x47
    "multirun",             // 0x48
    "rush",                 // 0x49
    "scout_with",           // 0x4A
    "define_max",           // 0x4B
    "train",                // 0x4C
    "target_expansion",     // 0x4D
    "wait_train",           // 0x4E
    "set_attacks",          // 0x4F
    "set_gencmd",           // 0x50
    "make_patrol",          // 0x51
    "give_money",           // 0x52
    "prep_down",            // 0x53
    "resources_jump",       // 0x54
    "enter_transport",      // 0x55
    "exit_transport",       // 0x56
    "sharedvision_on",      // 0x57
    "sharedvision_off",     // 0x58
    "nuke_location",        // 0x59
    "harass_location",      // 0x5A
    "implode",              // 0x5B
    "guard_all",            // 0x5C
    "enemyowns_jump",       // 0x5D
    "enemyresources_jump",  // 0x5E
    "if_dif",               // 0x5F
    "easy_attack",          // 0x60
    "kill_thread",          // 0x61
    "killable",             // 0x62
    "wait_finishattack",    // 0x63
    "quick_attack",         // 0x64
    "junkyard_dog",         // 0x65
    "fake_nuke",            // 0x66
    "disruption_web",       // 0x67
    "recall_location",      // 0x68
    "set_randomseed",       // 0x69
    "if_owned",             // 0x6A
    "create_nuke",          // 0x6B
    "create_unit",          // 0x6C
    "nuke_pos",             // 0x6D
    "help_iftrouble",       // 0x6E
    "allies_watch",         // 0x6F
    "try_townpoint",        // 0x70
    "attack_to",            // 0x71
    "attack_timeout",       // 0x72
    "issue_order",          // 0x73
    "deaths",               // 0x74
    "idle_orders",          // 0x75
    "if_attacking",         // 0x76
    "unstart_campaign",     // 0x77
    "max_workers",          // 0x78
    "under_attack",         // 0x79
    "aicontrol",            // 0x7a
    "bring_jump",           // 0x7b
    "create_script",        // 0x7c
    "player_jump",          // 0x7d
    "aise_kills",           // 0x7e
    "wait_rand",            // 0x7f,
    "upgrade_jump",         // 0x80
    "tech_jump",            // 0x81
    "random_call",          // 0x82
    "attack_rand",          // 0x83
    "supply",               // 0x84,
    "time",                 // 0x85,
    "resources",            // 0x86,
    "set_id",               // 0x87,
    "remove_build",         // 0x88
    "guard",                // 0x89,
    "base_layout_old",      // 0x8a,
    "print",                // 0x8b,
    "attacking",            // 0x8c,
    "base_layout",          // 0x8d,
    "unit_avail",           // 0x8e,
    "load_bunkers",         // 0x8f,
    "ping",                 // 0x90,
    "reveal_area",          // 0x91,
    "tech_avail",           // 0x92,
    "remove_creep",         // 0x93,
    "save_bank",            // 0x94,
    "load_bank",            // 0x95,
    "bank_data_old",        // 0x96,
    "unit_name",            // 0x97,
    "bank_data",            // 0x98,
    "lift_land",            // 0x99,
    "queue",                // 0x9a,
    "aise_debug",           // 0x9b,
    "replace_unit",         // 0x9c,
    "defense",              // 0x9d
    "__9e",
    "__9f",
    "bw_kills",             // 0xa0
    "build_at",             // 0xa1
    "debug_name",           // 0xa2
];

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum OpParam {
    #[allow(dead_code)]
    End = 0,
    Address,
    ShortAddress,
    U8,
    U16,
    U32,
    Unit,
    Tech,
    Upgrade,
    String,
    Area,
    Point,
    IdleOrderFlags,
    _Last,
}

#[derive(Copy, Clone)]
struct OpcodeParam(u64);

const OP_PARAM_SHIFT: u32 = 4;

impl OpcodeParam {
    const NONE: OpcodeParam = OpcodeParam(0);
    const fn one(param: OpParam) -> OpcodeParam {
        OpcodeParam(param as u64)
    }

    const fn two(a: OpParam, b: OpParam) -> OpcodeParam {
        OpcodeParam((a as u64) | ((b as u64) << OP_PARAM_SHIFT))
    }

    const fn three(a: OpParam, b: OpParam, c: OpParam) -> OpcodeParam {
        OpcodeParam(
            (a as u64) | ((b as u64) << OP_PARAM_SHIFT) |
            ((c as u64) << (OP_PARAM_SHIFT * 2))
        )
    }

    const fn four(a: OpParam, b: OpParam, c: OpParam, d: OpParam) -> OpcodeParam {
        OpcodeParam(
            (a as u64) | ((b as u64) << OP_PARAM_SHIFT) |
            ((c as u64) << (OP_PARAM_SHIFT * 2)) |
            ((d as u64) << (OP_PARAM_SHIFT * 3))
        )
    }

    const fn five(a: OpParam, b: OpParam, c: OpParam, d: OpParam, e: OpParam) -> OpcodeParam {
        OpcodeParam(
            (a as u64) | ((b as u64) << OP_PARAM_SHIFT) |
            ((c as u64) << (OP_PARAM_SHIFT * 2)) |
            ((d as u64) << (OP_PARAM_SHIFT * 3)) |
            ((e as u64) << (OP_PARAM_SHIFT * 4))
        )
    }

    const fn many(arr: &[OpParam]) -> OpcodeParam {
        let mut i = arr.len() - 1;
        let mut result = 0u64;
        loop {
            result = (result << OP_PARAM_SHIFT) | (arr[i] as u64);
            if i == 0 {
                return OpcodeParam(result);
            }
            i -= 1;
        }
    }
}

static OPCODE_PARAMS: &[OpcodeParam] = &[
    OpcodeParam::one(OpParam::Address), //  goto
    OpcodeParam::two(OpParam::Unit, OpParam::ShortAddress), //  notowns_jump
    OpcodeParam::one(OpParam::U16), //  wait
    OpcodeParam::NONE, //  start_town
    OpcodeParam::NONE, //  start_areatown
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  expand
    OpcodeParam::three(OpParam::U8, OpParam::Unit, OpParam::U8), //  build
    OpcodeParam::three(OpParam::U8, OpParam::Upgrade, OpParam::U8), //  upgrade
    OpcodeParam::two(OpParam::Tech, OpParam::U8), //  tech
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  wait_build
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  wait_buildstart
    OpcodeParam::NONE, //  attack_clear
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  attack_add
    OpcodeParam::NONE, //  attack_prepare
    OpcodeParam::NONE, //  attack_do
    OpcodeParam::NONE, //  wait_secure
    OpcodeParam::NONE, //  capt_expand
    OpcodeParam::NONE, //  build_bunkers
    OpcodeParam::NONE, //  wait_bunkers
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defensebuild_gg
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defensebuild_ag
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defensebuild_ga
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defensebuild_aa
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defenseuse_gg
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defenseuse_ag
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defenseuse_ga
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  defenseuse_aa
    OpcodeParam::NONE, //  defenseclear_gg
    OpcodeParam::NONE, //  defenseclear_ag
    OpcodeParam::NONE, //  defenseclear_ga
    OpcodeParam::NONE, //  defenseclear_aa
    OpcodeParam::one(OpParam::U8), //  send_suicide
    OpcodeParam::NONE, //  player_enemy
    OpcodeParam::NONE, //  player_ally
    OpcodeParam::one(OpParam::U8), //  default_min
    OpcodeParam::NONE, //  defaultbuild_off
    OpcodeParam::NONE, //  stop
    OpcodeParam::NONE, //  switch_rescue
    OpcodeParam::NONE, //  move_dt
    OpcodeParam::two(OpParam::ShortAddress, OpParam::String), //  debug
    OpcodeParam::NONE, //  fatal_error
    OpcodeParam::NONE, //  enter_bunker
    OpcodeParam::NONE, //  value_area
    OpcodeParam::NONE, //  transports_off
    OpcodeParam::NONE, //  check_transports
    OpcodeParam::one(OpParam::U8), //  nuke_rate
    OpcodeParam::one(OpParam::U16), //  max_force
    OpcodeParam::NONE, //  clear_combatdata
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  random_jump
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  time_jump
    OpcodeParam::NONE, //  farms_notiming
    OpcodeParam::NONE, //  farms_timing
    OpcodeParam::NONE, //  build_turrets
    OpcodeParam::NONE, //  wait_turrets
    OpcodeParam::NONE, //  default_build
    OpcodeParam::one(OpParam::U16), //  harass_factor
    OpcodeParam::NONE, //  start_campaign
    OpcodeParam::three(OpParam::ShortAddress, OpParam::ShortAddress, OpParam::ShortAddress), //  race_jump
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  region_size
    OpcodeParam::one(OpParam::U8), //  get_oldpeons
    OpcodeParam::one(OpParam::ShortAddress), //  groundmap_jump
    OpcodeParam::two(OpParam::Unit, OpParam::U8), //  place_guard
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  wait_force
    OpcodeParam::one(OpParam::Unit), //  guard_resources
    OpcodeParam::one(OpParam::ShortAddress), //  call
    OpcodeParam::NONE, //  return
    OpcodeParam::one(OpParam::ShortAddress), //  eval_harass
    OpcodeParam::one(OpParam::U8), //  creep
    OpcodeParam::one(OpParam::ShortAddress), //  panic
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  player_need
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  do_morph
    OpcodeParam::NONE, //  wait_upgrades
    OpcodeParam::one(OpParam::ShortAddress), //  multirun
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  rush
    OpcodeParam::one(OpParam::Unit), //  scout_with
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  define_max
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  train
    OpcodeParam::NONE, //  target_expansion
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  wait_train
    OpcodeParam::one(OpParam::U8), //  set_attacks
    OpcodeParam::NONE, //  set_gencmd
    OpcodeParam::NONE, //  make_patrol
    OpcodeParam::NONE, //  give_money
    OpcodeParam::three(OpParam::U8, OpParam::U8, OpParam::Unit), //  prep_down
    OpcodeParam::three(OpParam::U16, OpParam::U16, OpParam::ShortAddress), //  resources_jump
    OpcodeParam::NONE, //  enter_transport
    OpcodeParam::NONE, //  exit_transport
    OpcodeParam::one(OpParam::U8), //  sharedvision_on
    OpcodeParam::one(OpParam::U8), //  sharedvision_off
    OpcodeParam::NONE, //  nuke_location
    OpcodeParam::NONE, //  harass_location
    OpcodeParam::NONE, //  implode
    OpcodeParam::NONE, //  guard_all
    OpcodeParam::two(OpParam::Unit, OpParam::ShortAddress), //  enemyowns_jump
    OpcodeParam::three(OpParam::U16, OpParam::U16, OpParam::ShortAddress), //  enemyresources_jump
    OpcodeParam::three(OpParam::U8, OpParam::U8, OpParam::ShortAddress), //  if_dif
    OpcodeParam::two(OpParam::U8, OpParam::Unit), //  easy_attack
    OpcodeParam::NONE, //  kill_thread
    OpcodeParam::NONE, //  killable
    OpcodeParam::NONE, //  wait_finishattack
    OpcodeParam::NONE, //  quick_attack
    OpcodeParam::NONE, //  junkyard_dog
    OpcodeParam::NONE, //  fake_nuke
    OpcodeParam::NONE, //  disruption_web
    OpcodeParam::NONE, //  recall_location
    OpcodeParam::one(OpParam::U32), //  set_randomseed
    OpcodeParam::two(OpParam::Unit, OpParam::ShortAddress), //  if_owned
    OpcodeParam::NONE, //  create_nuke
    OpcodeParam::three(OpParam::Unit, OpParam::U16, OpParam::U16), //  create_unit
    OpcodeParam::two(OpParam::U16, OpParam::U16), //  nuke_pos
    OpcodeParam::NONE, //  help_iftrouble
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  allies_watch
    OpcodeParam::two(OpParam::U8, OpParam::ShortAddress), //  try_townpoint
    OpcodeParam::two(OpParam::Point, OpParam::Point), //  attack_to
    OpcodeParam::one(OpParam::U32), //  attack_timeout
    OpcodeParam::many(&[
        OpParam::U8, OpParam::U16, OpParam::Unit, OpParam::Area,
        OpParam::Area, OpParam::Unit, OpParam::U16,
    ]), //  issue_order
    OpcodeParam::many(&[
        OpParam::U8, OpParam::U8, OpParam::U32, OpParam::Unit, OpParam::Address,
    ]), //  deaths
    OpcodeParam::many(&[
        OpParam::U8, OpParam::U16, OpParam::U16, OpParam::Unit,
        OpParam::U16, OpParam::Unit, OpParam::U8, OpParam::IdleOrderFlags,
    ]), //  idle_orders
    OpcodeParam::one(OpParam::Address), //  if_attacking
    OpcodeParam::NONE, //  unstart_campaign
    OpcodeParam::one(OpParam::U8), //  max_workers
    OpcodeParam::one(OpParam::U8), //  under_attack
    OpcodeParam::one(OpParam::U8), //  aicontrol
    OpcodeParam::many(&[
        OpParam::U8, OpParam::U8, OpParam::U32, OpParam::Unit,
        OpParam::Area, OpParam::Address,
    ]), // bring_jump
    OpcodeParam::five(
        OpParam::Address, OpParam::U8, OpParam::Area, OpParam::U8, OpParam::U8,
    ), //  create_script
    //  player_jump
    OpcodeParam::two(OpParam::String, OpParam::Address),
    //  aise_kills
    OpcodeParam::many(&[
        OpParam::U8, OpParam::U8, OpParam::U8, OpParam::U32, OpParam::Unit, OpParam::Address
    ]),
    // wait_rand
    OpcodeParam::two(OpParam::U32, OpParam::U32),
    // upgrade_jump
    OpcodeParam::five(OpParam::U8, OpParam::U8, OpParam::Upgrade, OpParam::U8, OpParam::Address),
    // tech_jump
    OpcodeParam::five(OpParam::U8, OpParam::U8, OpParam::Tech, OpParam::U8, OpParam::Address),
    // random_call
    OpcodeParam::two(OpParam::U8, OpParam::Address),
    // attack_rand
    OpcodeParam::three(OpParam::U8, OpParam::U8, OpParam::Unit),
    // supply
    OpcodeParam::many(&[
        OpParam::U8, OpParam::U8, OpParam::U16, OpParam::U8,
        OpParam::Unit, OpParam::U8, OpParam::Address,
    ]), // time
    OpcodeParam::four(OpParam::U8, OpParam::U32, OpParam::U8, OpParam::Address),
    // resources
    OpcodeParam::five(OpParam::U8, OpParam::U8, OpParam::U8, OpParam::U32, OpParam::Address),
    // set_id
    OpcodeParam::one(OpParam::U8),
    // remove_build
    OpcodeParam::three(OpParam::U8, OpParam::Unit, OpParam::U8),
    // guard
    OpcodeParam::five(OpParam::Unit, OpParam::Point, OpParam::U8, OpParam::U8, OpParam::U8),
    // base_layout_old
    OpcodeParam::five(OpParam::Unit, OpParam::U8, OpParam::Area, OpParam::U8, OpParam::U8),
    // print
    OpcodeParam::one(OpParam::String),
    // attacking
    OpcodeParam::two(OpParam::U8, OpParam::Address),
    // base_layout
    OpcodeParam::many(&[
        OpParam::Unit, OpParam::U8, OpParam::Area, OpParam::U8, OpParam::U8, OpParam::U8
    ]),
    // unit_avail
    OpcodeParam::five(OpParam::U8, OpParam::U8, OpParam::U8, OpParam::Unit, OpParam::Address),
    // load_bunkers
    OpcodeParam::many(&[
        OpParam::Area, OpParam::Unit, OpParam::U8, OpParam::Unit, OpParam::U8, OpParam::U8
    ]),
    // ping
    OpcodeParam::three(OpParam::U16, OpParam::U16, OpParam::U8),
    // reveal_area
    OpcodeParam::four(OpParam::U8, OpParam::Area, OpParam::U16, OpParam::U8),
    // tech_avail
    OpcodeParam::five(OpParam::U8, OpParam::U8, OpParam::Tech, OpParam::U8, OpParam::Address),
    // remove_creep
    OpcodeParam::one(OpParam::Area),
    // save_bank
    OpcodeParam::one(OpParam::String),
    // load_bank
    OpcodeParam::one(OpParam::String),
    // bank_data_old
    OpcodeParam::five(OpParam::U8, OpParam::String, OpParam::String, OpParam::U32, OpParam::Address),
    // unit_name
    OpcodeParam::five(OpParam::U8, OpParam::Unit, OpParam::Area, OpParam::String, OpParam::U8),
    // bank_data
    OpcodeParam::five(OpParam::U8, OpParam::U32, OpParam::String, OpParam::String, OpParam::Address),
    // lift_land
    OpcodeParam::many(&[
        OpParam::Unit, OpParam::U8, OpParam::Area, OpParam::Area,
        OpParam::U8, OpParam::U8, OpParam::U8,
    ]),
    // queue
    OpcodeParam::many(&[
        OpParam::U8, OpParam::Unit, OpParam::Unit, OpParam::U8, OpParam::U8,
        OpParam::Area, OpParam::U8,
    ]),
    // aise_debug
    OpcodeParam::one(OpParam::String),
    // replace_unit
    OpcodeParam::two(OpParam::Unit, OpParam::Unit),
    // defense_command
    OpcodeParam::five(OpParam::U16, OpParam::Unit, OpParam::U8, OpParam::U8, OpParam::U8),
    OpcodeParam::NONE,
    OpcodeParam::NONE,
    //  bw_kills
    OpcodeParam::five(OpParam::U8, OpParam::U8, OpParam::U32, OpParam::Unit, OpParam::Address),
    //  build_at
    OpcodeParam::three(OpParam::Unit, OpParam::Point, OpParam::U32),
    // debug_name
    OpcodeParam::one(OpParam::String),
];

unsafe extern fn debug_tab_scripts(api: *const DebugUiDraw, _: *mut c_void) {
    let draw = DebugUiDrawHelper(api);
    let globals = Globals::get("debug tab scripts");
    let mut debug_ui = DEBUG_UI.lock();
    let debug_ui = &mut *debug_ui;
    let first_bw = bw::first_ai_script();
    if ListIter(first_bw).any(|x| x == debug_ui.current_script) == false {
        debug_ui.current_script = null_mut();
    }
    // Draw script list
    draw.scroll_area(300, |draw| {
        ui_player_filter(draw, &mut debug_ui.script_player_filter, true);
        draw.separator();
        for script in ListIter(first_bw) {
            let index = if script == debug_ui.current_script {
                1
            } else {
                0
            };
            let mut state = 1;
            let player = (*script).player as u8;
            if let Some(filter) = debug_ui.script_player_filter {
                if player != filter {
                    continue;
                }
            }
            let player_name = player_name(player);
            let mut text =
                format!("({},{}) {}", (*script).center.x, (*script).center.y, player_name);
            let aise_script = Script::ptr_from_bw(script);
            if globals.ai_scripts.contains(aise_script) {
                if let Some(name) = (*aise_script).debug_name() {
                    let _ = write!(&mut text, " - {name}");
                }
            }
            let color = DebugUiColor::player(player);
            draw.clickable_label(&text, color, index, &mut state);
            if state == 0 {
                // Was clicked
                debug_ui.current_script = script;
                debug_ui.call_stack_frame = 0;
            }
        }
    });
    // Draw selected script
    if !debug_ui.current_script.is_null() {
        let script = debug_ui.current_script;
        draw.complex_line(
            &format!("[], wait {}", (*script).wait),
            &[complex_line_town((*script).town)],
        );
        let aise_script = Script::ptr_from_bw(script);
        let mut disasm_pos = (*script).pos;
        if globals.ai_scripts.contains(aise_script) {
            let call_stack_size = (*aise_script).call_stack.len() as u32;
            if call_stack_size != 0 {
                draw.label("Call stack:");
                draw.clickable_label(
                    &format!("0: <{:x}>", (*script).pos),
                    DebugUiColor::rgb(0xffffff),
                    0,
                    &mut debug_ui.call_stack_frame,
                );
                for i in 0..call_stack_size {
                    let pos = (*aise_script).call_stack[i as usize];
                    draw.clickable_label(
                        &format!("{}: <{:x}>", i + 1, pos),
                        DebugUiColor::rgb(0xffffff),
                        i + 1,
                        &mut debug_ui.call_stack_frame,
                    );
                    if debug_ui.call_stack_frame == i + 1 {
                        // Was clicked
                        disasm_pos = pos;
                    }
                }
            }
        } else {
            draw.label("Warning: Script not owned by aise?");
        }
        draw.label("Next commands:");
        disasm_script(draw, script, disasm_pos);
    }
}

unsafe extern fn debug_tab_requests(api: *const DebugUiDraw, _: *mut c_void) {
    let draw = DebugUiDrawHelper(api);
    let mut debug_ui = DEBUG_UI.lock();
    let debug_ui = &mut *debug_ui;
    draw.label("Request discard log");
    ui_player_filter(draw, &mut debug_ui.request_player_filter, false);
    draw.separator();
    if let Some(player) = debug_ui.request_player_filter {
        let logs = logs();
        if let Some(&log) = (*logs).requests.get(player as usize) {
            draw.debug_log(log);
        }
    }
}

unsafe fn ui_player_filter(draw: DebugUiDrawHelper, state: &mut Option<u8>, allow_all: bool) {
    let mut state_u32 = match state {
        Some(s) => *s as u32,
        None => u32::MAX,
    };
    let text = if allow_all {
        "All Players"
    } else {
        "None"
    };
    draw.clickable_label(text, DebugUiColor::rgb(0xffffff), u32::MAX, &mut state_u32);
    let players = bw::players();
    for i in 0..8 {
        if (*players.add(i as usize)).player_type == 1 {
            let name = player_name(i);
            let color = DebugUiColor::player(i);
            draw.clickable_label(&name, color, i as u32, &mut state_u32);
        }
    }
    if state_u32 == u32::MAX {
        *state = None;
    } else {
        *state = Some(state_u32 as u8);
    }
}

unsafe fn disasm_script(
    draw: DebugUiDrawHelper,
    script: *mut bw::AiScript,
    pos: u32,
) {
    let mut read = ScriptData::new_with_pos(script, pos, false);
    let buf = &mut String::new();
    let mut unk_param = false;
    'op_loop: for _ in 0..20 {
        if read.is_invalid() {
            draw.label("INVALID");
            break 'op_loop;
        }
        buf.clear();
        let opcode = read.read_u8();
        let name = match OPCODE_NAMES.get(opcode as usize) {
            Some(&s) => s,
            None => {
                let _ = write!(buf, "Unknown {opcode:02x}");
                draw.label(buf);
                break 'op_loop;
            }
        };
        buf.push_str(name);
        let params = match OPCODE_PARAMS.get(opcode as usize) {
            Some(&s) => s,
            None => OpcodeParam::NONE,
        };
        let mut pos = params.0;
        'param_loop: while pos != 0 {
            buf.push(' ');
            let ty = pos & ((1 << OP_PARAM_SHIFT) - 1);
            let ty: OpParam = if ty < OpParam::_Last as u64 {
                mem::transmute(ty as u8)
            } else {
                OpParam::_Last
            };
            match ty {
                OpParam::Address | OpParam::ShortAddress => {
                    let value = if ty == OpParam::Address {
                        read.read_jump_pos()
                    } else {
                        read.read_u16() as u32
                    };
                    let _ = write!(buf, "<{value:x}>");
                }
                OpParam::U8 | OpParam::U16 | OpParam::U32 => {
                    let value = if ty == OpParam::U8 {
                        read.read_u8() as u32
                    } else if ty == OpParam::U16 {
                        read.read_u16() as u32
                    } else {
                        read.read_u32()
                    };
                    let _ = write!(buf, "{value}");
                }
                OpParam::Unit => {
                    // TODO unit name
                    let units = read.read_unit_match();
                    let units = units.as_slice();
                    if units.len() == 1 {
                        let _ = write!(buf, "{}", units[0].0);
                    } else {
                        buf.push('[');
                        for i in 0..units.len() {
                            if i != 0 {
                                buf.push_str(", ");
                            }
                            let _ = write!(buf, "{}", units[i].0);
                        }
                        buf.push(']');
                    }
                }
                OpParam::Tech | OpParam::Upgrade => {
                    // TODO tech/upgrade name
                    let id = read.read_u16();
                    let _ = write!(buf, "{}", id);
                }
                OpParam::String => {
                    let text = read.read_string();
                    buf.push_str(&String::from_utf8_lossy(text));
                }
                OpParam::Point => {
                    // TODO Loc.x
                    let x = read.read_u16();
                    let y = read.read_u16();
                    let _ = write!(buf, "({},{})", x, y);
                }
                OpParam::Area => {
                    // TODO Loc.x etc
                    let x = read.read_u16();
                    let y = read.read_u16();
                    let radius = read.read_u16();
                    if radius == 0 {
                        let _ = write!(buf, "({},{})~{}", x, y, radius);
                    } else {
                        let _ = write!(buf, "({},{})", x, y);
                    }
                }
                _ => {
                    buf.push_str("???");
                    unk_param = true;
                    break 'param_loop;
                }
            }
            pos = pos >> OP_PARAM_SHIFT;
        }
        draw.label(buf);
        if unk_param {
            break;
        }
        match opcode {
            // goto, stop, race_jump, return
            0x00 | 0x24 | 0x39 | 0x41 => break,
            _ => (),
        }
    }
}

fn complex_line_unit_id(id: UnitId) -> ComplexLineParam {
    ComplexLineParam {
        data: id.0 as *mut _,
        ty: 1,
    }
}

fn complex_line_town(town: *mut bw::AiTown) -> ComplexLineParam {
    ComplexLineParam {
        data: town as *mut _,
        ty: 4,
    }
}

fn player_name(player: u8) -> Cow<'static, str> {
    if player < 12 {
        unsafe {
            let players = bw::players();
            let player = players.add(player as usize);
            let name_len = (*player).name.iter().position(|&x| x == 0)
                .unwrap_or_else(|| (*player).name.len());
            let bytes = &(*player).name[..name_len];
            String::from_utf8_lossy(bytes)
        }
    } else {
        Cow::Owned(format!("Player {}", player + 1))
    }
}

#[cold]
#[inline(never)]
pub unsafe fn log_request_error(
    player: u8,
    request: &bw::AiSpendingRequest,
    error: &RequestSatisfyError,
) {
    // TODO samase support for tech/upgrade ids or just integers
    let mut params = [complex_line_unit_id(UnitId(0)); 4];
    match request.ty {
        1 | 2 | 3 | 4 | 7 | 8 => {
            params[0] = complex_line_unit_id(UnitId(request.id));
        }
        _ => return,
    }
    let mut param_len = 1;
    let mut format = match error {
        RequestSatisfyError::Resources => "[] []: Not enough resources",
        RequestSatisfyError::NotAvailable => "[] []: UMS disabled unit",
        RequestSatisfyError::BuildLimit => "[] []: At define_max limit",
        RequestSatisfyError::NeedDatReqs => "[] []: No dat requirements defined",
        RequestSatisfyError::NoGeysers => "[] []: No empty vespene geysers",
        RequestSatisfyError::DatReq(ref reqs) => {
            if let Some(&err) = reqs.get(0) {
                match err {
                    DatReqSatisfyError::NeedUnit(id) => {
                        params[1] = complex_line_unit_id(id);
                        param_len = 2;
                        "[] []: Need []"
                    }
                    DatReqSatisfyError::TooManyUnits(id) => {
                        params[1] = complex_line_unit_id(id);
                        param_len = 2;
                        "[] []: Too many []"
                    }
                    DatReqSatisfyError::NeedTech(_id) => {
                        "[] []: Need tech"
                    }
                    DatReqSatisfyError::NeedAddonless => {
                        "[] []: No addonless parent"
                    }
                    DatReqSatisfyError::NeedEmptySilo => {
                        "[] []: No empty silos"
                    }
                    DatReqSatisfyError::NeedHangarSpace => {
                        "[] []: No hangar space"
                    }
                    DatReqSatisfyError::Disabled => {
                        "[] []: Dat requirement contains disabled / blank"
                    }
                }
            } else {
                "???"
            }
        }
    };
    match request.ty {
        3 | 4 | 5 | 6 | 7 => {
            // Add town as param 0
            params.rotate_right(1);
            params[0] = complex_line_town((*request).val as *mut bw::AiTown);
            param_len += 1;
        }
        _ => {
            // Remove "[] " from the format string
            format = format.trim_start_matches("[] ");
        }
    }
    let logs = logs();
    let log = match (*logs).requests.get(player as usize) {
        Some(&s) => s,
        None => return,
    };
    ((*logs).add_data)(
        log,
        &FfiStr::from_str(format),
        params.as_ptr(),
        param_len,
        null_mut(),
    );
}
