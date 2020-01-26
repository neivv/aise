extern crate bw_dat;

#[macro_use]
extern crate log;

#[macro_use]
extern crate whack;

#[macro_use]
mod macros;

pub mod mpqdraft;
pub mod samase;

#[cfg(feature = "opengl")]
mod gl;

mod ai;
mod ai_spending;
mod aiscript;
mod block_alloc;
mod bw;
mod datreq;
mod globals;
mod idle_orders;
mod list;
mod order;
mod recurse_checked_mutex;
mod rng;
mod swap_retain;
mod unit;
mod unit_search;
mod windows;

use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, Ordering};

use libc::c_void;
use parking_lot::Mutex;

use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use bw_dat::Unit;

use crate::globals::Globals;
use crate::unit::UnitExt;

lazy_static::lazy_static! {
    static ref PATCHER: Mutex<whack::Patcher> = Mutex::new(whack::Patcher::new());
}

fn init() {
    if cfg!(debug_assertions) {
        let _ = fern::Dispatch::new()
            .format(|out, message, record| {
                out.finish(format_args!(
                    "{}[{}:{}][{}] {}",
                    chrono::Local::now().format("[%Y-%m-%d][%H:%M:%S]"),
                    record.file().unwrap_or(""),
                    record.line().unwrap_or(0),
                    record.level(),
                    message
                ))
            })
            .level(log::LevelFilter::Trace)
            .chain(fern::log_file("aise.log").unwrap())
            .apply();
    }

    std::panic::set_hook(Box::new(|info| {
        use std::fmt::Write;

        #[cfg(debug_assertions)]
        fn backtrace() -> String {
            use std::path::Path;

            let mut backtrace = String::new();
            backtrace::trace(|frame| {
                let ip = frame.ip();
                let symbol_address = frame.symbol_address();

                backtrace::resolve(ip, |symbol| {
                    let mut line = format!("    {:p}", symbol_address);
                    if symbol_address != ip {
                        write!(line, " ({:p})", symbol_address).unwrap();
                    }
                    let module = windows::module_from_address(symbol_address as *mut _);
                    if let Some((name, base)) = module {
                        if let Some(fname) = Path::new(&name).file_name() {
                            write!(line, " {:?} {:p}", fname, base).unwrap();
                        } else {
                            write!(line, " {:?} {:p}", name, base).unwrap();
                        }
                    }
                    if let Some(name) = symbol.name() {
                        write!(line, " -- {}", name).unwrap();
                    }
                    if let Some(filename) = symbol.filename() {
                        if let Some(lineno) = symbol.lineno() {
                            write!(line, " -- {:?}:{}", filename, lineno).unwrap();
                        } else {
                            write!(line, " -- {:?}:???", filename).unwrap();
                        }
                    }
                    writeln!(backtrace, "{}", line).unwrap();
                });
                true // keep going to the next frame
            });
            backtrace
        }

        #[cfg(not(debug_assertions))]
        fn backtrace() -> String {
            "".into()
        }

        let mut msg = String::new();
        match info.location() {
            Some(s) => writeln!(msg, "Panic at {}:{}", s.file(), s.line()).unwrap(),
            None => writeln!(msg, "Panic at unknown location").unwrap(),
        }
        let payload = info.payload();
        let panic_msg = match payload.downcast_ref::<&str>() {
            Some(s) => s,
            None => match payload.downcast_ref::<String>() {
                Some(s) => &s[..],
                None => "(???)",
            },
        };
        writeln!(msg, "{}", panic_msg).unwrap();
        if cfg!(debug_assertions) {
            write!(msg, "Backtrace:\n{}", backtrace()).unwrap();
        }
        error!("{}", msg);
        windows::message_box("Aise panic", &msg);
        unsafe {
            TerminateProcess(GetCurrentProcess(), 0x4230daef);
        }
    }));
}

static IS_1161: AtomicBool = AtomicBool::new(false);

#[cfg(debug_assertions)]
fn feature_disabled(name: &str) -> bool {
    lazy_static::lazy_static! {
        static ref DISABLED_FEATURES: Mutex<Option<Vec<String>>> = Mutex::new(None);
    }
    let mut disabled_features = DISABLED_FEATURES.lock();
    let disabled_features = disabled_features.get_or_insert_with(|| unsafe {
        let feats = [
            "attack_to",
            "attack_timeout",
            "issue_order",
            "if_attacking",
            "unstart_campaign",
            "set_town_id",
            "remove_build",
            "max_workers",
            "under_attack",
            "aicontrol",
            "supply",
            "resources",
            "reveal_area",
            "load_bank",
            "remove_creep",
            "time",
            "attacking",
            "unit_name",
            "deaths",
            "wait_rand",
            "kills_command",
            "player_jump",
            "upgrade_jump",
            "load_bunkers",
            "unit_avail",
            "tech_jump",
            "tech_avail",
            "random_call",
            "attack_rand",
            "bring_jump",
            "base_layout",
            "queue",
            "lift_land",
            "guard",
            "create_script",
            "idle_orders",
            "everything_else",
        ];
        let (data, len) = match samase::read_file("samase\\aise_disabled_features.txt") {
            Some(s) => s,
            None => return Vec::new(),
        };
        let slice = std::slice::from_raw_parts(data, len);
        let mut result = Vec::new();
        for line in slice.split(|&x| x == b'\n') {
            let line = String::from_utf8_lossy(line);
            let line = line.trim();
            if line.starts_with("#") || line.starts_with(";") || line.is_empty() {
                continue;
            }
            if !feats.iter().any(|&x| x == line) {
                let msg = format!("Feature '{}' not known", line);
                windows::message_box("Aiscript extension plugin", &msg);
                TerminateProcess(GetCurrentProcess(), 0x4230daef);
            }
            result.push(line.into());
        }
        result
    });
    disabled_features.iter().any(|x| x == name)
}

#[cfg(not(debug_assertions))]
fn feature_disabled(_name: &str) -> bool {
    false
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    IS_1161.store(true, Ordering::Release);
    // 1.16.1 init
    unsafe {
        let f: fn() = || {
            let ctx = samase_shim::init_1161();
            samase::samase_plugin_init(ctx.api());

            let mut active_patcher = crate::PATCHER.lock();

            #[cfg(feature = "opengl")]
            gl::init_hooks(&mut active_patcher);

            let mut exe = active_patcher.patch_exe(0x00400000);
            bw::init_funcs(&mut exe);
            bw::init_vars(&mut exe);
            if !feature_disabled("everything_else") {
                exe.hook_opt(bw::increment_death_scores, aiscript::increment_deaths);
                exe.hook_opt(
                    bw::choose_placement_position,
                    aiscript::choose_building_placement,
                );
                exe.hook_opt(
                    bw::update_building_placement_state_hook,
                    aiscript::update_placement_hook,
                );
                exe.hook_opt(bw::ai_spellcast, aiscript::ai_spellcast_hook);
                exe.hook_opt(bw::get_unit_name, aiscript::unit_name_hook);
                exe.hook_opt(bw::ai_focus_unit_check, aiscript::ai_attack_focus_hook);
                exe.hook_opt(
                    bw::add_spending_request,
                    aiscript::add_spending_request_hook,
                );
            }
        };
        samase_shim::on_win_main(f);
    }
}

unsafe extern fn frame_hook() {
    let search = unit_search::UnitSearch::from_bw();
    let mut globals = Globals::get("frame hook");
    let globals = &mut *globals;
    let game = bw::game();
    aiscript::claim_bw_allocated_scripts(globals);
    ai::update_region_safety(&mut globals.region_safety_pos, game, &search);
    aiscript::attack_timeouts_frame_hook(globals, game);
    globals.idle_orders.step_frame(&mut globals.rng, &search);
    aiscript::under_attack_frame_hook(globals);
    aiscript::reveal_vision_hook(globals, game);
    ai::update_guard_needs(game, &mut globals.guards);
    ai::continue_incomplete_buildings();
    aiscript::lift_land_hook(&mut globals.lift_lands, &search, game);
    aiscript::queues_frame_hook(&mut globals.queues, &search, game);

    for unit in unit::active_units() {
        aiscript::bunker_fill_hook(&mut globals.bunker_states, unit, &search);
        if let Some(ai) = unit.building_ai() {
            let town = (*ai).town;
            if town != null_mut() {
                if let Some(max_workers) = aiscript::max_workers_for(globals, town) {
                    (*town).worker_limit = max_workers;
                }
            }
            let iter = (*ai)
                .train_queue_types
                .iter_mut()
                .zip((*ai).train_queue_values.iter_mut());
            for (ty, val) in iter {
                if *ty == 2 && *val != null_mut() {
                    let ai = *val as *mut bw::GuardAi;
                    if let Some(parent) = Unit::from_ptr((*ai).parent) {
                        if parent != unit {
                            // Guard ai share bug, remove the ai from queue
                            debug!(
                                "Guard AI share for unit 0x{:x} at {:?}",
                                unit.id().0,
                                unit.position()
                            );
                            *val = null_mut();
                            *ty = 0;
                        }
                    }
                }
            }
        }
        if let Some(ai) = unit.guard_ai() {
            // Guard ai share bug, just make this a military then.
            // Should happen for cocoons/lurker eggs, but do it for any just-in-case
            if (*ai).parent != *unit {
                debug!(
                    "Guard AI share for unit 0x{:x} at {:?}",
                    unit.id().0,
                    unit.position()
                );
                (**unit).ai = null_mut();
                let ai_regions = bw::ai_regions(unit.player() as u32);
                let region =
                    ai::ai_region(ai_regions, unit.position()).expect("Unit out of bounds??");
                ai::add_military_ai(unit, region, true);
            }
        }
    }

    #[cfg(feature = "opengl")]
    gl::new_frame(&globals);

    FIRST_STEP_ORDER_OF_FRAME.store(true, Ordering::Relaxed);
}

unsafe extern fn frame_hook_after() {
    let mut globals = Globals::get("frame hook after");
    aiscript::update_towns(&mut globals);
    aiscript::attack_timeouts_frame_hook_after(&mut globals);
}

// For hooking the point after frame's ai step but before any unit orders.
// Not in global struct for performance concern of locking its mutex thousand+
// extra times a frame.
static FIRST_STEP_ORDER_OF_FRAME: AtomicBool = AtomicBool::new(false);

unsafe extern fn step_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    if FIRST_STEP_ORDER_OF_FRAME.load(Ordering::Relaxed) {
        FIRST_STEP_ORDER_OF_FRAME.store(false, Ordering::Relaxed);
        let globals = Globals::get("step order hook (start)");
        let game = bw::game();
        // TODO Init lazily
        let unit_search = unit_search::UnitSearch::from_bw();
        ai_spending::frame_hook(game, &unit_search, &globals.ai_mode);
    }

    let unit = Unit::from_ptr(u as *mut bw::Unit).unwrap();
    let prev_order = unit.order();
    let mut temp_ai = None;
    match unit.order() {
        order::id::DIE => {
            let mut globals = Globals::get("step order hook (die)");
            globals.unit_removed(unit);
        }
        order::id::COMPUTER_AI => {
            if let Some(_) = unit.building_ai() {
                let player = unit.player();
                if (**unit).order_timer == 0 && player < 8 {
                    // Handle trains here instead of letting BW to handle them with
                    // stupid guard-related behaviour.
                    let ai = ai::PlayerAi::get(player);
                    ai.check_train(unit, bw::game());
                }
            }
        }
        order::id::ZERG_BIRTH => {
            // See below for unit morph.
            // Make ai temporarily null so bw doesn't try to use it if it's not building.
            let ai = (**unit).ai as *mut bw::BuildingAi;
            if ai != null_mut() && (*ai).ai_type != 3 && (*ai).ai_type != 2 {
                temp_ai = Some(ai as *mut c_void);
                (**unit).ai = null_mut();
            }
        }
        _ => (),
    }
    orig(u);
    match prev_order {
        order::id::UNIT_MORPH => {
            // Fix a bw bug where an unit will keep the egg's building ai for a few frames
            // after being born. If it dies during those frames, bw doesn't remove its ai
            // correctly since it isn't a building anymore.
            // Should be fine to add guard/military now, even though blizzard doesn't do that?
            let should_have_building_ai = |unit: Unit| {
                use bw_dat::unit as id;
                match unit.id() {
                    id::LARVA | id::OVERLORD | id::EGG => true,
                    id::VESPENE_GEYSER => false,
                    x => x.is_building(),
                }
            };
            if !should_have_building_ai(unit) {
                if let Some(building_ai) = unit.building_ai() {
                    let pos = (**unit).current_build_slot as usize;
                    let value = (*building_ai).train_queue_values[pos];
                    let ty = (*building_ai).train_queue_types[pos];

                    let town = (*building_ai).town;
                    let first_free = &mut (*(*town).free_buildings).first_free;
                    list::ListEntry::move_to(building_ai, &mut (*town).buildings, first_free);
                    (**unit).ai = null_mut();

                    match ty {
                        // Military
                        1 => ai::add_military_ai(unit, value as *mut bw::AiRegion, false),
                        // Guard
                        2 => {
                            let guard = value as *mut bw::GuardAi;
                            // Should ideally be caught in frame_hook fix for guard ais, but
                            // this is to not regress over bw.
                            // Cocoons/lurker eggs shouldn't have building ai and not reach here.
                            if (*guard).parent.is_null() {
                                (*guard).parent = *unit;
                                (*guard).home = (*guard).other_home;
                                (**unit).ai = guard as *mut c_void;
                            } else {
                                let ai_regions = bw::ai_regions(unit.player() as u32);
                                let region = ai::ai_region(ai_regions, unit.position())
                                    .expect("Unit out of bounds??");
                                ai::add_military_ai(unit, region, false);
                            }
                        }
                        // New guard
                        _ => ai::add_guard_ai(unit),
                    }
                }
            }
        }
        _ => (),
    }
    if let Some(ai) = temp_ai {
        (**unit).ai = ai;
        // Add ai for dual birthed units
        if unit.id().flags() & 0x400 != 0 {
            // Bw inserts shown units at second pos for some reason..
            if let Some(other) = unit::active_units().nth(1) {
                // This gets actually checked several times since the birth order lasts a few
                // frames, but it should be fine.
                if other.id() == unit.id() && !other.has_ai() {
                    if other.player() == unit.player() {
                        let ai_regions = bw::ai_regions(other.player() as u32);
                        let region = ai::ai_region(ai_regions, other.position())
                            .expect("Unit out of bounds??");
                        ai::add_military_ai(other, region, false);
                    }
                }
            }
        }
    }
}

unsafe extern fn step_order_hidden_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    let unit = Unit::from_ptr(u as *mut bw::Unit).unwrap();
    match unit.order() {
        order::id::DIE => {
            let mut globals = Globals::get("step order hidden hook (die)");
            globals.unit_removed(unit);
        }
        _ => (),
    }
    orig(u);
}

fn lower_bound_by_key<T, C: Ord, F: Fn(&T) -> C>(slice: &[T], val: C, key: F) -> usize {
    use std::cmp::Ordering;
    slice
        .binary_search_by(|a| match key(a) < val {
            true => Ordering::Less,
            false => Ordering::Greater,
        })
        .unwrap_err()
}
