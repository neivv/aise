extern crate bw_dat;

#[cfg(debug_assertions)]
extern crate backtrace;
extern crate bincode;
#[macro_use]
extern crate bitflags;
extern crate byteorder;
extern crate chrono;
extern crate fern;
#[macro_use]
extern crate lazy_static;
extern crate libc;
#[macro_use]
extern crate log;
#[macro_use]
extern crate memoffset;
extern crate rand;
#[macro_use]
extern crate scopeguard;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate smallvec;
extern crate thread_local;
extern crate winapi;

#[macro_use]
extern crate whack;
extern crate samase_shim;

#[macro_use]
mod macros;

pub mod mpqdraft;
pub mod samase;

mod ai;
mod aiscript;
mod block_alloc;
mod bw;
mod datreq;
mod game;
mod globals;
mod idle_orders;
mod list;
mod order;
mod rng;
mod swap_retain;
mod unit;
mod windows;

use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};

use libc::c_void;

use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use globals::Globals;

lazy_static! {
    static ref PATCHER: whack::Patcher = whack::Patcher::new();
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
            }).level(log::LevelFilter::Trace)
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

static IS_1161: AtomicBool = ATOMIC_BOOL_INIT;

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    IS_1161.store(true, Ordering::Release);
    // 1.16.1 init
    unsafe {
        let f: fn() = || {
            let ctx = samase_shim::init_1161();
            samase::samase_plugin_init(ctx.api());
        };
        samase_shim::on_win_main(f);

        let mut active_patcher = ::PATCHER.lock().unwrap();
        let mut exe = active_patcher.patch_exe(0x00400000);
        bw::init_funcs(&mut exe);
        bw::init_vars(&mut exe);
        exe.hook_opt(bw::increment_death_scores, aiscript::increment_deaths);
        exe.hook_opt(
            bw::choose_placement_position,
            aiscript::choose_building_placement,
        );

        bw::IS_1161.store(true, std::sync::atomic::Ordering::Release);
    }
}

unsafe extern fn frame_hook() {
    let mut globals = Globals::get();
    let globals = &mut *globals;
    let game = game::Game::get();
    aiscript::claim_bw_allocated_scripts(globals);
    aiscript::clean_unsatisfiable_requests(globals);
    aiscript::attack_timeouts_frame_hook(globals, game);
    globals.idle_orders.step_frame(&mut globals.rng);
    aiscript::under_attack_frame_hook(globals);
    ai::update_guard_needs(game, &mut globals.guards);
    ai::continue_incomplete_buildings();
    for unit in unit::active_units() {
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
                    if (*ai).parent != null_mut() {
                        // Guard ai share bug, remove the ai from queue
                        debug!("Guard AI share for unit at {:?}", unit.position());
                        *val = null_mut();
                        *ty = 0;
                    }
                }
            }
        }
    }
}

unsafe extern fn frame_hook_after() {
    let mut globals = Globals::get();
    aiscript::update_towns(&mut globals);
    aiscript::attack_timeouts_frame_hook_after(&mut globals);
}

unsafe extern fn step_order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    let unit = unit::Unit(u as *mut bw::Unit);
    match unit.order() {
        order::id::DIE => {
            let mut globals = Globals::get();
            globals.idle_orders.unit_removed(unit);
        }
        order::id::COMPUTER_AI => {
            if let Some(_) = unit.building_ai() {
                let player = unit.player();
                if (*unit.0).order_timer == 0 && player < 8 {
                    // Handle trains here instead of letting BW to handle them with
                    // stupid guard-related behaviour.
                    let ai = ai::PlayerAi::get(player);
                    ai.check_train(unit, game::Game::get());
                }
            }
        }
        _ => (),
    }
    orig(u);
}
