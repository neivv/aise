#[macro_use] extern crate whack;
extern crate bw_dat;

extern crate backtrace;
extern crate byteorder;
extern crate chrono;
extern crate fern;
extern crate kernel32;
#[macro_use] extern crate lazy_static;
extern crate libc;
#[macro_use] extern crate log;
#[macro_use] extern crate scopeguard;
extern crate user32;
extern crate winapi;

pub mod mpqdraft;
pub mod samase;

mod aiscript;
mod bw;
mod unit;
mod order;
mod windows;

use std::path::Path;
use std::sync::atomic::{AtomicBool, ATOMIC_BOOL_INIT, Ordering};

use libc::c_void;

fn init(samase: bool) {
    if cfg!(debug_assertions) {
        let _ = fern::Dispatch::new()
            .format(|out, message, record| {
                out.finish(format_args!("{}[{}:{}][{}] {}",
                    chrono::Local::now()
                        .format("[%Y-%m-%d][%H:%M:%S]"),
                    record.location().file(),
                    record.location().line(),
                    record.level(),
                    message))
            })
            .level(log::LogLevelFilter::Trace)
            .chain(fern::log_file("ais_attackto.log").unwrap())
            .apply();
    }
    std::panic::set_hook(Box::new(|info| {
        use std::fmt::Write;
        let mut msg = String::new();
        match info.location() {
            Some(s) => writeln!(msg, "Panic at {}:{}", s.file(), s.line()).unwrap(),
            None => writeln!(msg, "Panic at unknown location").unwrap()
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
        write!(msg, "Backtrace:\n{}", backtrace).unwrap();
        error!("{}", msg);
        windows::message_box("Ais_attackto panic", &msg);
        unsafe { kernel32::TerminateProcess(kernel32::GetCurrentProcess(), 0x4230daef); }
    }));

    SAMASE_INIT.store(samase, Ordering::Release);
    if !samase {
        patch();
    }
}

static SAMASE_INIT: AtomicBool = ATOMIC_BOOL_INIT;

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    init(false);
}

lazy_static! {
    static ref PATCHER: whack::Patcher = whack::Patcher::new();
}

fn patch() {
    unsafe {
        let mut active_patcher = PATCHER.lock().unwrap();

        {
            let mut exe = active_patcher.patch_exe(0x00400000);

            bw_dat::init_1161(&mut exe);
            bw::v1161::init_funcs(&mut exe);
            bw::v1161::init_vars(&mut exe);
            aiscript::add_aiscript_opcodes(&mut exe);
            exe.call_hook(bw::v1161::StepObjects, frame_hook_1161);
            exe.hook_opt(bw::v1161::StepOrder, step_order_hook_1161);
        }
        bw_dat::init_1161_post();
    }
}

unsafe fn frame_hook_1161() {
    frame_hook();
}

unsafe extern fn frame_hook() {
    let current_frame = (*bw::game()).frame_count;
    if current_frame == 0 {
        aiscript::game_start_init();
    }
    aiscript::step_idle_orders();
}

unsafe extern fn step_order_hook(unit: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    step_order_hook_1161(unit as *mut bw::Unit, &|x| orig(x as *mut c_void));
}

fn step_order_hook_1161(u: *mut bw::Unit, orig: &Fn(*mut bw::Unit)) {
    let unit = unit::Unit(u);
    if unit.order() == order::id::DIE {
        aiscript::remove_from_idle_orders(&unit);
    }
    orig(u);
}
