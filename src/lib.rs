#[macro_use]
extern crate whack;

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

mod aiscript;
mod bw;
mod windows;

use std::path::Path;

fn init() {
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

    patch();
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern fn Initialize() {
    init();
}

lazy_static! {
    static ref PATCHER: whack::Patcher = whack::Patcher::new();
}

fn patch() {
    unsafe {
        let mut active_patcher = PATCHER.lock().unwrap();

        {
            let mut exe = active_patcher.patch_exe(0x00400000);

            bw::init_funcs(&mut exe);
            bw::init_vars(&mut exe);
            aiscript::add_aiscript_opcodes(&mut exe);
        }
    }
}
