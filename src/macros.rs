#[macro_export]
macro_rules! debug_fatal {
    ($($toks:tt)*) => {
        if cfg!(debug_assertions) {
            panic!($($toks)*);
        } else {
            error!($($toks)*);
        }
    }
}

/// A macro for creating static thread locals with assumption that they are generally accessed
/// from a single thread. Also can't run into windows's TLS limitations.
///
/// The value can be easily accessed by calling $fun(), which will initialize it with expr if it
/// has not been before. Otherwise thread_local::ThreadLocal api works as well. The
/// initialization is not guaranteed to be run, and can reuse an older value from another thread.
#[macro_export]
macro_rules! ome2_thread_local {
    () => {};

    ($name:ident: $ty:ty = $fun:ident($expr:expr); $($rest:tt)*) => (
        ome2_thread_local!($name: $ty = $fun($expr));
        ome2_thread_local!($($rest)*);
    );

    ($name:ident: $ty:ty = $fun:ident($expr:expr)) => (
        lazy_static::lazy_static!(static ref $name: ::thread_local::ThreadLocal<$ty> =
            ::thread_local::ThreadLocal::new(););
        fn $fun() -> &'static $ty {
            $name.get_or(|| $expr)
        }
    );
}

#[macro_export]
#[cfg(debug_assertions)]
macro_rules! bw_print {
    ($lit:expr $(,)*) => {{
        crate::samase::print_text(concat!($lit, "\0").as_ptr());
        trace!("Print '{}'", $lit);
    }};

    ($lit:expr, $($toks:tt)*) => {{
        crate::macros::print_and_drop(format_args!($lit, $($toks)*));
    }};
}

#[macro_export]
#[cfg(not(debug_assertions))]
macro_rules! bw_print {
    ($lit:expr $(,)*) => {{
        crate::samase::print_text(concat!($lit, "\0").as_ptr());
    }};

    ($lit:expr, $($toks:tt)*) => {{
        crate::macros::print_and_drop(format_args!($lit, $($toks)*));
    }};
}

// For keeping binsize low with bw_print
#[inline(never)]
pub fn print_and_drop(args: std::fmt::Arguments) {
    let mut text = String::new();
    let _ = std::fmt::write(&mut text, args);
    #[cfg(debug_assertions)]
    trace!("Print '{}'", text);
    text.push('\0');
    crate::samase::print_text(text.as_ptr());
}
