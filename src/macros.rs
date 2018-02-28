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
/// has not been before. Otherwise thread_local::CachedThreadLocal api works as well. The
/// initialization is not guaranteed to be run, and can reuse an older value from another thread.
#[macro_export]
macro_rules! ome2_thread_local {
    () => {};

    ($name:ident: $ty:ty = $fun:ident($expr:expr); $($rest:tt)*) => (
        ome2_thread_local!($name: $ty = $fun($expr));
        ome2_thread_local!($($rest)*);
    );

    ($name:ident: $ty:ty = $fun:ident($expr:expr)) => (
        lazy_static!(static ref $name: ::thread_local::CachedThreadLocal<$ty> =
            ::thread_local::CachedThreadLocal::new(););
        fn $fun() -> &'static $ty {
            $name.get_or(|| Box::new($expr))
        }
    );
}
