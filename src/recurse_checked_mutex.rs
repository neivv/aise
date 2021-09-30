use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicUsize, Ordering};

use winapi::um::processthreadsapi::GetCurrentThreadId;

// Repr C to move the 3 metadata words first as a optimization, the mutex can be pretty big
// since parking_lot is able to store T without boxing.
// Would be nice to have &'static str be only one word, but &&str would be worse to use
#[repr(C)]
pub struct Mutex<T> {
    locking_thread: AtomicUsize,
    locking_call: UnsafeCell<&'static str>,
    inner: parking_lot::Mutex<T>,
}

pub struct MutexGuard<'a, T: 'a> {
    mutex: &'a Mutex<T>,
    guard: parking_lot::MutexGuard<'a, T>,
}

unsafe impl<T> Sync for Mutex<T> {}

impl<T> Mutex<T> {
    pub const fn new(value: T) -> Mutex<T> {
        Mutex {
            inner: parking_lot::const_mutex(value),
            locking_thread: AtomicUsize::new(0),
            locking_call: UnsafeCell::new(""),
        }
    }

    #[inline]
    pub fn lock<'s>(&'s self, locking_call: &'static str) -> MutexGuard<'s, T> {
        MutexGuard {
            mutex: self,
            guard: self.lock_inner(locking_call),
        }
    }

    fn lock_inner<'s>(&'s self, locking_call: &'static str) -> parking_lot::MutexGuard<'s, T> {
        let self_thread_id;
        unsafe {
            self_thread_id = GetCurrentThreadId();
            if self.locking_thread.load(Ordering::Relaxed) as u32 == self_thread_id {
                panic!(
                    "Thread {} to lock a mutex recursively from '{}', it was already locked by '{}'",
                    self_thread_id, locking_call, *self.locking_call.get(),
                );
            }
        }
        let guard = self.inner.lock();
        self.locking_thread
            .store(self_thread_id as usize, Ordering::Relaxed);
        unsafe {
            *self.locking_call.get() = locking_call;
        }
        guard
    }
}

impl<T: Default> Default for Mutex<T> {
    fn default() -> Self {
        Mutex {
            inner: Default::default(),
            locking_thread: AtomicUsize::new(0),
            locking_call: UnsafeCell::new(""),
        }
    }
}

impl<'a, T: 'a> Drop for MutexGuard<'a, T> {
    fn drop(&mut self) {
        self.mutex.locking_thread.store(0, Ordering::Relaxed);
    }
}

impl<'a, T: 'a> std::ops::Deref for MutexGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.guard
    }
}

impl<'a, T: 'a> std::ops::DerefMut for MutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.guard
    }
}
