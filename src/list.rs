use bw;

pub trait ListEntry {
    unsafe fn next(*mut Self) -> *mut *mut Self;
}

impl ListEntry for bw::AiTown {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &mut (*x).next
    }
}

impl ListEntry for bw::WorkerAi {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &mut (*x).next
    }
}

impl ListEntry for bw::BuildingAi {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &mut (*x).next
    }
}

pub struct ListIter<T: ListEntry>(pub *mut T);

impl<T: ListEntry> Iterator for ListIter<T> {
    type Item = *mut T;
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.0.is_null() {
                None
            } else {
                let value = self.0;
                self.0 = *ListEntry::next(value);
                Some(value)
            }
        }
    }
}
