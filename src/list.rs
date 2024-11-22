use crate::bw;

pub trait ListEntry: Sized {
    unsafe fn next(x: *mut Self) -> *mut *mut Self;
    unsafe fn prev(x: *mut Self) -> *mut *mut Self;

    // Remove cfg filters if these happen to be needed
    unsafe fn remove(value: *mut Self, list_head: *mut *mut Self) {
        let next = ListEntry::next(value);
        let prev = ListEntry::prev(value);
        if (*next).is_null() {
            // Nothing
        } else {
            *ListEntry::prev(*next) = *prev;
        }
        if (*prev).is_null() {
            assert!(*list_head == value);
            *list_head = *next;
        } else {
            *ListEntry::next(*prev) = *next;
        }
    }

    unsafe fn add(value: *mut Self, list_head: *mut *mut Self) {
        let next = *list_head;
        if !next.is_null() {
            *ListEntry::prev(next) = value;
        }
        *ListEntry::next(value) = next;
        *list_head = value;
        *ListEntry::prev(value) = std::ptr::null_mut();
    }

    unsafe fn move_to(value: *mut Self, old: *mut *mut Self, new: *mut *mut Self) {
        ListEntry::remove(value, old);
        ListEntry::add(value, new);
    }
}

impl ListEntry for bw::AiTown {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).prev
    }
}

impl ListEntry for bw::GuardAi {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).prev
    }
}

impl ListEntry for bw::WorkerAi {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).prev
    }
}

impl ListEntry for bw::BuildingAi {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).prev
    }
}

impl ListEntry for bw::MilitaryAi {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).prev
    }
}

impl ListEntry for bw::Unit {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        (&raw mut (*x).flingy.next) as *mut *mut Self
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        (&raw mut (*x).flingy.prev) as *mut *mut Self
    }
}

impl ListEntry for bw::AiScript {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &raw mut (*x).prev
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

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;
    use std::ptr::null_mut;

    fn generate_list(amount: usize) -> Vec<bw::AiTown> {
        unsafe {
            let mut list: Vec<bw::AiTown> = (0..amount).map(|_| mem::zeroed()).collect();
            for i in 0..amount {
                list[i].prev = match i == 0 {
                    true => null_mut(),
                    false => &mut list[i - 1],
                };
                list[i].next = match i == amount - 1 {
                    true => null_mut(),
                    false => &mut list[i + 1],
                };
            }
            list
        }
    }

    unsafe fn validate_list(head: *mut bw::AiTown, amount: usize) {
        let mut prev = null_mut();
        let mut entry = head;
        while !entry.is_null() {
            assert!((*entry).prev == prev);
            prev = entry;
            entry = (*entry).next;
        }
        if !prev.is_null() {
            assert!((*prev).next.is_null());
        }
        assert_eq!(ListIter(head).count(), amount);
    }

    #[test]
    fn remove() {
        unsafe {
            let mut list = generate_list(6);
            let mut first: *mut bw::AiTown = &mut list[0];
            validate_list(first, 6);
            ListEntry::remove(&mut list[3], &mut first);
            validate_list(first, 5);
            assert!(first == &mut list[0]);
            ListEntry::remove(&mut list[2], &mut first);
            validate_list(first, 4);
            assert!(first == &mut list[0]);
            ListEntry::remove(&mut list[0], &mut first);
            validate_list(first, 3);
            assert!(first == &mut list[1]);
            ListEntry::remove(&mut list[5], &mut first);
            validate_list(first, 2);
            assert!(first == &mut list[1]);

            let mut list = generate_list(1);
            let mut first: *mut bw::AiTown = &mut list[0];
            validate_list(first, 1);
            ListEntry::remove(&mut list[0], &mut first);
            validate_list(first, 0);
            assert!(first == null_mut());
        }
    }

    #[test]
    fn add() {
        unsafe {
            let mut list = generate_list(6);
            let mut first: *mut bw::AiTown = &mut list[0];
            validate_list(first, 6);
            let mut new: bw::AiTown = mem::zeroed();
            ListEntry::add(&mut new, &mut first);
            validate_list(first, 7);
            assert!(first == &mut new);

            let mut first = null_mut();
            let mut new: bw::AiTown = mem::zeroed();
            validate_list(first, 0);
            ListEntry::add(&mut new, &mut first);
            validate_list(first, 1);
            assert!(first == &mut new);
        }
    }

    #[test]
    fn move_to() {
        unsafe {
            let mut list1 = generate_list(6);
            let mut list2 = generate_list(2);
            let mut first1: *mut bw::AiTown = &mut list1[0];
            let mut first2: *mut bw::AiTown = &mut list2[0];
            validate_list(first1, 6);
            validate_list(first2, 2);

            ListEntry::move_to(&mut list1[4], &mut first1, &mut first2);
            validate_list(first1, 5);
            validate_list(first2, 3);
            assert!(first2 == &mut list1[4]);

            ListEntry::move_to(&mut list1[0], &mut first1, &mut first2);
            validate_list(first1, 4);
            validate_list(first2, 4);
            assert!(first2 == &mut list1[0]);

            ListEntry::move_to(&mut list2[0], &mut first2, &mut first1);
            validate_list(first1, 5);
            validate_list(first2, 3);
            assert!(first1 == &mut list2[0]);
            assert!(first2 == &mut list1[0]);

            ListEntry::move_to(&mut list1[0], &mut first2, &mut first1);
            validate_list(first1, 6);
            validate_list(first2, 2);
            assert!(first1 == &mut list1[0]);
        }
    }
}
