use std::mem;
use std::ptr::{self, null_mut};

/// Allocating memory in blocks.
/// Main reason for writing this was to get fast "is this pointer allocated by bw or aise"
/// checks w/ contains().
///
/// Since this is used for allocating scripts that'll be given to bw, it'll mainly work with
/// *mut T pointers instead of a nicer api.
///
/// Doesn't release memory back to OS atm, since the aiscript code will sanity fail on 8k
/// scripts, which is still less than 0.5 Mbytes.
#[derive(Debug)]
pub struct BlockAllocSet<T> {
    blocks: Vec<Block<T>>,
    alloc_count: usize,
}

impl<T> Default for BlockAllocSet<T> {
    fn default() -> BlockAllocSet<T> {
        BlockAllocSet::new()
    }
}

#[derive(Debug)]
struct Block<T> {
    start: *mut T,
    first_free: *mut FreeChunk,
    allocations: Vec<*mut T>,
}

unsafe impl<T: Send> Send for Block<T> {}

impl<T> Drop for Block<T> {
    fn drop(&mut self) {
        unsafe {
            for &ptr in &self.allocations {
                ptr::drop_in_place(ptr);
            }
            Vec::from_raw_parts(self.start, 0, entries_in_chunk::<T>());
        }
    }
}

struct FreeChunk {
    next: *mut FreeChunk,
    /// Amount of T that this chunk can contain.
    count: usize,
}

impl<T> BlockAllocSet<T> {
    pub const fn new() -> BlockAllocSet<T> {
        BlockAllocSet {
            blocks: Vec::new(),
            alloc_count: 0,
        }
    }

    /// Note: Returns true for dangling pointers that are inside blocks as well.
    pub fn contains(&self, object: *mut T) -> bool {
        self.blocks.iter().any(|x| x.contains(object))
    }

    pub fn len(&self) -> usize {
        self.alloc_count
    }

    pub fn alloc(&mut self, value: T) -> *mut T {
        // todo? lazy and works for aiscript
        assert!(mem::size_of::<T>() >= mem::size_of::<FreeChunk>());
        unsafe {
            let ptr = match self.blocks.iter_mut().position(|x| !x.first_free.is_null()) {
                Some(s) => {
                    let block = &mut self.blocks[s];
                    let ptr = if (*block.first_free).count != 1 {
                        (*block.first_free).count -= 1;
                        (block.first_free as *mut T).offset((*block.first_free).count as isize)
                    } else {
                        let ptr = block.first_free as *mut T;
                        block.first_free = (*block.first_free).next;
                        ptr
                    };
                    block.allocations.push(ptr);
                    ptr
                }
                None => {
                    let entry_count = entries_in_chunk::<T>();
                    let memory = alloc_bytes::<T>(entry_count);
                    let first_free = memory as *mut FreeChunk;
                    (*first_free).next = null_mut();
                    (*first_free).count = entry_count - 1;
                    let ptr = memory.offset(entry_count as isize - 1);
                    self.blocks.push(Block {
                        start: memory,
                        allocations: {
                            let mut vec = Vec::with_capacity(entry_count);
                            vec.push(ptr);
                            vec
                        },
                        first_free,
                    });
                    ptr
                }
            };
            ptr::write(ptr, value);
            self.alloc_count += 1;
            ptr
        }
    }

    // Dead code otherwise
    #[cfg(test)]
    pub unsafe fn free(&mut self, value: *mut T) {
        let block = self
            .blocks
            .iter_mut()
            .find(|x| x.contains(value))
            .expect("Invalid free");
        block.free_inner(value);
        let alloc_pos = block
            .allocations
            .iter()
            .position(|&x| x == value)
            .expect("Invalid free");
        block.allocations.swap_remove(alloc_pos);
        self.alloc_count -= 1;
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = *mut T> + 'a {
        self.blocks
            .iter()
            .flat_map(|x| x.allocations.iter().cloned())
    }

    pub fn retain<F: FnMut(*mut T) -> bool>(&mut self, mut f: F) {
        for block in &mut self.blocks {
            let mut i = 0;
            while i < block.allocations.len() {
                let ptr = block.allocations[i];
                if f(ptr) {
                    i += 1;
                } else {
                    unsafe {
                        block.free_inner(ptr);
                    }
                    block.allocations.swap_remove(i);
                    self.alloc_count -= 1;
                }
            }
        }
    }
}

impl<T> Block<T> {
    fn contains(&self, value: *mut T) -> bool {
        unsafe {
            let end = self.start.offset(entries_in_chunk::<T>() as isize - 1);
            self.start as usize <= value as usize && end as usize >= value as usize
        }
    }

    // Caller has to update self.allocations
    unsafe fn free_inner(&mut self, value: *mut T) {
        if cfg!(debug_assertions) {
            assert!((value as usize - self.start as usize) % mem::size_of::<T>() == 0);
            let mut free = self.first_free;
            while !free.is_null() {
                assert!(
                    free as usize > value as usize ||
                        free.offset((*free).count as isize) as usize <= value as usize
                );
                free = (*free).next;
            }
            assert!((value as usize - self.start as usize) % mem::size_of::<T>() == 0);
        }
        ptr::drop_in_place(value);
        let free = value as *mut FreeChunk;
        (*free).next = self.first_free;
        (*free).count = 1;
        self.first_free = free;
    }
}

fn entries_in_chunk<T>() -> usize {
    64
}

fn alloc_bytes<T>(count: usize) -> *mut T {
    let mut vec = Vec::<T>::with_capacity(count);
    let ptr = vec.as_mut_ptr();
    mem::forget(vec);
    ptr
}

#[cfg(test)]
mod test {
    use super::*;

    #[repr(align(16))] // Makes size large enough for 64bit
    struct S(u64);

    #[test]
    fn basic() {
        let mut cont = BlockAllocSet::new();
        let mut ptrs = Vec::new();
        for i in 0..256 {
            let ptr = cont.alloc(S(i));
            ptrs.push(ptr);
        }
        ptrs.sort();
        ptrs.dedup();
        assert_eq!(ptrs.len(), 256);
        for &ptr in &ptrs {
            assert!(cont.contains(ptr));
        }
        let mut local = S(235);
        assert!(!cont.contains(&mut local));
        let mut boxed = Box::new(S(235));
        assert!(!cont.contains(&mut *boxed));
        assert_eq!(cont.len(), 256);

        for i in 0..128 {
            let ptr = ptrs[i * 2];
            unsafe {
                cont.free(ptr);
            }
        }
        assert_eq!(cont.len(), 128);

        for ptr in cont.iter() {
            assert!(ptrs.iter().position(|&x| x == ptr).unwrap() & 1 == 1);
        }
        // Check that iter produces 128 unique values
        let mut iter_ptrs = cont.iter().collect::<Vec<_>>();
        iter_ptrs.sort();
        iter_ptrs.dedup();
        assert_eq!(iter_ptrs.len(), 128);
    }

    #[test]
    fn retain() {
        let mut cont = BlockAllocSet::new();
        for i in 0..256 {
            cont.alloc(S(i));
        }
        cont.retain(|x| unsafe { (*x).0 & 1 == 0 });
        assert_eq!(cont.len(), 128);

        for ptr in cont.iter() {
            assert_eq!(unsafe { (*ptr).0 } & 0, 0);
        }
        // Check that iter produces 128 unique values
        let mut iter_ptrs = cont.iter().collect::<Vec<_>>();
        iter_ptrs.sort();
        iter_ptrs.dedup();
        assert_eq!(iter_ptrs.len(), 128);
    }

    #[test]
    fn reuse() {
        let mut cont = BlockAllocSet::new();
        let mut ptrs = Vec::new();
        for i in 0..256 {
            let ptr = cont.alloc(S(i));
            ptrs.push(ptr);
        }
        cont.retain(|x| unsafe { (*x).0 & 1 == 0 });
        let mut reused_ptrs = 0;
        // Assuming that all ptrs will be reused with 128 extra allocs
        for i in 0..256 {
            let new = cont.alloc(S(1000 + i));
            if ptrs.iter().any(|&x| x == new) {
                reused_ptrs += 1;
            }
        }
        assert_eq!(reused_ptrs, 128);
        for i in 0..128 {
            assert_eq!(unsafe { (*ptrs[i * 2]).0 }, i as u64 * 2);
            assert!(unsafe { (*ptrs[i * 2 + 1]).0 } >= 1000);
        }
    }
}
