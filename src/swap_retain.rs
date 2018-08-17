pub trait SwapRetain<T> {
    fn swap_retain<F: FnMut(&mut T) -> bool>(&mut self, f: F);
}

impl<T> SwapRetain<T> for Vec<T> {
    fn swap_retain<F: FnMut(&mut T) -> bool>(&mut self, mut f: F) {
        let mut i = 0;
        while i < self.len() {
            if f(&mut self[i]) {
                i += 1;
            } else {
                self.swap_remove(i);
            }
        }
    }
}
