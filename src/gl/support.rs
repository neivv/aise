use crate::bw;
use crate::list::ListIter;

pub struct UiList<Value: UiListable> {
    pos: usize,
    order: Vec<Value>,
}

pub trait UiListable: Eq + Sized {
    fn get_list() -> Vec<Self>;
}

impl UiListable for *mut bw::AiScript {
    fn get_list() -> Vec<Self> {
        unsafe {
            let mut new_scripts = ListIter(bw::first_ai_script()).collect::<Vec<_>>();
            new_scripts.sort_by_key(|&s| ((*s).player, (*s).town as usize, s as usize));
            new_scripts
        }
    }
}

impl UiListable for *mut bw::AiTown {
    fn get_list() -> Vec<Self> {
        unsafe {
            let mut vals = (0..8)
                .flat_map(|i| ListIter(bw::first_active_ai_town(i)))
                .collect::<Vec<_>>();
            vals.sort_by_key(|&s| ((*s).player, s as usize));
            vals
        }
    }
}

impl<Value: UiListable> Default for UiList<Value> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Value: UiListable> UiList<Value> {
    pub fn new() -> UiList<Value> {
        UiList {
            pos: 0,
            order: Vec::new(),
        }
    }

    pub fn current(&self) -> Option<&Value> {
        self.order.get(self.pos)
    }

    pub fn current_pos(&self) -> usize {
        self.pos
    }

    pub fn len(&self) -> usize {
        self.order.len()
    }

    pub fn update(&mut self) {
        let new_values = Value::get_list();
        if self.pos >= self.order.len() {
            self.order = new_values;
            self.pos = 0;
            return;
        }
        while self.pos > 0 {
            let new_pos = {
                let old_current = &self.order[self.pos];
                new_values.iter().position(|x| x == old_current)
            };
            if let Some(pos) = new_pos {
                self.order = new_values;
                self.pos = pos;
                return;
            }
            self.pos -= 1;
        }
        self.order = new_values;
    }

    pub fn page_back(&mut self, amount: usize) {
        self.pos = self.pos.saturating_sub(amount);
    }

    pub fn page_forward(&mut self, amount: usize) {
        self.pos = self
            .pos
            .saturating_add(amount)
            .min(self.order.len().saturating_sub(1));
    }
}
