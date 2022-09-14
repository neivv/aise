use bw_dat::{Pathing, Region};

const PENDING_ID_COUNT: usize = 128;

#[repr(C)] // To move larger arrays to end
pub struct RegionNeighbourSearch {
    cycle: u8,
    pending_ids: [u16; PENDING_ID_COUNT],
    // If value == cycle => checked or pending
    checked: [u8; 5000],
}

impl RegionNeighbourSearch {
    pub const fn new() -> RegionNeighbourSearch {
        RegionNeighbourSearch {
            checked: [0; 5000],
            pending_ids: [0; 128],
            cycle: 0,
        }
    }

    fn new_cycle(&mut self) {
        let next = self.cycle.wrapping_add(1);
        self.cycle = next;
        if next == 0 {
            self.checked.fill(0);
            self.cycle = 1;
        }
    }

    pub fn neighbours<'a>(
        &'a mut self,
        pathing: Pathing,
        region: u16,
        max_depth: u16,
    ) -> IterNeighbours<'a> {
        self.new_cycle();
        if let Some(out) = self.checked.get_mut(region as usize) {
            *out = self.cycle;
        }
        let mut iter = IterNeighbours {
            buf: self,
            pathing,
            max_depth,
            pending_pos: 0,
            pending_read_pos: 0,
            pending_count: 0,
        };
        iter.add_neighbours(region, 1);
        iter
    }
}

impl Default for RegionNeighbourSearch {
    fn default() -> Self {
        Self::new()
    }
}

pub struct IterNeighbours<'a> {
    buf: &'a mut RegionNeighbourSearch,
    pathing: Pathing,
    max_depth: u16,
    pending_pos: u8,
    pending_read_pos: u8,
    pending_count: u8,
}

impl<'a> Iterator for IterNeighbours<'a> {
    type Item = (u16, Region);
    fn next(&mut self) -> Option<(u16, Region)> {
        if self.pending_count == 0 {
            return None;
        }
        self.pending_count -= 1;
        let pos = self.pending_read_pos as usize;
        self.pending_read_pos = self.pending_read_pos.wrapping_add(2);
        if self.pending_read_pos as usize == PENDING_ID_COUNT {
            self.pending_read_pos = 0;
        }
        let pending = self.buf.pending_ids.get(pos..(pos + 2))?;
        let id = pending[0];
        let depth = pending[1];
        self.add_neighbours(id, depth.wrapping_add(1));
        let region = self.pathing.region(id)?;
        Some((id, region))
    }
}

impl<'a> IterNeighbours<'a> {
    const fn pending_limit() -> usize {
        PENDING_ID_COUNT / 2
    }

    fn add_neighbours(&mut self, region: u16, depth: u16) {
        if depth > self.max_depth || self.pending_count as usize >= Self::pending_limit() {
            return;
        }
        let buf = &mut *self.buf;
        if let Some(region) = self.pathing.region(region) {
            for neighbour_id in region.neighbour_ids() {
                if let Some(entry) = buf.checked.get_mut(neighbour_id as usize) {
                    if *entry != buf.cycle {
                        if self.pending_count as usize >= Self::pending_limit() {
                            return;
                        }
                        *entry = buf.cycle;
                        let pos = self.pending_pos as usize;
                        if let Some(pending) = buf.pending_ids.get_mut(pos..(pos + 2)) {
                            pending[0] = neighbour_id;
                            pending[1] = depth;
                            let mut new_pos = pos + 2;
                            if new_pos == PENDING_ID_COUNT {
                                new_pos = 0;
                            }
                            self.pending_count = self.pending_count.wrapping_add(1);
                            self.pending_pos = new_pos as u8;
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::bw;

    fn make_test_pathing(neighbours: &mut [u16; 1000]) -> bw::Pathing {
        // 0 - 1 - 2
        // |   |   |
        // 3-----4----11--14
        //  \    /
        //   5  9--10   15
        //  /|  |   |   |
        // 7-6--8   12--13--16
        unsafe {
            let mut pathing: bw::Pathing = std::mem::zeroed();
            pathing.region_count = 17;
            let neighbours_ptr = neighbours.as_mut_ptr();
            let mut neighbour_list_pos = 0;
            let mut set_neighbours = |i, list: &[u16]| {
                let new_pos = neighbour_list_pos + list.len();
                assert!(new_pos < 1000);
                {
                    let slice = std::slice::from_raw_parts_mut(
                        neighbours_ptr.add(neighbour_list_pos),
                        list.len(),
                    );
                    slice.copy_from_slice(list);
                }
                let region: &mut bw::Region = &mut pathing.regions[i];
                region.neighbour_ids = neighbours_ptr.add(neighbour_list_pos);
                region.all_neighbours = list.len() as u8;

                neighbour_list_pos = new_pos;
            };
            set_neighbours(0, &[1, 3]);
            set_neighbours(1, &[0, 3, 2, 4]);
            set_neighbours(2, &[1, 4, 11]);
            set_neighbours(3, &[0, 1, 4, 5]);
            set_neighbours(4, &[1, 3, 9, 2, 11]);
            set_neighbours(5, &[3, 6, 7]);
            set_neighbours(6, &[5, 7]);
            set_neighbours(7, &[5, 7, 8]);
            set_neighbours(8, &[6, 9]);
            set_neighbours(9, &[4, 8, 10,]);
            set_neighbours(10, &[9, 12]);
            set_neighbours(11, &[2, 4, 14]);
            set_neighbours(12, &[10, 13]);
            set_neighbours(13, &[12, 15, 16]);
            set_neighbours(14, &[11]);
            set_neighbours(15, &[13]);
            set_neighbours(16, &[13]);
            pathing
        }
    }

    #[test]
    fn test_neighbours() {
        fn check(
            search: &mut RegionNeighbourSearch,
            pathing: Pathing,
            source: u16,
            depth: u16,
            compare: &[&[u16]],
        ) {
            let mut results: Vec<u16> = search.neighbours(pathing, source, depth)
                .map(|x| x.0)
                .collect();
            let mut pos = 0;
            for (i, &slice) in compare.iter().enumerate() {
                let end = (pos + slice.len()).min(results.len());
                let results_slice = &mut results[pos..end];
                results_slice.sort();
                let mut compare: Vec<_> = slice.into();
                compare.sort();
                if compare != results_slice {
                    panic!("depth {} results differ: {:?} vs {:?}", i + 1, results_slice, compare);
                }
                pos += slice.len();
            }
        }
        let mut buf = [0; 1000];
        let mut pathing = make_test_pathing(&mut buf);
        let pathing = unsafe { Pathing::from_ptr(&mut pathing) };
        let mut search = RegionNeighbourSearch::new();
        check(
            &mut search, pathing, 0, 3,
            &[&[1, 3], &[2, 4, 5], &[7, 6, 9, 11]],
        );
        check(
            &mut search, pathing, 16, 5,
            &[&[13], &[12, 15], &[10], &[9], &[4, 8]],
        );
        check(
            &mut search, pathing, 4, 2,
            &[&[3, 1, 9, 2, 11], &[0, 5, 8, 10, 14]],
        );
        // 0 - 1 - 2
        // |   |   |
        // 3-----4----11--14
        //  \    /
        //   5  9--10   15
        //  /|  |   |   |
        // 7-6--8   12--13--16

    }
}
