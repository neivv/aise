use std::iter::FromIterator;

use bw_dat::Unit;
use once_cell::unsync::OnceCell;

use crate::bw::{self, Point, Rect};
use crate::unit;

pub struct UnitSearch {
    values: Vec<(Unit, Rect)>,
    max_width: u16,
}

pub struct LazyUnitSearch(OnceCell<UnitSearch>);

impl LazyUnitSearch {
    pub fn new() -> LazyUnitSearch {
        LazyUnitSearch(OnceCell::new())
    }

    pub fn get(&self) -> &UnitSearch {
        self.0.get_or_init(|| unsafe { UnitSearch::from_bw() })
    }
}

const DEFAULT_CAPACITY: usize = 2048;

impl FromIterator<(Unit, Rect)> for UnitSearch {
    fn from_iter<I: IntoIterator<Item = (Unit, Rect)>>(i: I) -> UnitSearch {
        let iter = i.into_iter();
        let size = DEFAULT_CAPACITY.max(iter.size_hint().0);
        let mut values = Vec::with_capacity(size);
        values.extend(iter);
        values.sort_by_key(|x| x.1.left);
        let max_width = values
            .iter()
            .map(|x| (x.1.right - x.1.left) as u16)
            .max()
            .unwrap_or(0);
        UnitSearch {
            values,
            max_width,
        }
    }
}

impl UnitSearch {
    pub unsafe fn from_bw() -> UnitSearch {
        unit::active_units()
            .map(|unit| (unit, unit.collision_rect()))
            .collect()
    }

    pub fn search_iter<'s>(&'s self, rect: &Rect) -> SearchIter<'s> {
        if rect.right <= 0 || rect.bottom <= 0 {
            return SearchIter {
                search: self,
                pos: 0,
                end: 0,
                left: 0,
                top: 0,
                bottom: 0,
            };
        }
        let start = lower_bound(
            &self.values,
            rect.left.saturating_sub(self.max_width as i16).max(0) as u16,
        );
        let end = lower_bound(&self.values, rect.right as u16);
        SearchIter {
            search: self,
            pos: start,
            end,
            left: rect.left.max(0) as u16,
            top: rect.top.max(0) as u16,
            bottom: rect.bottom as u16,
        }
    }

    pub fn find_nearest<F>(&self, pos: Point, mut filter: F) -> Option<(Unit, u32)>
    where
        F: FnMut(Unit) -> bool,
    {
        // Dumb algorithm for now
        let mut closest_distance = ::std::u32::MAX;
        let mut nearest = None;
        for &(unit, ref rect) in &self.values {
            // Since values are sorted by rect.left, if rect is on right of point and diff between
            // point.x and left is greater than best result, we can end the search.
            if rect.left > pos.x {
                // That being said, can't just compare `rect.left - pos.x >= closest_distance`,
                // due to int div rounding errors at small distances, so add a 8 pixel safety
                // padding.
                // (Alternatively this function could just return results that can be slightly
                // wrong in near ties)
                let is_too_far = ((rect.left as i32 - pos.x as i32) as u32).saturating_sub(8) >=
                    closest_distance;
                if is_too_far {
                    break;
                }
            }
            let distance = bw::distance(pos, unit.position());
            if distance < closest_distance && filter(unit) {
                nearest = Some(unit);
                closest_distance = distance;
            }
        }
        nearest.map(|u| (u, closest_distance))
    }
}

pub struct SearchIter<'s> {
    search: &'s UnitSearch,
    pos: usize,
    end: usize,
    left: u16,
    top: u16,
    bottom: u16,
}

impl<'s> Iterator for SearchIter<'s> {
    type Item = Unit;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == self.end {
            return None;
        }
        let search = self.search;
        while self.pos < self.end {
            let pos = self.pos;
            self.pos += 1;
            let cbox = &search.values[pos].1;
            let ok = (cbox.right as u16) > self.left &&
                (cbox.top as u16) < self.bottom &&
                (cbox.bottom as u16) > self.top;
            if ok {
                return Some(search.values[pos].0);
            }
        }
        None
    }
}

fn lower_bound(slice: &[(Unit, Rect)], val: u16) -> usize {
    crate::lower_bound_by_key(slice, val, |x| x.1.left as u16)
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod test {
    use super::*;

    use std::mem;

    use crate::bw;

    fn rect_from_tuple(area: (i16, i16, i16, i16)) -> Rect {
        Rect {
            left: area.0,
            top: area.1,
            right: area.2,
            bottom: area.3,
        }
    }

    fn rect_overlaps(a: (i16, i16, i16, i16), b: &Rect) -> bool {
        let a = rect_from_tuple(a);
        a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top
    }

    fn check(cboxes: &[(i16, i16, i16, i16)], area: (i16, i16, i16, i16), count: usize) {
        let mut units = (0..cboxes.len())
            .map(|_| unsafe { mem::zeroed() })
            .collect::<Vec<bw::Unit>>();

        let search = cboxes
            .iter()
            .enumerate()
            .map(|(i, tuple)| {
                let unit = unsafe {
                    Unit::from_ptr(units.as_mut_ptr().wrapping_offset(i as isize)).unwrap()
                };
                (unit, rect_from_tuple(*tuple))
            })
            .collect::<UnitSearch>();

        let area = rect_from_tuple(area);
        let results = search.search_iter(&area).collect::<Vec<_>>();
        assert_eq!(results.len(), count, "Area {:?}", area);
        for (i, unit) in units.iter_mut().enumerate() {
            let ptr = unit as *mut bw::Unit;
            if results.iter().any(|&x| *x == ptr) {
                assert!(
                    rect_overlaps(cboxes[i], &area),
                    "Cbox {:?} didn't overlap {:?}",
                    cboxes[i],
                    area,
                );
            } else {
                assert!(
                    !rect_overlaps(cboxes[i], &area),
                    "Cbox {:?} overlapped {:?}",
                    cboxes[i],
                    area,
                );
            }
        }
    }

    #[test]
    fn basic() {
        let positions = vec![
            (10, 10, 20, 20),
            (10, 10, 20, 20),
            (30, 10, 50, 20),
            (10, 20, 20, 30),
            (60, 60, 80, 90),
            (40, 60, 50, 70),
        ];
        check(&positions, (9, 9, 11, 11), 2);
        check(&positions, (9, 9, 21, 11), 2);
        check(&positions, (9, 9, 11, 21), 3);
        check(&positions, (9, 9, 10, 10), 0);
        check(&positions, (10, 10, 10, 10), 0);
        check(&positions, (19, 19, 20, 20), 2);
        check(&positions, (19, 19, 21, 21), 3);
        check(&positions, (20, 20, 21, 21), 0);
        check(&positions, (19, 20, 21, 21), 1);
        check(&positions, (10, 10, 41, 41), 4);
        check(&positions, (0, 0, 41, 41), 4);
        check(&positions, (50, 50, 80, 80), 1);
        check(&positions, (49, 50, 80, 80), 2);
        check(&positions, (50, 00, 80, 80), 1);
        check(&positions, (49, 00, 80, 80), 3);
        check(&positions, (20, 20, 30, 30), 0);
        check(&positions, (19, 20, 30, 30), 1);
        check(&positions, (20, 20, 31, 30), 0);
    }

    #[test]
    fn empty() {
        let positions = vec![];
        check(&positions, (9, 9, 11, 11), 0);
        check(&positions, (10, 10, 10, 10), 0);
    }

    #[test]
    fn negative_coords() {
        // Negative coords should be accepted as search area, as it's common to offset
        // them without clamping to 0.
        let positions = vec![
            (10, 10, 20, 20),
            (0, 1535, 255, 1545),
        ];
        check(&positions, (-1, -1, 11, 11), 1);
        check(&positions, (-1, -26613, 163, -24486), 0);
    }
}
