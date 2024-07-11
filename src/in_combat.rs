use bw_dat::{Game, Unit, WeaponId};
use rustc_hash::FxHashMap;

use crate::bw;
use crate::unit::{self, HashableUnit};

/// Since in_combat checks "is this unit being targeted by anyone", it requires going through
/// all units in game, so cache and sort the targeting information to make multiple checks
/// in a frame faster.
///
/// This is currently used by idle_orders InCombat flag, as well as ai spending unit morphs.
pub struct InCombatCache {
    inner: Option<InCombatCacheInited>,
}

struct InCombatCacheInited {
    // target, targeter, sorted by target, filtered to only have targeting in_combat cares
    // about.
    targeting_units: Vec<(Unit, Unit)>,
    results: FxHashMap<HashableUnit, bool>,
}

impl InCombatCache {
    pub fn new() -> InCombatCache {
        InCombatCache {
            inner: None,
        }
    }

    fn get_inner(&mut self, game: Game) -> &mut InCombatCacheInited {
        self.inner.get_or_insert_with(|| {
            // Also include hidden_units for bunkers
            let mut targeting_units = unit::active_units()
                .chain(unit::hidden_units())
                .filter_map(|targeter| {
                    targeter
                        .target()
                        .filter(|x| x.player() < 8 && targeter.player() < 8)
                        .filter(|x| !game.allied(targeter.player(), x.player()))
                        .map(|target| (target, targeter))
                })
                .collect::<Vec<_>>();
            targeting_units.sort_unstable_by_key(|x| *(x.0) as usize);
            InCombatCacheInited {
                targeting_units,
                results: FxHashMap::with_capacity_and_hasher(32, Default::default()),
            }
        })
    }

    pub fn is_in_combat(&mut self, unit: Unit, game: Game) -> bool {
        // Check that either the unit has recently attacked, or an enemy is within attack
        // range, targeting the unit.
        fn weapon_to_target(attacker: Unit, target: Unit) -> Option<WeaponId> {
            if target.is_air() {
                attacker.id().air_weapon()
            } else {
                attacker.id().ground_weapon()
            }
        }

        fn has_cooldown_active(unit: Unit) -> bool {
            unsafe { (**unit).ground_cooldown > 0 || (**unit).air_cooldown > 0 }
        }

        fn is_unit_in_combat(unit: Unit, target: Unit) -> bool {
            if !has_cooldown_active(unit) {
                return false;
            }
            if let Some(weapon) = weapon_to_target(unit, target) {
                let range = weapon.max_range();
                let own_area = unit.collision_rect();
                bw::rect_distance(&own_area, &target.collision_rect()) <= range
            } else {
                false
            }
        }

        let target = unit
            .target()
            .filter(|x| !game.allied(unit.player(), x.player()));
        if let Some(target) = target {
            if is_unit_in_combat(unit, target) {
                return true;
            }
        }
        let inner = self.get_inner(game);
        let entry = inner.results.entry(HashableUnit(unit));
        let targeting_units = &inner.targeting_units;
        *entry.or_insert_with(|| {
            let first = crate::lower_bound_by_key(targeting_units, *unit, |x| *(x.0));
            if let Some(units) = targeting_units.get(first..) {
                let in_range = units
                    .iter()
                    .take_while(|x| x.0 == unit)
                    .any(|&(_, enemy)| is_unit_in_combat(enemy, unit));
                in_range
            } else {
                false
            }
        })
    }
}
