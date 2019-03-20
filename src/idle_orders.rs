use std::ptr::null_mut;
use std::sync::Arc;

use fxhash::FxHashMap;

use bw_dat::{self, order, OrderId, UnitId, WeaponId};

use aiscript::{PlayerMatch, Position, ReadModifierType, ScriptData, UnitMatch};
use bw;
use game::Game;
use globals::Globals;
use rng::Rng;
use swap_retain::SwapRetain;
use unit::{self, HashableUnit, Unit};
use unit_search::UnitSearch;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IdleOrders {
    orders: Vec<(Arc<IdleOrder>, IdleOrderState)>,
    deathrattles: Vec<Arc<IdleOrder>>,
    ongoing: Vec<OngoingOrder>,
    returning_cloaked: Vec<ReturningCloaked>,
}

impl IdleOrders {
    pub fn unit_removed(&mut self, unit: Unit) {
        for x in self.ongoing.iter().filter(|x| x.target == Some(unit)) {
            x.user.issue_order_ground(order::MOVE, x.home);
        }
        self.ongoing
            .swap_retain(|x| unit != x.user && Some(unit) != x.target);
        self.returning_cloaked.swap_retain(|x| unit != x.unit);
    }

    pub unsafe fn step_frame(&mut self, rng: &mut Rng, units: &UnitSearch) {
        for i in 0..8 {
            let ai = bw::player_ai(i);
            if (*ai).spell_cooldown > 200 {
                // Keep spell cooldown active if default spellcasting was disabled
                (*ai).spell_cooldown = 250;
            }
        }
        let game = Game::get();
        let current_frame = game.frame_count();
        let deathrattles = &self.deathrattles;
        let ongoing = &mut self.ongoing;
        let returning_cloaked = &mut self.returning_cloaked;
        let mut cache = InCombatCache::new();
        // Handle ongoing orders.
        // Return home if finished, cloak if a cloaker, panic if health gets low.
        // Yes, it may consider an order ongoing even if the unit is targeting the
        // target for other reasons. Acceptable?
        ongoing.swap_retain(|o| {
            let mut retain = match o.target {
                None => o.user.orders().any(|x| x.id == o.order),
                Some(target) => o
                    .user
                    .orders()
                    .filter_map(|x| x.target)
                    .any(|x| x == target),
            };
            if retain {
                fn can_personnel_cloak(id: UnitId) -> bool {
                    use bw_dat::unit::*;
                    match id {
                        GHOST | SAMIR_DURAN | INFESTED_DURAN | SARAH_KERRIGAN |
                        INFESTED_KERRIGAN | ALEXEI_STUKOV => true,
                        _ => false,
                    }
                }

                if o.user.health() < o.panic_health {
                    for decl in deathrattles {
                        let mut ctx =
                            decl.target_validation_context(game, units, &mut cache, Some(o.user));
                        if decl.unit_valid(o.user, &[], CheckTargetingFlags::Yes, &ctx) {
                            let panic_target = find_target(o.user, &decl, &[], &mut ctx);
                            if let Some((target, distance)) = panic_target {
                                if distance < decl.radius as u32 {
                                    let (target, pos) = target_pos(&target, decl);
                                    // Prevent panicing in future
                                    o.panic_health = 0;
                                    o.target = target;
                                    o.decl = decl.clone();
                                    o.order = decl.order;
                                    o.user.issue_order(decl.order, pos, target);
                                }
                            }
                        }
                    }
                } else {
                    if can_personnel_cloak(o.user.id()) {
                        step_cloak(o, game);
                    }
                    // Check every 32 frames if target is still valid, pick something
                    // else otherwise.
                    if game.frame_count() & 0x1f == o.start_frame & 0x1f {
                        let mut new_target = None;
                        if let Some(target) = o.target {
                            // This only checks the "common" flags and invincibility,
                            // as some of the targeting things may change so much
                            // that the orders would always be reset.
                            // Maybe should also not recheck target order? Depends a lot on
                            // the order though.
                            let flags_valid = o.decl.target_flags.match_status(
                                target,
                                o.decl.player,
                                CheckTargetingFlags::No,
                                Some(o.user),
                                game,
                                units,
                            );
                            if !flags_valid || target.is_invincible() {
                                let mut ctx = o.decl.target_validation_context(
                                    game,
                                    units,
                                    &mut cache,
                                    Some(o.user),
                                );
                                new_target = find_target(o.user, &o.decl, &[], &mut ctx);
                                // Drop the order unless new target is Some and good distance.
                                retain = false;
                            }
                        }
                        if let Some((new_target, distance)) = new_target {
                            if distance < o.decl.radius as u32 {
                                retain = true;
                                let (target, pos) = target_pos(&new_target, &o.decl);
                                o.target = target;
                                o.user.issue_order(o.order, pos, target);
                            }
                        }
                    }
                }
            }
            if !retain {
                o.user.issue_order_ground(order::MOVE, o.home);
                if o.cloaked {
                    returning_cloaked.push(ReturningCloaked {
                        unit: o.user,
                        start_point: o.user.position(),
                    });
                }
            }
            retain
        });
        // Decloak units that are far enough from target.
        returning_cloaked.swap_retain(|ret| {
            // No accessing inside dropships, so keep the unit as is
            if ret.unit.is_hidden() {
                return true;
            }
            if !ret.unit.is_invisible() {
                return false;
            }
            let pos = ret.unit.position();
            let distance = bw::distance(pos, ret.start_point);
            if distance > 32 * 12 || ((*ret.unit.0).move_target == pos && distance > 32 * 2) {
                ret.unit.issue_secondary_order(bw_dat::order::DECLOAK);
                false
            } else {
                true
            }
        });
        // Start new idle orders, if any can be started.
        for &mut (ref decl, ref mut state) in self.orders.iter_mut().rev() {
            if state.next_frame <= current_frame {
                let pair = find_user_target_pair(&decl, &ongoing, game, units, &mut cache);
                if let Some((user, target)) = pair {
                    let (order_target, pos) = target_pos(&target, &decl);
                    let home = match user.order() {
                        order::MOVE => (*user.0).order_target_pos,
                        _ => user.position(),
                    };
                    user.issue_order(decl.order, pos, order_target);
                    ongoing.push(OngoingOrder {
                        user,
                        start_frame: game.frame_count(),
                        target: order_target,
                        decl: decl.clone(),
                        home,
                        order: decl.order,
                        panic_health: panic_health(&user),
                        cloaked: false,
                    });
                    // Round to multiple of decl.rate so that priority is somewhat useful.
                    // Adds [rate, rate * 2) frames of wait.
                    let rate = if decl.rate.lower == decl.rate.upper {
                        decl.rate.lower as u32
                    } else {
                        rng.synced_rand((decl.rate.lower as u32)..(decl.rate.upper as u32 + 1))
                    };

                    state.next_frame = current_frame
                        .saturating_sub(1)
                        .checked_div(rate)
                        .unwrap_or(current_frame)
                        .saturating_add(2)
                        .saturating_mul(rate);
                } else {
                    state.next_frame = current_frame + 24 * 10;
                }
            }
        }
    }
}

unsafe fn step_cloak(order: &mut OngoingOrder, game: Game) {
    if !order.cloaked && !order.user.is_invisible() {
        let tech = bw_dat::tech::PERSONNEL_CLOAKING;
        let order_energy = order.order.tech().map(|x| x.energy_cost()).unwrap_or(0);
        let min_energy = tech
            .energy_cost()
            .saturating_add(25)
            .saturating_add(order_energy)
            .saturating_mul(256);
        if order.user.energy() as u32 > min_energy {
            let has_tech =
                game.tech_researched(order.user.player(), tech) || order.user.id().is_hero();
            if has_tech {
                let distance = bw::distance(order.user.position(), (*order.user.0).move_target);
                if distance < 32 * 24 {
                    order.user.issue_secondary_order(bw_dat::order::CLOAK);
                    order.cloaked = true;
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct ReturningCloaked {
    unit: Unit,
    start_point: bw::Point,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct OngoingOrder {
    user: Unit,
    target: Option<Unit>,
    decl: Arc<IdleOrder>,
    start_frame: u32,
    home: bw::Point,
    order: OrderId,
    panic_health: i32, // Try deathrattles if the hp drops below this
    cloaked: bool,
}

unsafe impl Send for OngoingOrder {}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct Rate {
    lower: u16,
    upper: u16,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrderCount {
    modifier: ReadModifierType,
    value: u16,
    range: u16,
    players: PlayerMatch,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrder {
    priority: u8,
    order: OrderId,
    limit: u16,
    unit_id: UnitMatch,
    target_unit_id: UnitMatch,
    radius: u16,
    target_flags: IdleOrderFlags,
    self_flags: IdleOrderFlags,
    rate: Rate,
    player: u8,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrderFlags {
    simple: u8,
    status_required: u16,
    status_not: u16,
    units_dat_required: u32,
    units_dat_not: u32,
    tiles_required: u32,
    tiles_not: u32,
    targeting_filter: TargetingFlags,
    order: Option<OrderId>,
    numeric: Vec<(IdleOrderNumeric, Comparision, i32)>,
    count: Option<IdleOrderCount>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
enum Comparision {
    LessThanPercentage,
    GreaterThanPercentage,
    LessThan,
    GreaterThan,
}

bitflags! {
    #[derive(Deserialize, Serialize)]
    struct TargetingFlags: u8 {
        const CURRENT_UNIT = 0x1;
        const ENEMY = 0x2;
        const ALLY = 0x4;
        const OWN = 0x8;
        const NOTHING = 0x10;
        const NOT_CURRENT_UNIT_FILTER = Self::ENEMY.bits | Self::ALLY.bits | Self::OWN.bits |
            Self::NOTHING.bits;
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
enum IdleOrderNumeric {
    Hp,
    Shields,
    Energy,
    Health,
    Hangar,
    Cargo,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct IdleOrderState {
    next_frame: u32,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum CheckTargetingFlags {
    Yes,
    No,
}

impl IdleOrderState {
    fn new() -> IdleOrderState {
        IdleOrderState {
            next_frame: 0,
        }
    }
}

pub unsafe extern fn idle_orders(script: *mut bw::AiScript) {
    // idle_orders(order, rate, count, unit_id, radius, target_id, priority, flags)
    // Flag 0x1 = Don't target enemies,
    //      0x2 = Target own,
    //      0x4 = Target allies,
    //      0x8 = Pick unseen targets
    //      0x10 = Pick invisible targets
    //      0x20 = In combat
    //      0x40 = Deathrattle
    //      0x100 ~ 0x2000 = Extensions
    //      0x4000 = Remove matching, no error on mismatch
    //      0x8000 = Remove matching

    let mut read = ScriptData::new(script);
    let order = OrderId(read.read_u8());
    let rate = read.read_u16();
    let limit = read.read_u16();
    let unit_id = read.read_unit_match();
    let radius = read.read_u16();
    let target_unit_id = read.read_unit_match();
    let priority = read.read_u8();
    let mut delete_flags = 0;
    let mut target_flags;
    let mut self_flags;
    let mut rate = Rate {
        upper: rate,
        lower: rate,
    };
    {
        unsafe fn parse_flags(
            read: &mut ScriptData,
            flags: &mut IdleOrderFlags,
            mut self_flags: Option<&mut IdleOrderFlags>,
            delete_flags: &mut u16,
            rate: &mut Rate,
        ) -> bool {
            loop {
                let val = read.read_u16();
                match (val & 0x2f00) >> 8 {
                    0 => {
                        flags.simple = (val & 0xff) as u8;
                        *delete_flags = val & 0xc000;
                        return true;
                    }
                    1 => {
                        flags.status_required = ((val & 0xff) | ((val & 0x4000) >> 6)) as u16;
                    }
                    2 => {
                        flags.status_not = ((val & 0xff) | ((val & 0x4000) >> 6)) as u16;
                    }
                    3 => {
                        let amount = read.read::<i32>();
                        let comparision = match val & 0xf {
                            0 => Comparision::LessThan,
                            1 => Comparision::GreaterThan,
                            2 => Comparision::LessThanPercentage,
                            3 => Comparision::GreaterThanPercentage,
                            x => {
                                bw_print!(
                                    "idle_orders: invalid encoding, invalid comparison: {}",
                                    x
                                );
                                return false;
                            }
                        };
                        let ty = match (val >> 4) & 0xf {
                            0 => IdleOrderNumeric::Hp,
                            1 => IdleOrderNumeric::Shields,
                            2 => IdleOrderNumeric::Health,
                            3 => IdleOrderNumeric::Energy,
                            4 => IdleOrderNumeric::Hangar,
                            5 => IdleOrderNumeric::Cargo,
                            x => {
                                bw_print!("idle_orders: invalid encoding, invalid numeric: {}", x);
                                return false;
                            }
                        };

                        flags.numeric.push((ty, comparision, amount));
                    }
                    4 => {
                        if val & 0xff == 0 {
                            if let Some(ref mut self_flags) = self_flags {
                                let ok = parse_flags(read, *self_flags, None, delete_flags, rate);
                                if !ok {
                                    return false;
                                }
                            }
                        } else {
                            bw_print!("idle_orders: invalid self encoding");
                            return false;
                        }
                    }
                    5 => {
                        flags.order = Some(OrderId((val & 0xff) as u8));
                    }
                    6 => {
                        let units_dat_flags = read.read_u32();
                        match val & 0xff {
                            0 => flags.units_dat_required = units_dat_flags,
                            1 => flags.units_dat_not = units_dat_flags,
                            _ => {
                                bw_print!("idle_orders: invalid encoding");
                                return false;
                            }
                        }
                    }
                    7 => {
                        match TargetingFlags::from_bits((val & 0xff) as u8) {
                            Some(s) => flags.targeting_filter = s,
                            None => {
                                bw_print!("idle_orders: invalid encoding");
                                return false;
                            }
                        };
                    }
                    8 => {
                        let mut first = read.read_u16();
                        let mut second = read.read_u16();
                        if first > second {
                            rate.lower = second;
                            rate.upper = first;
                        } else {
                            rate.lower = first;
                            rate.upper = second;
                        }
                    }
                    9 => {
                        let modifier = read.read_u8();
                        let value = read.read_u16();
                        let range = read.read_u16();
                        let game = Game::get();
                        let players = read.read_player_match(game);
                        let modifier = match modifier {
                            0 => ReadModifierType::AtLeast,
                            1 => ReadModifierType::AtMost,
                            10 => ReadModifierType::Exactly,
                            x => {
                                bw_print!("Unsupported modifier in count flag: {:x}", x);
                                ReadModifierType::AtLeast
                            }
                        };
                        flags.count = Some(IdleOrderCount {
                            modifier,
                            value,
                            range,
                            players,
                        });
                    }
                    10 => {
                        let tile = read.read_u8();
                        // 0x1 = Walkable, 0x2 = Buildable, 0x4 = Creep, 0x8 = Ramp
                        let tile_flags = (if tile & 0x1 != 0 { 0x10000 } else { 0 }) |
                            (if tile & 0x2 != 0 { 0x0080_0000 } else { 0 }) |
                            (if tile & 0x4 != 0 { 0x0040_0000 } else { 0 }) |
                            (if tile & 0x8 != 0 { 0x2000_0000 } else { 0 });
                        match val & 0xff {
                            0 => flags.tiles_required = tile_flags,
                            1 => flags.tiles_not = tile_flags,
                            _ => {
                                bw_print!("idle_orders: invalid encoding (tile)");
                                return false;
                            }
                        }
                    }
                    _ => bw_print!("idle_orders: invalid encoding"),
                }
            }
        }
        target_flags = IdleOrderFlags {
            simple: 0,
            status_required: 0,
            status_not: 0,
            units_dat_required: 0,
            units_dat_not: 0,
            tiles_required: 0,
            tiles_not: 0,
            targeting_filter: TargetingFlags::empty(),
            order: None,
            numeric: Vec::new(),
            count: None,
        };
        self_flags = IdleOrderFlags {
            simple: 0,
            status_required: 0,
            status_not: 0,
            units_dat_required: 0,
            units_dat_not: 0,
            tiles_required: 0,
            tiles_not: 0,
            targeting_filter: TargetingFlags::empty(),
            order: None,
            numeric: Vec::new(),
            count: None,
        };
        let ok = parse_flags(
            &mut read,
            &mut target_flags,
            Some(&mut self_flags),
            &mut delete_flags,
            &mut rate,
        );
        if !ok {
            // Kill (hang) the script since we have no idea if it is left in a pos where
            // the next command is more unknown idle_orders data or what.
            (*script).wait = 0x7fff_ffff;
            return;
        }
    };
    if crate::feature_disabled("idle_orders") {
        return;
    }
    if order.0 >= 254 {
        if order.0 == 255 {
            // Disable default spellcasting
            (*bw::player_ai((*script).player)).spell_cooldown = 250;
        } else {
            // Enable
            (*bw::player_ai((*script).player)).spell_cooldown = 0;
        }
        return;
    }
    let mut globals = Globals::get("ais idle_orders");
    let orders = &mut globals.idle_orders;
    let deathrattle = target_flags.simple & 0x40 != 0;
    if delete_flags != 0 {
        let silent_fail = delete_flags & 0x4000 != 0;
        let matchee = IdleOrder {
            order,
            limit,
            unit_id,
            target_unit_id,
            radius,
            priority,
            target_flags,
            self_flags,
            rate,
            player: (*script).player as u8,
        };
        let cmp = |x: &IdleOrder| *x == matchee;
        let index = match deathrattle {
            false => orders.orders.iter().position(|x| cmp(&x.0)),
            true => orders.deathrattles.iter().position(|x| cmp(x)),
        };
        match index {
            Some(s) => match deathrattle {
                false => {
                    orders.orders.remove(s);
                }
                true => {
                    orders.deathrattles.remove(s);
                }
            },
            None => {
                if !silent_fail {
                    bw_print!(
                        "idle_orders: Unable to find match to remove for {:#?}",
                        matchee
                    );
                }
            }
        }
    } else {
        let pos = match deathrattle {
            false => orders
                .orders
                .binary_search_by_key(&priority, |x| x.0.priority)
                .unwrap_or_else(|x| x),
            true => orders
                .deathrattles
                .binary_search_by_key(&priority, |x| x.priority)
                .unwrap_or_else(|x| x),
        };
        let order = Arc::new(IdleOrder {
            order,
            limit,
            unit_id,
            target_unit_id,
            radius,
            priority,
            target_flags,
            self_flags,
            rate,
            player: (*script).player as u8,
        });
        match deathrattle {
            false => orders.orders.insert(pos, (order, IdleOrderState::new())),
            true => orders.deathrattles.insert(pos, order),
        }
    }
}

impl IdleOrder {
    /// Idle order user
    fn unit_valid(
        &self,
        user: Unit,
        ongoing: &[OngoingOrder],
        check_targeting: CheckTargetingFlags,
        ctx: &IdleOrderTargetContext,
    ) -> bool {
        let energy_cost = self.order.tech().map(|x| x.energy_cost() << 8).unwrap_or(0);
        user.player() == self.player &&
            self.unit_id.matches(&user) &&
            user.order() != self.order &&
            user.can_issue_order(self.order) &&
            user.energy() as u32 >= energy_cost &&
            self.self_flags.match_status(
                user,
                self.player,
                check_targeting,
                None,
                ctx.game,
                ctx.units,
            ) &&
            !ongoing.iter().any(|x| x.user == user)
    }

    fn target_validation_context<'a>(
        &self,
        game: Game,
        units: &'a UnitSearch,
        cache: &'a mut InCombatCache,
        current_unit: Option<Unit>,
    ) -> IdleOrderTargetContext<'a> {
        let mut result = IdleOrderTargetContext {
            acceptable_players: [false; 12],
            game,
            current_unit,
            units,
            cache,
        };

        let accept_enemies = self.target_flags.simple & 0x1 == 0;
        let accept_own = self.target_flags.simple & 0x2 != 0;
        let accept_allies = self.target_flags.simple & 0x4 != 0;
        for i in 0..12 {
            if i == self.player {
                result.acceptable_players[i as usize] = accept_own;
            } else {
                if !game.allied(self.player, i) {
                    result.acceptable_players[i as usize] = accept_enemies;
                } else {
                    result.acceptable_players[i as usize] = accept_allies;
                }
            }
        }
        result
    }

    fn target_valid(
        &self,
        unit: Unit,
        ongoing: &[OngoingOrder],
        check_targeting: CheckTargetingFlags,
        ctx: &mut IdleOrderTargetContext,
    ) -> bool {
        let accept_unseen = self.target_flags.simple & 0x8 != 0;
        let accept_invisible = self.target_flags.simple & 0x10 != 0;
        let in_combat = self.target_flags.simple & 0x20 != 0;
        let player_mask = 1 << self.player;
        let player = unit.player() as usize;

        if unit.is_invincible() {
            return false;
        }
        let flags_ok = self.target_flags.match_status(
            unit,
            self.player,
            check_targeting,
            ctx.current_unit,
            ctx.game,
            ctx.units,
        );
        if !flags_ok {
            return false;
        }
        if !self.target_unit_id.matches(&unit) || !ctx.acceptable_players[player] {
            return false;
        }
        if !accept_unseen {
            let unseen = unsafe {
                unit.sprite()
                    .map(|s| (*s).visibility_mask & player_mask == 0)
                    .unwrap_or(true)
            };
            if unseen {
                return false;
            }
        }
        if !accept_invisible {
            let fail = unsafe {
                unit.is_invisible() && (*unit.0).detection_status & player_mask as u32 == 0
            };
            if fail {
                return false;
            }
        }
        if in_combat {
            if !ctx.cache.is_in_combat(unit, ctx.game) {
                return false;
            }
        }
        let already_targeted_count = ongoing
            .iter()
            .filter(|x| {
                x.target == Some(unit) && x.order == self.order && x.user.player() == player as u8
            })
            .count();
        already_targeted_count < self.limit as usize
    }
}

struct IdleOrderTargetContext<'a> {
    acceptable_players: [bool; 12],
    game: Game,
    units: &'a UnitSearch,
    cache: &'a mut InCombatCache,
    current_unit: Option<Unit>,
}

fn target_pos(target: &Unit, decl: &IdleOrder) -> (Option<Unit>, bw::Point) {
    let pos = target.position();
    let no_detection = unsafe {
        target.is_invisible() && (*target.0).detection_status & (1 << decl.player) as u32 == 0
    };
    let order_target = if no_detection { null_mut() } else { target.0 };
    (Unit::from_ptr(order_target), pos)
}

fn panic_health(unit: &Unit) -> i32 {
    let id = unit.id();
    let max_health = id.hitpoints().saturating_add(id.shields());
    (max_health / 4).min(unit.health() / 2)
}

fn find_user_target_pair(
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
    game: Game,
    units: &UnitSearch,
    cache: &mut InCombatCache,
) -> Option<(Unit, Unit)> {
    fn find_normal(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        let unit = unit::active_units()
            .find(|&u| decl.unit_valid(u, ongoing, CheckTargetingFlags::Yes, ctx))?;
        // Instead of a *perfect* solution of trying to find closest user-target pair,
        // find closest target for the first unit, and then find closest user for
        // the target (if the distance is large enough for it to matter)
        let (target, distance) = find_target(unit, decl, &ongoing, ctx)?;
        let (user, distance) = {
            if distance > 16 * 32 || distance > decl.radius as u32 {
                match find_user(target, decl, &ongoing, ctx) {
                    None => (unit, distance),
                    Some(s) => s,
                }
            } else {
                (unit, distance)
            }
        };
        if distance < decl.radius as u32 {
            Some((user, target, distance))
        } else {
            None
        }
    }

    fn both_targeting_each_other(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        unit::active_units()
            .filter_map(|x| x.target().map(|target| (x, target)))
            .filter(|&(user, target)| target.target() == Some(user))
            .filter(|&(user, target)| {
                decl.target_valid(target, ongoing, CheckTargetingFlags::No, ctx) &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::No, ctx)
            })
            .map(|(user, tgt)| (user, tgt, bw::distance(user.position(), tgt.position())))
            .min_by_key(|x| x.2)
            .filter(|x| x.2 < decl.radius as u32)
    }

    fn target_targeting_user(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        unit::active_units()
            .filter_map(|x| x.target().map(|user| (user, x)))
            .filter(|&(user, target)| {
                decl.target_valid(target, ongoing, CheckTargetingFlags::No, ctx) &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::Yes, ctx)
            })
            .map(|(user, tgt)| (user, tgt, bw::distance(user.position(), tgt.position())))
            .min_by_key(|x| x.2)
            .filter(|x| x.2 < decl.radius as u32)
    }

    fn user_targeting_target(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        unit::active_units()
            .filter_map(|x| x.target().map(|target| (x, target)))
            .filter(|&(user, target)| {
                decl.target_valid(target, ongoing, CheckTargetingFlags::Yes, ctx) &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::No, ctx)
            })
            .map(|(user, tgt)| (user, tgt, bw::distance(user.position(), tgt.position())))
            .min_by_key(|x| x.2)
            .filter(|x| x.2 < decl.radius as u32)
    }

    fn update_best(best: &mut Option<(Unit, Unit, u32)>, new: Option<(Unit, Unit, u32)>) {
        let better = best.map(|x| x.2).unwrap_or(!0) > new.map(|x| x.2).unwrap_or(!0);
        if better {
            *best = new;
        }
    }

    let mut best = None;
    // If the current unit targeting flag is set, do a special search, as they won't work
    // with just picking one unit and iterating from there.
    // But if there are other targeting flags, do a normal search that will work with those.
    let self_current_unit = decl
        .self_flags
        .targeting_filter
        .contains(TargetingFlags::CURRENT_UNIT);
    let self_normal = decl.self_flags.targeting_filter != TargetingFlags::CURRENT_UNIT;
    let target_current_unit = decl
        .target_flags
        .targeting_filter
        .contains(TargetingFlags::CURRENT_UNIT);
    let target_normal = decl.target_flags.targeting_filter != TargetingFlags::CURRENT_UNIT;

    let mut ctx = decl.target_validation_context(game, units, cache, None);
    if self_current_unit && target_current_unit {
        update_best(
            &mut best,
            both_targeting_each_other(decl, ongoing, &mut ctx),
        );
    }
    if self_current_unit && target_normal {
        update_best(&mut best, user_targeting_target(decl, ongoing, &mut ctx));
    }
    if target_current_unit && self_normal {
        update_best(&mut best, target_targeting_user(decl, ongoing, &mut ctx));
    }
    if self_normal && target_normal {
        update_best(&mut best, find_normal(decl, ongoing, &mut ctx));
    }
    best.map(|x| (x.0, x.1))
}

fn find_target(
    user: Unit,
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
    ctx: &mut IdleOrderTargetContext,
) -> Option<(Unit, u32)> {
    ctx.units.find_nearest(user.position(), |unit| {
        if unit == user {
            return false;
        }
        decl.target_valid(unit, ongoing, CheckTargetingFlags::Yes, ctx)
    })
}

fn find_user(
    target: Unit,
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
    ctx: &IdleOrderTargetContext,
) -> Option<(Unit, u32)> {
    ctx.units.find_nearest(target.position(), |unit| {
        unit != target && decl.unit_valid(unit, ongoing, CheckTargetingFlags::Yes, ctx)
    })
}

impl IdleOrderFlags {
    fn match_status(
        &self,
        unit: Unit,
        decl_player: u8,
        check_targeting: CheckTargetingFlags,
        current_unit: Option<Unit>,
        game: Game,
        units: &UnitSearch,
    ) -> bool {
        unsafe {
            if let Some(order) = self.order {
                if unit.order() != order {
                    return false;
                }
            }
            if let Some(ref s) = self.count {
                let mut position = Position::from_point(unit.position().x, unit.position().y);
                position.extend_area(s.range as i16);
                let unit_count = units
                    .search_iter(&position.area)
                    .filter(|u| s.players.matches(u.player()))
                    .count();

                if !s.modifier.compare(unit_count as u32, s.value as u32) {
                    return false;
                }
            }

            #[cfg_attr(rustfmt, rustfmt_skip)]
            let status_ok = if self.status_required != 0 || self.status_not != 0 {
                let flags = (if (*unit.0).ensnare_timer != 0 { 1 } else { 0 } << 0) |
                    (if (*unit.0).plague_timer != 0 { 1 } else { 0 } << 1) |
                    (if (*unit.0).lockdown_timer != 0 { 1 } else { 0 } << 2) |
                    (if (*unit.0).irradiate_timer != 0 { 1 } else { 0 } << 3) |
                    (if (*unit.0).parasited_by_players != 0 { 1 } else { 0 } << 4) |
                    (if (*unit.0).is_blind != 0 { 1 } else { 0 } << 5) |
                    (if (*unit.0).matrix_timer != 0 { 1 } else { 0 } << 6) |
                    (if (*unit.0).maelstrom_timer != 0 { 1 } else { 0 } << 7) |
                    (if (*unit.0).stim_timer != 0 { 1 } else { 0 } << 8);
                self.status_required & flags == self.status_required &&
                    self.status_not & flags == 0
            } else {
                true
            };
            if !status_ok {
                return false;
            }
            if self.units_dat_required != 0 || self.units_dat_not != 0 {
                let flags = unit.id().flags();
                if flags & self.units_dat_required != self.units_dat_required {
                    return false;
                }
                if flags & self.units_dat_not != 0 {
                    return false;
                }
            }
            if self.tiles_required != 0 || self.tiles_not != 0 {
                let map_width = (*game.0).map_width_tiles;
                let tile = *(*bw::tile_flags).offset(
                    (unit.position().x as u16 / 32) as isize +
                        (map_width * (unit.position().y as u16 / 32)) as isize,
                );

                if tile & self.tiles_required != self.tiles_required {
                    return false;
                }
                if tile & self.tiles_not != 0 {
                    return false;
                }
            }
            if check_targeting == CheckTargetingFlags::Yes && !self.targeting_filter.is_empty() {
                let ok = match unit.target() {
                    None => self.targeting_filter.contains(TargetingFlags::NOTHING),
                    Some(target) => {
                        let targeting_current_unit =
                            self.targeting_filter.contains(TargetingFlags::CURRENT_UNIT) &&
                                current_unit.map(|x| x == target).unwrap_or(false);
                        if targeting_current_unit {
                            true
                        } else {
                            let player = target.player();
                            if player == decl_player {
                                self.targeting_filter.contains(TargetingFlags::OWN)
                            } else if game.allied(decl_player, player) {
                                self.targeting_filter.contains(TargetingFlags::ALLY)
                            } else {
                                self.targeting_filter.contains(TargetingFlags::ENEMY)
                            }
                        }
                    }
                };
                if !ok {
                    return false;
                }
            }
            self.numeric.iter().all(|&(ty, compare, amount)| {
                let id = unit.id();
                let (val, max) = match ty {
                    IdleOrderNumeric::Hp => (unit.hitpoints(), id.hitpoints()),
                    IdleOrderNumeric::Shields => {
                        if !id.has_shields() {
                            return false;
                        }
                        (unit.shields(), id.shields())
                    }
                    IdleOrderNumeric::Health => (
                        unit.hitpoints().saturating_add(unit.shields()),
                        id.hitpoints().saturating_add(id.shields()),
                    ),
                    IdleOrderNumeric::Hangar => {
                        use bw_dat::unit::*;
                        match id {
                            VULTURE | JIM_RAYNOR_VULTURE => (unit.spider_mines(game) as i32, 3),
                            NUCLEAR_SILO => (unit.has_nuke() as i32, 1),
                            // Should this check upgrades for max amount?
                            CARRIER | GANTRITHOR => (unit.hangar_count() as i32, 8),
                            REAVER | WARBRINGER => (unit.hangar_count() as i32, 10),
                            _ => (0, 0),
                        }
                    }
                    IdleOrderNumeric::Cargo => (
                        unit.cargo_count() as i32,
                        unit.id().cargo_space_provided() as i32,
                    ),
                    // TODO max energy
                    IdleOrderNumeric::Energy => (unit.energy() as i32, 250 * 256),
                };
                match compare {
                    Comparision::LessThan => val < amount,
                    Comparision::GreaterThan => val > amount,
                    Comparision::LessThanPercentage => {
                        val.saturating_mul(100).checked_div(max).unwrap_or(0) < amount
                    }
                    Comparision::GreaterThanPercentage => {
                        val.saturating_mul(100).checked_div(max).unwrap_or(0) > amount
                    }
                }
            })
        }
    }
}

/// Since in_combat checks "is this unit being targeted by anyone", it requires going through
/// all units in game, so cache and sort the targeting information to make multiple checks
/// in a frame faster.
struct InCombatCache {
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
            targeting_units.sort_unstable_by_key(|x| (x.0).0 as usize);
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
            unsafe { (*unit.0).ground_cooldown > 0 || (*unit.0).air_cooldown > 0 }
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
            let first = crate::lower_bound_by_key(targeting_units, unit.0, |x| (x.0).0);
            let mut units = targeting_units
                .iter()
                .skip(first)
                .take_while(|x| x.0 == unit);
            let in_range = units.any(|&(_, enemy)| is_unit_in_combat(enemy, unit));
            in_range
        })
    }
}
