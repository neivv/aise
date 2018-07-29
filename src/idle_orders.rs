use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};

use bw_dat::{self, order, OrderId, UnitId};

use aiscript::{read_u16, read_u32, read_u8, read_unit_match, UnitMatch};
use bw;
use game::Game;
use globals::Globals;
use swap_retain::SwapRetain;
use unit::{self, Unit};

pub const IDLE_ORDERS_DISABLED: AtomicBool = ATOMIC_BOOL_INIT;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IdleOrders {
    orders: Vec<(IdleOrder, IdleOrderState)>,
    deathrattles: Vec<IdleOrder>,
    ongoing: Vec<OngoingOrder>,
    returning_cloaked: Vec<ReturningCloaked>,
}

impl IdleOrders {
    pub fn unit_removed(&mut self, unit: Unit) {
        for x in self.ongoing.iter().filter(|x| x.target == Some(unit)) {
            unsafe {
                bw::issue_order(x.user.0, order::MOVE, x.home, null_mut(), unit::id::NONE);
            }
        }
        self.ongoing
            .swap_retain(|x| unit != x.user && Some(unit) != x.target);
        self.returning_cloaked.swap_retain(|x| unit != x.unit);
    }

    pub unsafe fn step_frame(&mut self) {
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
        // Yes, it may consider an order ongoing even if the unit is targeting the
        // target for other reasons. Acceptable?
        ongoing.swap_retain(|o| {
            let retain = match o.target {
                None => o.user.orders().any(|x| x.id == o.order),
                Some(target) => o
                    .user
                    .orders()
                    .filter_map(|x| x.target)
                    .any(|x| x == target),
            };
            if !retain {
                bw::issue_order(o.user.0, order::MOVE, o.home, null_mut(), unit::id::NONE);
                if o.cloaked {
                    returning_cloaked.push(ReturningCloaked {
                        unit: o.user,
                        start_point: o.user.position(),
                    });
                }
            } else {
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
                        if decl.unit_valid(&o.user, &[], CheckTargetingFlags::Yes, game) {
                            let ctx = decl.target_validation_context(game, Some(o.user));
                            let panic_target = find_target(&o.user, &decl, &[], &ctx);
                            if let Some((target, distance)) = panic_target {
                                if distance < decl.radius as u32 {
                                    let (target, pos) = target_pos(&target, decl);
                                    // Prevent panicing in future
                                    o.panic_health = 0;
                                    o.target = target;
                                    o.order = decl.order;
                                    let target_ptr = target.map(|x| x.0).unwrap_or(null_mut());
                                    bw::issue_order(
                                        o.user.0,
                                        decl.order,
                                        pos,
                                        target_ptr,
                                        unit::id::NONE,
                                    );
                                }
                            }
                        }
                    }
                } else if can_personnel_cloak(o.user.id()) {
                    step_cloak(o, game);
                }
            }
            retain
        });
        returning_cloaked.swap_retain(|ret| {
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
        for &mut (ref decl, ref mut state) in self.orders.iter_mut().rev() {
            if state.next_frame <= current_frame {
                if let Some((user, target)) = find_user_target_pair(decl, &ongoing, game) {
                    let (order_target, pos) = target_pos(&target, decl);
                    let home = match user.order() {
                        order::MOVE => (*user.0).order_target_pos,
                        _ => user.position(),
                    };
                    let target_ptr = order_target.map(|x| x.0).unwrap_or(null_mut());
                    bw::issue_order(user.0, decl.order, pos, target_ptr, unit::id::NONE);
                    ongoing.push(OngoingOrder {
                        user,
                        target: order_target,
                        home,
                        order: decl.order,
                        panic_health: panic_health(&user),
                        cloaked: false,
                    });
                    // Round to multiple of decl.rate so that priority is somewhat useful.
                    // Adds [rate, rate * 2) frames of wait.
                    let rate = decl.rate as u32;
                    state.next_frame = current_frame
                        .saturating_sub(1)
                        .checked_div(rate)
                        .unwrap_or(current_frame)
                        .saturating_add(2) * rate;
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
    home: bw::Point,
    order: OrderId,
    panic_health: i32, // Try deathrattles if the hp drops below this
    cloaked: bool,
}

unsafe impl Send for OngoingOrder {}

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
    rate: u16,
    player: u8,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrderFlags {
    simple: u8,
    status_required: u8,
    status_not: u8,
    units_dat_required: u32,
    units_dat_not: u32,
    targeting_filter: TargetingFlags,
    order: Option<OrderId>,
    numeric: Vec<(IdleOrderNumeric, Comparision, i32)>,
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
    let order = OrderId(read_u8(script));
    let rate = read_u16(script);
    let limit = read_u16(script);
    let unit_id = read_unit_match(script);
    let radius = read_u16(script);
    let target_unit_id = read_unit_match(script);
    let priority = read_u8(script);
    let mut delete_flags = 0;
    let mut target_flags;
    let mut self_flags;
    {
        unsafe fn parse_flags(
            script: *mut bw::AiScript,
            flags: &mut IdleOrderFlags,
            mut self_flags: Option<&mut IdleOrderFlags>,
            delete_flags: &mut u16,
        ) -> bool {
            loop {
                let val = read_u16(script);
                match (val & 0x2f00) >> 8 {
                    0 => {
                        flags.simple = (val & 0xff) as u8;
                        *delete_flags = val & 0xc000;
                        return true;
                    }
                    1 => flags.status_required = (val & 0xff) as u8,
                    2 => flags.status_not = (val & 0xff) as u8,
                    3 => {
                        let amount = read_u32(script) as i32;
                        let comparision = match val & 0xf {
                            0 => Comparision::LessThan,
                            1 => Comparision::GreaterThan,
                            2 => Comparision::LessThanPercentage,
                            3 => Comparision::GreaterThanPercentage,
                            _ => {
                                bw::print_text("idle_orders: invalid encoding");
                                return false;
                            }
                        };
                        let ty = match (val >> 4) & 0xf {
                            0 => IdleOrderNumeric::Hp,
                            1 => IdleOrderNumeric::Shields,
                            2 => IdleOrderNumeric::Health,
                            3 => IdleOrderNumeric::Energy,
                            _ => {
                                bw::print_text("idle_orders: invalid encoding");
                                return false;
                            }
                        };
                        flags.numeric.push((ty, comparision, amount));
                    }
                    4 => {
                        if val & 0xff == 0 {
                            if let Some(ref mut self_flags) = self_flags {
                                let ok = parse_flags(script, *self_flags, None, delete_flags);
                                if !ok {
                                    return false;
                                }
                            }
                        } else {
                            bw::print_text("idle_orders: invalid encoding");
                            return false;
                        }
                    }
                    5 => {
                        flags.order = Some(OrderId((val & 0xff) as u8));
                    }
                    6 => {
                        let units_dat_flags = read_u32(script);
                        match val & 0xff {
                            0 => flags.units_dat_required = units_dat_flags,
                            1 => flags.units_dat_not = units_dat_flags,
                            _ => {
                                bw::print_text("idle_orders: invalid encoding");
                                return false;
                            }
                        }
                    }
                    7 => {
                        match TargetingFlags::from_bits((val & 0xff) as u8) {
                            Some(s) => flags.targeting_filter = s,
                            None => {
                                bw::print_text("idle_orders: invalid encoding");
                                return false;
                            }
                        };
                    }
                    _ => bw::print_text("idle_orders: invalid encoding"),
                }
            }
        }
        target_flags = IdleOrderFlags {
            simple: 0,
            status_required: 0,
            status_not: 0,
            units_dat_required: 0,
            units_dat_not: 0,
            targeting_filter: TargetingFlags::empty(),
            order: None,
            numeric: Vec::new(),
        };
        self_flags = IdleOrderFlags {
            simple: 0,
            status_required: 0,
            status_not: 0,
            units_dat_required: 0,
            units_dat_not: 0,
            targeting_filter: TargetingFlags::empty(),
            order: None,
            numeric: Vec::new(),
        };
        let ok = parse_flags(
            script,
            &mut target_flags,
            Some(&mut self_flags),
            &mut delete_flags,
        );
        if !ok {
            return;
        }
    };
    if IDLE_ORDERS_DISABLED.load(Ordering::Acquire) == true {
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
    let mut globals = Globals::get();
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
            None => if !silent_fail {
                bw::print_text(&format!(
                    "idle_orders: Unable to find match to remove for {:#?}",
                    matchee
                ));
            },
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
        let order = IdleOrder {
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
        user: &Unit,
        ongoing: &[OngoingOrder],
        check_targeting: CheckTargetingFlags,
        game: Game,
    ) -> bool {
        let energy_cost = self.order.tech().map(|x| x.energy_cost() << 8).unwrap_or(0);
        user.player() == self.player &&
            self.unit_id.matches(user) &&
            user.order() != self.order &&
            user.energy() as u32 >= energy_cost &&
            self.self_flags
                .match_status(user, self.player, check_targeting, None, game) &&
            !ongoing.iter().any(|x| x.user == *user)
    }

    fn target_validation_context(
        &self,
        game: Game,
        current_unit: Option<Unit>,
    ) -> IdleOrderTargetContext {
        let mut result = IdleOrderTargetContext {
            acceptable_players: [false; 12],
            game,
            current_unit,
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
        ctx: &IdleOrderTargetContext,
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
            &unit,
            self.player,
            check_targeting,
            ctx.current_unit,
            ctx.game,
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
            let ok = unit
                .target()
                .map(|x| {
                    let targeting_enemy = !ctx.game.allied(player as u8, x.player());
                    targeting_enemy && unit.order().is_attack_order()
                })
                .unwrap_or(false);
            if !ok {
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

struct IdleOrderTargetContext {
    acceptable_players: [bool; 12],
    game: Game,
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
) -> Option<(Unit, Unit)> {
    fn find_normal(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        let unit = unit::active_units()
            .find(|u| decl.unit_valid(u, ongoing, CheckTargetingFlags::Yes, ctx.game))?;
        // Instead of a *perfect* solution of trying to find closest user-target pair,
        // find closest target for the first unit, and then find closest user for
        // the target (if the distance is large enough for it to matter)
        let (target, distance) = find_target(&unit, decl, &ongoing, ctx)?;
        let (user, distance) = {
            if distance > 16 * 32 || distance > decl.radius as u32 {
                match find_user(&target, decl, &ongoing, ctx.game) {
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
        ctx: &IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        unit::active_units()
            .filter_map(|x| x.target().map(|target| (x, target)))
            .filter(|&(user, target)| target.target() == Some(user))
            .filter(|&(_, target)| decl.target_valid(target, ongoing, CheckTargetingFlags::No, ctx))
            .filter(|(u, _)| decl.unit_valid(u, ongoing, CheckTargetingFlags::No, ctx.game))
            .map(|(user, tgt)| (user, tgt, bw::distance(user.position(), tgt.position())))
            .min_by_key(|x| x.2)
            .filter(|x| x.2 < decl.radius as u32)
    }

    fn target_targeting_user(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        unit::active_units()
            .filter_map(|x| x.target().map(|user| (user, x)))
            .filter(|&(_, target)| decl.target_valid(target, ongoing, CheckTargetingFlags::No, ctx))
            .filter(|(u, _)| decl.unit_valid(u, ongoing, CheckTargetingFlags::Yes, ctx.game))
            .map(|(user, tgt)| (user, tgt, bw::distance(user.position(), tgt.position())))
            .min_by_key(|x| x.2)
            .filter(|x| x.2 < decl.radius as u32)
    }

    fn user_targeting_target(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &IdleOrderTargetContext,
    ) -> Option<(Unit, Unit, u32)> {
        unit::active_units()
            .filter_map(|x| x.target().map(|target| (x, target)))
            .filter(|&(_, target)| {
                decl.target_valid(target, ongoing, CheckTargetingFlags::Yes, ctx)
            })
            .filter(|(u, _)| decl.unit_valid(u, ongoing, CheckTargetingFlags::No, ctx.game))
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

    let ctx = decl.target_validation_context(game, None);
    if self_current_unit && target_current_unit {
        update_best(&mut best, both_targeting_each_other(decl, ongoing, &ctx));
    }
    if self_current_unit && target_normal {
        update_best(&mut best, user_targeting_target(decl, ongoing, &ctx));
    }
    if target_current_unit && self_normal {
        update_best(&mut best, target_targeting_user(decl, ongoing, &ctx));
    }
    if self_normal && target_normal {
        update_best(&mut best, find_normal(decl, ongoing, &ctx));
    }
    best.map(|x| (x.0, x.1))
}

fn find_target(
    user: &Unit,
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
    ctx: &IdleOrderTargetContext,
) -> Option<(Unit, u32)> {
    unit::find_nearest(user.position(), |unit| {
        if unit == user {
            return false;
        }
        decl.target_valid(*unit, ongoing, CheckTargetingFlags::Yes, ctx)
    })
}

fn find_user(
    target: &Unit,
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
    game: Game,
) -> Option<(Unit, u32)> {
    unit::find_nearest(target.position(), |unit| {
        unit != target && decl.unit_valid(unit, ongoing, CheckTargetingFlags::Yes, game)
    })
}

impl IdleOrderFlags {
    fn match_status(
        &self,
        unit: &Unit,
        decl_player: u8,
        check_targeting: CheckTargetingFlags,
        current_unit: Option<Unit>,
        game: Game,
    ) -> bool {
        unsafe {
            if let Some(order) = self.order {
                if unit.order() != order {
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
                    (if (*unit.0).maelstrom_timer != 0 { 1 } else { 0 } << 7);
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
                    },
                    IdleOrderNumeric::Health => (
                        unit.hitpoints().saturating_add(unit.shields()),
                        id.hitpoints().saturating_add(id.shields()),
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
