use std::sync::Arc;

use bumpalo::Bump;
use bumpalo::collections::Vec as BumpVec;
use serde::{Deserialize, Serialize};

use bw_dat::{order, Game, OrderId, Unit, UnitId};

use crate::aiscript::{PlayerMatch, Position, ReadModifierType, ScriptData, UnitMatch};
use crate::bw;
use crate::globals::Globals;
use crate::in_combat::InCombatCache;
use crate::rng::Rng;
use crate::swap_retain::SwapRetain;
use crate::unit::{self, SerializableUnit, UnitExt};
use crate::unit_search::LazyUnitSearch;
use crate::StepUnitsCtx;

#[derive(Serialize, Deserialize)]
pub(crate) struct IdleOrders {
    orders: Vec<(Arc<IdleOrder>, IdleOrderState)>,
    deathrattles: Vec<Arc<IdleOrder>>,
    ongoing: Vec<OngoingOrder>,
    returning_cloaked: Vec<ReturningCloaked>,
    #[serde(skip)]
    bump: Option<Bump>,
}

impl Clone for IdleOrders {
    fn clone(&self) -> IdleOrders {
        IdleOrders {
            orders: self.orders.clone(),
            deathrattles: self.deathrattles.clone(),
            ongoing: self.ongoing.clone(),
            returning_cloaked: self.returning_cloaked.clone(),
            bump: None,
        }
    }
}

struct StepFrameState<'b, 'f> {
    bump: &'b Bump,
    /// Unit id -> buffer idx map.
    /// If nonzero, index (+1) to unit_buffers.
    unit_id_to_buf_idx: &'b mut [u16],
    unit_buffers_reserved: u16,
    /// Lists of units of specific unit id.
    unit_buffers: BumpVec<'b, &'b [Unit]>,
    in_combat_cache: &'f mut InCombatCache,
    unit_search: &'f LazyUnitSearch,
}

impl IdleOrders {
    pub const fn new() -> IdleOrders {
        IdleOrders {
            orders: Vec::new(),
            deathrattles: Vec::new(),
            ongoing: Vec::new(),
            returning_cloaked: Vec::new(),
            bump: None,
        }
    }

    pub fn unit_removed(&mut self, unit: Unit) {
        for x in self
            .ongoing
            .iter()
            .filter(|x| x.target.map(|x| x.0) == Some(unit))
        {
            x.user.0.issue_order_ground(order::MOVE, x.home);
        }
        self.ongoing
            .swap_retain(|x| unit != x.user.0 && Some(unit) != x.target.map(|x| x.0));
        self.returning_cloaked.swap_retain(|x| unit != x.unit.0);
    }

    pub unsafe fn step_frame(
        &mut self,
        rng: &mut Rng,
        ctx: &mut StepUnitsCtx<'_>,
        tile_flags: *mut u32,
    ) {
        for i in 0..8 {
            let ai = bw::player_ai(i);
            if (*ai).spell_cooldown > 200 {
                // Keep spell cooldown active if default spellcasting was disabled
                (*ai).spell_cooldown = 250;
            }
        }
        let game = ctx.game;
        let pathing = bw::pathing();
        let current_frame = game.frame_count();
        let deathrattles = &self.deathrattles;
        let ongoing = &mut self.ongoing;
        let returning_cloaked = &mut self.returning_cloaked;
        let bump = self.bump.get_or_insert_with(|| Bump::new());
        bump.reset();
        let mut step_state = StepFrameState::new(bump, ctx);
        for &(ref decl, ref state) in self.orders.iter() {
            if state.next_frame <= current_frame {
                step_state.prepare_order_check(decl);
            }
        }
        step_state.build_unit_buffers(game);
        // Handle ongoing orders.
        // Return home if finished, cloak if a cloaker, panic if health gets low.
        // Yes, it may consider an order ongoing even if the unit is targeting the
        // target for other reasons. Acceptable?
        ongoing.swap_retain(|o| {
            let user = o.user.0;
            let mut retain = if user.is_hidden() {
                false
            } else {
                match o.target {
                    None => user.orders().any(|x| x.id == o.order),
                    Some(target) => o
                        .user
                        .orders()
                        .filter_map(|x| x.target)
                        .any(|x| x == target.0),
                }
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

                if user.health() < o.panic_health {
                    for decl in deathrattles {
                        let mut ctx = decl.target_validation_context(
                            game,
                            &mut step_state,
                            Some(user),
                            pathing,
                            tile_flags,
                        );
                        if decl.unit_valid(user, &[], CheckTargetingFlags::Yes, &mut ctx) {
                            let panic_target = ctx.find_target(user, &decl, &[]);
                            if let Some((target, distance)) = panic_target {
                                if distance < decl.radius as u32 {
                                    let (target, pos) = target_pos(target, decl);
                                    // Prevent panicing in future
                                    o.panic_health = 0;
                                    o.target = target.map(SerializableUnit);
                                    o.decl = decl.clone();
                                    o.order = decl.order;
                                    user.issue_order(decl.order, pos, target);
                                }
                            }
                        }
                    }
                } else {
                    if can_personnel_cloak(user.id()) {
                        step_cloak(o, game);
                    }
                    // Check every 32 frames if target is still valid, pick something
                    // else otherwise.
                    if current_frame & 0x1f == o.start_frame & 0x1f {
                        let mut new_target = None;
                        if let Some(target) = o.target {
                            // This only checks the "common" flags and invincibility,
                            // as some of the targeting things may change so much
                            // that the orders would always be reset.
                            // Maybe should also not recheck target order? Depends a lot on
                            // the order though.
                            let flags_valid = o.decl.target_flags.match_status(
                                target.0,
                                o.decl.player,
                                CheckTargetingFlags::No,
                                Some(user),
                                game,
                                step_state.unit_search,
                                tile_flags,
                            );
                            if !flags_valid || target.is_invincible() {
                                let mut ctx = o.decl.target_validation_context(
                                    game,
                                    &mut step_state,
                                    Some(user),
                                    pathing,
                                    tile_flags,
                                );
                                new_target = ctx.find_target(user, &o.decl, &[]);
                                // Drop the order unless new target is Some and good distance.
                                retain = false;
                            }
                        }
                        if let Some((new_target, distance)) = new_target {
                            if distance < o.decl.radius as u32 {
                                retain = true;
                                let (target, pos) = target_pos(new_target, &o.decl);
                                o.target = target.map(SerializableUnit);
                                user.issue_order(o.order, pos, target);
                            }
                        }
                    }
                }
            }
            if !retain && !user.is_hidden() {
                user.issue_order_ground(order::MOVE, o.home);
                if o.cloaked {
                    returning_cloaked.push(ReturningCloaked {
                        unit: SerializableUnit(user),
                        start_point: user.position(),
                    });
                }
            }
            retain
        });
        // Decloak units that are far enough from target.
        returning_cloaked.swap_retain(|ret| {
            let unit = ret.unit.0;
            // No accessing inside dropships, so keep the unit as is
            if unit.is_hidden() {
                return true;
            }
            if !unit.is_invisible() {
                return false;
            }
            let pos = unit.position();
            let distance = bw::distance(pos, ret.start_point);
            if distance > 32 * 12 ||
                ((**unit).flingy.move_target.pos == pos && distance > 32 * 2)
            {
                unit.issue_secondary_order(bw_dat::order::DECLOAK);
                false
            } else {
                true
            }
        });
        // Start new idle orders, if any can be started.
        for &mut (ref decl, ref mut state) in self.orders.iter_mut().rev() {
            if state.next_frame <= current_frame {
                let pair = find_user_target_pair(
                    &decl,
                    &ongoing,
                    game,
                    &mut step_state,
                    pathing,
                    tile_flags,
                    state,
                );
                let rate = if decl.rate.lower == decl.rate.upper {
                    decl.rate.lower as u32
                } else {
                    rng.synced_rand(
                        (decl.rate.lower as u32)..((decl.rate.upper as u32).saturating_add(1))
                    )
                };
                if let Some((user, target)) = pair {
                    let (order_target, pos) = target_pos(target, &decl);
                    let home = match user.order() {
                        order::MOVE => (**user).order_target.pos,
                        _ => user.position(),
                    };
                    user.issue_order(decl.order, pos, order_target);
                    ongoing.push(OngoingOrder {
                        user: SerializableUnit(user),
                        start_frame: current_frame,
                        target: order_target.map(SerializableUnit),
                        decl: decl.clone(),
                        home,
                        order: decl.order,
                        panic_health: panic_health(user),
                        cloaked: false,
                    });
                    // Round to multiple of decl.rate so that priority is somewhat useful.
                    // Adds [rate, rate * 2) frames of wait.

                    state.next_frame = current_frame
                        .saturating_sub(1)
                        .checked_div(rate)
                        .unwrap_or(current_frame)
                        .saturating_add(2)
                        .saturating_mul(rate);
                } else {
                    // Clamp next check to be once in half a second .. 10 second range
                    state.next_frame = current_frame.wrapping_add(rate.clamp(12, 24 * 10));
                }
            }
        }
    }
}

unsafe fn step_cloak(order: &mut OngoingOrder, game: Game) {
    let user = order.user.0;
    if !order.cloaked && !user.is_invisible() {
        let move_target = (**user).flingy.move_target.pos;
        let full_distance = bw::distance(order.home, move_target);
        if full_distance < 32 * 24 {
            // Don't cloak if the order is short range
            return;
        }
        let tech = bw_dat::tech::PERSONNEL_CLOAKING;
        let order_energy = order.order.tech().map(|x| x.energy_cost()).unwrap_or(0);
        let min_energy = tech
            .energy_cost()
            .saturating_add(25)
            .saturating_add(order_energy)
            .saturating_mul(256);
        if user.energy() as u32 > min_energy {
            let has_tech = game.tech_researched(user.player(), tech) || user.id().is_hero();
            if has_tech {
                let distance = bw::distance(user.position(), move_target);
                if distance < 32 * 24 {
                    user.issue_secondary_order(bw_dat::order::CLOAK);
                    order.cloaked = true;
                }
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
struct ReturningCloaked {
    unit: SerializableUnit,
    start_point: bw::Point,
}

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
struct OngoingOrder {
    user: SerializableUnit,
    target: Option<SerializableUnit>,
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

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
struct IdleOrderCount {
    modifier: ReadModifierType,
    value: u16,
    range: u16,
    players: PlayerMatch,
}

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
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
    numeric: Vec<(IdleOrderNumeric, Comparison, i32)>,
    count: Option<IdleOrderCount>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
enum Comparison {
    LessThanPercentage,
    GreaterThanPercentage,
    LessThan,
    GreaterThan,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
    struct TargetingFlags: u8 {
        const CURRENT_UNIT = 0x1;
        const ENEMY = 0x2;
        const ALLY = 0x4;
        const OWN = 0x8;
        const NOTHING = 0x10;
        const NOT_CURRENT_UNIT_FILTER =
            Self::ENEMY.bits() | Self::ALLY.bits() | Self::OWN.bits() | Self::NOTHING.bits();
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
    /// Used to start search at a different point in candidate users than previous frame.
    search_pos: SearchPos,
}

/// (unit id, index in list)
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
struct SearchPos(u16, u16);

#[derive(Copy, Clone, Eq, PartialEq)]
enum CheckTargetingFlags {
    Yes,
    No,
}

impl IdleOrderState {
    const fn new() -> IdleOrderState {
        IdleOrderState {
            next_frame: 0,
            search_pos: SearchPos(0, 0),
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
                            0 => Comparison::LessThan,
                            1 => Comparison::GreaterThan,
                            2 => Comparison::LessThanPercentage,
                            3 => Comparison::GreaterThanPercentage,
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
                        let first = read.read_u16();
                        let second = read.read_u16();
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
                        let game = bw::game();
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
                        "idle_orders: Unable to find match to remove for {}",
                        format_idle_order_for_print(&matchee),
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
        ctx: &mut IdleOrderTargetContext<'_, '_, '_>,
    ) -> bool {
        let energy_cost = self.order.tech().map(|x| x.energy_cost() << 8).unwrap_or(0);
        user.player() == self.player &&
            self.unit_id.matches(&user) &&
            user.order() != self.order &&
            user.can_issue_order(self.order) &&
            user.energy() as u32 >= energy_cost &&
            !user.is_hallucination() &&
            self.self_flags.match_status(
                user,
                self.player,
                check_targeting,
                None,
                ctx.game,
                ctx.step_state.unit_search,
                ctx.tile_flags,
            ) &&
            !ongoing.iter().any(|x| x.user.0 == user)
    }

    fn target_validation_context<'a, 'b, 'f>(
        &self,
        game: Game,
        step_state: &'a mut StepFrameState<'b, 'f>,
        current_unit: Option<Unit>,
        pathing: *mut bw::Pathing,
        tile_flags: *mut u32,
    ) -> IdleOrderTargetContext<'a, 'b, 'f> {
        let mut result = IdleOrderTargetContext {
            acceptable_players: [false; 12],
            game,
            current_unit,
            step_state,
            pathing,
            tile_flags,
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
        user: Unit,
        unit: Unit,
        ongoing: &[OngoingOrder],
        check_targeting: CheckTargetingFlags,
        ctx: &mut IdleOrderTargetContext<'_, '_, '_>,
    ) -> bool {
        let accept_unseen = self.target_flags.simple & 0x8 != 0;
        let accept_invisible = self.target_flags.simple & 0x10 != 0;
        let in_combat = self.target_flags.simple & 0x20 != 0;
        let player_mask = 1 << self.player;
        let player = unit.player() as usize;

        if unit.is_invincible() || !unit.is_completed() {
            return false;
        }
        if !self.target_unit_id.matches(&unit) || !ctx.acceptable_players[player] {
            return false;
        }
        if !accept_unseen {
            let unseen = unit
                .sprite()
                .map(|s| s.visibility_mask() & player_mask == 0)
                .unwrap_or(true);
            if unseen {
                return false;
            }
        }
        if !accept_invisible {
            let fail = unsafe {
                unit.is_invisible() && (**unit).detection_status & player_mask as u32 == 0
            };
            if fail {
                return false;
            }
        }
        let flags_ok = self.target_flags.match_status(
            unit,
            self.player,
            check_targeting,
            ctx.current_unit,
            ctx.game,
            ctx.step_state.unit_search,
            ctx.tile_flags,
        );
        if !flags_ok {
            return false;
        }

        // For repair, the player must have resources needed for repairing target
        // (At least 1 mine/gas depending on unit costs)
        if self.order == order::REPAIR {
            let target_id = unit.id();
            if ctx.game.minerals(self.player) == 0 && target_id.mineral_cost() != 0 {
                return false;
            }
            if ctx.game.gas(self.player) == 0 && target_id.gas_cost() != 0 {
                return false;
            }
        }

        if in_combat {
            if !ctx.step_state.in_combat_cache.is_in_combat(unit, ctx.game) {
                return false;
            }
        }
        if self.order.is_attack_order() {
            if unsafe { !can_attack_unit(ctx.pathing, user, unit) } {
                return false;
            }
        }
        let already_targeted_count = ongoing
            .iter()
            .filter(|x| {
                x.target.map(|x| x.0) == Some(unit) &&
                    x.order == self.order &&
                    x.user.player() == player as u8
            })
            .count();
        already_targeted_count < self.limit as usize
    }
}

fn format_idle_order_for_print(order: &IdleOrder) -> String {
    fn format_rate(rate: &Rate) -> String {
        if rate.lower == rate.upper {
            format!("{}", rate.lower)
        } else {
            format!("{}-{}", rate.lower, rate.upper)
        }
    }
    // Could also format flags, but it's more work to implement..
    format!(
        "Player {}, order {}, limit {}, radius {}, rate {}\nuser {}, target {}",
        order.player, order.order.0, order.limit, order.radius, format_rate(&order.rate),
        order.unit_id, order.target_unit_id,
    )
}

unsafe fn can_attack_unit(pathing: *mut bw::Pathing, attacker: Unit, target: Unit) -> bool {
    use bw_dat::unit;
    if attacker.is_disabled() {
        return false;
    }
    if target.is_invincible() || target.is_hidden() {
        return false;
    }
    if target.is_invisible_hidden_to(attacker.player()) {
        return false;
    }
    let turret = attacker.subunit_turret();
    match attacker.id() {
        unit::CARRIER | unit::GANTRITHOR => true,
        unit::REAVER | unit::WARBRINGER => {
            !target.is_air() && are_on_same_region_group(pathing, attacker, target)
        }
        unit::QUEEN | unit::MATRIARCH => can_be_infested(target),
        _ => {
            if attacker.id() == unit::ARBITER && attacker.has_ai() {
                // Ai arbiters don't attack
                return false;
            }
            if target.is_air() {
                turret.id().air_weapon().is_some()
            } else {
                turret.id().ground_weapon().is_some()
            }
        }
    }
}

fn can_be_infested(unit: Unit) -> bool {
    unit.is_completed() && unit.id() == bw_dat::unit::COMMAND_CENTER && unit.hp_percent() < 50
}

unsafe fn are_on_same_region_group(pathing: *mut bw::Pathing, a: Unit, b: Unit) -> bool {
    let a_region = get_region(pathing, &a.position());
    let b_region = get_region(pathing, &b.position());
    (*a_region).group == (*b_region).group
}

unsafe fn get_region(pathing: *mut bw::Pathing, position: &bw::Point) -> *mut bw::Region {
    let tile_index = position.y as usize / 32 * 0x100 + position.x as usize / 32;
    let region = (*pathing).map_tile_regions[tile_index];
    if region >= 0x2000 {
        let split = &(*pathing).split_regions[region as usize - 0x2000];
        let bit_index = ((position.y as usize / 8) & 0x3) * 4 + ((position.x as usize / 8) & 0x3);
        let region = if split.minitile_flags & (1 << bit_index) == 0 {
            split.region_false
        } else {
            split.region_true
        };
        &mut (*pathing).regions[region as usize]
    } else {
        &mut (*pathing).regions[region as usize]
    }
}

struct IdleOrderTargetContext<'a, 'b, 'f> {
    acceptable_players: [bool; 12],
    game: Game,
    pathing: *mut bw::Pathing,
    step_state: &'a mut StepFrameState<'b, 'f>,
    current_unit: Option<Unit>,
    tile_flags: *mut u32,
}

fn target_pos(target: Unit, decl: &IdleOrder) -> (Option<Unit>, bw::Point) {
    let pos = target.position();
    let no_detection = unsafe {
        target.is_invisible() && (**target).detection_status & (1 << decl.player) as u32 == 0
    };
    let order_target = if no_detection { None } else { Some(target) };
    (order_target, pos)
}

fn panic_health(unit: Unit) -> i32 {
    let id = unit.id();
    let max_health = id.hitpoints().saturating_add(id.shields());
    (max_health / 4).min(unit.health() / 2)
}

fn find_user_target_pair(
    decl: &IdleOrder,
    ongoing: &[OngoingOrder],
    game: Game,
    step_state: &mut StepFrameState<'_, '_>,
    pathing: *mut bw::Pathing,
    tile_flags: *mut u32,
    state: &mut IdleOrderState,
) -> Option<(Unit, Unit)> {
    fn find_normal(
        decl: &IdleOrder,
        search_pos: &mut SearchPos,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext<'_, '_, '_>,
    ) -> Option<(Unit, Unit, u32)> {
        let unit = ctx.find_unit_for_decl_normal(decl, search_pos, ongoing)?;
        // Instead of a *perfect* solution of trying to find closest user-target pair,
        // find closest target for the first unit, and then find closest user for
        // the target (if the distance is large enough for it to matter)
        let (target, distance) = ctx.find_target(unit, decl, &ongoing)?;
        let (user, distance) = {
            if distance > 16 * 32 || distance > decl.radius as u32 {
                match ctx.find_user(search_pos, target, decl, &ongoing) {
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
        ctx: &mut IdleOrderTargetContext<'_, '_, '_>,
    ) -> Option<(Unit, Unit, u32)> {
        ctx.find_unit_target_pair_targeting_each_other(decl, ongoing)
    }

    fn target_targeting_user(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext<'_, '_, '_>,
    ) -> Option<(Unit, Unit, u32)> {
        ctx.find_unit_target_pair_with_target_targeting(decl, ongoing)
    }

    fn user_targeting_target(
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
        ctx: &mut IdleOrderTargetContext<'_, '_, '_>,
    ) -> Option<(Unit, Unit, u32)> {
        ctx.find_unit_target_pair_with_user_targeting(decl, ongoing)
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

    let mut ctx = decl.target_validation_context(game, step_state, None, pathing, tile_flags);

    if ctx.has_no_users_or_targets(decl) {
        return None;
    }

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
        update_best(&mut best, find_normal(decl, &mut state.search_pos, ongoing, &mut ctx));
    }
    best.map(|x| (x.0, x.1))
}

impl IdleOrderFlags {
    fn match_status(
        &self,
        unit: Unit,
        decl_player: u8,
        check_targeting: CheckTargetingFlags,
        current_unit: Option<Unit>,
        game: Game,
        units: &LazyUnitSearch,
        tile_flags: *mut u32,
    ) -> bool {
        unsafe {
            if let Some(order) = self.order {
                if unit.order() != order {
                    return false;
                }
            }

            #[cfg_attr(rustfmt, rustfmt_skip)]
            let status_ok = if self.status_required != 0 || self.status_not != 0 {
                let flags = (if (**unit).ensnare_timer != 0 { 1 } else { 0 } << 0) |
                    (if (**unit).plague_timer != 0 { 1 } else { 0 } << 1) |
                    (if (**unit).lockdown_timer != 0 { 1 } else { 0 } << 2) |
                    (if (**unit).irradiate_timer != 0 { 1 } else { 0 } << 3) |
                    (if (**unit).parasited_by_players != 0 { 1 } else { 0 } << 4) |
                    (if (**unit).is_blind != 0 { 1 } else { 0 } << 5) |
                    (if (**unit).matrix_timer != 0 { 1 } else { 0 } << 6) |
                    (if (**unit).maelstrom_timer != 0 { 1 } else { 0 } << 7) |
                    (if (**unit).stim_timer != 0 { 1 } else { 0 } << 8);
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
                let map_width = game.map_width_tiles();
                let tile = *tile_flags.add(
                    (unit.position().x / 32) as usize +
                        map_width as usize * (unit.position().y / 32) as usize,
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
            let numeric_ok = self.numeric.iter().all(|&(ty, compare, amount)| {
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
                            VULTURE | JIM_RAYNOR_VULTURE => (unit.mine_amount(game) as i32, 3),
                            NUCLEAR_SILO => (unit.has_nuke() as i32, 1),
                            // Should this check upgrades for max amount?
                            CARRIER | GANTRITHOR => (unit.fighter_amount() as i32, 8),
                            REAVER | WARBRINGER => (unit.fighter_amount() as i32, 10),
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
                    Comparison::LessThan => val < amount,
                    Comparison::GreaterThan => val > amount,
                    Comparison::LessThanPercentage => {
                        val.saturating_mul(100).checked_div(max).unwrap_or(0) < amount
                    }
                    Comparison::GreaterThanPercentage => {
                        val.saturating_mul(100).checked_div(max).unwrap_or(0) > amount
                    }
                }
            });
            if !numeric_ok {
                return false;
            }

            if let Some(ref s) = self.count {
                let mut position = Position::from_point(unit.position().x, unit.position().y);
                position.extend_area(s.range as i16);
                let unit_count = units.get()
                    .search_iter(&position.area)
                    .filter(|u| s.players.matches(u.player()))
                    .count();

                if !s.modifier.compare(unit_count as u32, s.value as u32) {
                    return false;
                }
            }

            true
        }
    }
}

impl<'b, 'f> StepFrameState<'b, 'f> {
    fn new(bump: &'b Bump, ctx: &'f mut StepUnitsCtx<'_>) -> StepFrameState<'b, 'f> {
        let unit_id_to_buf_idx = bump.alloc_slice_fill_copy(UnitId::entry_amount() as usize, 0);
        StepFrameState {
            bump,
            unit_id_to_buf_idx,
            unit_buffers_reserved: 0,
            unit_buffers: BumpVec::new_in(bump),
            in_combat_cache: &mut ctx.in_combat_cache,
            unit_search: &ctx.unit_search,
        }
    }

    fn prepare_order_check(&mut self, decl: &IdleOrder) {
        if !decl.unit_id.has_groups() {
            for &id in decl.unit_id.as_slice() {
                self.prepare_unit_buffer(id);
            }
        }
        if !decl.target_unit_id.has_groups() {
            for &id in decl.target_unit_id.as_slice() {
                self.prepare_unit_buffer(id);
            }
        }
    }

    fn prepare_unit_buffer(&mut self, id: UnitId) {
        if let Some(val) = self.unit_id_to_buf_idx.get_mut(id.0 as usize) {
            if *val == 0 {
                self.unit_buffers_reserved += 1;
                *val = self.unit_buffers_reserved;
            }
        }
    }

    /// Walks through active units, filling self.unit_buffers with
    /// units that were requested.
    fn build_unit_buffers(&mut self, game: Game) {
        let buffer_count = self.unit_buffers_reserved as usize;
        if buffer_count == 0 {
            return;
        }
        let mut unit_buffers = BumpVec::from_iter_in(
            (0..buffer_count).map(|_| BumpVec::new_in(self.bump)),
            self.bump,
        );
        for (i, &val) in self.unit_id_to_buf_idx.iter().enumerate() {
            if val != 0 {
                let index = val as usize - 1;
                let unit_id = UnitId(i as u16);
                let count = approximate_active_unit_count(game, unit_id);
                unit_buffers[index].reserve(count as usize);
            }
        }
        for unit in unit::active_units() {
            if let Some(&val) = self.unit_id_to_buf_idx.get(unit.id().0 as usize) {
                if val != 0 {
                    let index = val as usize - 1;
                    unit_buffers[index].push(unit);
                }
            }
        }
        self.unit_buffers = BumpVec::from_iter_in(
            unit_buffers.into_iter().map(|x| x.into_bump_slice()),
            self.bump,
        );
    }

    fn unit_buffer(&mut self, game: Game, id: UnitId) -> &'b [Unit] {
        let val = match self.unit_id_to_buf_idx.get(id.0 as usize) {
            Some(s) => *s,
            None => {
                warn!("Invalid unit id accessed {:x}", id.0);
                return &[];
            }
        };
        if val != 0 {
            let index = val as usize - 1;
            return self.unit_buffers[index];
        }
        // The specific unit id wasn't buffered yet, do expensive iteration through
        // all active units to add it.
        // Can happen if the user switches targets mid order / deathrattle order.

        let count = approximate_active_unit_count(game, id);
        let mut results = BumpVec::with_capacity_in(count as usize, self.bump);
        for unit in unit::active_units() {
            if unit.id() == id {
                results.push(unit);
            }
        }
        let results = results.into_bump_slice();
        self.unit_buffers.push(results);
        self.unit_id_to_buf_idx[id.0 as usize] = self.unit_buffers.len() as u16;
        results
    }
}

impl<'a, 'b, 'f> IdleOrderTargetContext<'a, 'b, 'f> {
    fn find_unit_for_decl_normal(
        &mut self,
        decl: &IdleOrder,
        search_pos: &mut SearchPos,
        ongoing: &[OngoingOrder],
    ) -> Option<Unit> {
        self.find_user_for(
            search_pos,
            decl,
            |ctx, u| decl.unit_valid(u, ongoing, CheckTargetingFlags::Yes, ctx),
        )
    }

    fn find_unit_target_pair_targeting_each_other(
        &mut self,
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
    ) -> Option<(Unit, Unit, u32)> {
        let mut best = None;
        let mut best_distance = u32::MAX;
        self.iter_users_for(
            &mut SearchPos(0, 0),
            decl,
            |ctx, user| {
                let target = match user.target() {
                    Some(s) if s.target() == Some(user) => s,
                    _ => return,
                };
                let ok =
                    decl.target_valid(user, target, ongoing, CheckTargetingFlags::No, ctx) &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::No, ctx);
                if !ok {
                    return;
                }
                let distance = bw::distance(user.position(), target.position());
                if distance < best_distance {
                    best = Some((user, target, distance));
                    best_distance = distance;
                }
            },
        );
        best
    }

    fn find_unit_target_pair_with_target_targeting(
        &mut self,
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
    ) -> Option<(Unit, Unit, u32)> {
        let mut best = None;
        let mut best_distance = u32::MAX;
        self.iter_targets_for(
            decl,
            |ctx, target| {
                let user = match target.target() {
                    Some(s) => s,
                    None => return,
                };
                let ok =
                    decl.target_valid(user, target, ongoing, CheckTargetingFlags::No, ctx) &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::Yes, ctx);
                if !ok {
                    return;
                }
                let distance = bw::distance(user.position(), target.position());
                if distance < best_distance {
                    best = Some((user, target, distance));
                    best_distance = distance;
                }
            },
        );
        best
    }

    fn find_unit_target_pair_with_user_targeting(
        &mut self,
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
    ) -> Option<(Unit, Unit, u32)> {
        let mut best = None;
        let mut best_distance = u32::MAX;
        self.iter_users_for(
            &mut SearchPos(0, 0),
            decl,
            |ctx, user| {
                let target = match user.target() {
                    Some(s) => s,
                    None => return,
                };
                let ok =
                    decl.target_valid(user, target, ongoing, CheckTargetingFlags::Yes, ctx) &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::No, ctx);
                if !ok {
                    return;
                }
                let distance = bw::distance(user.position(), target.position());
                if distance < best_distance {
                    best = Some((user, target, distance));
                    best_distance = distance;
                }
            },
        );
        best
    }

    fn find_target(
        &mut self,
        user: Unit,
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
    ) -> Option<(Unit, u32)> {
        let mut best = None;
        let mut best_distance = u32::MAX;
        if decl.target_unit_id.has_groups() {
            // Maybe could always just search area? not just when there are groups
            let area = bw::Rect::from_point_radius(user.position(), decl.radius as i16);
            for target in self.step_state.unit_search.get().search_iter(&area) {
                let distance = bw::distance(user.position(), target.position());
                if distance >= best_distance {
                    continue;
                }
                let ok = target != user &&
                    decl.target_valid(user, target, ongoing, CheckTargetingFlags::Yes, self);
                if !ok {
                    continue;
                }
                best = Some((target, distance));
                best_distance = distance;
            }
            best
        } else {
            self.iter_targets_for(
                decl,
                |ctx, target| {
                    let distance = bw::distance(user.position(), target.position());
                    if distance >= best_distance {
                        return;
                    }
                    let ok = target != user &&
                        decl.target_valid(user, target, ongoing, CheckTargetingFlags::Yes, ctx);
                    if !ok {
                        return;
                    }
                    best = Some((target, distance));
                    best_distance = distance;
                },
            );
            best
        }
    }

    fn find_user(
        &mut self,
        search_pos: &mut SearchPos,
        target: Unit,
        decl: &IdleOrder,
        ongoing: &[OngoingOrder],
    ) -> Option<(Unit, u32)> {
        let mut best = None;
        let mut best_distance = u32::MAX;
        self.iter_users_for(
            search_pos,
            decl,
            |ctx, user| {
                let ok = target != user &&
                    decl.unit_valid(user, ongoing, CheckTargetingFlags::Yes, ctx);
                if !ok {
                    return;
                }
                let distance = bw::distance(user.position(), target.position());
                if distance < best_distance {
                    best = Some((user, distance));
                    best_distance = distance;
                }
            },
        );
        best
    }

    fn find_user_for<F>(
        &mut self,
        search_pos: &mut SearchPos,
        decl: &IdleOrder,
        mut filter: F,
    ) -> Option<Unit>
    where F: FnMut(&mut IdleOrderTargetContext<'_, '_, '_>, Unit) -> bool,
    {
        let mut result = None;
        self.iter_units_in_match(search_pos, &decl.unit_id, |ctx, unit| {
            if filter(ctx, unit) {
                result = Some(unit);
                Iter::Stop
            } else {
                Iter::Continue
            }
        });
        result
    }

    /// Calls callback with all units that can be used as an user for `decl`.
    /// Does not guarantee that the units passed to callback actually are usable
    /// for `decl` at all; a valid implementation can just pass all active units
    /// to callback.
    fn iter_users_for<F>(
        &mut self,
        search_pos: &mut SearchPos,
        decl: &IdleOrder,
        mut callback: F,
    )
    where F: FnMut(&mut IdleOrderTargetContext<'_, '_, '_>, Unit)
    {
        self.iter_units_in_match(search_pos, &decl.unit_id, |ctx, user| {
            callback(ctx, user);
            Iter::Continue
        });
    }

    /// Calls callback with all units that can be used as a target for `decl`.
    /// Does not guarantee that the units passed to callback actually are usable
    /// for `decl` at all; a valid implementation can just pass all active units
    /// to callback.
    fn iter_targets_for<F>(&mut self, decl: &IdleOrder, mut callback: F)
    where F: FnMut(&mut IdleOrderTargetContext<'_, '_, '_>, Unit)
    {
        let search_pos = &mut SearchPos(0, 0);
        self.iter_units_in_match(search_pos, &decl.target_unit_id, |ctx, target| {
            callback(ctx, target);
            Iter::Continue
        });
    }

    fn iter_units_in_match<F>(
        &mut self,
        search_pos: &mut SearchPos,
        units: &UnitMatch,
        mut callback: F,
    )
    where F: FnMut(&mut IdleOrderTargetContext<'_, '_, '_>, Unit) -> Iter,
    {
        if units.has_groups() {
            // Just walk through all units.
            // Ineffective so callers should try to handle groups in more effective way
            // instead (As of writing this they don't)
            for unit in unit::active_units() {
                callback(self, unit);
            }
        } else {
            let start_id = search_pos.0 as usize;
            let start_unit = search_pos.1 as usize;
            let units_slice = units.as_slice();
            let (first_units, second_units) = if start_id >= units_slice.len() {
                (units_slice, &[][..])
            } else {
                (&units_slice[start_id..], &units_slice[..start_id])
            };
            let mut new_start_id = start_id;
            let mut new_start_unit = start_unit;
            let mut first = true;
            'outer: for unit_ids in [first_units, second_units] {
                for &id in unit_ids {
                    let units = self.step_state.unit_buffer(self.game, id);
                    let (first, second) = if first {
                        first = false;
                        if start_unit >= units.len() {
                            (&units[..0], units)
                        } else {
                            (&units[start_unit..], &units[..start_unit])
                        }
                    } else {
                        (&units[..], &units[..0])
                    };

                    for &unit in first {
                        new_start_unit = new_start_unit.wrapping_add(1);
                        if callback(self, unit) == Iter::Stop {
                            if new_start_unit >= units.len() {
                                // Matched on last unit, make next check's
                                // id be from next id.
                                new_start_id = new_start_id.wrapping_add(1);
                                new_start_unit = 0;
                            }
                            break 'outer;
                        }
                    }
                    new_start_id = new_start_id.wrapping_add(1);
                    new_start_unit = 0;
                    // Ends up checking the units with start_id but earlier pos
                    // before other ids, shouldn't matter that much.
                    for &unit in second {
                        if callback(self, unit) == Iter::Stop {
                            break 'outer;
                        }
                    }
                }
            }
            if new_start_id >= units.count() {
                new_start_id -= units.count();
            }
            search_pos.0 = new_start_id as u16;
            search_pos.1 = new_start_unit as u16;
        }
    }

    /// A cheap check to early exit if no user-target pairs cannot definitely be found
    /// (Can return false even if there won't be user-target pair in the end)
    fn has_no_users_or_targets(
        &mut self,
        decl: &IdleOrder,
    ) -> bool {
        let game = self.game;
        if !decl.unit_id.has_groups() {
            let none = decl.unit_id.as_slice()
                .iter()
                .all(|&id| game.completed_count(decl.player, id) == 0);
            if none {
                return true;
            }
        }
        if !decl.target_unit_id.has_groups() {
            let none = decl.target_unit_id.as_slice()
                .iter()
                .all(|&id| {
                    (0..12).all(|player| {
                        self.acceptable_players[player as usize] == false ||
                            game.unit_count(player, id) == 0
                    })
                });
            if none {
                return true;
            }
        }
        false
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Iter {
    Stop,
    Continue,
}

fn approximate_active_unit_count(game: Game, unit_id: UnitId) -> u32 {
    // Assuming that non-egg non-building units are hidden if incomplete
    if unit_id.is_building() ||
        unit_id == unit::id::EGG ||
        unit_id == unit::id::LURKER_EGG ||
        unit_id == unit::id::COCOON
    {
        (0..12).map(|i| game.unit_count(i, unit_id)).sum::<u32>()
    } else {
        (0..12).map(|i| game.completed_count(i, unit_id)).sum::<u32>()
    }
}
