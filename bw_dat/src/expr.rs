pub use crate::parse_expr::{
    IntFunc, IntFuncType, BoolFunc, BoolFuncType, CustomState, NoCustom, CustomParser, Expr,
};
pub use crate::parse_expr::{BoolExpr as BoolExprTree, IntExpr as IntExprTree};

use std::fmt;

use bitflags::bitflags;

use crate::game::Game;
use crate::parse_expr::{self, DefaultParser};
use crate::unit::{Unit};
use crate::{DatType, order, UpgradeId, TechId};

#[derive(Debug)]
pub struct Error {
    message: String,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

pub type IntExpr = CustomIntExpr<NoCustom>;
pub type BoolExpr = CustomBoolExpr<NoCustom>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct CustomIntExpr<C: CustomState> {
    ty: parse_expr::IntExpr<C>,
    required_context: RequiredContext,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct CustomBoolExpr<C: CustomState> {
    ty: parse_expr::BoolExpr<C>,
    required_context: RequiredContext,
}

bitflags! {
    #[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
    pub struct RequiredContext: u8 {
        const UNIT = 0x1;
        const GAME = 0x2;
        const MAP_TILE_FLAGS = 0x4;
    }
}

pub trait CustomEval: Sized {
    type State: CustomState;
    fn eval_int(&mut self, param: &<Self::State as CustomState>::IntExt) -> i32;
    fn eval_bool(&mut self, param: &<Self::State as CustomState>::BoolExt) -> bool;
}

pub struct EvalCtx<E: CustomEval> {
    pub unit: Option<Unit>,
    pub game: Option<Game>,
    pub map_tile_flags: Option<*mut u32>,
    pub custom: E,
}

pub struct DefaultEval;
impl CustomEval for DefaultEval {
    type State = NoCustom;

    fn eval_int(&mut self, val: &parse_expr::Void) -> i32 {
        match *val {
        }
    }

    fn eval_bool(&mut self, val: &parse_expr::Void) -> bool {
        match *val {
        }
    }
}

static SIN_TABLE: [u16; 91] = [
    0, 4, 8, 13, 17, 22, 26, 31, 35, 40, 44, 48,
    53, 57, 61, 66, 70, 74, 79, 83, 87, 91, 95, 100,
    104, 108, 112, 116, 120, 124, 127, 131, 135, 139, 143, 146,
    150, 154, 157, 161, 164, 167, 171, 174, 177, 181, 184, 187,
    190, 193, 196, 198, 201, 204, 207, 209, 212, 214, 217, 219,
    221, 223, 226, 228, 230, 232, 233, 235, 237, 238, 240, 242,
    243, 244, 246, 247, 248, 249, 250, 251, 252, 252, 253, 254,
    254, 255, 255, 255, 255, 255, 256,
];

static TAN_TABLE: [i32; 91] = [
    0, 4, 9, 13, 18, 22, 27, 31, 36, 41, 45, 50,
    54, 59, 64, 69, 73, 78, 83, 88, 93, 98, 103, 109,
    114, 119, 125, 130, 136, 142, 148, 154, 160, 166, 173, 179,
    186, 193, 200, 207, 215, 223, 231, 239, 247, 256, 265, 275,
    284, 294, 305, 316, 328, 340, 352, 366, 380, 394, 410, 426,
    443, 462, 481, 502, 525, 549, 575, 603, 634, 667, 703, 743,
    788, 837, 893, 955, 1027, 1109, 1204, 1317, 1452, 1616, 1822, 2085,
    2436, 2926, 3661, 4885, 7331, 14666, i32::MAX,
];

impl<E: CustomEval> EvalCtx<E> {
    pub fn eval_int(&mut self, expr: &CustomIntExpr<E::State>) -> i32 {
        if expr.required_context.contains(RequiredContext::UNIT) && self.unit.is_none() {
            return i32::MIN;
        }
        if expr.required_context.contains(RequiredContext::GAME) && self.game.is_none() {
            return i32::MIN;
        }
        if expr.required_context.contains(RequiredContext::MAP_TILE_FLAGS) &&
            self.map_tile_flags.is_none()
        {
            return i32::MIN;
        }
        self.eval_int_r(&expr.ty)
    }

    fn eval_int_r(&mut self, expr: &parse_expr::IntExpr<E::State>) -> i32 {
        use crate::parse_expr::IntExpr::*;
        use crate::parse_expr::IntFuncType::*;
        match expr {
            Add(x) => self.eval_int_r(&x.0).saturating_add(self.eval_int_r(&x.1)),
            Sub(x) => self.eval_int_r(&x.0).saturating_sub(self.eval_int_r(&x.1)),
            Mul(x) => self.eval_int_r(&x.0).saturating_mul(self.eval_int_r(&x.1)),
            Div(x) => {
                self.eval_int_r(&x.0).checked_div(self.eval_int_r(&x.1))
                    .unwrap_or(i32::MAX)
            }
            Modulo(x) => {
                let div = self.eval_int_r(&x.1);
                if div == 0 {
                    i32::MAX
                } else {
                    self.eval_int_r(&x.0) % div
                }
            }
            BitAnd(x) => self.eval_int_r(&x.0) & self.eval_int_r(&x.1),
            BitOr(x) => self.eval_int_r(&x.0) | self.eval_int_r(&x.1),
            BitXor(x) => self.eval_int_r(&x.0) ^ self.eval_int_r(&x.1),
            LeftShift(x) => (self.eval_int_r(&x.0) as u32)
                .checked_shl(self.eval_int_r(&x.1) as u32)
                .unwrap_or(0) as i32,
            RightShift(x) => (self.eval_int_r(&x.0) as u32)
                .checked_shr(self.eval_int_r(&x.1) as u32)
                .unwrap_or(0) as i32,
            Not(x) => !self.eval_int_r(x),
            Integer(i) => *i,
            Custom(x) => self.custom.eval_int(x),
            Func(x) => {
                unsafe {
                    let unit = self.unit.unwrap_or(Unit::from_ptr(16 as *mut _).unwrap());
                    let game = self.game.unwrap_or(Game::from_ptr(16 as *mut _));
                    match x.ty {
                        StimTimer => (**unit).stim_timer as i32,
                        EnsnareTimer => (**unit).ensnare_timer as i32,
                        MaelstromTimer => (**unit).maelstrom_timer as i32,
                        DeathTimer => (**unit).death_timer as i32,
                        LockdownTimer => (**unit).lockdown_timer as i32,
                        StasisTimer => (**unit).stasis_timer as i32,
                        IrradiateTimer => (**unit).irradiate_timer as i32,
                        MatrixTimer => (**unit).matrix_timer as i32,
                        MatrixHitpoints => (**unit).defensive_matrix_dmg as i32,
                        AcidSporeCount => unit.acid_spore_count() as i32,
                        Fighters => unit.fighter_amount() as i32,
                        Mines => unit.mine_amount(game) as i32,
                        Hitpoints => unit.hitpoints(),
                        HitpointsPercent => unit.hitpoints().checked_mul(100)
                            .and_then(|x| x.checked_div(unit.id().hitpoints()))
                            .unwrap_or(100),
                        Shields => unit.shields(),
                        ShieldsPercent => unit.shields().checked_mul(100)
                            .and_then(|x| x.checked_div(unit.id().shields()))
                            .unwrap_or(100),
                        Energy => unit.energy() as i32,
                        Kills => unit.kills() as i32,
                        FrameCount => game.frame_count() as i32,
                        Tileset => (**game).tileset as i32,
                        Minerals => game.minerals(unit.player()) as i32,
                        Gas => game.gas(unit.player()) as i32,
                        CarriedResourceAmount => unit.carried_resource_amount() as i32,
                        GroundCooldown => (**unit).ground_cooldown as i32,
                        AirCooldown => (**unit).air_cooldown as i32,
                        SpellCooldown => (**unit).spell_cooldown as i32,
                        Speed => (**unit).flingy.current_speed,
                        SigOrder => (**unit).order_signal as i32,
                        Player => (**unit).player as i32,
                        UnitId => (**unit).unit_id as i32,
                        Order => unit.order().0 as i32,
                        Sin | Cos | Tan => {
                            let val = self.eval_int_r(&x.args[0]);
                            sin_cos_tan(val, x.ty)
                        }
                        Asin | Acos | Atan => {
                            let val = self.eval_int_r(&x.args[0]);
                            asin_acos_atan(val, x.ty)
                        }
                        Min | Max | Clamp => {
                            let a1 = self.eval_int_r(&x.args[0]);
                            let a2 = self.eval_int_r(&x.args[1]);
                            if x.ty == Max {
                                a1.max(a2)
                            } else if x.ty == Min {
                                a1.min(a2)
                            } else {
                                let a3 = self.eval_int_r(&x.args[2]);
                                // Not using a2.clamp(a1, a2) to never panic
                                a1.max(a2).min(a3)
                            }
                        }
                        Deaths | UnitCountCompleted | UnitCountAny => {
                            let player = match self.eval_int_r(&x.args[0]) {
                                x if x >= 0 && x < 12 => x,
                                _ => return i32::MIN,
                            };
                            let unit = self.eval_int_r(&x.args[1]);
                            let player = player as u8;
                            let unit = crate::UnitId(unit as u16);
                            match x.ty {
                                Deaths => game.unit_deaths(player, unit) as i32,
                                UnitCountCompleted => game.completed_count(player, unit) as i32,
                                UnitCountAny | _ => game.unit_count(player, unit) as i32,
                            }
                        }
                        Upgrade => {
                            let player = match self.eval_int_r(&x.args[0]) {
                                x if x >= 0 && x < 12 => x as u8,
                                _ => return i32::MIN,
                            };
                            let upgrade = self.eval_int_r(&x.args[1]);
                            game.upgrade_level(player, UpgradeId(upgrade as u16)).into()
                        }
                        TileHeight => {
                            let map_width = game.map_width_tiles();
                            let pos =  unit.position();
                            let x = pos.x >> 5;
                            let y = pos.y >> 5;
                            let index = (y as usize) * (map_width as usize) + (x as usize);
                            if let Some(flags) = self.map_tile_flags {
                                ((*flags.add(index) & 0x0600_0000) >> 0x19) as i32
                            } else {
                                0
                            }
                        }
                        Dat => {
                            let dat = match self.eval_int_r(&x.args[0]) {
                                0 => DatType::Units,
                                1 => DatType::Weapons,
                                2 => DatType::Flingy,
                                3 => DatType::Sprites,
                                4 => DatType::Images,
                                5 => DatType::Orders,
                                6 => DatType::Upgrades,
                                7 => DatType::TechData,
                                8 => DatType::SfxData,
                                9 => DatType::PortData,
                                10 => DatType::Buttons,
                                _ => return 0,
                            };
                            let dat = &crate::DAT_GLOBALS[dat as usize];
                            let field = self.eval_int_r(&x.args[1]) as u32;
                            // Assuming that anything outside u16 range is wrong
                            let id = match u16::try_from(self.eval_int_r(&x.args[2])) {
                                Ok(o) => o as u32,
                                Err(_) => return 0,
                            };
                            crate::dat_read_opt(dat, id, field).unwrap_or(0) as i32
                        }
                    }
                }
            }
        }
    }

    pub fn eval_bool(&mut self, expr: &CustomBoolExpr<E::State>) -> bool {
        if expr.required_context.contains(RequiredContext::UNIT) && self.unit.is_none() {
            return false;
        }
        if expr.required_context.contains(RequiredContext::GAME) && self.game.is_none() {
            return false;
        }
        if expr.required_context.contains(RequiredContext::MAP_TILE_FLAGS) &&
            self.map_tile_flags.is_none()
        {
            return false;
        }
        self.eval_bool_r(&expr.ty)
    }

    fn eval_bool_r(&mut self, expr: &parse_expr::BoolExpr<E::State>) -> bool {
        use crate::parse_expr::BoolExpr::*;
        use crate::parse_expr::BoolFuncType::*;
        let game = unsafe { self.game.unwrap_or(Game::from_ptr(16 as *mut _)) };
        match expr {
            And(x) => self.eval_bool_r(&x.0) && self.eval_bool_r(&x.1),
            Or(x) => self.eval_bool_r(&x.0) || self.eval_bool_r(&x.1),
            LessThan(x) => self.eval_int_r(&x.0) < self.eval_int_r(&x.1),
            LessOrEqual(x) => self.eval_int_r(&x.0) <= self.eval_int_r(&x.1),
            GreaterThan(x) => self.eval_int_r(&x.0) > self.eval_int_r(&x.1),
            GreaterOrEqual(x) => self.eval_int_r(&x.0) >= self.eval_int_r(&x.1),
            EqualInt(x) => self.eval_int_r(&x.0) == self.eval_int_r(&x.1),
            EqualBool(x) => self.eval_bool_r(&x.0) == self.eval_bool_r(&x.1),
            Not(x) => !self.eval_bool_r(&x),
            Custom(x) => self.custom.eval_bool(&x),
            Func(x) => {
                unsafe {
                    let unit = self.unit.unwrap_or(Unit::from_ptr(16 as *mut _).unwrap());
                    match x.ty {
                        True => true,
                        False => false,
                        Parasited => (**unit).parasited_by_players != 0,
                        Blind => (**unit).is_blind != 0,
                        UnderStorm => (**unit).is_under_storm != 0,
                        LiftedOff => !unit.is_landed_building() && unit.id().is_building(),
                        IsBuilding => unit.id().is_building(),
                        LandedBuilding => unit.is_landed_building(),
                        BuildingUnit => unit.currently_building().is_some(),
                        InTransport => {
                            (**unit).flags & 0x20 == 0 && (**unit).flags & 0x40 != 0
                        }
                        InBunker => {
                            (**unit).flags & 0x20 != 0 && (**unit).flags & 0x40 != 0
                        }
                        CarryingPowerup => unit.powerup().is_some(),
                        CarryingMinerals => (**unit).carried_powerup_flags & 0x2 != 0,
                        CarryingGas => (**unit).carried_powerup_flags & 0x1 != 0,
                        Burrowed => unit.is_burrowed(),
                        Disabled => unit.is_disabled(),
                        Completed => unit.is_completed(),
                        SelfCloaked => {
                            let cloak_order = unit.secondary_order() == order::CLOAK;
                            cloak_order && unit.is_invisible() && !unit.has_free_cloak()
                        }
                        ArbiterCloaked => unit.has_free_cloak() && !unit.is_burrowed(),
                        Cloaked => unit.is_invisible() && !unit.is_burrowed(),
                        UnderDweb => unit.is_under_dweb(),
                        Hallucination => unit.is_hallucination(),
                        Tech => {
                            let player = match self.eval_int_r(&x.args[0]) {
                                x if x >= 0 && x < 12 => x as u8,
                                _ => return false,
                            };
                            let tech = self.eval_int_r(&x.args[1]);
                            game.tech_researched(player, TechId(tech as u16))
                        }
                        OnCreep | TerrainProtection | Unbuildable => {
                            let mask = if x.ty == OnCreep {
                                0x0040_0000
                            } else if x.ty == TerrainProtection {
                                0x0010_0000
                            } else {
                                0x0080_0000
                            };
                            let map_width = game.map_width_tiles();
                            let pos =  unit.position();
                            let x = pos.x >> 5;
                            let y = pos.y >> 5;
                            let index = (y as usize) * (map_width as usize) + (x as usize);
                            if let Some(flags) = self.map_tile_flags {
                                (*flags.add(index) & mask) != 0
                            } else {
                                false
                            }
                        }
                    }
                }
            }
        }
    }
}

fn format_err<C: CustomState>(e: &parse_expr::Error<'_, C>, _input: &[u8]) -> Error {
    let msg = match e {
        parse_expr::Error::Msg(pos, msg) => {
            format!("Starting from {}\n{}", String::from_utf8_lossy(pos), msg)
        }
        parse_expr::Error::Eof => format!("Unexpected end of input"),
        parse_expr::Error::NotBoolean(_) => format!("Expected boolean, got integer"),
        parse_expr::Error::NotInteger(_) => format!("Expected integer, got boolean"),
    };
    Error {
        message: msg,
    }
}

impl IntExpr {
    /// Fails if the entire byte slice isn't parsed.
    pub fn parse(bytes: &[u8]) -> Result<IntExpr, Error> {
        IntExpr::parse_part(bytes)
            .and_then(|(result, rest)| {
                if !rest.is_empty() {
                    let msg = format!("Trailing characters: {}", String::from_utf8_lossy(rest));
                    Err(Error { message: msg })
                } else {
                    Ok(result)
                }
            })
    }

    /// Parses expression and returns what's left from input.
    pub fn parse_part(bytes: &[u8]) -> Result<(IntExpr, &[u8]), Error> {
        IntExpr::parse_part_custom(bytes, &mut DefaultParser)
    }

    pub fn eval_with_unit(
        &self,
        unit: Unit,
        game: Game,
        map_tile_flags: *mut u32,
    ) -> i32 {
        let mut ctx = EvalCtx {
            unit: Some(unit),
            game: Some(game),
            map_tile_flags: Some(map_tile_flags),
            custom: DefaultEval,
        };
        ctx.eval_int(&self)
    }
}

impl<C: CustomState> CustomIntExpr<C> {
    pub fn from_tree(tree: parse_expr::IntExpr<C>) -> CustomIntExpr<C> {
        CustomIntExpr {
            required_context: int_expr_required_context(&tree),
            ty: tree,
        }
    }

    pub fn parse_part_custom<'a, P: CustomParser<State = C>>(
        bytes: &'a [u8],
        custom: &mut P,
    ) -> Result<(CustomIntExpr<C>, &'a [u8]), Error> {
        let mut parser = crate::parse_expr::Parser::new(custom);
        parser.int_expr(bytes)
            .map_err(|e| format_err(&e, bytes))
            .map(|(result, rest)| (CustomIntExpr {
                required_context: int_expr_required_context(&result),
                ty: result,
            }, rest))
    }

    pub fn inner(&self) -> &IntExprTree<C> {
        &self.ty
    }

    pub fn required_context(&self) -> RequiredContext {
        self.required_context
    }
}

impl BoolExpr {
    /// Fails if the entire byte slice isn't parsed.
    pub fn parse(bytes: &[u8]) -> Result<BoolExpr, Error> {
        BoolExpr::parse_part(bytes)
            .and_then(|(result, rest)| {
                if !rest.is_empty() {
                    let msg = format!("Trailing characters: {}", String::from_utf8_lossy(rest));
                    Err(Error { message: msg })
                } else {
                    Ok(result)
                }
            })
    }

    /// Parses expression and returns what's left from input.
    pub fn parse_part(bytes: &[u8]) -> Result<(BoolExpr, &[u8]), Error> {
        BoolExpr::parse_part_custom(bytes, &mut DefaultParser)
    }

    pub fn eval_with_unit(
        &self,
        unit: Unit,
        game: Game,
        map_tile_flags: *mut u32,
    ) -> bool {
        let mut ctx = EvalCtx {
            unit: Some(unit),
            game: Some(game),
            map_tile_flags: Some(map_tile_flags),
            custom: DefaultEval,
        };
        ctx.eval_bool(&self)
    }
}

impl<C: CustomState> CustomBoolExpr<C> {
    pub fn from_tree(tree: parse_expr::BoolExpr<C>) -> CustomBoolExpr<C> {
        CustomBoolExpr {
            required_context: bool_expr_required_context(&tree),
            ty: tree,
        }
    }

    pub fn parse_part_custom<'a, P: CustomParser<State = C>>(
        bytes: &'a [u8],
        custom: &mut P,
    ) -> Result<(CustomBoolExpr<C>, &'a [u8]), Error> {
        let mut parser = crate::parse_expr::Parser::new(custom);
        parser.bool_expr(bytes)
            .map_err(|e| format_err(&e, bytes))
            .map(|(result, rest)| (CustomBoolExpr {
                required_context: bool_expr_required_context(&result),
                ty: result,
            }, rest))
    }

    pub fn inner(&self) -> &BoolExprTree<C> {
        &self.ty
    }

    pub fn required_context(&self) -> RequiredContext {
        self.required_context
    }
}

fn bool_expr_required_context<C: CustomState>(expr: &parse_expr::BoolExpr<C>) -> RequiredContext {
    use crate::parse_expr::BoolExpr::*;
    use crate::parse_expr::BoolFuncType::*;
    match expr {
        And(x) | Or(x) | EqualBool(x) => {
            bool_expr_required_context(&x.0) | bool_expr_required_context(&x.1)
        }
        LessThan(x) | LessOrEqual(x) | GreaterThan(x) | GreaterOrEqual(x) | EqualInt(x) => {
            int_expr_required_context(&x.0) | int_expr_required_context(&x.1)
        }
        Not(x) => bool_expr_required_context(&x),
        Func(x) => match x.ty {
            True | False => RequiredContext::empty(),
            Parasited | Blind | UnderStorm | LiftedOff | IsBuilding | LandedBuilding |
                BuildingUnit | InTransport |
                InBunker | CarryingPowerup | CarryingMinerals | CarryingGas | Burrowed |
                Disabled | Completed | SelfCloaked | ArbiterCloaked | Cloaked |
                UnderDweb | Hallucination => RequiredContext::UNIT,
            Tech => {
                int_expr_required_context(&x.args[0]) | int_expr_required_context(&x.args[1]) |
                    RequiredContext::GAME
            }
            OnCreep | TerrainProtection | Unbuildable => {
                RequiredContext::UNIT | RequiredContext::GAME | RequiredContext::MAP_TILE_FLAGS
            }
        },
        Custom(_) => RequiredContext::empty(),
    }
}

fn int_expr_required_context<C: CustomState>(expr: &parse_expr::IntExpr<C>) -> RequiredContext {
    use crate::parse_expr::IntExpr::*;
    use crate::parse_expr::IntFuncType::*;
    match expr {
        Add(x) | Sub(x) | Mul(x) | Div(x) | Modulo(x) | BitAnd(x) | BitOr(x) | BitXor(x) |
            LeftShift(x) | RightShift(x) => {
            int_expr_required_context(&x.0) | int_expr_required_context(&x.1)
        }
        Not(x) => int_expr_required_context(x),
        Integer(_) => RequiredContext::empty(),
        Func(x) => match x.ty {
            StimTimer | EnsnareTimer | MaelstromTimer | DeathTimer | LockdownTimer |
                StasisTimer | IrradiateTimer | MatrixTimer | MatrixHitpoints |
                AcidSporeCount | Fighters | Hitpoints | HitpointsPercent |
                Shields | ShieldsPercent | Energy | Kills | CarriedResourceAmount |
                GroundCooldown | AirCooldown | SpellCooldown | Speed | SigOrder |
                Player | UnitId | Order =>
            {
                RequiredContext::UNIT
            }
            Mines | Minerals | Gas => {
                RequiredContext::UNIT | RequiredContext::GAME
            }
            FrameCount | Tileset => RequiredContext::GAME,
            Sin | Cos | Tan | Asin | Acos | Atan => int_expr_required_context(&x.args[0]),
            Min | Max | Clamp | Dat => {
                let argc = if matches!(x.ty, Clamp | Dat) { 3 } else { 2 };
                let mut ret = RequiredContext::empty();
                for i in 0..argc {
                    ret |= int_expr_required_context(&x.args[i]);
                }
                ret
            }
            Deaths | Upgrade | UnitCountCompleted | UnitCountAny => {
                int_expr_required_context(&x.args[0]) | int_expr_required_context(&x.args[1]) |
                    RequiredContext::GAME
            }
            TileHeight => {
                RequiredContext::UNIT | RequiredContext::GAME | RequiredContext::MAP_TILE_FLAGS
            }
        },
        Custom(_) => RequiredContext::empty(),
    }
}

fn sin_cos_tan(mut val: i32, ty: IntFuncType) -> i32 {
    if ty == IntFuncType::Cos {
        val = val.checked_add(90)
            .unwrap_or_else(|| val.rem_euclid(360) + 90);
    }
    if val < 0 {
        val += 360;
    } else if val >= 360 {
        val -= 360;
    }
    if val < 0 || val >= 360 {
        val = val.rem_euclid(360);
    }
    let mut val = val as usize;
    if ty == IntFuncType::Tan {
        if val >= 180 {
            val -= 180;
        }
        if val < 91 {
            // tan(90) is +INF / -INF; give positive max value
            TAN_TABLE[val]
        } else {
            -TAN_TABLE[90 - (val - 90)]
        }
    } else {
        if val < 180 {
            if val < 90 {
                SIN_TABLE[val] as i32
            } else {
                SIN_TABLE[90 - (val - 90)] as i32
            }
        } else {
            if val < 270 {
                -(SIN_TABLE[val - 180] as i32)
            } else {
                -(SIN_TABLE[90 - (val - 270)] as i32)
            }
        }
    }
}

fn asin_acos_atan(val: i32, ty: IntFuncType) -> i32 {
    let key = if val < 0 {
        val.checked_neg().unwrap_or(i32::MAX)
    } else {
        val
    };
    let index = if ty == IntFuncType::Atan {
        match TAN_TABLE.binary_search_by(|&x| x.cmp(&key)) {
            Ok(x) | Err(x) => x.min(90),
        }
    } else {
        let idx = match SIN_TABLE
            .binary_search_by(|&x| (x as i32).cmp(&key))
        {
            Ok(x) | Err(x) => x.min(90),
        };
        if ty == IntFuncType::Acos {
            90 - idx
        } else {
            idx
        }
    };
    if val < 0 {
        if ty == IntFuncType::Asin {
            -(index as i32)
        } else {
            // Correct for both acos/atan which are symmetric at 90 degrees
            180 - index as i32
        }
    } else {
        index as i32
    }
}

#[test]
fn test_sin_asin() {
    for i in -720..720 {
        let sin = sin_cos_tan(i, IntFuncType::Sin);
        let asin = asin_acos_atan(sin, IntFuncType::Asin);
        let mut expected_asin = i;
        while expected_asin > 180 {
            expected_asin -= 360;
        }
        while expected_asin < -180 {
            expected_asin += 360;
        }
        if expected_asin > 90 {
            expected_asin = 90 - (expected_asin - 90);
        }
        if expected_asin < -90 {
            expected_asin = -90 + (expected_asin + 90);
        }
        if asin != expected_asin {
            // Near 90/270 degrees several degrees give same result
            let ok = sin_cos_tan(asin, IntFuncType::Sin) ==
                sin_cos_tan(expected_asin, IntFuncType::Sin);
            if !ok {
                panic!(
                    "sin/asin not reverse at {} degrees; sin {}, asin {}; expected {}",
                    i, sin, asin, expected_asin,
                );
            }
        }
    }
}

#[test]
fn test_cos_acos() {
    for i in -720..720 {
        let cos = sin_cos_tan(i, IntFuncType::Cos);
        let acos = asin_acos_atan(cos, IntFuncType::Acos);
        let mut expected_acos = i;
        while expected_acos < -0 {
            expected_acos += 360;
        }
        if expected_acos > 180 {
            expected_acos = 180 - (expected_acos - 180);
        }
        if acos != expected_acos {
            let ok = sin_cos_tan(acos, IntFuncType::Cos) ==
                sin_cos_tan(expected_acos, IntFuncType::Cos);
            if !ok {
                panic!(
                    "cos/acos not reverse at {} degrees; cos {}, acos {}; expected {}",
                    i, cos, acos, expected_acos,
                );
            }
        }
    }
}

#[test]
fn test_tan_atan() {
    //for i in -720..720 {
    for i in 0..720 {
        let tan = sin_cos_tan(i, IntFuncType::Tan);
        let atan = asin_acos_atan(tan, IntFuncType::Atan);
        let mut expected_atan = i;
        while expected_atan < -0 {
            expected_atan += 180;
        }
        while expected_atan > 1800 {
            expected_atan -= 180;
        }
        if atan != expected_atan {
            let ok = sin_cos_tan(atan, IntFuncType::Tan) ==
                sin_cos_tan(expected_atan, IntFuncType::Tan);
            if !ok {
                panic!(
                    "tan/atan not reverse at {} degrees; tan {}, atan {}; expected {}",
                    i, tan, atan, expected_atan,
                );
            }
        }
    }
}
