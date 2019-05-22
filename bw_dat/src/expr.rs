pub use crate::parse_expr::{IntFunc, BoolFunc};
pub use crate::parse_expr::{BoolExpr as BoolExprTree, IntExpr as IntExprTree};

use std::any::Any;
use std::fmt;
use std::ptr::null_mut;

use bitflags::bitflags;

use crate::game::Game;
use crate::parse_expr;
use crate::unit::{self, Unit};
use crate::order;

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

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct IntExpr {
    ty: parse_expr::IntExpr,
    required_context: RequiredContext,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct BoolExpr {
    ty: parse_expr::BoolExpr,
    required_context: RequiredContext,
}

bitflags! {
    pub struct RequiredContext: u8 {
        const UNIT = 0x1;
        const GAME = 0x2;
    }
}

pub trait CustomEval: Sized {
    fn eval_int(&mut self, param: &Box<dyn Any>) -> i32;
    fn eval_bool(&mut self, param: &Box<dyn Any>) -> bool;
}

pub struct EvalCtx<E: CustomEval> {
    pub unit: Option<Unit>,
    pub game: Option<Game>,
    pub custom: E,
}

struct DefaultEval;
impl CustomEval for DefaultEval {
    fn eval_int(&mut self, _: &Box<dyn Any>) -> i32 {
        panic!("Custom eval term");
    }

    fn eval_bool(&mut self, _: &Box<dyn Any>) -> bool {
        panic!("Custom eval term");
    }
}

impl<E: CustomEval> EvalCtx<E> {
    fn eval_int(&mut self, expr: &parse_expr::IntExpr) -> i32 {
        use crate::parse_expr::IntExpr::*;
        use crate::parse_expr::IntFuncType::*;
        match expr {
            Add(x) => self.eval_int(&x.0).saturating_add(self.eval_int(&x.1)),
            Sub(x) => self.eval_int(&x.0).saturating_sub(self.eval_int(&x.1)),
            Mul(x) => self.eval_int(&x.0).saturating_mul(self.eval_int(&x.1)),
            Div(x) => {
                self.eval_int(&x.0).checked_div(self.eval_int(&x.1))
                    .unwrap_or(i32::max_value())
            }
            Modulo(x) => {
                let div = self.eval_int(&x.1);
                if div == 0 {
                    i32::max_value()
                } else {
                    self.eval_int(&x.0) % div
                }
            }
            Integer(i) => *i,
            //Custom(x) => self.custom.eval_int(x),
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
                        AcidSporeCount => (**unit).acid_spore_count as i32,
                        Fighters => unit.fighter_amount() as i32,
                        Mines => unit.mine_amount(game) as i32,
                        Hitpoints => unit.hitpoints(),
                        HitpointsPercent => unit.hitpoints() * 100 / unit.id().hitpoints(),
                        Shields => unit.shields(),
                        ShieldsPercent => unit.shields() * 100 / unit.id().shields(),
                        Energy => unit.energy() as i32,
                        Kills => (**unit).kills as i32,
                        FrameCount => game.frame_count() as i32,
                        Tileset => (**game).tileset as i32,
                        Minerals => (**game).minerals[unit.player() as usize] as i32,
                        Gas => (**game).gas[unit.player() as usize] as i32,
                        CarriedResourceAmount => {
                            if unit.id().is_worker() {
                                (**unit).unit_specific[0xf] as i32
                            } else {
                                0
                            }
                        }
                        GroundCooldown => (**unit).ground_cooldown as i32,
                        AirCooldown => (**unit).air_cooldown as i32,
                        SpellCooldown => (**unit).spell_cooldown as i32,
                        Speed => (**unit).speed,
                        SigOrder => (**unit).order_signal as i32,
                        Player => (**unit).player as i32,
                        UnitId => (**unit).unit_id as i32,
                        Order => (**unit).order as i32,
                        Sin => {
                            let val = self.eval_int(&x.args[0]);
                            ((val as f32).to_radians().sin() * 256.0) as i32
                        }
                        Cos => {
                            let val = self.eval_int(&x.args[0]);
                            ((val as f32).to_radians().cos() * 256.0) as i32
                        }
                        Deaths => {
                            let player = match self.eval_int(&x.args[0]) {
                                x if x >= 0 && x < 12 => x,
                                _ => return i32::min_value(),
                            };
                            let unit = match self.eval_int(&x.args[1]) {
                                x if x >= 0 && x < unit::NONE.0 as i32 => x,
                                _ => return i32::min_value(),
                            };
                            (**game).deaths[unit as usize][player as usize] as i32
                        }
                    }
                }
            }
        }
    }

    fn eval_bool(&mut self, expr: &parse_expr::BoolExpr) -> bool {
        use crate::parse_expr::BoolExpr::*;
        use crate::parse_expr::BoolFuncType::*;
        match expr {
            And(x) => self.eval_bool(&x.0) && self.eval_bool(&x.1),
            Or(x) => self.eval_bool(&x.0) || self.eval_bool(&x.1),
            LessThan(x) => self.eval_int(&x.0) < self.eval_int(&x.1),
            LessOrEqual(x) => self.eval_int(&x.0) <= self.eval_int(&x.1),
            GreaterThan(x) => self.eval_int(&x.0) > self.eval_int(&x.1),
            GreaterOrEqual(x) => self.eval_int(&x.0) <= self.eval_int(&x.1),
            EqualInt(x) => self.eval_int(&x.0) == self.eval_int(&x.1),
            EqualBool(x) => self.eval_bool(&x.0) == self.eval_bool(&x.1),
            Not(x) => !self.eval_bool(&x),
            //Custom(x) => self.custom.eval_bool(&x),
            Func(x) => {
                unsafe {
                    let unit = self.unit.unwrap_or(Unit::from_ptr(16 as *mut _).unwrap());
                    match x.ty {
                        True => true,
                        False => false,
                        Parasited => (**unit).parasited_by_players != 0,
                        Blind => (**unit).is_blind != 0,
                        UnderStorm => (**unit).is_under_storm != 0,
                        LiftedOff => (**unit).flags & 0x2 == 0,
                        BuildingUnit => (**unit).currently_building != null_mut(),
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
                    }
                }
            }
        }
    }
}

fn format_err(e: &parse_expr::Error<'_>, _input: &[u8]) -> Error {
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
        IntExpr::parse_part_custom(bytes)
    }

    pub fn parse_part_custom(bytes: &[u8]) -> Result<(IntExpr, &[u8]), Error> {
        let parser = crate::parse_expr::Parserr::new();
        parser.int_expr(bytes)
            .map_err(|e| format_err(&e, bytes))
            .map(|(result, rest)| (IntExpr {
                required_context: int_expr_required_context(&result),
                ty: result,
            }, rest))
    }

    pub fn inner(&self) -> &IntExprTree {
        &self.ty
    }

    pub fn eval_with_unit(&self, unit: Unit, game: Game) -> i32 {
        let mut ctx = EvalCtx {
            unit: Some(unit),
            game: Some(game),
            custom: DefaultEval,
        };
        ctx.eval_int(&self.ty)
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
        BoolExpr::parse_part_custom(bytes)
    }

    pub fn parse_part_custom(bytes: &[u8]) -> Result<(BoolExpr, &[u8]), Error> {
        let parser = crate::parse_expr::Parserr::new();
        parser.bool_expr(bytes)
            .map_err(|e| format_err(&e, bytes))
            .map(|(result, rest)| (BoolExpr {
                required_context: bool_expr_required_context(&result),
                ty: result,
            }, rest))
    }

    pub fn inner(&self) -> &BoolExprTree {
        &self.ty
    }

    pub fn eval_with_unit(&self, unit: Unit, game: Game) -> bool {
        let mut ctx = EvalCtx {
            unit: Some(unit),
            game: Some(game),
            custom: DefaultEval,
        };
        ctx.eval_bool(&self.ty)
    }
}

fn bool_expr_required_context(expr: &parse_expr::BoolExpr) -> RequiredContext {
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
            Parasited | Blind | UnderStorm | LiftedOff | BuildingUnit | InTransport |
                InBunker | CarryingPowerup | CarryingMinerals | CarryingGas | Burrowed |
                Disabled | Completed | SelfCloaked | ArbiterCloaked | Cloaked |
                UnderDweb | Hallucination => RequiredContext::UNIT,
        },
        //Custom(_) => RequiredContext::empty(),
    }
}

fn int_expr_required_context(expr: &parse_expr::IntExpr) -> RequiredContext {
    use crate::parse_expr::IntExpr::*;
    use crate::parse_expr::IntFuncType::*;
    match expr {
        Add(x) | Sub(x) | Mul(x) | Div(x) | Modulo(x) => {
            int_expr_required_context(&x.0) | int_expr_required_context(&x.1)
        }
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
            Sin | Cos => int_expr_required_context(&x.args[0]),
            Deaths => {
                int_expr_required_context(&x.args[0]) | int_expr_required_context(&x.args[1]) |
                    RequiredContext::GAME
            }
        },
        //Custom(_) => RequiredContext::empty(),
    }
}
