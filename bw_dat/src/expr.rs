pub use crate::parse_expr::{IntExpr, BoolExpr};

use std::fmt;
use std::ptr::null_mut;

use combine::parser::Parser;

use crate::game::Game;
use crate::parse_expr;
use crate::unit::{self, Unit};
use crate::order;

#[derive(Debug)]
pub struct Error {
    message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl std::error::Error for Error {}

fn format_combine_err(e: &parse_expr::SingleError<u8, &[u8], usize>, input: &[u8]) -> Error {
    use std::fmt::Write;

    use combine::easy;

    fn format_info(i: &easy::Info<u8, &[u8]>) -> String {
        match i {
            easy::Info::Token(x) => format!("{}", x),
            easy::Info::Range(x) => format!("{}", String::from_utf8_lossy(x)),
            easy::Info::Owned(x) => format!("{}", x),
            easy::Info::Borrowed(x) => format!("{}", x),
        }
    }
    let mut msg = format!("Starting from {}\n", String::from_utf8_lossy(&input[e.pos..]));
    if let Some(ref err) = e.error {
        match **err {
            easy::Error::Expected(ref i) => {
                writeln!(msg, "Expected {}", format_info(i)).unwrap()
            }
            easy::Error::Unexpected(ref i) => {
                writeln!(msg, "Unexpected {}", format_info(i)).unwrap()
            }
            easy::Error::Message(ref i) => {
                writeln!(msg, "Note: {}", format_info(i)).unwrap()
            }
            _ => (),
        }
    }
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
        use combine::stream::state::State;
        use crate::parse_expr::SingleErrorStream;
        parse_expr::int_expr().parse(SingleErrorStream::new(State::new(bytes)))
            .map_err(|e| format_combine_err(&e, bytes))
            .map(|(result, rest)| (result, rest.inner.input))
    }

    pub fn eval_with_unit(&self, unit: Unit, game: Game) -> i32 {
        use crate::parse_expr::IntExpr::*;
        use crate::parse_expr::IntFunc::*;
        match self {
            Add(x) => {
                x.0.eval_with_unit(unit, game).saturating_add(x.1.eval_with_unit(unit, game))
            }
            Sub(x) => {
                x.0.eval_with_unit(unit, game).saturating_sub(x.1.eval_with_unit(unit, game))
            }
            Mul(x) => {
                x.0.eval_with_unit(unit, game).saturating_mul(x.1.eval_with_unit(unit, game))
            }
            Div(x) => {
                x.0.eval_with_unit(unit, game) / (x.1.eval_with_unit(unit, game))
            }
            Modulo(x) => {
                x.0.eval_with_unit(unit, game) % (x.1.eval_with_unit(unit, game))
            }
            Integer(i) => *i,
            Func(x) => {
                unsafe {
                    match x {
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
                        Sin(degrees) => {
                            let val = degrees.eval_with_unit(unit, game);
                            ((val as f32).to_radians().sin() * 256.0) as i32
                        }
                        Cos(degrees) => {
                            let val = degrees.eval_with_unit(unit, game);
                            ((val as f32).to_radians().cos() * 256.0) as i32
                        }
                        Deaths(values) => {
                            let player = match values.0.eval_with_unit(unit, game) {
                                x if x >= 0 && x < 12 => x,
                                _ => return i32::min_value(),
                            };
                            let unit = match values.1.eval_with_unit(unit, game) {
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
        use combine::stream::state::State;
        use crate::parse_expr::SingleErrorStream;
        parse_expr::bool_expr().parse(SingleErrorStream::new(State::new(bytes)))
            .map_err(|e| format_combine_err(&e, bytes))
            .map(|(result, rest)| (result, rest.inner.input))
    }

    pub fn eval_with_unit(&self, unit: Unit, game: Game) -> bool {
        use crate::parse_expr::BoolExpr::*;
        use crate::parse_expr::BoolFunc::*;
        match self {
            And(x) => x.0.eval_with_unit(unit, game) && x.1.eval_with_unit(unit, game),
            Or(x) => x.0.eval_with_unit(unit, game) || x.1.eval_with_unit(unit, game),
            LessThan(x) => x.0.eval_with_unit(unit, game) < x.1.eval_with_unit(unit, game),
            LessOrEqual(x) => x.0.eval_with_unit(unit, game) <= x.1.eval_with_unit(unit, game),
            GreaterThan(x) => x.0.eval_with_unit(unit, game) > x.1.eval_with_unit(unit, game),
            GreaterOrEqual(x) => x.0.eval_with_unit(unit, game) >= x.1.eval_with_unit(unit, game),
            EqualInt(x) => x.0.eval_with_unit(unit, game) == x.1.eval_with_unit(unit, game),
            EqualBool(x) => x.0.eval_with_unit(unit, game) == x.1.eval_with_unit(unit, game),
            Not(x) => !x.eval_with_unit(unit, game),
            Func(x) => {
                unsafe {
                    match x {
                        True => true,
                        False => true,
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
