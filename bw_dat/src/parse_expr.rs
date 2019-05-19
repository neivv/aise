use std::fmt::{self, Display, Debug};
use std::cmp::PartialEq;

use combine::{
    Parser, Positioned, RangeStreamOnce,
    attempt, look_ahead, not_followed_by, many, many1, optional, skip_many,
};
use combine::{ConsumedResult, ParseError, StreamOnce, RangeStream};
use combine::byte::{alpha_num, byte, digit, hex_digit, letter, spaces};
use combine::easy;
use combine::error::{Consumed, Tracked, StreamError};
use combine::parser::function;
use combine::range::{range, recognize};
use combine::stream::{Resetable, StreamErrorFor};
use combine::stream::state::{IndexPositioner, State};
use combine::choice;

// bool_expr -> {
//  p1_bool_expr || p1_bool_expr [|| ..]
//  p1_bool_expr && p1_bool_expr [&& ..]
//  p1_bool_expr
// }
// p1_bool_expr -> {
//  int_expr < int_expr
//  int_expr <= int_expr
//  int_expr > int_expr
//  int_expr >= int_expr
//  p2_bool_expr == p2_bool_expr
//  p2_bool_expr != p2_bool_expr
//  int_expr == int_expr
//  int_expr != int_expr
//  p2_bool_expr
// }
// p2_bool_expr -> {
//  true
//  false
//  !p2_bool_expr
//  (bool_expr)
//  bool_fun[(...)]
// }
// int_expr {
//  p1_int_expr +- p1_int_expr [+- ..]
//  p1_int_expr
// }
// p1_int_expr {
//  p2_int_expr */% p2_int_expr [*/% ..]
//  p2_int_expr
// }
// p2_int_expr {
//  0xhex
//  dec
//  int_fun[(...)]
//  (int_expr)
// }
// int_fun {
//  stim_timer
//  ensnare_timer
//  maelstrom_timer
//  death_timer
//  lockdown_timer
//  irradiate_timer
//  stasis_timer
//  plague_timer
//  irradiate_timer
//  matrix_timer
//  matrix_hitpoints
//  acid_spore_count
//  fighters
//  mines
//  hitpoints
//  hitpoints_percent
//  shields
//  shields_percent
//  energy
//  kills
//  frame_count
//  tileset
//  minerals
//  gas
//  carried_resource_amount
//  ground_cooldown
//  air_cooldown
//  spell_cooldown
//  speed
//  sigorder
// }
// bool_fun {
//  parasited
//  blind
//  under_storm
//  lifted_off
//  building_unit
//  in_transport
//  in_bunker
//  carrying_powerup
//  carrying_minerals
//  carrying_gas
//  burrowed
//  disabled
//  completed
//  self_cloaked
//  arbiter_cloaked
//  cloaked
//  under_dweb
//  hallucination
// }

type Bytes<'a> = SingleErrorStream<State<&'a [u8], IndexPositioner>>;

#[derive(Debug)]
pub struct SingleErrorStream<I: StreamOnce> {
    pub inner: I,
}

#[derive(PartialEq)]
pub struct SingleError<I: PartialEq, R: PartialEq, P> {
    pub error: Option<Box<easy::Error<I, R>>>,
    pub pos: P,
}

impl<Item, Range, Position> ParseError<Item, Range, Position> for
    SingleError<Item, Range, Position>
where Item: PartialEq,
      Range: PartialEq,
      Position: PartialEq,
{
    type StreamError = easy::Error<Item, Range>;
    fn empty(position: Position) -> Self {
        SingleError {
            error: None,
            pos: position,
        }
    }

    fn from_error(position: Position, e: Self::StreamError) -> Self {
        SingleError {
            error: Some(Box::new(e)),
            pos: position,
        }
    }

    fn set_position(&mut self, position: Position) {
        self.pos = position;
    }

    fn add(&mut self, e: Self::StreamError) {
        self.error = Some(Box::new(e));
    }

    fn set_expected<F: FnOnce(&mut Tracked<Self>)>(
        self_: &mut Tracked<Self>,
        _info: Self::StreamError,
        f: F,
    ) {
        // wtf
        f(self_);
    }

    fn is_unexpected_end_of_input(&self) -> bool {
        // I really don't know how I'd do this sensibly
        false
    }

    fn into_other<T>(self) -> T
    where T: ParseError<Item, Range, Position>,
    {
        match self.error {
            Some(s) => T::from_error(self.pos, StreamError::into_other(*s)),
            None => T::empty(self.pos),
        }
    }
}

impl<I, R, P> std::error::Error for SingleError<I, R, P>
where I: PartialEq + Display + Debug,
      R: PartialEq + Display + Debug,
{
}

impl<I, R, P> Debug for SingleError<I, R, P>
where I: PartialEq + Debug,
      R: PartialEq + Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            Some(ref s) => write!(f, "{:?}", s),
            None => write!(f, "(No error)"),
        }
    }
}

impl<I, R, P> Display for SingleError<I, R, P>
where I: PartialEq + Display,
      R: PartialEq + Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            Some(ref s) => write!(f, "{}", s),
            None => write!(f, "(No error)"),
        }
    }
}

impl<I: StreamOnce> SingleErrorStream<I> {
    pub fn new(inner: I) -> SingleErrorStream<I> {
        SingleErrorStream {
            inner,
        }
    }
}

impl<I: StreamOnce + Positioned> Positioned for SingleErrorStream<I> {
    fn position(&self) -> I::Position {
        self.inner.position()
    }
}

impl<I: Resetable + StreamOnce> Resetable for SingleErrorStream<I> {
    type Checkpoint = I::Checkpoint;

    fn checkpoint(&self) -> Self::Checkpoint {
        self.inner.checkpoint()
    }

    fn reset(&mut self, checkpoint: Self::Checkpoint) {
        self.inner.reset(checkpoint)
    }
}

impl<I: RangeStream> RangeStreamOnce for SingleErrorStream<I> {
    fn uncons_range(&mut self, size: usize) -> Result<Self::Range, StreamErrorFor<Self>> {
        self.inner.uncons_range(size).map_err(StreamError::into_other)
    }

    fn uncons_while<F>(&mut self, f: F) -> Result<Self::Range, StreamErrorFor<Self>>
    where F: FnMut(Self::Item) -> bool,
    {
        self.inner.uncons_while(f).map_err(StreamError::into_other)
    }

    fn distance(&self, end: &Self::Checkpoint) -> usize {
        self.inner.distance(end)
    }
}

impl<I: StreamOnce> StreamOnce for SingleErrorStream<I> {
    type Item = I::Item;
    type Range = I::Range;
    type Position = I::Position;
    type Error = SingleError<Self::Item, Self::Range, Self::Position>;

    fn uncons(&mut self) -> Result<Self::Item, StreamErrorFor<Self>> {
        self.inner.uncons().map_err(StreamError::into_other)
    }
}

struct Stateless<P: Parser>(P);

impl<P: Parser> Parser for Stateless<P> {
    type Input = P::Input;
    type Output = P::Output;
    type PartialState = ();

    fn parse_lazy(
        &mut self,
        input: &mut Self::Input,
    ) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input)
    }
}

/// Function arguments
/// Given "a, b, c()) + 2", returns "a, b, c()" and ") + 2"
fn func_arg_list<'a>() -> impl Parser<Input = Bytes<'a>, Output = &'a [u8]> {
    Stateless(byte(b'(').with(function::parser(|input: &mut Bytes<'a>| {
        let mut parens = 1;
        let pos = input.position();
        input.uncons_while(|byte| {
            if byte == b'(' {
                parens += 1;
                true
            } else if byte == b')' {
                parens -= 1;
                parens != 0
            } else {
                true
            }
        }).map(|x| {
            (x, Consumed::Consumed(()))
        }).map_err(|e| {
            Consumed::Consumed(SingleError::from_error(pos, e).into())
        })
    })).skip(byte(b')')))
}

fn identifier<'a>() -> impl Parser<Input = Bytes<'a>, Output = &'a [u8]> {
    Stateless(recognize(letter().or(byte(b'_')).with(skip_many(alpha_num().or(byte(b'_'))))))
}

fn func<'a>() -> impl Parser<Input = Bytes<'a>, Output = (&'a [u8], &'a [u8])> {
    Stateless(identifier()
        .and(optional(func_arg_list()))
        .skip(spaces())
        .map(|(x, y)| (x, y.unwrap_or(b""))))
}

macro_rules! decl_func {
    (construct, [], $ename:ident::$variant_name:ident, $params:expr,) => {{
        if $params != b"" {
            let err: easy::Error<_, _>
                = StreamError::unexpected_message("Can't have parameters");
            Err(err)
        } else {
            Ok($ename::$variant_name)
        }
    }};
    (construct, [$construct:expr], $ename:ident::$variant_name:ident, $params:expr,) => {{
        $construct($params)
    }};
    ($name:ident, $ename:ident,
        $(
            $conf_name:expr,
            $variant_name:ident $( ( $($variant_ty:ty),* ) )? $( -> $construct:expr )? ,
        )*
    ) => {
        fn $name<'a>() -> impl Parser<Input = Bytes<'a>, Output = $ename> {
            Stateless(func().and_then(|(name, params)| -> Result<_, easy::Error<_, _>> {
                let result = match name {
                    $($conf_name => decl_func!(
                        construct,
                        [$($construct)?],
                        $ename::$variant_name,
                        params,
                    )?,)*
                    other => {
                        let message = format!("Invalid name {}", String::from_utf8_lossy(other));
                        return Err(StreamError::unexpected_message(message));
                    }
                };
                Ok(result)
            }))
        }

        #[derive(Debug, Eq, PartialEq, Clone, Hash)]
        pub enum $ename {
            $($variant_name $( ($($variant_ty),*) )? ,)*
        }
    };
}

decl_func!(
    int_func, IntFunc,
    b"stim_timer", StimTimer,
    b"ensnare_timer", EnsnareTimer,
    b"maelstrom_timer", MaelstromTimer,
    b"death_timer", DeathTimer,
    b"lockdown_timer", LockdownTimer,
    b"stasis_timer", StasisTimer,
    b"irradiate_timer", IrradiateTimer,
    b"matrix_timer", MatrixTimer,
    b"matrix_hitpoints", MatrixHitpoints,
    b"acid_spore_count", AcidSporeCount,
    b"fighters", Fighters,
    b"mines", Mines,
    b"hitpoints", Hitpoints,
    b"hitpoints_percent", HitpointsPercent,
    b"shields", Shields,
    b"shields_percent", ShieldsPercent,
    b"energy", Energy,
    b"kills", Kills,
    b"frame_count", FrameCount,
    b"tileset", Tileset,
    b"minerals", Minerals,
    b"gas", Gas,
    b"carried_resource_amount", CarriedResourceAmount,
    b"ground_cooldown", GroundCooldown,
    b"air_cooldown", AirCooldown,
    b"spell_cooldown", SpellCooldown,
    b"speed", Speed,
    b"sigorder", SigOrder,
    b"player", Player,
    b"unit_id", UnitId,
    b"sin", Sin(Box<IntExpr>) -> |x| -> Result<IntFunc, easy::Error<_, _>> {
        parse_single_expr(x).map(|x| IntFunc::Sin(Box::new(x)))
    },
    b"cos", Cos(Box<IntExpr>) -> |x| -> Result<IntFunc, easy::Error<_, _>> {
        parse_single_expr(x).map(|x| IntFunc::Cos(Box::new(x)))
    },
    b"deaths", Deaths(Box<(IntExpr, IntExpr)>) -> |x| -> Result<IntFunc, easy::Error<_, _>> {
        let args = parse_many_args(x)
            .and_then(|x| match x.len() {
                2 => Ok((x[0], x[1])),
                _ => Err(()),
            });
        let args = match args {
            Ok(o) => o,
            Err(()) => return Err(StreamError::message_static_message("Invalid argument list")),
        };
        let player = parse_single_expr(&args.0)?;
        let unit_id = parse_single_expr(&args.1)?;
        Ok(IntFunc::Deaths(Box::new((player, unit_id))))
    },
);

fn parse_many_args<'a>(x: &'a [u8]) -> Result<Vec<&'a [u8]>, ()> {
    x.split(|&x| x == b',')
        .map(|x| {
            let start = x.iter().position(|&x| x != b' ').ok_or(())?;
            let end = x.len() - x.iter().rev().position(|&x| x != b' ').ok_or(())?;
            Ok(&x[start..end])
        })
        .collect::<Result<Vec<_>, ()>>()
}

fn parse_single_expr<'a>(x: &'a [u8]) -> Result<IntExpr, easy::Error<u8, &'a [u8]>> {
    let mut parser = int_expr();
    let (result, rest) = parser.parse(SingleErrorStream::new(State::new(x)))
        .map_err(|e| match e.error {
            Some(s) => *s,
            None => unreachable!(),
        })?;
    if !rest.inner.input.is_empty() {
        let msg = "Extra characters in arguments";
        return Err(StreamError::message_static_message(msg));
    }
    Ok(result)
}

decl_func!(
    bool_func, BoolFunc,
    // Shrug, true/false work this way
    b"true", True,
    b"false", False,
    b"parasited", Parasited,
    b"blind", Blind,
    b"under_storm", UnderStorm,
    b"lifted_off", LiftedOff,
    b"building_unit", BuildingUnit,
    b"in_transport", InTransport,
    b"in_bunker", InBunker,
    b"carrying_powerup", CarryingPowerup,
    b"carrying_minerals", CarryingMinerals,
    b"carrying_gas", CarryingGas,
    b"burrowed", Burrowed,
    b"disabled", Disabled,
    b"completed", Completed,
    b"self_cloaked", SelfCloaked,
    b"arbiter_cloaked", ArbiterCloaked,
    b"cloaked", Cloaked,
    b"under_dweb", UnderDweb,
    b"hallucination", Hallucination,
);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum IntExpr {
    Add(Box<(IntExpr, IntExpr)>),
    Sub(Box<(IntExpr, IntExpr)>),
    Mul(Box<(IntExpr, IntExpr)>),
    Div(Box<(IntExpr, IntExpr)>),
    Modulo(Box<(IntExpr, IntExpr)>),
    Integer(i32),
    Func(IntFunc),
}

pub fn int_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = IntExpr> {
    Stateless(Box::new(p1_int_expr().skip(spaces()).and(
        many::<Vec<_>, _>(byte(b'+').or(byte(b'-')).skip(spaces()).and(p1_int_expr()))
    ).map(|(mut left, rest)| {
        for (op, right) in rest {
            match op {
                b'+' => left = IntExpr::Add(Box::new((left, right))),
                b'-' | _  => left = IntExpr::Sub(Box::new((left, right))),
            }
        }
        left
    })))
}

fn p1_int_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = IntExpr> {
    Stateless(p2_int_expr().skip(spaces()).and(
        many::<Vec<_>, _>(
            choice!(
                byte(b'*'),
                byte(b'/'),
                byte(b'%')
            ).skip(spaces()).and(p2_int_expr())
        )
    ).and_then(|(mut left, rest)| -> Result<_, easy::Error<_, _>> {
        for (op, right) in rest {
            match op {
                b'*' => left = IntExpr::Mul(Box::new((left, right))),
                b'/' | b'%' | _ => {
                    match right {
                        IntExpr::Integer(0) => {
                            let msg = "Cannot divide by zero";
                            return Err(StreamError::message_static_message(msg));
                        }
                        IntExpr::Integer(_) => (),
                        _ => {
                            let msg = "Can only divide by a constant";
                            return Err(StreamError::message_static_message(msg));
                        }
                    }
                    left = if op == b'/' {
                        IntExpr::Div(Box::new((left, right)))
                    } else {
                        IntExpr::Modulo(Box::new((left, right)))
                    };
                }
            }
        }
        Ok(left)
    }))
}

fn p2_int_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = IntExpr> {
    let lazy = function::parser(|input| int_expr().parse_stream(input));
    Stateless(choice!(
        Stateless(integer().map(|x| IntExpr::Integer(x))),
        Stateless(int_func().map(|x| IntExpr::Func(x))),
        Stateless(byte(b'(').skip(spaces()).with(lazy).skip(byte(b')')))
    ))
}

fn integer<'a>() -> impl Parser<Input = Bytes<'a>, Output = i32> {
    use std::str;
    Stateless(optional(byte(b'-'))
        .skip(look_ahead(digit()))
        .and(
            optional(range(&b"0x"[..])).and(
                recognize(skip_many(hex_digit()))
            )
        )
        .skip(not_followed_by(alpha_num()))
        .skip(spaces())
        .and_then(|(neg, (hex, int))| -> Result<_, easy::Error<_, _>> {
            let base = match hex.is_some() {
                true => 16,
                false => 10,
            };
            let int_parse_result = str::from_utf8(int).ok()
                .and_then(|x| i32::from_str_radix(x, base).ok());
            match int_parse_result {
                Some(o) => Ok(match neg.is_some() {
                    true => 0 - o,
                    false => o,
                }),
                None => Err(StreamError::unexpected_message("Invalid integer literal")),
            }
        }))
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum BoolExpr {
    And(Box<(BoolExpr, BoolExpr)>),
    Or(Box<(BoolExpr, BoolExpr)>),
    LessThan(Box<(IntExpr, IntExpr)>),
    LessOrEqual(Box<(IntExpr, IntExpr)>),
    GreaterThan(Box<(IntExpr, IntExpr)>),
    GreaterOrEqual(Box<(IntExpr, IntExpr)>),
    EqualInt(Box<(IntExpr, IntExpr)>),
    EqualBool(Box<(BoolExpr, BoolExpr)>),
    Not(Box<BoolExpr>),
    Func(BoolFunc),
}

pub fn bool_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = BoolExpr> {
    Stateless(p1_bool_expr().skip(spaces()).and(
        many1::<Vec<_>, _>(range(&b"||"[..]).skip(spaces()).and(p1_bool_expr()))
            .or(many::<Vec<_>, _>(range(&b"&&"[..]).skip(spaces()).and(p1_bool_expr())))
    ).map(|(mut left, rest)| {
        for (op, right) in rest {
            match op {
                b"||" => left = BoolExpr::Or(Box::new((left, right))),
                b"&&" | _  => left = BoolExpr::And(Box::new((left, right))),
            }
        }
        left
    }))
}

fn p1_bool_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = BoolExpr> {
    Stateless(attempt(int_expr()).and(
        choice!(
            range(&b"<="[..]),
            range(&b"<"[..]),
            range(&b">="[..]),
            range(&b">"[..]),
            range(&b"=="[..]),
            range(&b"!="[..])
        ).skip(spaces())
    ).and(int_expr()).map(|((left, op), right)| {
        match op {
            b"<" => BoolExpr::LessThan(Box::new((left, right))),
            b"<=" => BoolExpr::LessOrEqual(Box::new((left, right))),
            b">" => BoolExpr::GreaterThan(Box::new((left, right))),
            b">=" => BoolExpr::GreaterOrEqual(Box::new((left, right))),
            b"==" => BoolExpr::EqualInt(Box::new((left, right))),
            b"!=" | _ => BoolExpr::Not(Box::new(BoolExpr::EqualInt(Box::new((left, right))))),
        }
    }).or(
        p2_bool_expr().skip(spaces()).and(Stateless(
            optional(range(&b"=="[..]).or(range(&b"!="[..])).skip(spaces()).and(p2_bool_expr()))
        )).map(|(mut left, rest)| {
            if let Some((op, right)) = rest {
                match op {
                    b"==" => left = BoolExpr::EqualBool(Box::new((left, right))),
                    b"!=" | _ => {
                        left =
                            BoolExpr::Not(Box::new(BoolExpr::EqualBool(Box::new((left, right)))));
                    }
                }
            }
            left
        })
    ))
}

fn p2_bool_expr<'a>() -> impl Parser<Input = Bytes<'a>, Output = BoolExpr> {
    let lazy = function::parser(|input| bool_expr().parse_stream(input));
    let lazy_p2 = function::parser(|input| p2_bool_expr().parse_stream(input));
    Stateless(choice!(
        Stateless(byte(b'!').with(lazy_p2).map(|x| BoolExpr::Not(Box::new(x)))),
        Stateless(bool_func().map(|x| BoolExpr::Func(x))),
        Stateless(byte(b'(').skip(spaces()).with(lazy).skip(byte(b')')))
    ))
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;

    fn error_contains<T: Debug, A: PartialEq, B: PartialEq, C>(
        result: Result<T, SingleError<A, B, C>>,
        expected: &str,
    ) -> bool {
        result.unwrap_err().error.into_iter().any(|x| {
            match *x {
                easy::Error::Unexpected(i) |
                    easy::Error::Expected(i) |
                    easy::Error::Message(i) =>
                {
                    match i {
                        easy::Info::Owned(s) => s.contains(expected),
                        easy::Info::Borrowed(s) => s.contains(expected),
                        _ => false,
                    }
                }
                _ => false,
            }
        })
    }

    fn empty_unwrap<'a, T, A: Debug + PartialEq, B: Debug + PartialEq, C: Debug>(
        result: Result<(T, Bytes), SingleError<A, B, C>>,
    ) -> T {
        let result = result.unwrap();
        assert_eq!(result.1.inner.input.len(), 0);
        result.0
    }

    fn s<'a>(text: &'a [u8]) -> Bytes {
        SingleErrorStream::new(State::new(text))
    }

    #[test]
    fn test_int_func() {
        let mut parser = int_func();
        let mut parse = |text| {
            parser.parse(s(text))
        };
        assert_eq!(empty_unwrap(parse(b"hitpoints")), IntFunc::Hitpoints);
        assert_eq!(empty_unwrap(parse(b"hitpoints()")), IntFunc::Hitpoints);
        assert_eq!(
            empty_unwrap(parse(b"acid_spore_count")),
            IntFunc::AcidSporeCount
        );
        assert!(error_contains(parse(b"hitpoints(s)"), "Can't have parameter"));
        assert!(error_contains(parse(b"unknown"), "Invalid"));
    }

    #[test]
    fn test_int_expr() {
        let mut parser = int_expr();
        let mut parse = |text| {
            parser.parse(s(text))
        };
        assert_eq!(empty_unwrap(parse(b"20")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"0x20")), IntExpr::Integer(0x20));
        assert_eq!(empty_unwrap(parse(b"-20")), IntExpr::Integer(-20));
        assert_eq!(empty_unwrap(parse(b"-0x20")), IntExpr::Integer(-0x20));
        assert_eq!(empty_unwrap(parse(b"(20)")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"((20))")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"energy")), IntExpr::Func(IntFunc::Energy));
        assert_eq!(
            empty_unwrap(parse(b"2+2")),
            IntExpr::Add(Box::new((IntExpr::Integer(2), IntExpr::Integer(2))))
        );
        assert_eq!(
            empty_unwrap(parse(b"2 + 2")),
            IntExpr::Add(Box::new((IntExpr::Integer(2), IntExpr::Integer(2))))
        );
        assert_eq!(
            empty_unwrap(parse(b"6 * 8")),
            IntExpr::Mul(Box::new((IntExpr::Integer(6), IntExpr::Integer(8))))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 + 2 - 6")),
            IntExpr::Sub(Box::new((
                IntExpr::Add(Box::new((IntExpr::Integer(8), IntExpr::Integer(2)))),
                IntExpr::Integer(6)
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 + 2 * 6")),
            IntExpr::Add(Box::new((
                IntExpr::Integer(8),
                IntExpr::Mul(Box::new((IntExpr::Integer(2), IntExpr::Integer(6))))
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 * 2 - 6")),
            IntExpr::Sub(Box::new((
                IntExpr::Mul(Box::new((IntExpr::Integer(8), IntExpr::Integer(2)))),
                IntExpr::Integer(6)
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"8 * (2 - 6)")),
            IntExpr::Mul(Box::new((
                IntExpr::Integer(8),
                IntExpr::Sub(Box::new((IntExpr::Integer(2), IntExpr::Integer(6))))
            )))
        );
        assert!(error_contains(parse(b"4294967295"), "Invalid integer literal"));
        assert!(error_contains(parse(b"5 / 0"), "divide by zero"));
        assert!(parse(b"54k").is_err());
    }

    #[test]
    fn test_bool_expr() {
        let mut parser = bool_expr();
        let mut parse = |text| {
            parser.parse(s(text))
        };
        let fun_true = || BoolExpr::Func(BoolFunc::True);
        let fun_false = || BoolExpr::Func(BoolFunc::False);
        assert_eq!(empty_unwrap(parse(b"true")), fun_true());
        assert_eq!(empty_unwrap(parse(b"false")), fun_false());
        assert_eq!(empty_unwrap(parse(b"((true))")), fun_true());
        assert_eq!(empty_unwrap(parse(b"blind")), BoolExpr::Func(BoolFunc::Blind));
        assert_eq!(
            empty_unwrap(parse(b"true || false")),
            BoolExpr::Or(Box::new((fun_true(), fun_false())))
        );
        assert_eq!(
            empty_unwrap(parse(b"true && false")),
            BoolExpr::And(Box::new((fun_true(), fun_false())))
        );
        assert_eq!(
            empty_unwrap(parse(b"true && false && true")),
            BoolExpr::And(Box::new((
                BoolExpr::And(Box::new((fun_true(), fun_false()))),
                fun_true(),
            )))
        );
        assert_eq!(
            empty_unwrap(parse(b"true && (false || true)")),
            BoolExpr::And(Box::new((
                fun_true(),
                BoolExpr::Or(Box::new((fun_false(), fun_true()))),
            )))
        );

        assert_eq!(
            empty_unwrap(parse(b"!false")),
            BoolExpr::Not(Box::new(fun_false())),
        );
        assert_eq!(
            empty_unwrap(parse(b"!true && (false || true)")),
            BoolExpr::And(Box::new((
                BoolExpr::Not(Box::new(fun_true())),
                BoolExpr::Or(Box::new((fun_false(), fun_true()))),
            )))
        );

        assert_eq!(
            empty_unwrap(parse(b"true == false")),
            BoolExpr::EqualBool(Box::new((fun_true(), fun_false())))
        );
        assert_eq!(
            empty_unwrap(parse(b"1 == 2")),
            BoolExpr::EqualInt(Box::new((IntExpr::Integer(1), IntExpr::Integer(2))))
        );
        assert_eq!(
            empty_unwrap(parse(b"1 + 8 == 2")),
            BoolExpr::EqualInt(Box::new((
                IntExpr::Add(Box::new((IntExpr::Integer(1), IntExpr::Integer(8)))),
                IntExpr::Integer(2),
            )))
        );

        assert_eq!(parse(b"true || false && true").unwrap().1.inner.input.len(), "&& true".len());
        parse(b"! true").unwrap_err();

        assert_eq!(
            empty_unwrap(parse(b"energy >= 50 && energy < 100")),
            BoolExpr::And(Box::new((
                BoolExpr::GreaterOrEqual(Box::new((
                    IntExpr::Func(IntFunc::Energy),
                    IntExpr::Integer(50),
                ))),
                BoolExpr::LessThan(Box::new((
                    IntExpr::Func(IntFunc::Energy),
                    IntExpr::Integer(100),
                ))),
            )))
        );
    }

    #[test]
    fn test_trig() {
        let mut parser = int_func();
        let mut parse = |text| {
            parser.parse(s(text))
        };
        assert_eq!(empty_unwrap(parse(b"sin(5)")), IntFunc::Sin(Box::new(IntExpr::Integer(5))));
        assert_eq!(
            empty_unwrap(parse(b"cos(frame_count)")),
            IntFunc::Cos(Box::new(IntExpr::Func(IntFunc::FrameCount))),
        );
        parse(b"sin()").unwrap_err();
        parse(b"sin(5, 6)").unwrap_err();
        parse(b"sin(5").unwrap_err();
    }

    #[test]
    fn test_complex_int_expr() {
        let mut parser = int_expr();
        let mut parse = |text| {
            parser.parse(s(text))
        };
        assert_eq!(
            empty_unwrap(parse(b"(sin(hitpoints)) / 2")),
            IntExpr::Div(Box::new((
                IntExpr::Func(IntFunc::Sin(Box::new(IntExpr::Func(IntFunc::Hitpoints)))),
                IntExpr::Integer(2),
            )))
        );
    }

    #[test]
    fn test_2arg_fn() {
        let mut parser = int_func();
        let mut parse = |text| {
            parser.parse(s(text))
        };
        assert_eq!(
            empty_unwrap(parse(b"deaths(player, 5)")),
            IntFunc::Deaths(Box::new((IntExpr::Func(IntFunc::Player), IntExpr::Integer(5)))),
        );
        assert_eq!(
            empty_unwrap(parse(b"deaths( player, 5  )")),
            IntFunc::Deaths(Box::new((IntExpr::Func(IntFunc::Player), IntExpr::Integer(5)))),
        );
    }
}
