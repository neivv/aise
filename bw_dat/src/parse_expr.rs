use fxhash::FxHashMap;
use once_cell::sync::Lazy;

#[derive(Debug)]
pub enum Error<'a, C: CustomState> {
    Msg(&'a [u8], &'static str),
    Eof,
    NotBoolean(IntExpr<C>),
    NotInteger(BoolExpr<C>),
}

pub trait CustomState {
    type IntExt: std::fmt::Debug + Eq + PartialEq + std::hash::Hash;
    type BoolExt: std::fmt::Debug + Eq + PartialEq + std::hash::Hash;
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct NoCustom;
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Void {
}

impl CustomState for NoCustom {
    type IntExt = Void;
    type BoolExt = Void;
}

pub trait CustomParser {
    type State: CustomState;
    fn parse_int<'a>(&mut self, input: &'a [u8]) ->
        Option<(<Self::State as CustomState>::IntExt, &'a [u8])>;
    fn parse_bool<'a>(&mut self, input: &'a [u8]) ->
        Option<(<Self::State as CustomState>::BoolExt, &'a [u8])>;
}

pub struct DefaultParser;

impl CustomParser for DefaultParser {
    type State = NoCustom;
    fn parse_int<'a>(&mut self, _: &'a [u8]) ->
        Option<(<Self::State as CustomState>::IntExt, &'a [u8])> { None }

    fn parse_bool<'a>(&mut self, _: &'a [u8]) ->
        Option<(<Self::State as CustomState>::BoolExt, &'a [u8])> { None }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum IntExpr<C: CustomState> {
    Add(Box<(IntExpr<C>, IntExpr<C>)>),
    Sub(Box<(IntExpr<C>, IntExpr<C>)>),
    Mul(Box<(IntExpr<C>, IntExpr<C>)>),
    Div(Box<(IntExpr<C>, IntExpr<C>)>),
    Modulo(Box<(IntExpr<C>, IntExpr<C>)>),
    Integer(i32),
    Func(IntFunc<C>),
    Custom(C::IntExt),
}

macro_rules! decl_funcc {
    (argc, $argc:expr) => {
        $argc
    };
    (argc,) => {
        0
    };
    ($stname:ident, $ename:ident,
        $($conf_name:expr, $variant_name:ident $( ( $argc:expr ) )?  ,)*
    ) => {
        static $stname: &[(&[u8], ($ename, u8))] = &[
            $( ($conf_name, ($ename::$variant_name, decl_funcc!(argc, $($argc)?) )), )*
        ];

        #[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
        pub enum $ename {
            $($variant_name,)*
        }
    };
}

decl_funcc!(
    INT_FUNCS, IntFuncType,
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
    b"order", Order,
    b"sin", Sin(1),
    b"cos", Cos(1),
    b"deaths", Deaths(2),
    b"upgrade", Upgrade(2),
    b"unit_count_completed", UnitCountCompleted(2),
    b"unit_count_any", UnitCountAny(2),
);

decl_funcc!(
    BOOL_FUNCS, BoolFuncType,
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
    b"tech", Tech(2),
);

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct BoolFunc<C: CustomState> {
    pub ty: BoolFuncType,
    pub args: Box<[IntExpr<C>]>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct IntFunc<C: CustomState> {
    pub ty: IntFuncType,
    pub args: Box<[IntExpr<C>]>,
}

static PARSER_MAPS: Lazy<ParserMaps> = Lazy::new(|| ParserMaps {
    int_funcs: INT_FUNCS.iter().cloned().collect(),
    bool_funcs: BOOL_FUNCS.iter().cloned().collect(),
});

struct ParserMaps {
    int_funcs: FxHashMap<&'static [u8], (IntFuncType, u8)>,
    bool_funcs: FxHashMap<&'static [u8], (BoolFuncType, u8)>,
}

pub struct Parser<'a, P: CustomParser> {
    maps: &'static ParserMaps,
    custom_state: &'a mut P,
}

impl<'b, C: CustomState, P: CustomParser<State = C>> Parser<'b, P> {
    pub fn new(custom_state: &mut P) -> Parser<P> {
        Parser {
            maps: &PARSER_MAPS,
            custom_state,
        }
    }

    fn int_func<'a>(&mut self, input: &'a [u8]) -> Result<(IntFunc<C>, &'a [u8]), Error<'a, C>> {
        let next_nonalpha = input.iter().position(|&x| {
            !x.is_ascii_alphanumeric() && x != b'_'
        }).unwrap_or(input.len());
        let rest = skip_spaces(&input[next_nonalpha..]);
        let (ty, argc) = if let Some(&s) = self.maps.int_funcs.get(&input[..next_nonalpha]) {
            s
        } else {
            return Err(Error::Msg(input, "Invalid name"));
        };
        let (args, rest) = self.parse_args(argc, rest)?;
        let func = IntFunc {
            ty,
            args: args.into_boxed_slice(),
        };
        Ok((func, rest))
    }

    fn bool_func<'a>(&mut self, input: &'a [u8]) -> Result<(BoolFunc<C>, &'a [u8]), Error<'a, C>> {
        let next_nonalpha = input.iter().position(|&x| {
            !x.is_ascii_alphanumeric() && x != b'_'
        }).unwrap_or(input.len());
        let rest = skip_spaces(&input[next_nonalpha..]);
        let (ty, argc) = if let Some(&s) = self.maps.bool_funcs.get(&input[..next_nonalpha]) {
            s
        } else {
            return Err(Error::Msg(input, "Invalid name"));
        };
        let (args, rest) = self.parse_args(argc, rest)?;
        let func = BoolFunc {
            ty,
            args: args.into_boxed_slice(),
        };
        Ok((func, rest))
    }

    fn parse_args<'a>(
        &mut self,
        argc: u8,
        input: &'a [u8],
    ) -> Result<(Vec<IntExpr<C>>, &'a [u8]), Error<'a, C>> {
        let mut rest = input;
        let mut args = Vec::with_capacity(argc as usize);
        if rest.get(0).cloned() == Some(b'(') {
            rest = skip_spaces(&rest[1..]);
            if rest.get(0).cloned() != Some(b')') {
                loop {
                    let (expr, new_rest) = self.int_expr(rest)?;
                    args.push(expr);
                    rest = skip_spaces(&new_rest);
                    match rest.get(0).cloned() {
                        Some(b')') => {
                            rest = skip_spaces(&rest[1..]);
                            break;
                        }
                        Some(b',') => {
                            rest = skip_spaces(&rest[1..]);
                        }
                        _ => {
                            return Err(Error::Msg(rest, "Invalid argument"));
                        }
                    }
                }
            } else {
                rest = skip_spaces(&rest[1..]);
            }
        }
        if args.len() != argc as usize {
            return Err(Error::Msg(rest, "Wrong amount of arguments"));
        }
        Ok((args, rest))
    }

    pub fn int_expr<'a>(
        &mut self,
        input: &'a [u8],
    ) -> Result<(IntExpr<C>, &'a [u8]), Error<'a, C>> {
        #[derive(Copy, Clone)]
        enum Operator {
            Add,
            Sub,
            Mul,
            Div,
            Mod,
            OpenBrace,
        }

        #[inline(always)]
        fn apply_op<C: CustomState>(
            op: Operator,
            val: Box<(IntExpr<C>, IntExpr<C>)>,
        ) -> IntExpr<C> {
            match op {
                Operator::Add => IntExpr::Add(val),
                Operator::Sub => IntExpr::Sub(val),
                Operator::Mul => IntExpr::Mul(val),
                Operator::Div => IntExpr::Div(val),
                Operator::Mod | _ => IntExpr::Modulo(val),
            }
        }

        let mut out_stack = Vec::new();
        let mut op_stack = Vec::new();
        let mut input = input;
        let mut error = Error::Eof;
        'outer_loop: loop {
            let &first_byte = match input.get(0) {
                Some(s) => s,
                None => break,
            };
            let rest = match first_byte {
                b'(' => {
                    op_stack.push(Operator::OpenBrace);
                    input = skip_spaces(&input[1..]);
                    continue;
                }
                b'0' ..= b'9' | b'-' => {
                    let (int, rest) = parse_i32(input)?;
                    out_stack.push(IntExpr::Integer(int));
                    rest
                }
                _ => {
                    if let Some((result, rest)) = self.custom_state.parse_int(input) {
                        out_stack.push(IntExpr::Custom(result));
                        rest
                    } else {
                        let (func, rest) = match self.int_func(input) {
                            Ok(o) => o,
                            Err(e) => {
                                error = e;
                                break;
                            }
                        };
                        out_stack.push(IntExpr::Func(func));
                        rest
                    }
                }
            };
            input = rest;
            'op_loop: loop {
                let rest = skip_spaces(input);
                let &operator = match rest.get(0) {
                    Some(s) => s,
                    None => {
                        input = rest;
                        break 'outer_loop;
                    }
                };
                let op = match operator {
                    b'+' => Operator::Add,
                    b'-' => Operator::Sub,
                    b'*' => Operator::Mul,
                    b'/' => Operator::Div,
                    b'%' => Operator::Mod,
                    b')' => {
                        while let Some(op) = op_stack.pop() {
                            if let Operator::OpenBrace = op {
                                input = &rest[1..];
                                continue 'op_loop;
                            }
                            let right = out_stack.pop().unwrap();
                            let left = out_stack.pop().unwrap();
                            let val = Box::new((left, right));
                            out_stack.push(apply_op(op, val));
                        }
                        input = rest;
                        break 'outer_loop;
                    }
                    _ => {
                        input = rest;
                        break 'outer_loop;
                    }
                };
                loop {
                    match op_stack.last().cloned() {
                        Some(Operator::OpenBrace) | None => break,
                        Some(Operator::Add) | Some(Operator::Sub) => match op {
                            Operator::Mul | Operator::Div | Operator::Mod => break,
                            _ => (),
                        },
                        _ => (),
                    }
                    let right = out_stack.pop().unwrap();
                    let left = out_stack.pop().unwrap();
                    let val = Box::new((left, right));
                    out_stack.push(apply_op(op_stack.pop().unwrap(), val));
                }
                op_stack.push(op);
                input = skip_spaces(&rest[1..]);
                break;
            }
        }
        while let Some(op) = op_stack.pop() {
            if let Operator::OpenBrace = op {
                return Err(Error::Msg(input, "Unclosed '('"));
            }
            if out_stack.len() < 2 {
                if matches!(error, Error::Eof) {
                    return Err(Error::Msg(input, "Missing right operand"));
                } else {
                    return Err(error);
                }
            }
            let right = out_stack.pop().unwrap();
            let left = out_stack.pop().unwrap();
            let val = Box::new((left, right));
            out_stack.push(apply_op(op, val));
        }
        if out_stack.is_empty() {
            Err(error)
        } else {
            Ok((out_stack.remove(0), input))
        }
    }

    pub fn bool_expr<'a>(
        &mut self,
        input: &'a [u8],
    ) -> Result<(BoolExpr<C>, &'a [u8]), Error<'a, C>> {
        #[derive(Copy, Clone, Eq, PartialEq)]
        enum Operator {
            And,
            Or,
            Eq,
            Neq,
            LessThan,
            LessOrEq,
            GreaterThan,
            GreaterOrEq,
            Not,
            OpenBrace,
        }

        enum Expr<C: CustomState> {
            Bool(BoolExpr<C>),
            Int(IntExpr<C>),
        }

        fn apply_op<C: CustomState>(
            stack: &mut Vec<Expr<C>>,
            op: Operator,
        ) -> Result<BoolExpr<C>, Error<'static, C>> {
            let right = match stack.pop() {
                Some(s) => s,
                None => return Err(Error::Eof),
            };
            let left = stack.pop();
            match op {
                Operator::And | Operator::Or => {
                    if left.is_none() {
                        return Err(Error::Eof);
                    }
                    let (left, right) = match (left.unwrap(), right) {
                        (Expr::Bool(l), Expr::Bool(r)) => (l, r),
                        (Expr::Int(e), _) | (_, Expr::Int(e)) => {
                            return Err(Error::NotBoolean(e));
                        }
                    };
                    let val = Box::new((left, right));
                    if let Operator::And = op {
                        Ok(BoolExpr::And(val))
                    } else {
                        Ok(BoolExpr::Or(val))
                    }
                }
                Operator::Eq => {
                    if left.is_none() {
                        return Err(Error::Eof);
                    }
                    match (left.unwrap(), right) {
                        (Expr::Bool(l), Expr::Bool(r)) => {
                            Ok(BoolExpr::EqualBool(Box::new((l, r))))
                        }
                        (Expr::Int(l), Expr::Int(r)) => {
                            Ok(BoolExpr::EqualInt(Box::new((l, r))))
                        }
                        (Expr::Int(e), _) | (_, Expr::Int(e)) => {
                            Err(Error::NotBoolean(e))
                        }
                    }
                }
                Operator::Neq => {
                    if left.is_none() {
                        return Err(Error::Eof);
                    }
                    match (left.unwrap(), right) {
                        (Expr::Bool(l), Expr::Bool(r)) => {
                            Ok(BoolExpr::Not(Box::new(BoolExpr::EqualBool(Box::new((l, r))))))
                        }
                        (Expr::Int(l), Expr::Int(r)) => {
                            Ok(BoolExpr::Not(Box::new(BoolExpr::EqualInt(Box::new((l, r))))))
                        }
                        (Expr::Int(e), _) | (_, Expr::Int(e)) => {
                            Err(Error::NotBoolean(e))
                        }
                    }
                }
                Operator::GreaterThan | Operator::GreaterOrEq | Operator::LessThan |
                    Operator::LessOrEq =>
                {
                    if left.is_none() {
                        return Err(Error::Eof);
                    }
                    let (left, right) = match (left.unwrap(), right) {
                        (Expr::Int(l), Expr::Int(r)) => (l, r),
                        (Expr::Bool(e), _) | (_, Expr::Bool(e)) => {
                            return Err(Error::NotInteger(e));
                        }
                    };
                    let val = Box::new((left, right));
                    match op {
                        Operator::GreaterThan => Ok(BoolExpr::GreaterThan(val)),
                        Operator::GreaterOrEq => Ok(BoolExpr::GreaterOrEqual(val)),
                        Operator::LessThan => Ok(BoolExpr::LessThan(val)),
                        Operator::LessOrEq | _ => Ok(BoolExpr::LessOrEqual(val)),
                    }
                }
                Operator::Not | _ => {
                    if let Some(left) = left {
                        stack.push(left);
                    }
                    match right {
                        Expr::Bool(r) => Ok(BoolExpr::Not(Box::new(r))),
                        Expr::Int(e) => Err(Error::NotBoolean(e)),
                    }
                }
            }
        }

        let mut out_stack = Vec::new();
        let mut op_stack = Vec::new();
        let mut input = input;
        let mut error = Error::Eof;
        'outer_loop: loop {
            let &first_byte = match input.get(0) {
                Some(s) => s,
                None => break,
            };
            let rest = match first_byte {
                b'(' => {
                    op_stack.push(Operator::OpenBrace);
                    input = skip_spaces(&input[1..]);
                    continue;
                }
                b'!' => {
                    op_stack.push(Operator::Not);
                    input = &input[1..];
                    continue;
                }
                _ => {
                    if let Some((result, rest)) = self.custom_state.parse_bool(input) {
                        out_stack.push(Expr::Bool(BoolExpr::Custom(result)));
                        rest
                    } else {
                        let (func, rest) = match self.bool_func(input) {
                            Ok(o) => (Expr::Bool(BoolExpr::Func(o.0)), o.1),
                            Err(_) => match self.int_expr(input) {
                                Ok(o) => (Expr::Int(o.0), o.1),
                                Err(e) => {
                                    error = e;
                                    break;
                                }
                            },
                        };
                        out_stack.push(func);
                        rest
                    }
                }
            };
            input = rest;
            'op_loop: loop {
                input = skip_spaces(input);
                let &operator = match input.get(0) {
                    Some(s) => s,
                    None => break,
                };
                let rest = &input[1..];
                let op = match operator {
                    b'|' => if rest.get(0).cloned() == Some(b'|') {
                        input = &rest[1..];
                        Operator::Or
                    } else {
                        break 'outer_loop;
                    },
                    b'&' => if rest.get(0).cloned() == Some(b'&') {
                        input = &rest[1..];
                        Operator::And
                    } else {
                        break 'outer_loop;
                    },
                    b'<' => if rest.get(0).cloned() == Some(b'=') {
                        input = &rest[1..];
                        Operator::LessOrEq
                    } else {
                        input = rest;
                        Operator::LessThan
                    },
                    b'>' => if rest.get(0).cloned() == Some(b'=') {
                        input = &rest[1..];
                        Operator::GreaterOrEq
                    } else {
                        input = rest;
                        Operator::GreaterThan
                    },
                    b'=' => if rest.get(0).cloned() == Some(b'=') {
                        input = &rest[1..];
                        Operator::Eq
                    } else {
                        break 'outer_loop;
                    },
                    b'!' => if rest.get(0).cloned() == Some(b'=') {
                        input = &rest[1..];
                        Operator::Neq
                    } else {
                        break 'outer_loop;
                    },
                    b')' => {
                        while let Some(op) = op_stack.pop() {
                            if let Operator::OpenBrace = op {
                                input = rest;
                                continue 'op_loop;
                            }
                            let val = apply_op(&mut out_stack, op)?;
                            out_stack.push(Expr::Bool(val));
                        }
                        break 'outer_loop;
                    }
                    _ => {
                        break 'outer_loop;
                    }
                };
                loop {
                    let last = match op_stack.last() {
                        Some(&s) => s,
                        None => break,
                    };
                    match last {
                        Operator::OpenBrace => break,
                        Operator::Or | Operator::And => match op {
                            Operator::Eq | Operator::Neq | Operator::LessThan | Operator::LessOrEq |
                                Operator::GreaterThan | Operator::GreaterOrEq => break,
                            _ => (),
                        }
                        _ => (),
                    };
                    if last != op {
                        let conflict = match last {
                            Operator::Not => false,
                            Operator::Eq | Operator::Neq | Operator::LessThan | Operator::LessOrEq |
                                Operator::GreaterThan | Operator::GreaterOrEq =>
                            {
                                match op {
                                    Operator::Or | Operator::And => false,
                                    _ => true,
                                }
                            }
                            _ => true,
                        };
                        if conflict {
                            return Err(Error::Msg(rest, "Conflicting operators"));
                        }
                    }
                    op_stack.pop();
                    let val = apply_op(&mut out_stack, last)?;
                    out_stack.push(Expr::Bool(val));
                }
                op_stack.push(op);
                break;
            }
            input = skip_spaces(input);
        }
        while let Some(op) = op_stack.pop() {
            if let Operator::OpenBrace = op {
                return Err(Error::Msg(input, "Unclosed '('"));
            }
            let val = match apply_op(&mut out_stack, op) {
                Ok(o) => o,
                Err(e) => {
                    return if matches!(error, Error::Eof) {
                        Err(match e {
                            Error::Eof => Error::Msg(input, "Missing right operand"),
                            x => x,
                        })
                    } else {
                        Err(error)
                    }
                }
            };
            out_stack.push(Expr::Bool(val));
        }
        if out_stack.is_empty() {
            Err(error)
        } else {
            match out_stack.remove(0) {
                Expr::Bool(x) => Ok((x, input)),
                Expr::Int(x) => Err(Error::NotBoolean(x)),
            }
        }
    }
}

fn parse_i32<C: CustomState>(input: &[u8]) -> Result<(i32, &[u8]), Error<C>> {
    let (input, neg) = match input.get(0) {
        Some(b'-') => (&input[1..], true),
        Some(_) => (input, false),
        None => return Err(Error::Eof),
    };
    let (input, base) = match input.get(..2) {
        Some(b"0x") => (&input[2..], 16),
        Some(_) => (input, 10),
        None => (input, 10),
    };
    let end = input.iter().position(|&x| !x.is_ascii_alphanumeric()).unwrap_or(input.len());
    let text = unsafe { std::str::from_utf8_unchecked(&input[..end]) };
    let val = i32::from_str_radix(text, base)
        .map_err(|_| Error::Msg(input, "Invalid integer literal"))?;
    if neg {
        Ok((0 - val, &input[end..]))
    } else {
        Ok((val, &input[end..]))
    }
}

fn skip_spaces(input: &[u8]) -> &[u8] {
    match input.iter().position(|&x| x != b' ' && x != b'\t') {
        Some(s) => &input[s..],
        None => &[],
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum BoolExpr<C: CustomState> {
    And(Box<(BoolExpr<C>, BoolExpr<C>)>),
    Or(Box<(BoolExpr<C>, BoolExpr<C>)>),
    LessThan(Box<(IntExpr<C>, IntExpr<C>)>),
    LessOrEqual(Box<(IntExpr<C>, IntExpr<C>)>),
    GreaterThan(Box<(IntExpr<C>, IntExpr<C>)>),
    GreaterOrEqual(Box<(IntExpr<C>, IntExpr<C>)>),
    EqualInt(Box<(IntExpr<C>, IntExpr<C>)>),
    EqualBool(Box<(BoolExpr<C>, BoolExpr<C>)>),
    Not(Box<BoolExpr<C>>),
    Func(BoolFunc<C>),
    Custom(C::BoolExt),
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;

    type Error<'a> = super::Error<'a, NoCustom>;

    fn assert_error_contains<T: Debug>(result: Result<T, Error<'_>>, expected: &str) {
        match result.unwrap_err() {
            super::Error::Msg(ctx, msg) => {
                if !msg.contains(expected) {
                    panic!(
                        "Expected '{}' in '{}' (Ctx \"{}\")",
                        expected, msg, String::from_utf8_lossy(ctx),
                    );
                }
            }
            _ => panic!("Expected {}", expected),
        }
    }

    fn empty_unwrap<'a, T>(result: Result<(T, &'a [u8]), Error<'a>>) -> T {
        let result = match result {
            Ok(o) => o,
            Err(e) => match e {
                super::Error::Msg(ctx, msg) => {
                    panic!("Unwrap fail: {}, context \"{}\"", msg, String::from_utf8_lossy(ctx));
                }
                e => panic!("Unwrap fail: {:?}", e),
            }
        };
        if result.1.len() != 0 {
            panic!("Trailing bytes: \"{}\"", String::from_utf8_lossy(result.1));
        }
        result.0
    }

    fn noarg(ty: IntFuncType) -> IntFunc<NoCustom> {
        IntFunc {
            ty,
            args: Box::new([]),
        }
    }

    fn noargb(ty: BoolFuncType) -> BoolFunc<NoCustom> {
        BoolFunc {
            ty,
            args: Box::new([]),
        }
    }

    #[test]
    fn test_int_func() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.int_func(text)
        };
        assert_eq!(empty_unwrap(parse(b"hitpoints")), noarg(IntFuncType::Hitpoints));
        assert_eq!(empty_unwrap(parse(b"hitpoints()")), noarg(IntFuncType::Hitpoints));
        assert_eq!(
            empty_unwrap(parse(b"acid_spore_count")),
            noarg(IntFuncType::AcidSporeCount),
        );
        assert_error_contains(parse(b"hitpoints(1)"), "Wrong amount");
        assert_error_contains(parse(b"unknown"), "Invalid");
    }

    #[test]
    fn test_int_expr() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.int_expr(text)
        };
        assert_eq!(empty_unwrap(parse(b"20")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"0x20")), IntExpr::Integer(0x20));
        assert_eq!(empty_unwrap(parse(b"-20")), IntExpr::Integer(-20));
        assert_eq!(empty_unwrap(parse(b"-0x20")), IntExpr::Integer(-0x20));
        assert_eq!(empty_unwrap(parse(b"(20)")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"((20))")), IntExpr::Integer(20));
        assert_eq!(empty_unwrap(parse(b"energy")), IntExpr::Func(noarg(IntFuncType::Energy)));
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
        assert_error_contains(parse(b"4294967295"), "Invalid integer literal");
        assert!(parse(b"54k").is_err());
    }

    #[test]
    fn test_bool_expr() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.bool_expr(text)
        };
        let fun_true = || BoolExpr::Func(noargb(BoolFuncType::True));
        let fun_false = || BoolExpr::Func(noargb(BoolFuncType::False));
        assert_eq!(empty_unwrap(parse(b"true")), fun_true());
        assert_eq!(empty_unwrap(parse(b"false")), fun_false());
        assert_eq!(empty_unwrap(parse(b"((true))")), fun_true());
        assert_eq!(empty_unwrap(parse(b"blind")), BoolExpr::Func(noargb(BoolFuncType::Blind)));
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

        assert_error_contains(parse(b"true || false && true"), "Conflicting operators");
        parse(b"! true").unwrap_err();

        assert_eq!(
            empty_unwrap(parse(b"energy >= 50 && energy < 100")),
            BoolExpr::And(Box::new((
                BoolExpr::GreaterOrEqual(Box::new((
                    IntExpr::Func(noarg(IntFuncType::Energy)),
                    IntExpr::Integer(50),
                ))),
                BoolExpr::LessThan(Box::new((
                    IntExpr::Func(noarg(IntFuncType::Energy)),
                    IntExpr::Integer(100),
                ))),
            )))
        );
    }

    #[test]
    fn test_trig() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.int_func(text)
        };
        assert_eq!(
            empty_unwrap(parse(b"sin(5)")),
            IntFunc {
                ty: IntFuncType::Sin,
                args: Box::new([IntExpr::Integer(5)]),
            },
        );
        assert_eq!(
            empty_unwrap(parse(b"cos(frame_count)")),
            IntFunc {
                ty: IntFuncType::Cos,
                args: Box::new([IntExpr::Func(noarg(IntFuncType::FrameCount))]),
            },
        );
        parse(b"sin()").unwrap_err();
        parse(b"sin(5, 6)").unwrap_err();
        parse(b"sin(5").unwrap_err();
    }

    #[test]
    fn test_complex_int_expr() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.int_expr(text)
        };
        assert_eq!(
            empty_unwrap(parse(b"(sin(hitpoints)) / 2")),
            IntExpr::Div(Box::new((
                IntExpr::Func(IntFunc {
                    ty: IntFuncType::Sin,
                    args: Box::new([IntExpr::Func(noarg(IntFuncType::Hitpoints))]),
                }),
                IntExpr::Integer(2),
            )))
        );
    }

    #[test]
    fn test_2arg_fn() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.int_func(text)
        };
        assert_eq!(
            empty_unwrap(parse(b"deaths(player, 5)")),
            IntFunc {
                ty: IntFuncType::Deaths,
                args: Box::new([
                    IntExpr::Func(noarg(IntFuncType::Player)),
                    IntExpr::Integer(5),
                ]),
            },
        );
        assert_eq!(
            empty_unwrap(parse(b"deaths( player, 5  )")),
            IntFunc {
                ty: IntFuncType::Deaths,
                args: Box::new([
                    IntExpr::Func(noarg(IntFuncType::Player)),
                    IntExpr::Integer(5),
                ]),
            },
        );
    }

    #[test]
    fn bool_expr_rhs_error() {
        let default = &mut DefaultParser;
        let mut parser = Parser::new(default);
        let mut parse = |text| {
            parser.bool_expr(text)
        };
        assert_error_contains(parse(b"1 == 2 && asd > 90"), "Invalid name");
    }
}
