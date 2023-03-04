use crate::ast::*;

use pest::{
    error::{Error as ParseError, ErrorVariant},
    iterators::Pair,
    pratt_parser::PrattParser,
    Parser,
};
type ParseResult<T> = Result<T, Box<ParseError<Rule>>>;
/// cbindgen:ignore
// Bug: https://github.com/eqrion/cbindgen/issues/286

trait IntoParseResult<T> {
    fn into_parse_result(self, pair: Pair<Rule>) -> ParseResult<T>;
}

impl<T, E> IntoParseResult<T> for Result<T, E>
where
    E: ToString,
{
    fn into_parse_result(self, pair: Pair<Rule>) -> ParseResult<T> {
        self.map_err(|e| {
            let span = pair.as_span();

            let err_var = ErrorVariant::CustomError {
                message: e.to_string(),
            };

            Box::new(ParseError::new_from_span(err_var, span))
        })
    }
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        // Precedence is defined lowest to highest
        PrattParser::new()
        .op(Op::prefix(Rule::prefix))
        .op(Op::infix(Rule::infix, Left))
        .op(Op::postfix(Rule::postfix))
        // .op(Op::infix(Rule::logic_or, Left))
        // .op(Op::infix(Rule::logic_and, Left))
        // .op(Op::prefix(Rule::not))
        // .op(Op::infix(Rule::cmp_lt, Left)
        //     | Op::infix(Rule::cmp_gt, Left)
        //     | Op::infix(Rule::cmp_le, Left)
        //     | Op::infix(Rule::cmp_ge, Left)
        //     | Op::infix(Rule::cmp_eq, Left)
        //     | Op::infix(Rule::cmp_ne, Left))
        // .op(Op::infix(Rule::bitws_or, Left))
        // .op(Op::infix(Rule::bitws_xor, Left))
        // .op(Op::infix(Rule::bitws_and, Left))
        // .op(Op::infix(Rule::bitws_shl, Left) | Op::infix(Rule::bitws_shr, Left))
        // .op(Op::infix(Rule::arith_add, Left) | Op::infix(Rule::arith_sub, Left))
        // .op(Op::infix(Rule::arith_mul, Left)
        //     | Op::infix(Rule::arith_div, Left)
        //     | Op::infix(Rule::arith_mod, Left))
        // .op(Op::prefix(Rule::neg))
        // .op(Op::postfix(Rule::call_access))
        // .op(Op::postfix(Rule::array_access))
        // .op(Op::postfix(Rule::dot_access))
    };
}

#[derive(Parser, Default)]
#[grammar = "llpoly.pest"]
pub struct PolygonParser {}

impl PolygonParser {
    pub fn parse_src(&self, input: &str) -> ParseResult<TransUnit> {
        // trans_unit = { SOI ~ definition* ~ EOI }
        let trans_unit_pairs = PolygonParser::parse(Rule::trans_unit, input)?;
        let mut trans_unit = TransUnit { funcs: Vec::new() };
        for d in trans_unit_pairs {
            let definition = parse_definition(d.into_inner().next().unwrap()).unwrap();
            match definition {
                Definition::Function(f) => trans_unit.funcs.push(f),
            }
        }
        Ok(trans_unit)
    }
}

enum Definition {
    Function(Function),
}

// definition = { function }
fn parse_definition(pair: Pair<Rule>) -> ParseResult<Definition> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::function => Ok(Definition::Function(parse_function(rhs)?)),
        _ => unreachable!(),
    }
}
// function = { TOK_DEFINE ~ func_name ~ TOK_FUNCTION ~ TOK_LPAREN
//     ~ func_params ~ TOK_RPAREN
//     ~ ret_ty
//     ~ (block | short_block) }
fn parse_function(pair: Pair<Rule>) -> ParseResult<Function> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str();
    let params = parse_func_params(inner.next().unwrap())?;
    let ret_ty = parse_ret_ty(inner.next().unwrap())?;
    let block_pair = inner.next().unwrap();
    let block = match block_pair.as_rule() {
        Rule::block => parse_block(block_pair)?,
        Rule::short_block => parse_short_block(block_pair)?,
        _ => unreachable!(),
    };
    Ok(Function {
        name: name.to_string(),
        meta: Box::<NodeMeta>::default(),
        params,
        ret_ty,
        body: block,
    })
}
// short_block = { "=>" ~ expr }
// will be parsed as a block with a single return statement
fn parse_short_block(pair: Pair<Rule>) -> ParseResult<Block> {
    let mut inner = pair.into_inner();
    let expr = parse_expr(inner.next().unwrap())?;
    let stmt = Stmt::Return(ReturnStmt {
        meta: Box::<NodeMeta>::default(),
        expr: Some(expr),
    });
    Ok(Block { stmts: vec![stmt] })
}

// func_params = { typed_ids? }
fn parse_func_params(pair: Pair<Rule>) -> ParseResult<Vec<Param>> {
    let inner = pair.into_inner();
    let mut params = Vec::new();
    for p in inner {
        params.push(parse_typed_id(p)?);
    }
    Ok(params)
}

// ret_ty = { ("->" ~ ty)? }
fn parse_ret_ty(pair: Pair<Rule>) -> ParseResult<Ty> {
    let mut inner = pair.into_inner();
    // _debug_rule(&inner.peek().unwrap());
    match inner.next() {
        Some(p) => parse_ty(p),
        None => Ok(Ty::int(64)), // todo: detect return type from usage
    }
}

// Only for debugging. Print the rule name of the pair.
fn _debug_rule(pair: &Pair<Rule>) {
    println!("Rule: {:?}, source: {}", pair.as_rule(), pair.as_str());
}

// typed_id = { ID ~ ":" ~ ty }
fn parse_typed_id(pair: Pair<Rule>) -> ParseResult<Param> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str();

    let ty = parse_ty(inner.next().unwrap())?;
    Ok(Param {
        name: name.to_string(),
        meta: Box::<NodeMeta>::default(),
        ty,
    })
}

// ty = _{ basic_ty | user_ty | array_ty | pointer_ty | generic_ty | void_ty }
fn parse_ty(rhs: Pair<Rule>) -> ParseResult<Ty> {
    // _debug_rule(&rhs);
    match rhs.as_rule() {
        Rule::basic_ty => Ok(Ty::Basic(parse_basic_ty(rhs)?)),
        Rule::array_ty => Ok(Ty::Array(parse_array_ty(rhs)?)),
        Rule::pointer_ty => Ok(Ty::Pointer(parse_pointer_ty(rhs)?)),
        Rule::user_ty => Ok(Ty::User(parse_user_ty(rhs)?)),
        _ => {
            println!(
                "Rule: {:?} is not enabled for llpoly, source: {}",
                rhs.as_rule(),
                rhs.as_str()
            );
            unreachable!()
        }
    }
}

// basic_ty = { int_ty | float_ty | char_ty | str_ty | bool_ty }
fn parse_basic_ty(pair: Pair<Rule>) -> ParseResult<BasicTy> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::int_ty => Ok(BasicTy::Int(parse_int_ty(rhs)?)),
        Rule::float_ty => Ok(BasicTy::Float(parse_float_ty(rhs)?)),
        Rule::char_ty => Ok(BasicTy::Char()),
        Rule::str_ty => Ok(BasicTy::Str()),
        Rule::bool_ty => Ok(BasicTy::Bool()),
        _ => unreachable!(),
    }
}

// int_ty = { "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
// | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" }
fn parse_int_ty(pair: Pair<Rule>) -> ParseResult<IntTy> {
    match pair.as_str() {
        "i8" => Ok(IntTy::I8()),
        "i16" => Ok(IntTy::I16()),
        "i32" => Ok(IntTy::I32()),
        "i64" => Ok(IntTy::I64()),
        "i128" => Ok(IntTy::I128()),
        "isize" => Ok(IntTy::ISize()),
        "u8" => Ok(IntTy::U8()),
        "u16" => Ok(IntTy::U16()),
        "u32" => Ok(IntTy::U32()),
        "u64" => Ok(IntTy::U64()),
        "u128" => Ok(IntTy::U128()),
        "usize" => Ok(IntTy::USize()),
        _ => unreachable!(),
    }
}

// float_ty = { "f32" | "f64" }
fn parse_float_ty(pair: Pair<Rule>) -> ParseResult<FloatTy> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_str() {
        "f32" => Ok(FloatTy::F32()),
        "f64" => Ok(FloatTy::F64()),
        _ => unreachable!(),
    }
}

// user_ty = { ID }
fn parse_user_ty(pair: Pair<Rule>) -> ParseResult<UserTy> {
    let rhs = pair.into_inner().next().unwrap();
    Ok(UserTy {
        name: rhs.as_str().to_string(),
    })
}

// array_ty = { "[" ~ array_size? ~ ty ~ "]" }
fn parse_array_ty(pair: Pair<Rule>) -> ParseResult<ArrayTy> {
    let mut inner = pair.into_inner();

    let len = match inner.next() {
        Some(p) => Some(parse_array_size(p)?),
        None => None,
    };
    let ty = parse_ty(inner.next().unwrap())?;
    Ok(ArrayTy {
        ty: Box::new(ty),
        _size_expr: len,
        size: 0, // Will be evaluated later
    })
}

// array_size = { expr ~ "times"}
fn parse_array_size(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    let mut inner = pair.into_inner();
    let expr = parse_expr(inner.next().unwrap())?;

    Ok(expr)
}
// pointer_ty = { "PtrTo" ~ "<" ~ ty ~ ">" }
fn parse_pointer_ty(pair: Pair<Rule>) -> ParseResult<PointerTy> {
    let mut inner = pair.into_inner();

    let ty = parse_ty(inner.next().unwrap())?;
    Ok(PointerTy { ty: Box::new(ty) })
}

// void_ty = { "void" }
// fn parse_void_ty(pair: Pair<Rule>) -> ParseResult<VoidTy> {
//     Ok(VoidTy {})
// }

/* #endregion */

// block = { "{" ~ statement* ~ "}" }
fn parse_block(pair: Pair<Rule>) -> ParseResult<Block> {
    let inner = pair.into_inner();

    let mut statements = Vec::new();
    for p in inner {
        statements.push(parse_statement(p)?);
    }
    Ok(Block { stmts: statements })
}

// statement = { NEWLINE* ~  (
//     expr_stmt
//     | return_stmt
//  )* ~ NEWLINE }
fn parse_statement(pair: Pair<Rule>) -> ParseResult<Stmt> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::expr_stmt => Ok(Stmt::Expr(parse_expr_stmt(rhs)?)),
        Rule::return_stmt => Ok(Stmt::Return(parse_return_stmt(rhs)?)),
        _ => {
            _debug_rule(&rhs);
            todo!()
        }
    }
}

// expr_stmt = { expr }
fn parse_expr_stmt(pair: Pair<Rule>) -> ParseResult<ExprStmt> {
    let rhs = pair.into_inner().next().unwrap();
    Ok(ExprStmt {
        meta: Box::<NodeMeta>::default(),

        expr: parse_expr(rhs)?,
    })
}

// return_stmt = { "return" ~ expr? }
fn parse_return_stmt(pair: Pair<Rule>) -> ParseResult<ReturnStmt> {
    let mut inner = pair.into_inner();
    let expr = match inner.next() {
        Some(p) => Some(parse_expr(p)?),
        None => None,
    };
    Ok(ReturnStmt {
        meta: Box::<NodeMeta>::default(),

        expr,
    })
}

// primary_expr = _{ call_expr | (TOK_LPAREN ~ expr ~ TOK_RPAREN) | ID | literal }
fn parse_primary_expr(pair: Pair<Rule>) -> ParseResult<PrimaryExpr> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::call_expr => Ok(PrimaryExpr::Call(parse_call_expr(rhs)?)),
        Rule::expr => Ok(PrimaryExpr::Grouping(parse_expr(rhs)?)),
        Rule::ID => Ok(PrimaryExpr::Ident(IdentExpr {
            meta: Box::<NodeMeta>::default(),

            name: rhs.as_str().to_string(),
        })),
        Rule::literal => Ok(PrimaryExpr::Literal(parse_literal(rhs)?)),
        _ => unreachable!(),
    }
}

// call_expr = { ID ~ TOK_LPAREN ~ call_args? ~ TOK_RPAREN }
fn parse_call_expr(pair: Pair<Rule>) -> ParseResult<CallExpr> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str();

    let args = match inner.next() {
        Some(p) => parse_call_args(p)?,
        None => Vec::new(),
    };
    Ok(CallExpr {
        meta: Box::<NodeMeta>::default(),

        callee: name.to_string(),
        args,
    })
}
// call_args = { expr ~ ("," ~ expr)* }
fn parse_call_args(pair: Pair<Rule>) -> ParseResult<Vec<Expr>> {
    let mut args = Vec::new();
    for p in pair.into_inner() {
        args.push(*parse_expr(p)?);
    }
    Ok(args)
}

// literal = { lit_integer | lit_float | lit_str | lit_bool }
fn parse_literal(pair: Pair<Rule>) -> ParseResult<LiteralExpr> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::lit_integer => Ok(LiteralExpr {
            meta: Box::<NodeMeta>::default(),

            value: Literal::Int(parse_lit_integer(rhs)?),
        }),
        Rule::lit_float => Ok(LiteralExpr {
            meta: Box::<NodeMeta>::default(),

            value: Literal::Float(parse_lit_float(rhs)?),
        }),
        Rule::lit_str => Ok(LiteralExpr {
            meta: Box::<NodeMeta>::default(),

            value: Literal::String(parse_lit_str(rhs)?),
        }),
        Rule::lit_bool => Ok(LiteralExpr {
            meta: Box::<NodeMeta>::default(),

            value: Literal::Bool(parse_lit_bool(rhs)?),
        }),
        _ => unreachable!(),
    }
}

// lit_integer = { digits }
fn parse_lit_integer(pair: Pair<Rule>) -> ParseResult<i64> {
    let rhs = pair.into_inner().next().unwrap();
    Ok(rhs.as_str().parse().unwrap())
}

// lit_float = { dec_digits+ ~ "." ~ dec_digits+ }
fn parse_lit_float(pair: Pair<Rule>) -> ParseResult<f64> {
    let rhs = pair.into_inner().next().unwrap();
    Ok(rhs.as_str().parse().unwrap())
}

// lit_str = ${ "\"" ~ str_inner ~ "\"" }
// str_inner = _{ (str_esc | str_char)* }
// str_char = _{ !("\"" | "\\") ~ ANY }
fn parse_lit_str(pair: Pair<Rule>) -> ParseResult<String> {
    _debug_rule(&pair);
    let char_pairs = pair.into_inner();

    let mut s = String::new();
    for char_pair in char_pairs {
        let rule = char_pair.as_rule();
        match rule {
            Rule::str_esc => s.push(parse_str_esc(char_pair)),
            _ => s.push(char_pair.as_str().chars().next().unwrap()),
        }
    }
    Ok(s)
}

// str_esc = ${ "\\" ~ str_char }
fn parse_str_esc(pair: Pair<Rule>) -> char {
    let pairs = pair.into_inner();
    match pairs.as_str() {
        "\"" => '"',
        "\\" => '\\',
        "n" => '\n',
        "r" => '\r',
        "t" => '\t',
        _ => unreachable!(),
    }
}

// lit_bool = { TOK_TRUE | TOK_FALSE }
fn parse_lit_bool(pair: Pair<Rule>) -> ParseResult<bool> {
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::TOK_TRUE => Ok(true),
        Rule::TOK_FALSE => Ok(false),
        _ => unreachable!(),
    }
}

// expr = { prefix* ~ primary_expr ~ postfix* ~ (infix ~ prefix* ~ primary_expr ~ postfix* )* }
fn parse_expr(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    let inner = pair.into_inner();
    let expr = PRATT_PARSER
        .map_primary(|x| Expr::Primary(parse_primary_expr(x).unwrap()))
        .map_infix(|lhs, op, rhs| {
            Expr::Infix(InfixExpr {
                meta: Box::<NodeMeta>::default(),
                lhs: Box::new(lhs),
                op: parse_infix(op).unwrap(),
                rhs: Box::new(rhs),
            })
        })
        .map_prefix(|op, rhs| {
            Expr::Prefix(PrefixExpr {
                meta: Box::<NodeMeta>::default(),

                op: parse_prefix(op).unwrap(),
                rhs: Box::new(rhs),
            })
        })
        .map_postfix(|lhs, op| {
            Expr::Postfix(PostfixExpr {
                meta: Box::<NodeMeta>::default(),

                lhs: Box::new(lhs),
                op: parse_postfix(op).unwrap(),
            })
        })
        .parse(inner.into_iter());
    Ok(Box::new(expr))
}

// infix = { infix_bitwise | infix_logic | infix_arith | infix_cmp}
fn parse_infix(op: Pair<Rule>) -> ParseResult<InfixOp> {
    let rhs = op.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::infix_bitwise => {
            let rhs = rhs.into_inner().next().unwrap();
            match rhs.as_rule() {
                Rule::bitws_and => Ok(InfixOp::BitAnd),
                Rule::bitws_or => Ok(InfixOp::BitOr),
                Rule::bitws_xor => Ok(InfixOp::BitXor),
                Rule::bitws_shl => Ok(InfixOp::BitShl),
                Rule::bitws_shr => Ok(InfixOp::BitShr),
                _ => unreachable!(),
            }
        }
        Rule::infix_logic => {
            let rhs = rhs.into_inner().next().unwrap();
            match rhs.as_rule() {
                Rule::logic_and => Ok(InfixOp::LogicAnd),
                Rule::logic_or => Ok(InfixOp::LogicOr),
                _ => unreachable!(),
            }
        }
        Rule::infix_arith => {
            let rhs = rhs.into_inner().next().unwrap();
            match rhs.as_rule() {
                Rule::arith_add => Ok(InfixOp::Add),
                Rule::arith_sub => Ok(InfixOp::Sub),
                Rule::arith_mul => Ok(InfixOp::Mul),
                Rule::arith_div => Ok(InfixOp::Div),
                Rule::arith_mod => Ok(InfixOp::Mod),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}
// prefix = { not | pos | neg }
fn parse_prefix(op: Pair<Rule>) -> ParseResult<PrefixOp> {
    let rhs = op.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::not => Ok(PrefixOp::Not),
        Rule::pos => Ok(PrefixOp::Pos),
        Rule::neg => Ok(PrefixOp::Neg),
        _ => unreachable!(),
    }
}
// postfix = { call_access | array_access | dot_access }
fn parse_postfix(op: Pair<Rule>) -> ParseResult<PostfixOp> {
    let op = op.into_inner().next().unwrap();
    match op.as_rule() {
        Rule::call_access => Ok(PostfixOp::CallAccess(Box::new(
            parse_call_access(op).unwrap(),
        ))),
        Rule::array_access => Ok(PostfixOp::ArrayAccess(Box::new(
            parse_array_access(op).unwrap(),
        ))),
        Rule::dot_access => Ok(PostfixOp::DotAccess(Box::new(
            parse_dot_access(op).unwrap(),
        ))),
        _ => unreachable!(),
    }
}

// call_access = { "." ~ ID ~ TOK_LPAREN ~ call_args? ~ TOK_RPAREN }
fn parse_call_access(pair: Pair<Rule>) -> ParseResult<CallAccess> {
    let mut inner = pair.into_inner();

    let callee = inner.next().unwrap().as_str().to_string();

    let args = match inner.next() {
        Some(p) => parse_call_args(p)?,
        None => Vec::new(),
    };

    Ok(CallAccess {
        callee,
        args,
        meta: Box::<NodeMeta>::default(),
    })
}

// array_access = { "[" ~ expr ~ "]" }
fn parse_array_access(pair: Pair<Rule>) -> ParseResult<ArrayAccess> {
    let mut inner = pair.into_inner();

    let index = parse_expr(inner.next().unwrap())?;

    Ok(ArrayAccess {
        meta: Box::<NodeMeta>::default(),
        index,
    })
}

// dot_access = { "." ~ ID }
fn parse_dot_access(pair: Pair<Rule>) -> ParseResult<DotAccess> {
    let mut inner = pair.into_inner();

    let field = inner.next().unwrap().as_str();
    Ok(DotAccess {
        meta: Box::<NodeMeta>::default(),
        field: field.to_string(),
    })
}
