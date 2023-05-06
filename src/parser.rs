use log::{trace};
use pest::{
    error::{Error as ParseError, ErrorVariant},
    iterators::Pair,
    pratt_parser::PrattParser,
    Parser,
};

use crate::ast::*;
type ParseResult<T> = Result<T, Box<ParseError<Rule>>>;

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

// Only for debugging. Print the rule name of the pair.
pub fn _debug_rule(name: &str, pair: &Pair<Rule>) {
    trace!(
        "parser: {} rule: {:?}, source: {}",
        name,
        pair.as_rule(),
        pair.as_str()
    );
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::*;
        // Precedence is defined lowest to highest
        let pratt = PrattParser::new()
        // Level 12
        .op(Op::infix(Rule::logic_or, Assoc::Left))
        // Level 11
        .op(Op::infix(Rule::logic_and, Assoc::Left))
        // Level 10
        .op(Op::infix(Rule::bit_or, Assoc::Left))
        // Level 9
        .op(Op::infix(Rule::bit_xor, Assoc::Left))
        // Level 8
        .op(Op::infix(Rule::bit_and, Assoc::Left))
        // Level 7
        .op(Op::infix(Rule::cmp_eq, Assoc::Left)
            | Op::infix(Rule::cmp_ne, Assoc::Left))
        // Level 6
        .op(Op::infix(Rule::cmp_lt, Assoc::Left)
            | Op::infix(Rule::cmp_gt, Assoc::Left)
            | Op::infix(Rule::cmp_le, Assoc::Left)
            | Op::infix(Rule::cmp_ge, Assoc::Left))
        // Level 5
        .op(Op::infix(Rule::bit_shl, Assoc::Left)
            | Op::infix(Rule::bit_shr, Assoc::Left))
        // Level 4
        .op(Op::infix(Rule::arith_add, Assoc::Left)
            | Op::infix(Rule::arith_sub, Assoc::Left))
        // Level 3
        .op(Op::infix(Rule::arith_mul, Assoc::Left)
            | Op::infix(Rule::arith_div, Assoc::Left)
            | Op::infix(Rule::arith_mod, Assoc::Left))
        // Level 2
        .op(Op::prefix(Rule::prefix_incr)
            | Op::prefix(Rule::prefix_not)
            | Op::prefix(Rule::prefix_bit_not)
            | Op::prefix(Rule::prefix_pos)
            | Op::prefix(Rule::prefix_neg))
        // Level 1
        .op(Op::postfix(Rule::postfix_incr)
            | Op::postfix(Rule::call_access)
            | Op::postfix(Rule::index_access)
            | Op::postfix(Rule::dot_access));
        pratt
    };
}

#[derive(Parser, Default)]
#[grammar = "sysy.pest"]
pub struct SysYParser {}

pub fn parse(src: &str) -> ParseResult<TransUnit> {
    let mut grammar_pairs = SysYParser::parse(Rule::grammar, src)?;
    let tu = parse_grammar(grammar_pairs.next().unwrap())?;
    Ok(tu)
}

// grammar = { trans_unit ~ EOI }
pub fn parse_grammar(pair: Pair<Rule>) -> ParseResult<TransUnit> {
    _debug_rule("parse_grammar", &pair);
    let tu = pair.into_inner().next().unwrap();
    parse_trans_unit(tu)
}

// trans_unit = { (func_decl | var_decls)* }
pub fn parse_trans_unit(pair: Pair<Rule>) -> ParseResult<TransUnit> {
    _debug_rule("parse_trans_unit", &pair);
    let rhs = pair.into_inner();
    let mut var_decls = Vec::new();
    let mut func_decls = Vec::new();
    for item in rhs {
        match item.as_rule() {
            Rule::func_decl => func_decls.push(parse_func_decl(item)?),
            Rule::var_decls => var_decls.append(&mut parse_var_decls(item)?),
            _ => unreachable!(),
        }
    }
    Ok(TransUnit {
        func_decls,
        var_decls,
    })
}

// var_decls = { const_var_decls | normal_var_decls }
pub fn parse_var_decls(pair: Pair<Rule>) -> ParseResult<Vec<VarDecl>> {
    _debug_rule("parse_var_decls", &pair);
    let rhs = pair.into_inner().next().unwrap();
    match rhs.as_rule() {
        Rule::const_var_decls => parse_const_var_decls(rhs),
        Rule::normal_var_decls => parse_normal_var_decls(rhs),
        _ => unreachable!(),
    }
}

// const_var_decls = { KW_CONST ~ basic_type ~ var_def ~ ("," ~ var_def)* ~ ";" }
pub fn parse_const_var_decls(pair: Pair<Rule>) -> ParseResult<Vec<VarDecl>> {
    _debug_rule("parse_const_var_decls", &pair);
    let mut inner = pair.into_inner().skip(1);
    let type_ = parse_basic_type(inner.next().unwrap())?;
    let mut var_defs = Vec::new();
    for item in inner {
        match item.as_rule() {
            Rule::var_def => var_defs.push(parse_var_def(item, &type_, true)?),
            _ => (),
        }
    }
    Ok(var_defs)
}

// normal_var_decls = { basic_type ~ var_def ~ ("," ~ var_def)* ~ ";" }
pub fn parse_normal_var_decls(pair: Pair<Rule>) -> ParseResult<Vec<VarDecl>> {
    _debug_rule("parse_normal_var_decls", &pair);
    let mut inner = pair.into_inner();
    let type_ = parse_basic_type(inner.next().unwrap())?;
    let mut var_defs = Vec::new();
    for item in inner {
        match item.as_rule() {
            Rule::var_def => var_defs.push(parse_var_def(item, &type_, false)?),
            _ => (),
        }
    }
    Ok(var_defs)
}

// var_def = { ID ~ ("[" ~ const_expr ~ "]")* ~ "=" ~ init_val | ID ~ ("[" ~ const_expr ~ "]")* }
pub fn parse_var_def(pair: Pair<Rule>, type_: &Type, is_const: bool) -> ParseResult<VarDecl> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_owned();
    let mut init = None;

    for item in inner {
        match item.as_rule() {
            Rule::init_val => init = Some(parse_init_val(item)?),
            _ => (),
        }
    }

    Ok(VarDecl {
        name,
        type_: type_.clone(),
        is_const,
        init,
    })
}

// init_val = { expr | array_init_val }
pub fn parse_init_val(pair: Pair<Rule>) -> ParseResult<InitVal> {
    _debug_rule("parse_init_val", &pair);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::expr => {
            let expr = parse_expr(inner)?;
            Ok(InitVal::Expr(expr))
        }
        Rule::array_init_val => {
            let array_init_val = parse_array_init_val(inner)?;
            Ok(InitVal::ArrayInitVal(array_init_val))
        }
        _ => unreachable!(),
    }
}

// array_init_val = { "{" ~ (init_val ~ ("," ~ init_val)*)? ~ "}" }
pub fn parse_array_init_val(pair: Pair<Rule>) -> ParseResult<ArrayInitVal> {
    _debug_rule("parse_array_init_val", &pair);
    let mut inner = pair.into_inner();
    let mut init_vals = Vec::new();

    if let Some(first_init_val_pair) = inner.next() {
        let first_init_val = parse_init_val(first_init_val_pair)?;
        init_vals.push(first_init_val);

        for init_val_pair in inner {
            let init_val = parse_init_val(init_val_pair)?;
            init_vals.push(init_val);
        }
    }

    Ok(ArrayInitVal(init_vals))
}

// func_decl = { func_type ~ ID ~ "(" ~ (func_params)? ~ ")" ~ block }
pub fn parse_func_decl(pair: Pair<Rule>) -> ParseResult<FuncDecl> {
    _debug_rule("parse_func_decl", &pair);
    let mut inner = pair.into_inner();
    let ret_ty = parse_func_type(inner.next().unwrap())?;
    let name = inner.next().unwrap().as_str().to_owned();
    let mut params = Vec::new();
    let mut block = None;

    for item in inner {
        match item.as_rule() {
            Rule::func_params => params = parse_func_params(item)?,
            Rule::block => block = Some(parse_block(item)?),
            _ => (),
        }
    }

    Ok(FuncDecl {
        name,
        params,
        ret_ty,
        body: block.unwrap(),
    })
}

// basic_type = { KW_INT | KW_FLOAT }
pub fn parse_basic_type(pair: Pair<Rule>) -> ParseResult<Type> {
    _debug_rule("parse_basic_type", &pair);
    match pair.as_str() {
        "int" => Ok(Type::Builtin(BuiltinType::Int)),
        "float" => Ok(Type::Builtin(BuiltinType::Float)),
        _ => unreachable!(),
    }
}

// func_type = { KW_VOID | KW_INT | KW_FLOAT }
pub fn parse_func_type(pair: Pair<Rule>) -> ParseResult<Type> {
    _debug_rule("parse_func_type", &pair);
    match pair.as_str() {
        "void" => Ok(Type::Builtin(BuiltinType::Void)),
        "int" => Ok(Type::Builtin(BuiltinType::Int)),
        "float" => Ok(Type::Builtin(BuiltinType::Float)),
        _ => unreachable!(),
    }
}

// func_params = { func_param ~ ("," ~ func_param)* }
pub fn parse_func_params(pair: Pair<Rule>) -> ParseResult<Vec<Param>> {
    _debug_rule("parse_func_params", &pair);
    let mut params = Vec::new();
    for item in pair.into_inner() {
        params.push(parse_func_param(item)?);
    }
    Ok(params)
}

// func_param = { basic_type ~ ID ~ ("[" ~ "]" ~ ("[" ~ const_expr ~ "]")*)? }
pub fn parse_func_param(pair: Pair<Rule>) -> ParseResult<Param> {
    _debug_rule("parse_func_param", &pair);
    let mut inner = pair.into_inner();
    let type_ = parse_basic_type(inner.next().unwrap())?;
    let name = inner.next().unwrap().as_str().to_owned();
    Ok(Param::new(name, type_))
}

// block = { "{" ~ (block_item)* ~ "}" }
pub fn parse_block(pair: Pair<Rule>) -> ParseResult<Block> {
    _debug_rule("parse_block", &pair);
    let mut stmts = Vec::new();
    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::block_item => stmts.push(parse_block_item(item)?),
            _ => (),
        }
    }
    Ok(Block { stmts })
}

// block_item = { var_decls | stmt }
pub fn parse_block_item(pair: Pair<Rule>) -> ParseResult<Box<Stmt>> {
    _debug_rule("parse_block_item", &pair);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::var_decls => {
            let decls = parse_var_decls(inner)?;
            Ok(Box::new(Stmt::VarDecls(VarDecls { decls })))
        }
        Rule::stmt => parse_stmt(inner),
        _ => unreachable!(),
    }
}

// stmt = { ... }
pub fn parse_stmt(pair: Pair<Rule>) -> ParseResult<Box<Stmt>> {
    _debug_rule("parse_stmt", &pair);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::assign_stmt => {
            let assign_stmt = parse_assign_stmt(inner)?;
            Ok(Box::new(Stmt::Assign(assign_stmt)))
        }
        Rule::expr_stmt => {
            let expr_stmt = parse_expr_stmt(inner)?;
            Ok(Box::new(Stmt::Expr(expr_stmt)))
        }
        Rule::block_stmt => {
            let block_stmt = parse_block_stmt(inner)?;
            Ok(Box::new(Stmt::Block(block_stmt)))
        }
        Rule::if_else_stmt => {
            let if_else_stmt = parse_if_else_stmt(inner)?;
            Ok(Box::new(Stmt::IfElse(if_else_stmt)))
        }
        Rule::if_stmt => {
            let if_stmt = parse_if_stmt(inner)?;
            Ok(Box::new(Stmt::If(if_stmt)))
        }
        Rule::while_stmt => {
            let while_stmt = parse_while_stmt(inner)?;
            Ok(Box::new(Stmt::While(while_stmt)))
        }
        Rule::for_stmt => {
            let for_stmt = parse_for_stmt(inner)?;
            Ok(Box::new(Stmt::For(for_stmt)))
        }
        Rule::break_stmt => {
            parse_break_stmt(inner)?;
            Ok(Box::new(Stmt::Break))
        }
        Rule::do_while_stmt => {
            let do_while_stmt = parse_do_while_stmt(inner)?;
            Ok(Box::new(Stmt::DoWhile(do_while_stmt)))
        }
        Rule::continue_stmt => {
            parse_continue_stmt(inner)?;
            Ok(Box::new(Stmt::Continue))
        }
        Rule::return_stmt => {
            let return_stmt = parse_return_stmt(inner)?;
            Ok(Box::new(Stmt::Return(return_stmt)))
        }
        _ => unreachable!(),
    }
}

// lhs = { ID ~ (index_access | dot_access)+ }
pub fn parse_lhs_expr(pair: Pair<Rule>) -> ParseResult<LhsExpr> {
    _debug_rule("parse_lhs_expr", &pair);
    let mut inner = pair.into_inner();
    let id = inner.next().unwrap().as_str().to_string();

    let mut access = Vec::new();

    for access_pair in inner {
        match access_pair.as_rule() {
            Rule::index_access => {
                let expr = parse_index_access(access_pair)?;
                access.push(LhsAccess::Index(expr));
            }
            Rule::dot_access => {
                let dot = parse_dot_access(access_pair)?;
                access.push(LhsAccess::Dot(dot.field));
            }
            _ => unreachable!(),
        }
    }

    Ok(LhsExpr::MixedAccess { id, access })
}

// index_access = { "[" ~ expr ~ "]" }
pub fn parse_index_access(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    _debug_rule("parse_index_access", &pair);
    let expr = parse_expr(pair.into_inner().next().unwrap())?;
    Ok(expr)
}

// assign_stmt = { lhs ~ "=" ~ expr ~ ";" }
pub fn parse_assign_stmt(pair: Pair<Rule>) -> ParseResult<AssignStmt> {
    _debug_rule("parse_assign_stmt", &pair);
    let mut inner = pair.into_inner();
    let lhs = parse_lhs_expr(inner.next().unwrap())?;
    let expr = parse_expr(inner.next().unwrap())?;
    Ok(AssignStmt { lhs, expr })
}

// expr_stmt = { (expr)? ~ ";" }
pub fn parse_expr_stmt(pair: Pair<Rule>) -> ParseResult<ExprStmt> {
    _debug_rule("parse_expr_stmt", &pair);
    let mut inner = pair.into_inner();
    let expr = inner.next().map(|pair| parse_expr(pair)).transpose()?;
    Ok(ExprStmt { expr })
}

// block_stmt = { block }
pub fn parse_block_stmt(pair: Pair<Rule>) -> ParseResult<Block> {
    _debug_rule("parse_block_stmt", &pair);
    let block = parse_block(pair.into_inner().next().unwrap())?;
    Ok(block)
}

// if_stmt = { KW_IF ~ "(" ~ cond ~ ")" ~ stmt }
pub fn parse_if_stmt(pair: Pair<Rule>) -> ParseResult<IfStmt> {
    _debug_rule("parse_if_stmt", &pair);
    let mut inner = pair.into_inner().skip(1);
    let cond = parse_expr(inner.next().unwrap())?;
    let stmt = parse_stmt(inner.next().unwrap())?;
    Ok(IfStmt { cond, stmt })
}

// if_else_stmt = { KW_IF ~ "(" ~ cond ~ ")" ~ stmt ~ KW_ELSE ~ stmt }
pub fn parse_if_else_stmt(pair: Pair<Rule>) -> ParseResult<IfElseStmt> {
    _debug_rule("parse_if_else_stmt", &pair);
    let mut inner = pair.into_inner().skip(1); // skip KW_IF
    let cond = parse_expr(inner.next().unwrap())?;
    let if_stmt = parse_stmt(inner.next().unwrap())?;
    let mut inner = inner.skip(1); // skip KW_ELSE
    let else_stmt = parse_stmt(inner.next().unwrap())?;
    Ok(IfElseStmt {
        cond,
        if_stmt,
        else_stmt,
    })
}

// while_stmt = { KW_WHILE ~ "(" ~ cond ~ ")" ~ stmt }
pub fn parse_while_stmt(pair: Pair<Rule>) -> ParseResult<WhileStmt> {
    _debug_rule("parse_while_stmt", &pair);
    let mut inner = pair.into_inner().skip(1);
    let cond = parse_expr(inner.next().unwrap())?;
    let stmt = parse_stmt(inner.next().unwrap())?;
    Ok(WhileStmt { cond, stmt })
}

// for_stmt = { KW_FOR ~ "(" ~ (expr)? ~ ";" ~ (expr)? ~ ";" ~ (expr)? ~ ")" ~ stmt }
pub fn parse_for_stmt(pair: Pair<Rule>) -> ParseResult<ForStmt> {
    _debug_rule("parse_for_stmt", &pair);
    let mut inner = pair.into_inner().skip(1);
    let init = inner.next().map(|pair| parse_expr(pair)).transpose()?;
    let cond = inner.next().map(|pair| parse_expr(pair)).transpose()?;
    let update = inner.next().map(|pair| parse_expr(pair)).transpose()?;
    let stmt = parse_stmt(inner.next().unwrap())?;
    Ok(ForStmt {
        init,
        cond,
        update,
        stmt,
    })
}

// break_stmt = { KW_BREAK ~ ";" }
pub fn parse_break_stmt(pair: Pair<Rule>) -> ParseResult<()> {
    _debug_rule("parse_break_stmt", &pair);
    pair.into_inner().next(); // consume KW_BREAK
    Ok(())
}

// do_while_stmt = { KW_DO ~ stmt ~ KW_WHILE ~ "(" ~ cond ~ ")" ~ ";" }
pub fn parse_do_while_stmt(pair: Pair<Rule>) -> ParseResult<DoWhileStmt> {
    _debug_rule("parse_do_while_stmt", &pair);
    let mut inner = pair.into_inner().skip(1);
    let stmt = parse_stmt(inner.next().unwrap())?;
    let mut inner = inner.skip(1); // skip KW_WHILE
    let cond = parse_expr(inner.next().unwrap())?;
    Ok(DoWhileStmt { stmt, cond })
}

// continue_stmt = { KW_CONTINUE ~ ";" }
pub fn parse_continue_stmt(pair: Pair<Rule>) -> ParseResult<()> {
    _debug_rule("parse_continue_stmt", &pair);
    pair.into_inner().next(); // consume KW_CONTINUE
    Ok(())
}

// return_stmt = { KW_RETURN ~ (expr)? ~ ";" }
pub fn parse_return_stmt(pair: Pair<Rule>) -> ParseResult<Option<Box<Expr>>> {
    _debug_rule("parse_return_stmt", &pair);
    let inner = pair.into_inner();
    // skip KW_RETURN
    let expr = inner
        .skip(1)
        .next()
        .map(|pair| parse_expr(pair))
        .transpose()?;
    Ok(expr)
}

// expr = { prefix* ~ primary_expr ~ postfix* ~ (infix ~ prefix* ~ primary_expr ~ postfix* )* }
pub fn parse_expr(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    _debug_rule("parse_expr", &pair);
    let inner = pair.into_inner();
    let expr = PRATT_PARSER
        .map_primary(|x| Expr::Primary(parse_primary_expr(x).unwrap()))
        .map_infix(|lhs, op, rhs| {
            Expr::Infix(InfixExpr {
                lhs: Box::new(lhs),
                op: parse_infix_op(op).unwrap(),
                rhs: Box::new(rhs),
            })
        })
        .map_prefix(|op, rhs| {
            Expr::Prefix(PrefixExpr {
                op: parse_prefix_op(op).unwrap(),
                rhs: Box::new(rhs),
            })
        })
        .map_postfix(|lhs, op| {
            Expr::Postfix(PostfixExpr {
                lhs: Box::new(lhs),
                op: parse_postfix_op(op).unwrap(),
            })
        })
        .parse(inner.into_iter());
    Ok(Box::new(expr))
}

// primary_expr = { group_expr | call_expr | lhs | ID | literal_expr }
pub fn parse_primary_expr(pair: Pair<Rule>) -> ParseResult<PrimaryExpr> {
    _debug_rule("parse_primary_expr", &pair);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::group_expr => {
            let group_expr = parse_group_expr(inner)?;
            Ok(PrimaryExpr::Group(group_expr))
        }
        Rule::call_expr => {
            let call_expr = parse_call_expr(inner)?;
            Ok(PrimaryExpr::Call(call_expr))
        }
        Rule::lhs_expr => {
            let lhs = parse_lhs_expr(inner)?;
            Ok(PrimaryExpr::Lhs(lhs))
        }
        Rule::id => {
            let id = parse_id(inner)?;
            Ok(PrimaryExpr::Ident(id))
        }
        Rule::literal_expr => {
            let literal_expr = parse_literal_expr(inner)?;
            Ok(PrimaryExpr::Literal(literal_expr))
        }
        _ => unreachable!(),
    }
}

// group_expr = { "(" ~ expr ~ ")" }
pub fn parse_group_expr(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    _debug_rule("parse_group_expr", &pair);
    let expr = parse_expr(pair.into_inner().next().unwrap())?;
    Ok(expr)
}

// literal_expr = { string | number }
pub fn parse_literal_expr(pair: Pair<Rule>) -> ParseResult<Literal> {
    _debug_rule("parse_literal_expr", &pair);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::string => {
            let string = parse_string(inner)?;
            Ok(string)
        }
        Rule::number => {
            let number = parse_number(inner)?;
            Ok(number)
        }
        _ => unreachable!(),
    }
}

// call_expr = { ID ~ "(" ~ (func_args)? ~ ")" }
pub fn parse_call_expr(pair: Pair<Rule>) -> ParseResult<CallExpr> {
    _debug_rule("parse_call_expr", &pair);
    let mut inner = pair.into_inner();
    let id = inner.next().unwrap().as_str().to_string();
    let args = inner.next().map(|pair| parse_func_args(pair)).transpose()?;
    Ok(CallExpr {
        id,
        args: args.unwrap_or(Vec::new()),
    })
}

// func_args = { func_arg ~ ("," ~ func_arg)* }
pub fn parse_func_args(pair: Pair<Rule>) -> ParseResult<Vec<Box<Expr>>> {
    _debug_rule("parse_func_args", &pair);
    let mut inner = pair.into_inner();
    let mut args = Vec::new();

    let first_func_arg_pair = inner.next().unwrap();
    let first_func_arg = parse_func_arg(first_func_arg_pair)?;
    args.push(first_func_arg);

    for func_arg_pair in inner {
        let func_arg = parse_func_arg(func_arg_pair)?;
        args.push(func_arg);
    }

    Ok(args)
}

// func_arg = { expr }
pub fn parse_func_arg(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    _debug_rule("parse_func_arg", &pair);
    let expr = parse_expr(pair.into_inner().next().unwrap())?;
    Ok(expr)
}

// prefix_op = _{ prefix_incr | prefix_decr | prefix_not | prefix_bit_not | prefix_pos | prefix_neg }
pub fn parse_prefix_op(pair: Pair<Rule>) -> ParseResult<PrefixOp> {
    _debug_rule("parse_prefix_op", &pair);
    let op = match pair.as_str() {
        "++" => PrefixOp::Incr,
        "--" => PrefixOp::Decr,
        "!" => PrefixOp::Not,
        "~" => PrefixOp::BitNot,
        "+" => PrefixOp::Pos,
        "-" => PrefixOp::Neg,
        _ => unreachable!(),
    };
    Ok(op)
}

// infix = { infix_bitwise | infix_logic | infix_arith | infix_cmp}
pub fn parse_infix_op(pair: Pair<Rule>) -> ParseResult<InfixOp> {
    _debug_rule("parse_infix_op", &pair);
    let op = match pair.as_rule() {
        Rule::bit_and => InfixOp::BitAnd,
        Rule::bit_or => InfixOp::BitOr,
        Rule::bit_xor => InfixOp::BitXor,
        Rule::bit_shl => InfixOp::BitShl,
        Rule::bit_shr => InfixOp::BitShr,
        Rule::logic_and => InfixOp::LogicAnd,
        Rule::logic_or => InfixOp::LogicOr,
        Rule::arith_add => InfixOp::Add,
        Rule::arith_sub => InfixOp::Sub,
        Rule::arith_mul => InfixOp::Mul,
        Rule::arith_div => InfixOp::Div,
        Rule::arith_mod => InfixOp::Mod,
        Rule::cmp_eq => InfixOp::Eq,
        Rule::cmp_ne => InfixOp::Ne,
        Rule::cmp_lt => InfixOp::Lt,
        Rule::cmp_gt => InfixOp::Gt,
        Rule::cmp_le => InfixOp::Le,
        Rule::cmp_ge => InfixOp::Ge,
        _ => unreachable!(),
    };
    Ok(op)
}

// postfix_op = _{ postfix_incr | postfix_decr | call_access | dot_access | index_access}
pub fn parse_postfix_op(pair: Pair<Rule>) -> ParseResult<PostfixOp> {
    _debug_rule("parse_postfix_op", &pair);
    let op = match pair.as_rule() {
        Rule::postfix_incr => PostfixOp::Incr,
        Rule::postfix_decr => PostfixOp::Decr,
        Rule::call_access => {
            let call_access = parse_call_access(pair)?;
            PostfixOp::CallAccess(call_access)
        }
        Rule::dot_access => {
            // let id = pair.into_inner().next().unwrap().as_str().to_string();
            let dot_access = parse_dot_access(pair)?;
            PostfixOp::DotAccess(dot_access)
        }
        Rule::index_access => {
            let index = parse_index_access(pair)?;
            PostfixOp::IndexAccess(IndexAccess { index })
        }
        _ => unreachable!(),
    };
    Ok(op)
}

pub fn parse_call_args(pair: Pair<Rule>) -> ParseResult<Vec<Box<Expr>>> {
    _debug_rule("parse_call_args", &pair);
    let mut args = Vec::new();
    for arg_pair in pair.into_inner() {
        let expr = parse_expr(arg_pair)?;
        args.push(expr);
    }
    Ok(args)
}

// call_access = { "." ~ ID ~ TOK_LPAREN ~ call_args? ~ TOK_RPAREN }
pub fn parse_call_access(pair: Pair<Rule>) -> ParseResult<CallAccess> {
    _debug_rule("parse_call_access", &pair);
    let mut inner = pair.into_inner();

    let id = inner.next().unwrap().as_str().to_string();

    let args = match inner.next() {
        Some(p) => parse_call_args(p)?,
        None => Vec::new(),
    };

    Ok(CallAccess { id, args })
}

// dot_access = { "." ~ ID }
pub fn parse_dot_access(pair: Pair<Rule>) -> ParseResult<DotAccess> {
    _debug_rule("parse_dot_access", &pair);
    let mut inner = pair.into_inner();

    let field = inner.next().unwrap().as_str();
    Ok(DotAccess {
        field: field.to_string(),
    })
}

// number = { FLOAT_CONSTANT | INT_CONSTANT }
pub fn parse_number(pair: Pair<Rule>) -> ParseResult<Literal> {
    _debug_rule("parse_number", &pair);
    let number_str = pair.as_str().trim();
    let span = pair.as_span();
    let number_pair = pair.into_inner().next().unwrap();
    let number_rule = number_pair.as_rule();
    let number: Literal = match number_rule {
        Rule::float => {
            let float_rule = number_pair.into_inner().next().unwrap().as_rule();
            match float_rule {
                Rule::hex_float => {
                    let val = parse_hex_float(number_str);
                    if val.is_err() {
                        return Err(Box::new(ParseError::new_from_span(
                            ErrorVariant::CustomError {
                                message: "invalid hex float".to_string(),
                            },
                            span,
                        )));
                    }
                    Literal::Float(val.unwrap())
                }
                _ => {
                    let val = number_str.parse::<f64>();
                    if val.is_err() {
                        return Err(Box::new(ParseError::new_from_span(
                            ErrorVariant::CustomError {
                                message: "invalid float".to_string(),
                            },
                            span,
                        )));
                    }
                    Literal::Float(val.unwrap())
                }
            }
        }
        Rule::int => {
            let int_rule = number_pair.into_inner().next().unwrap().as_rule();
            let radix = match int_rule {
                Rule::hex_int => 16,
                Rule::oct_int => 8,
                Rule::bin_int => 2,
                _ => 10,
            };
            // remove 0x prefix if present
            let number_str = number_str.trim_start_matches("0x");
            let val = i64::from_str_radix(number_str, radix);
            if val.is_err() {
                return Err(Box::new(ParseError::new_from_span(
                    ErrorVariant::CustomError {
                        message: "invalid int".to_string(),
                    },
                    span,
                )));
            }
            Literal::Int(val.unwrap())
        }
        _ => unreachable!(),
    };
    Ok(number)
}

use std::num::ParseIntError;

fn parse_hex_float(s: &str) -> Result<f64, ParseIntError> {
    let sign = if s.starts_with('-') {
        -1.0
    } else {
        1.0
    };
    
    let without_prefix = s.trim_start_matches("-").trim_start_matches("0x").trim_start_matches("0X");
    let mut parts = without_prefix.splitn(2, |c| c == 'p' || c == 'P');
    let mantissa = parts.next().unwrap();
    let exponent = parts.next().unwrap_or("0");

    let exponent_val = i32::from_str_radix(exponent, 10)?;

    let mut mantissa_parts = mantissa.split('.');
    let integer_part = mantissa_parts.next().unwrap_or("");
    let decimal_part = mantissa_parts.next().unwrap_or("");

    let integer_val = match integer_part {
        "" => 0,
        _ => u64::from_str_radix(integer_part, 16)?,
    };
    let decimal_val = match decimal_part {
        "" => 0,
        _ => u64::from_str_radix(decimal_part, 16)?,
    };
    let decimal_len = (decimal_part.len() * 4) as f64;

    let float_val = ((integer_val as f64) + (decimal_val as f64 / (2f64.powf(decimal_len))))
        * 2f64.powi(exponent_val);
    let float_val = float_val * sign;
    Ok(float_val)
}

#[test]
fn test_parse_hex_float() {
    let r = parse_hex_float("0x.AP-3");
    assert_eq!(r.unwrap(), 0.078125);

    // Test negative hex float
    let r = parse_hex_float("-0x.Cp-2");
    assert_eq!(r.unwrap(), -0.187500);

    // Test hex float with leading zeros
    let r = parse_hex_float("0x00.8p2");
    assert_eq!(r.unwrap(), 2.000000);

    // Test hex float with lowercase letters
    let r = parse_hex_float("0x.a8p-1");
    assert_eq!(r.unwrap(), 0.328125);

    // Test hex float with uppercase letters
    let r = parse_hex_float("0X.9P3");
    assert_eq!(r.unwrap(), 4.500000);

    // Test hex float with no fractional part
    let r = parse_hex_float("0x9P2");
    assert_eq!(r.unwrap(), 36.000000);

    // Test hex float with no integer part
    let r = parse_hex_float("0x.2P1");
    assert_eq!(r.unwrap(), 0.250000);

    // Test hex float with no exponent part
    let r = parse_hex_float("0x.B4");
    assert_eq!(r.unwrap(), 0.703125);

    // Test hex float with exponent of 0
    let r = parse_hex_float("0x.C1P0");
    assert_eq!(r.unwrap(), 0.75390625);

    // Test hex float with negative exponent
    let r = parse_hex_float("0x.2Bp-2");
    assert_eq!(r.unwrap(), 0.0419921875);
}

// const_expr = { expr }
pub fn parse_const_expr(pair: Pair<Rule>) -> ParseResult<Box<Expr>> {
    _debug_rule("parse_const_expr", &pair);
    let expr_pair = pair.into_inner().next().unwrap();
    let expr = parse_expr(expr_pair)?;
    Ok(expr)
}

// id = @{ ("_" | "$" | alpha | unicode) ~ ("_" | "$" | alpha_num | unicode)* }
pub fn parse_id(pair: Pair<Rule>) -> ParseResult<IdentExpr> {
    let id = pair.as_str().to_string();
    Ok(IdentExpr { name: id })
}

// string = ${ quote ~ inner_str ~ quote }
pub fn parse_string(pair: Pair<Rule>) -> ParseResult<Literal> {
    let inner_str_pair = pair.into_inner().next().unwrap();
    let string_content = parse_inner_str(inner_str_pair)?;
    Ok(Literal::String(string_content))
}

// char = ${ single_quote ~ inner_chr ~ single_quote }
pub fn parse_char(pair: Pair<Rule>) -> ParseResult<char> {
    let inner_chr_pair = pair.into_inner().next().unwrap();
    let char_content = parse_inner_chr(inner_chr_pair)?;
    Ok(char_content)
}

// inner_str = @{ (!("\"" | "\\") ~ ANY)* ~ (escape ~ inner_str)? }
pub fn parse_inner_str(pair: Pair<Rule>) -> ParseResult<String> {
    let mut string_content = String::new();
    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::escape => {
                let escaped_char = parse_escape(part)?;
                string_content.push(escaped_char);
            }
            _ => {
                string_content.push_str(part.as_str());
            }
        }
    }
    Ok(string_content)
}

// inner_chr = @{ escape | ANY }
pub fn parse_inner_chr(pair: Pair<Rule>) -> ParseResult<char> {
    let content = pair.as_str();
    let char_content = if content.starts_with('\\') {
        parse_escape(pair)?
    } else {
        content.chars().next().unwrap()
    };
    Ok(char_content)
}

pub fn parse_escape(pair: Pair<Rule>) -> ParseResult<char> {
    let escape_content = pair.as_str();
    let escaped_char = match escape_content {
        "\\\"" => '\"',
        "\\\\" => '\\',
        "\\r" => '\r',
        "\\n" => '\n',
        "\\t" => '\t',
        "\\0" => '\0',
        "\\'" => '\'',
        _ => {
            if escape_content.starts_with("\\u") {
                parse_unicode_escape(escape_content, pair)?
            } else {
                return Err(Box::new(ParseError::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("Invalid escape sequence: {}", escape_content),
                    },
                    pair.as_span(),
                )));
            }
        }
    };
    Ok(escaped_char)
}

pub fn parse_unicode_escape(escape_content: &str, pair: Pair<Rule>) -> ParseResult<char> {
    let hex_digits = &escape_content[3..escape_content.len() - 1];
    let code_point = u32::from_str_radix(hex_digits, 16).map_err(|_| {
        Box::new(ParseError::new_from_span(
            ErrorVariant::CustomError {
                message: format!("Invalid Unicode escape: {}", escape_content),
            },
            pair.as_span(),
        ))
    })?;
    let unicode_char = char::from_u32(code_point).ok_or_else(|| {
        Box::new(ParseError::new_from_span(
            ErrorVariant::CustomError {
                message: format!("Invalid Unicode code point: {}", code_point),
            },
            pair.as_span(),
        ))
    })?;
    Ok(unicode_char)
}
