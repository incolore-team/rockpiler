use crate::ast;

use super::{module::Module, sema::SemaContext, ty::Ty, value::*};

pub fn build_ir(ctx: &mut Module, se: &mut SemaContext, ast: &ast::TransUnit) {
    ast.funcs.iter().for_each(|f| {
        visit_func(ctx, se, f);
    });
}

fn visit_func(ctx: &mut Module, se: &mut SemaContext, ast: &ast::Function) {
    assert!(ctx.cur_func.is_none(), "function should be free after use");
    ctx.cur_func = ctx.resolve(ast.name.as_str()); // state-change
    let old_scope = se.cursor().path_string(); // state-change
    se.cursor_mut().enter_child();
    {
        assert!(ctx.cur_func.is_some(), "resolve failed: {}", ast.name);

        let entry_bb_id = ctx.register_bb(BasicBlock::new("entry".to_string()));
        let mut func = &mut ctx.cur_func_mut();

        func.entry_bb = Some(entry_bb_id);
        ctx.cur_bb = Some(entry_bb_id);

        visit_block(ctx, se, &ast.body);
    }
    se.cursor_mut().leave(); // state-restore
    assert!(se.cursor().path_string() == old_scope, "scope mismatch");
    ctx.cur_func = None; // state-restore
}

fn visit_block(ctx: &mut Module, se: &mut SemaContext, ast: &ast::Block) {
    let old_scope = se.cursor().path_string();
    se.cursor_mut().enter_child(); // state-change
    {
        for stmt in &ast.stmts {
            match stmt {
                ast::Stmt::Expr(expr_stmt) => {
                    visit_expr(ctx, se, &expr_stmt.expr);
                }
                ast::Stmt::Return(ret_stmt) => match ret_stmt.expr {
                    Some(ref expr) => {
                        let expr_id = visit_expr(ctx, se, expr);
                        let ret_inst = Inst::Ret(RetInst { val: Some(expr_id) });
                        ctx.register_inst(ret_inst);
                    }
                    None => {
                        let ret_inst = Inst::Ret(RetInst { val: None });
                        ctx.register_inst(ret_inst);
                    }
                },
            }
        }
    }
    se.cursor_mut().leave(); // state-restore
    assert!(se.cursor().path_string() == old_scope, "scope mismatch");
}

fn visit_expr(ctx: &mut Module, se: &mut SemaContext, ast: &ast::Expr) -> ValueId {
    match ast {
        ast::Expr::Infix(infix) => visit_infix_expr(ctx, se, infix),
        ast::Expr::Prefix(prefix) => visit_prefix_expr(ctx, se, prefix),
        ast::Expr::Postfix(postfix) => visit_postfix_expr(ctx, se, postfix),
        ast::Expr::Primary(primary) => visit_primary_expr(ctx, se, primary),
    }
}

fn visit_infix_expr(ctx: &mut Module, se: &mut SemaContext, ast: &ast::InfixExpr) -> ValueId {
    visit_expr(ctx, se, &ast.lhs);
    visit_expr(ctx, se, &ast.rhs);
    todo!();
}

fn visit_prefix_expr(ctx: &mut Module, se: &mut SemaContext, ast: &ast::PrefixExpr) -> ValueId {
    visit_expr(ctx, se, &ast.rhs);
    todo!();
}

fn visit_postfix_expr(ctx: &mut Module, se: &mut SemaContext, ast: &ast::PostfixExpr) -> ValueId {
    visit_expr(ctx, se, &ast.lhs);
    todo!();
}

fn visit_primary_expr(ctx: &mut Module, se: &mut SemaContext, ast: &ast::PrimaryExpr) -> ValueId {
    match ast {
        ast::PrimaryExpr::Grouping(grouping) => visit_expr(ctx, se, grouping.as_ref()),
        ast::PrimaryExpr::Call(call) => visit_call(ctx, se, call),
        ast::PrimaryExpr::Ident(ident) => visit_ident(ctx, se, ident),
        ast::PrimaryExpr::Literal(literal) => visit_literal(ctx, se, literal),
    }
}

fn visit_call(ctx: &mut Module, se: &mut SemaContext, ast: &ast::CallExpr) -> ValueId {
    let func = ctx.resolve(&ast.callee);
    assert!(func.is_some(), "resolve failed: {}", ast.callee);
    let mut call_inst = CallInst {
        func: func.unwrap(),
        args: Vec::new(),
    };
    for i in 0..ast.args.len() {
        let arg = &ast.args[i];
        call_inst.args.push(visit_expr(ctx, se, arg));
    }
    ctx.register_inst(Inst::Call(call_inst))
}

fn visit_ident(ctx: &mut Module, se: &mut SemaContext, ast: &ast::IdentExpr) -> ValueId {
    ctx.resolve(&ast.name).unwrap()
}

fn visit_literal(ctx: &mut Module, se: &mut SemaContext, ast: &ast::LiteralExpr) -> ValueId {
    match &ast.value {
        ast::Literal::Int(literal_int) => {
            let id = ctx.ur.next_id();
            let value = Value {
                display_name: String::new(),
                id,
                uses: Vec::new(),
                proto: ValueProto::ImmVal(ImmVal::Int(IntConstVal {
                    ty: Ty::int(32),
                    val: *literal_int,
                })),
            };
            ctx.ur.insert(value)
        }
        ast::Literal::Char(_literal_char) => todo!(),
        ast::Literal::Float(_literal_float) => todo!(),
        ast::Literal::Bool(_literal_bool) => todo!(),
        ast::Literal::String(literal_string) => ctx.find_or_create_const_str(literal_string),
    }
}
