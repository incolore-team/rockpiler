use crate::{
    ast,
    ir::{value::*, Ty},
};

use super::{
    module::Module,
    scope::{ScopeCursor, ScopeId},
    value::ValueId,
};

pub struct SemaContext<'a> {
    scope_cursor: ScopeCursor<'a>,
}

impl<'a> SemaContext<'a> {
    pub fn new(scope_cursor: ScopeCursor<'a>) -> Self {
        Self { scope_cursor }
    }

    pub fn cursor(&self) -> &ScopeCursor<'a> {
        &self.scope_cursor
    }

    pub fn cursor_mut(&mut self) -> &mut ScopeCursor<'a> {
        &mut self.scope_cursor
    }
}

pub fn sema_analyze(ctx: &mut Module, ast: &ast::TransUnit) {
    ast.funcs.iter().for_each(|f| {
        visit_func(ctx, f);
    });

    // context should not be in any scope or function after sema analysis
    assert!(ctx.cur_scope == ScopeId::root());
    assert!(ctx.cur_func.is_none());
    assert!(ctx.cur_bb.is_none());
}

impl Module {
    pub fn derive_scope(&mut self) {
        self.cur_scope = self.sr.derive(self.cur_scope);
    }

    pub fn close_scope(&mut self) {
        self.cur_scope = self.sr.leave(self.cur_scope);
    }

    pub fn declare(&mut self, name: String, value: ValueId) -> Option<(ScopeId, ValueId)> {
        log::trace!("declare {} in scope {}", name, self.cur_scope);
        self.sr.register(self.cur_scope, name, value)
    }

    pub fn resolve(&self, name: &str) -> Option<ValueId> {
        self.sr.resolve(self.cur_scope, name)
    }
}

pub fn visit_func(ctx: &mut Module, ast: &ast::Function) {
    let func_id = ctx.register_func(Func::new_global(
        ast.name.clone(),
        Ty::from(&ast.ret_ty),
        Vec::new(), // params is empty, will be filled later
    ));
    assert!(ctx.cur_func.is_none(), "function should be free after use");
    ctx.cur_func = Some(func_id); // state-change
    let mut params = Vec::new();
    for param in &ast.params {
        params.push(ctx.register_var(Var::new_param(param.name.clone(), Ty::from(&param.ty))));
    }

    ctx.declare(ast.name.clone(), func_id);
    // scope for func params
    ctx.derive_scope(); // state-change
    {
        for i in 0..ast.params.len() {
            let ret = ctx.declare(ast.params[i].name.clone(), params[i]);
            assert!(ret.is_none());
        }

        // fill params
        ctx.get_func_mut(func_id).params = params;

        visit_block(ctx, &ast.body);
    }
    ctx.close_scope(); // state-restore
    ctx.cur_func = None; // state-restore
}

// visit block and create scope for block
pub fn visit_block(ctx: &mut Module, ast: &ast::Block) {
    let old_scope = ctx.cur_scope;
    ctx.derive_scope(); // state-change
    for stmt in &ast.stmts {
        match stmt {
            ast::Stmt::Expr(expr_stmt) => {
                visit_expr(ctx, &expr_stmt.expr);
            }
            ast::Stmt::Return(ret_stmt) => match ret_stmt.expr {
                Some(ref expr) => {
                    visit_expr(ctx, expr);
                }
                None => {}
            },
        }
    }
    ctx.close_scope(); // state-restore
    assert!(ctx.cur_scope == old_scope, "scope should be restored");
}

fn visit_expr(ctx: &mut Module, ast: &ast::Expr) {
    match ast {
        ast::Expr::Infix(infix) => visit_infix_expr(ctx, infix),
        ast::Expr::Prefix(prefix) => visit_prefix_expr(ctx, prefix),
        ast::Expr::Postfix(postfix) => visit_postfix_expr(ctx, postfix),
        ast::Expr::Primary(primary) => visit_primary_expr(ctx, primary),
    }
}

fn visit_infix_expr(ctx: &mut Module, ast: &ast::InfixExpr) {
    visit_expr(ctx, &ast.lhs);
    visit_expr(ctx, &ast.rhs);
    todo!();
}

fn visit_prefix_expr(ctx: &mut Module, ast: &ast::PrefixExpr) {
    visit_expr(ctx, &ast.rhs);
    todo!();
}

fn visit_postfix_expr(ctx: &mut Module, ast: &ast::PostfixExpr) {
    visit_expr(ctx, &ast.lhs);
    todo!();
}

fn visit_primary_expr(ctx: &mut Module, ast: &ast::PrimaryExpr) {
    match ast {
        ast::PrimaryExpr::Grouping(grouping) => visit_expr(ctx, grouping.as_ref()),
        ast::PrimaryExpr::Call(call) => visit_call(ctx, call),
        ast::PrimaryExpr::Ident(ident) => visit_ident(ctx, ident),
        ast::PrimaryExpr::Literal(literal) => visit_literal(ctx, literal),
    }
}

fn visit_call(ctx: &mut Module, ast: &ast::CallExpr) {
    for i in 0..ast.args.len() {
        let arg = &ast.args[i];
        visit_expr(ctx, arg)
    }
}

fn visit_ident(ctx: &mut Module, ast: &ast::IdentExpr) {
    let name = &ast.name;
    let vid = ctx.resolve(name);
    if vid.is_none() {
        panic!("Undefined variable: {}", name);
    }
}

fn visit_literal(_ctx: &mut Module, _ast: &ast::LiteralExpr) {}
