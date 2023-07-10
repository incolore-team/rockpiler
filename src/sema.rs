use crate::{ast::*, infer_eval::InferEvaluator, scope::*, symbol::*};
#[derive(Debug, PartialEq, Clone)]
pub struct SemaRef {
    pub symbol_id: SymbolId,
    pub scope_id: ScopeId,
}

impl SemaRef {
    pub fn new(symbol_id: SymbolId, scope_id: ScopeId) -> Self {
        Self {
            symbol_id,
            scope_id,
        }
    }
}

pub trait ToSemaTrait {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable);
}

impl ToSemaTrait for TransUnit {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        for var_decl in &mut self.var_decls {
            var_decl.to_sema(symbol_table);
        }

        for func_decl in &mut self.func_decls {
            func_decl.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for FuncDecl {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        // 插入函数声明的符号
        let symbol = Symbol::Func(FuncSymbol::new(self.clone()));
        let symbol_id = symbol_table.insert_symbol(self.name.clone(), symbol);
        self.sema_ref = Some(SemaRef::new(symbol_id, symbol_table.scope_id()));
        // 进入函数作用域
        symbol_table.enter_scope();

        // 将函数参数添加到符号表中
        for param in &mut self.params {
            let symbol = Symbol::Var(VarSymbol::new(param.clone().into()));
            let symbol_id = symbol_table.insert_symbol(param.name.clone(), symbol);
            param.sema_ref = Some(SemaRef::new(symbol_id, symbol_table.scope_id()));
        }

        // 分析函数体
        self.body.to_sema(symbol_table);

        // 离开函数作用域
        symbol_table.leave_scope();
    }
}

fn eval_array_type(at: &mut ArrayType, symbol_table: &mut SymbolTable) {
    match at {
        ArrayType::Constant(const_at) => {
            const_at.size = const_at
                .size_info
                .as_ref()
                .unwrap()
                .eval_literal(&symbol_table)
                .unwrap()
                .into();
            if let Type::Array(at) = const_at.element_type.as_mut() {
                eval_array_type(at, symbol_table);
            }
        }
        _ => (),
    }
}

impl ToSemaTrait for VarDecl {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        match &mut self.type_ {
            Type::Array(at) => eval_array_type(at, symbol_table),
            _ => (),
        };
        let symbol = Symbol::Var(VarSymbol::new(self.clone()));
        let symbol_id = symbol_table.insert_symbol(self.name.clone(), symbol);
        self.sema_ref = Some(SemaRef::new(symbol_id, symbol_table.scope_id()));
    }
}

impl ToSemaTrait for Block {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        for stmt in &mut self.stmts {
            stmt.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for Stmt {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        match self {
            Stmt::VarDecls(var_decls) => var_decls.to_sema(symbol_table),
            Stmt::Expr(expr_stmt) => expr_stmt.to_sema(symbol_table),
            Stmt::Block(block) => block.to_sema(symbol_table),
            Stmt::IfElse(if_else_stmt) => if_else_stmt.to_sema(symbol_table),
            Stmt::While(while_stmt) => while_stmt.to_sema(symbol_table),
            Stmt::For(for_stmt) => for_stmt.to_sema(symbol_table),
            Stmt::Break => {}
            Stmt::DoWhile(do_while_stmt) => do_while_stmt.to_sema(symbol_table),
            Stmt::Continue => {}
            Stmt::Return(return_stmt) => {
                if let Some(expr) = &mut return_stmt.expr {
                    expr.to_sema(symbol_table);
                }
            }
        }
    }
}

impl ToSemaTrait for Expr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        match self {
            Expr::Infix(infix_expr) => infix_expr.to_sema(symbol_table),
            Expr::Prefix(prefix_expr) => prefix_expr.to_sema(symbol_table),
            Expr::Postfix(postfix_expr) => postfix_expr.to_sema(symbol_table),
            Expr::Primary(primary_expr) => primary_expr.to_sema(symbol_table),
        }
    }
}

impl ToSemaTrait for InfixExpr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.lhs.to_sema(symbol_table);
        self.rhs.to_sema(symbol_table);
        self.infer_ty = self.infer_type(&symbol_table);
    }
}

impl ToSemaTrait for PrefixExpr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.rhs.to_sema(symbol_table);
    }
}

impl ToSemaTrait for PostfixExpr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.lhs.to_sema(symbol_table);
        match &mut self.op {
            PostfixOp::Incr | PostfixOp::Decr => {}
            PostfixOp::CallAccess(call_access) => call_access.to_sema(symbol_table),
            PostfixOp::DotAccess(dot_access) => dot_access.to_sema(symbol_table),
            PostfixOp::IndexAccess(index_access) => index_access.to_sema(symbol_table),
        }
    }
}

impl ToSemaTrait for CallAccess {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        for arg in &mut self.args {
            arg.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for DotAccess {
    fn to_sema(&mut self, _symbol_table: &mut SymbolTable) {
        // 由于这里没有任何符号需要处理，所以不需要实现
    }
}

impl ToSemaTrait for IndexAccess {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.index.to_sema(symbol_table);
    }
}

impl ToSemaTrait for PrimaryExpr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        match self {
            PrimaryExpr::Group(expr) => expr.to_sema(symbol_table),
            PrimaryExpr::Call(call_expr) => call_expr.to_sema(symbol_table),
            PrimaryExpr::Ident(ident_expr) => ident_expr.to_sema(symbol_table),
            PrimaryExpr::Literal(literal) => literal.to_sema(symbol_table),
        }
    }
}

impl ToSemaTrait for CallExpr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        for arg in &mut self.args {
            arg.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for IdentExpr {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        let symbol = symbol_table.lookup_symbol(&mut self.id);
        if symbol.is_none() {
            panic!("Undefined identifier: {}", self.id);
        }
        self.sema_ref = Some(SemaRef::new(symbol.unwrap(), symbol_table.scope_id()));
    }
}

impl ToSemaTrait for Literal {
    fn to_sema(&mut self, _symbol_table: &mut SymbolTable) {
        // 字面量无需进行符号表相关操作
    }
}

impl ToSemaTrait for VarDecls {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        for var_decl in &mut self.decls {
            var_decl.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for ExprStmt {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        if let Some(expr) = &mut self.expr {
            expr.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for IfElseStmt {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.cond.to_sema(symbol_table);
        self.if_stmt.to_sema(symbol_table);
        if let Some(else_stmt) = &mut self.else_stmt {
            else_stmt.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for WhileStmt {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.cond.to_sema(symbol_table);
        self.body.to_sema(symbol_table);
    }
}

impl ToSemaTrait for ForStmt {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        if let Some(init) = &mut self.init {
            init.to_sema(symbol_table);
        }
        if let Some(cond) = &mut self.cond {
            cond.to_sema(symbol_table);
        }
        if let Some(update) = &mut self.update {
            update.to_sema(symbol_table);
        }
        self.body.to_sema(symbol_table);
    }
}

impl ToSemaTrait for DoWhileStmt {
    fn to_sema(&mut self, symbol_table: &mut SymbolTable) {
        self.stmt.to_sema(symbol_table);
        self.cond.to_sema(symbol_table);
    }
}
