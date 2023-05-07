use crate::{scope::SymbolTable, ast::*, symbol::{Symbol, FuncSymbol, VarSymbol}};

pub trait ToSemaTrait {
    fn to_sema(&self, symbol_table: &mut SymbolTable);
}

impl ToSemaTrait for TransUnit {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        for func_decl in &self.func_decls {
            func_decl.to_sema(symbol_table);
        }

        for var_decl in &self.var_decls {
            var_decl.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for FuncDecl {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        // 插入函数声明的符号
        let symbol = Symbol::Func(FuncSymbol::new(self.clone()));
        symbol_table.insert_symbol(self.name.clone(), symbol);

        // 进入函数作用域
        symbol_table.enter_scope();

        // 将函数参数添加到符号表中
        for param in &self.params {
            let symbol = Symbol::Var(VarSymbol::new(param.clone().into()));
            symbol_table.insert_symbol(param.name.clone(), symbol);
        }

        // 分析函数体
        self.body.to_sema(symbol_table);

        // 离开函数作用域
        symbol_table.leave_scope();
    }
}

impl ToSemaTrait for VarDecl {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        let symbol = Symbol::Var(VarSymbol::new(self.clone()));
        symbol_table.insert_symbol(self.name.clone(), symbol);
    }
}

impl ToSemaTrait for Block {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        for stmt in &self.stmts {
            stmt.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for Stmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        match self {
            Stmt::VarDecls(var_decls) => var_decls.to_sema(symbol_table),
            Stmt::Assign(assign_stmt) => assign_stmt.to_sema(symbol_table),
            Stmt::Expr(expr_stmt) => expr_stmt.to_sema(symbol_table),
            Stmt::Block(block) => block.to_sema(symbol_table),
            Stmt::If(if_stmt) => if_stmt.to_sema(symbol_table),
            Stmt::IfElse(if_else_stmt) => if_else_stmt.to_sema(symbol_table),
            Stmt::While(while_stmt) => while_stmt.to_sema(symbol_table),
            Stmt::For(for_stmt) => for_stmt.to_sema(symbol_table),
            Stmt::Break => {}
            Stmt::DoWhile(do_while_stmt) => do_while_stmt.to_sema(symbol_table),
            Stmt::Continue => {}
            Stmt::Return(expr_option) => {
                if let Some(expr) = expr_option {
                    expr.to_sema(symbol_table);
                }
            }
        }
    }
}

impl ToSemaTrait for Expr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        match self {
            Expr::Infix(infix_expr) => infix_expr.to_sema(symbol_table),
            Expr::Prefix(prefix_expr) => prefix_expr.to_sema(symbol_table),
            Expr::Postfix(postfix_expr) => postfix_expr.to_sema(symbol_table),
            Expr::Primary(primary_expr) => primary_expr.to_sema(symbol_table),
        }
    }
}

impl ToSemaTrait for InfixExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.lhs.to_sema(symbol_table);
        self.rhs.to_sema(symbol_table);
    }
}

impl ToSemaTrait for PrefixExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.rhs.to_sema(symbol_table);
    }
}

impl ToSemaTrait for PostfixExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.lhs.to_sema(symbol_table);
        match &self.op {
            PostfixOp::Incr | PostfixOp::Decr => {}
            PostfixOp::CallAccess(call_access) => call_access.to_sema(symbol_table),
            PostfixOp::DotAccess(dot_access) => dot_access.to_sema(symbol_table),
            PostfixOp::IndexAccess(index_access) => index_access.to_sema(symbol_table),
        }
    }
}

impl ToSemaTrait for CallAccess {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        for arg in &self.args {
            arg.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for DotAccess {
    fn to_sema(&self, _symbol_table: &mut SymbolTable) {
        // 由于这里没有任何符号需要处理，所以不需要实现
    }
}

impl ToSemaTrait for IndexAccess {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.index.to_sema(symbol_table);
    }
}

impl ToSemaTrait for PrimaryExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        match self {
            PrimaryExpr::Group(expr) => expr.to_sema(symbol_table),
            PrimaryExpr::Call(call_expr) => call_expr.to_sema(symbol_table),
            PrimaryExpr::Lhs(lhs_expr) => lhs_expr.to_sema(symbol_table),
            PrimaryExpr::Ident(ident_expr) => ident_expr.to_sema(symbol_table),
            PrimaryExpr::Literal(literal) => literal.to_sema(symbol_table),
        }
    }
}

impl ToSemaTrait for CallExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        for arg in &self.args {
            arg.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for IdentExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        let symbol = symbol_table.lookup_symbol(&self.name);
        if symbol.is_none() {
            panic!("Undefined identifier: {}", self.name);
        }
    }
}

impl ToSemaTrait for Literal {
    fn to_sema(&self, _symbol_table: &mut SymbolTable) {
        // 字面量无需进行符号表相关操作
    }
}

impl ToSemaTrait for VarDecls {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        for var_decl in &self.decls {
            var_decl.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for AssignStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.lhs.to_sema(symbol_table);
        self.expr.to_sema(symbol_table);
    }
}

impl ToSemaTrait for ExprStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        if let Some(expr) = &self.expr {
            expr.to_sema(symbol_table);
        }
    }
}

impl ToSemaTrait for IfStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.cond.to_sema(symbol_table);
        self.stmt.to_sema(symbol_table);
    }
}

impl ToSemaTrait for IfElseStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.cond.to_sema(symbol_table);
        self.if_stmt.to_sema(symbol_table);
        self.else_stmt.to_sema(symbol_table);
    }
}

impl ToSemaTrait for WhileStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.cond.to_sema(symbol_table);
        self.stmt.to_sema(symbol_table);
    }
}

impl ToSemaTrait for ForStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        if let Some(init) = &self.init {
            init.to_sema(symbol_table);
        }
        if let Some(cond) = &self.cond {
            cond.to_sema(symbol_table);
        }
        if let Some(update) = &self.update {
            update.to_sema(symbol_table);
        }
        self.stmt.to_sema(symbol_table);
    }
}

impl ToSemaTrait for DoWhileStmt {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        self.stmt.to_sema(symbol_table);
        self.cond.to_sema(symbol_table);
    }
}

impl ToSemaTrait for LhsExpr {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        match self {
            LhsExpr::MixedAccess { id, access } => {
                let symbol = symbol_table.lookup_symbol(id);
                if symbol.is_none() {
                    panic!("Undefined identifier: {}", id);
                }
                for access_item in access {
                    access_item.to_sema(symbol_table);
                }
            }
        }
    }
}

impl ToSemaTrait for LhsAccess {
    fn to_sema(&self, symbol_table: &mut SymbolTable) {
        match self {
            LhsAccess::Index(expr) => expr.to_sema(symbol_table),
            LhsAccess::Dot(_field_name) => {
                // 目前没有这样的feature，还不支持结构体
            }
        }
    }
}

