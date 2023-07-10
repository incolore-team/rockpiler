use crate::{ast::*, ir::*, scope::*};

pub fn build(ast: &mut TransUnit, syms: SymbolTable) -> Module {
    let mut builder = Builder::new(syms);
    builder.build_module(ast);
    builder.module
}

struct Builder {
    module: Module,

    // 只是为了函数少两个参数
    cur_func: Option<ValueId>,
    cur_bb: Option<ValueId>,
}

impl Builder {
    pub fn new(syms: SymbolTable) -> Self {
        Self {
            module: Module::new(syms),

            cur_func: None,
            cur_bb: None,
        }
    }

    pub fn cur_bb_mut(&mut self) -> &mut BasicBlockValue {
        self.module.get_bb_mut(self.cur_bb.unwrap())
    }

    pub fn cur_func_mut(&mut self) -> &mut FunctionValue {
        self.module.get_func_mut(self.cur_func.unwrap())
    }

    pub fn cur_bb(&self) -> &BasicBlockValue {
        self.module.get_bb(self.cur_bb.unwrap())
    }

    pub fn cur_func(&self) -> &FunctionValue {
        self.module.get_func(self.cur_func.unwrap())
    }

    pub fn set_insert_point(&mut self, bb: ValueId) {
        self.cur_bb = Some(bb);
    }

    pub fn build_module(&mut self, ast: &mut TransUnit) {
        for var_decl in &ast.var_decls {
            self.build_global_variable(var_decl);
        }

        for func_decl in &ast.func_decls {
            self.build_function(func_decl);
        }
    }

    pub fn build_function(&mut self, func_decl: &FuncDecl) {
        let name = func_decl.name.clone();
        let ret_ty = self.build_type(&func_decl.ret_ty);

        let mut params = Vec::new();
        for param in &func_decl.params {
            let param_ty = self.build_type(&param.type_);
            let param_name = param.name.clone();
            let param_value = self
                .module
                .values
                .alloc(Value::VariableValue(VariableValue {
                    name: param_name,
                    ty: param_ty,
                }));
            params.push(param_value);
        }

        let is_var_arg = false;
        let mut cur_func = FunctionValue::new(name, params, ret_ty, is_var_arg);
        let entry_bb = BasicBlockValue::new("entry".to_string());

        let entry_bb_id = self.module.values.alloc(Value::BasicBlock(entry_bb));
        cur_func
            .bbs
            .append_with_name(entry_bb_id, "entry".to_string());

        self.cur_bb = Some(entry_bb_id);

        let function_id = self.module.values.alloc(Value::Function(cur_func));
        self.module
            .functions
            .insert(func_decl.name.clone(), function_id);

        self.cur_func = Some(function_id);

        for stmt in &func_decl.body.stmts {
            self.build_statement(stmt);
        }
    }

    pub fn build_type(&mut self, type_: &Type) -> Type {
        return type_.clone();
    }

    pub fn build_global_variable(&mut self, var_decl: &VarDecl) {
        let name = var_decl.name.clone();
        let ty = var_decl.type_.clone();
        let initializer = var_decl
            .init
            .as_ref()
            .map(|init_val| self.build_init_val(init_val, &ty));

        let global_var = GlobalVariableValue {
            name: name.clone(),
            ty,
            initializer,
        };

        let global_var_id = self.module.values.alloc(Value::GlobalVariable(global_var));
        self.module.global_variables.insert(name, global_var_id);
        self.module
            .sym2def
            .insert(var_decl.sema_ref.as_ref().unwrap().symbol_id, global_var_id);
    }
    pub fn build_init_val(&mut self, init_val: &InitVal, type_: &Type) -> ValueId {
        match init_val {
            InitVal::Expr(expr) => self.build_expr(expr),
            InitVal::Array(array_init_val) => {
                let _ty = Type::Array(ArrayType::Constant(ConstantArrayType {
                    element_type: Box::new(type_.clone()), // Replace with the correct element type
                    size: array_init_val.0.len() as usize,
                    size_info: None,
                }));

                // let values = array_init_val
                //     .0
                //     .iter()
                //     .map(|init_val| self.build_init_val(init_val, type_))
                //     .collect();

                // let const_array = ConstArray { ty, values };
                // let const_id = self.module.values.alloc(Value::Const(ConstValue::Array(const_array)));
                // const_id
                todo!()
            }
        }
    }

    pub fn build_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::IfElse(if_stmt) => {
                self.build_if_statement(if_stmt);
            }
            Stmt::While(while_stmt) => {
                self.build_while_statement(while_stmt);
            }
            Stmt::For(for_stmt) => {
                self.build_for_statement(for_stmt);
            }
            Stmt::Return(return_stmt) => {
                self.build_return_statement(return_stmt);
            }
            Stmt::Expr(expr_stmt) => {
                self.build_expression_statement(expr_stmt);
            }
            Stmt::VarDecls(var_decls_stmt) => {
                self.build_var_decls_statement(var_decls_stmt);
            }
            _ => {
                panic!("{:?}", stmt);
                todo!()
            }
        }
    }

    pub fn build_var_decls_statement(&mut self, var_decls_stmt: &VarDecls) {
        for decl in &var_decls_stmt.decls {
            let alloca = AllocaInst {
                ty: self.build_type(&decl.type_),
                name: decl.name.clone(),
            };
            let alloca_id = self.module.values.alloc(alloca.into());
            self.cur_bb_mut().insts.push_back(alloca_id);
            match &decl.init {
                Some(init_val) => {
                    let init_val_id = self.build_init_val(init_val, &decl.type_);
                    let store_inst = StoreInst {
                        src: init_val_id,
                        dst: alloca_id,
                    };
                    let store_id = self.module.values.alloc(store_inst.into());
                    self.cur_bb_mut().insts.push_back(store_id);
                    self.module
                        .sym2def
                        .insert(decl.sema_ref.as_ref().unwrap().symbol_id, store_id);
                }
                None => {
                    self.module
                        .sym2def
                        .insert(decl.sema_ref.as_ref().unwrap().symbol_id, alloca_id);
                }
            }
        }
    }

    pub fn build_if_statement(&mut self, if_stmt: &IfElseStmt) {
        let condition = self.build_expr(&if_stmt.cond);
        todo!()
    }
    /*
    int main() {
        int i = 0;
        while (i < 10) {
            i++;
        }
        return 0;
    }

    ; LLVM IR 代码
    define dso_local i32 @main() {
    entry:
      %i = alloca i32, align 4
      store i32 0, i32* %i, align 4
      br label %while.cond

    while.cond:                                       ; preds = %while.body, %entry
      %0 = load i32, i32* %i, align 4
      %cmp = icmp slt i32 %0, 10
      br i1 %cmp, label %while.body, label %while.end

    while.body:                                       ; preds = %while.cond
      %1 = load i32, i32* %i, align 4
      %inc = add nsw i32 %1, 1
      store i32 %inc, i32* %i, align 4
      br label %while.cond

    while.end:                                        ; preds = %while.cond
      ret i32 0
    }
     */
    pub fn build_while_statement(&mut self, while_stmt: &WhileStmt) {
        let cond_bb = self.build_basic_block();
        let body_bb = self.build_basic_block();
        let end_bb = self.build_basic_block();

        self.set_insert_point(cond_bb);
        let cond_value = self.build_expr(&while_stmt.cond);
        self.build_br_inst(cond_value, body_bb, end_bb);

        self.set_insert_point(body_bb);
        self.build_statement(&while_stmt.body);
        self.build_jump_inst(cond_bb);
    }

    pub fn get_value(&self, value_id: ValueId) -> &Value {
        &self.module.values[value_id]
    }

    pub fn alloc_br_inst(&mut self, cond: ValueId, then_bb: ValueId, else_bb: ValueId) -> ValueId {
        let br = BranchInst {
            cond,
            then_bb,
            else_bb,
        };
        let br_id = self.module.values.alloc(br.into());
        br_id
    }

    pub fn build_br_inst(&mut self, cond: ValueId, true_bb: ValueId, false_bb: ValueId) -> ValueId {
        let br_id = self.alloc_br_inst(cond, true_bb, false_bb);
        self.cur_bb_mut().insts.push_back(br_id);
        br_id
    }

    pub fn alloc_jump_inst(&mut self, bb: ValueId) -> ValueId {
        let jump = JumpInst { bb };
        let jump_id = self.module.values.alloc(jump.into());
        jump_id
    }

    pub fn build_jump_inst(&mut self, bb: ValueId) -> ValueId {
        let jump_id = self.alloc_jump_inst(bb);
        self.cur_bb_mut().insts.push_back(jump_id);
        jump_id
    }

    pub fn alloc_basic_block(&mut self) -> ValueId {
        let bb = BasicBlockValue::default();
        let bb_id = self.module.values.alloc(Value::BasicBlock(bb));
        bb_id
    }
    pub fn build_basic_block(&mut self) -> ValueId {
        let bb_id = self.alloc_basic_block();
        self.cur_func_mut().bbs.append(bb_id);
        bb_id
    }

    /*
    // C++ 代码
    int main() {
        for (int i = 0; i < 10; i++) {
            // do something
        }
        return 0;
    }

    ; LLVM IR 代码
    define dso_local i32 @main() {
    entry:
      br label %for.cond

    for.cond:                                         ; preds = %for.body, %entry
      %i = phi i32 [ 0, %entry ], [ %inc, %for.body ]
      %cmp = icmp slt i32 %i, 10
      br i1 %cmp, label %for.body, label %for.end

    for.body:                                         ; preds = %for.cond
      ; 在这里放置循环体代码
      %inc = add nsw i32 %i, 1
      br label %for.cond

    for.end:                                          ; preds = %for.cond
      ret i32 0
    }
     */
    pub fn build_for_statement(&mut self, for_stmt: &ForStmt) {
        let init_value = for_stmt.init.as_ref().map(|init| self.build_expr(init));

        let cond_bb = self.build_basic_block();
        let body_bb = self.build_basic_block();
        let end_bb = self.build_basic_block();

        self.set_insert_point(cond_bb);
        let cond_value = for_stmt.cond.as_ref().map(|cond| self.build_expr(cond));
        if let Some(cond_value) = cond_value {
            self.build_br_inst(cond_value, body_bb, end_bb);
        } else {
            self.build_jump_inst(body_bb);
        }

        self.set_insert_point(body_bb);
        self.build_statement(&for_stmt.body);

        let _ = for_stmt
            .update
            .as_ref()
            .map(|update| self.build_expr(update));

        self.build_jump_inst(cond_bb);
    }

    pub fn build_return_statement(&mut self, return_stmt: &ReturnStmt) {
        let value = return_stmt.expr.as_ref().map(|expr| self.build_expr(expr));
        self.build_return_inst(value);
    }

    pub fn build_return_inst(&mut self, value: Option<ValueId>) -> ValueId {
        let ret = ReturnInst { value };
        let ret_id = self.module.values.alloc(ret.into());
        self.cur_bb_mut().insts.push_back(ret_id);
        ret_id
    }

    pub fn build_expression_statement(&mut self, expr_stmt: &ExprStmt) {
        if let Some(expr) = &expr_stmt.expr {
            self.build_expr(expr);
        }
    }
    pub fn build_expr(&mut self, expr: &Box<Expr>) -> ValueId {
        let expr = &**expr;
        match expr {
            Expr::Infix(infix_expr) => {
                let lhs = self.build_expr(&infix_expr.lhs);
                let rhs = self.build_expr(&infix_expr.rhs);

                if infix_expr.op == InfixOp::Assign {
                    let assign = StoreInst { src: rhs, dst: lhs };
                    let assign_id = self.module.values.alloc(assign.into());
                    self.cur_bb_mut().insts.push_back(assign_id);
                    if let Expr::Primary(PrimaryExpr::Ident(ident_expr)) = infix_expr.lhs.as_ref() {
                        self.module
                            .sym2def
                            .insert(ident_expr.sema_ref.as_ref().unwrap().symbol_id, assign_id);
                    }
                    return assign_id;
                }

                let bin_op = BinaryOperator {
                    ty: infix_expr.infer_ty.as_ref().unwrap().clone(),
                    operation: infix_expr.op.clone(),
                    left_operand: lhs,
                    right_operand: rhs,
                };
                self.module.values.alloc(bin_op.into())
            }
            Expr::Prefix(prefix_expr) => {
                let rhs = self.build_expr(&prefix_expr.rhs);
                let op = match prefix_expr.op {
                    PrefixOp::Incr => InfixOp::Add,
                    PrefixOp::Decr => InfixOp::Sub,
                    _ => todo!(),
                };
                let zero = ConstValue::zero_of(prefix_expr.infer_ty.as_ref().unwrap().clone());
                let zero_id = self.module.values.alloc(zero.into());
                let bin_op = BinaryOperator {
                    ty: prefix_expr.infer_ty.as_ref().unwrap().clone(),
                    operation: op,
                    left_operand: zero_id,
                    right_operand: rhs,
                };
                self.module.values.alloc(bin_op.into())
            }
            Expr::Postfix(postfix_expr) => {
                let _lhs = self.build_expr(&postfix_expr.lhs);
                let _op = match postfix_expr.op {
                    PostfixOp::Incr => InfixOp::Add,
                    PostfixOp::Decr => InfixOp::Sub,
                    PostfixOp::CallAccess(_) => {
                        todo!()
                    }
                    PostfixOp::DotAccess(_) => {
                        todo!()
                    }
                    PostfixOp::IndexAccess(_) => {
                        todo!()
                    }
                };
                todo!()
            }
            Expr::Primary(primary_expr) => match primary_expr {
                PrimaryExpr::Group(expr) => self.build_expr(expr),
                PrimaryExpr::Call(call_expr) => {
                    let func_id = self.module.functions.get(&call_expr.id).unwrap().clone();
                    let args = call_expr
                        .args
                        .iter()
                        .map(|arg| arg.clone()) // 克隆 arg，以便在迭代之外使用
                        .collect::<Vec<_>>(); // 将结果收集到一个临时的 Vec 中

                    let args = args.into_iter().map(|arg| self.build_expr(&arg)); // 使用临时 Vec 构建表达式，避免多次借用 self
                    let args = args.collect::<Vec<_>>(); // 将结果收集到一个临时的 Vec 中
                    let func_value = self.module.values.get(func_id).unwrap();
                    let call_inst = CallInst {
                        ty: func_value.ty(),
                        callee: func_id.clone(),
                        args,
                    };
                    self.module.values.alloc(call_inst.into())
                }
                PrimaryExpr::Ident(ident_expr) => {
                    /*
                    为了区分不同作用域中的相同名称的变量，可以在构建 IR 时为每个作用域的变量创建不同的名称。
                     */
                    let sym_id = ident_expr.sema_ref.as_ref().unwrap().symbol_id;
                    let _symbol = self.module.syms.resolve_symbol_by_id(sym_id);
                    // log::debug!("sym_id: {:?}, _symbol: {:?}", sym_id, _symbol);
                    // log::debug!("sym2def: {:?}", self.module.sym2def);
                    let var_val_id = self.module.sym2def.get(&sym_id).unwrap().clone();
                    var_val_id
                }
                PrimaryExpr::Literal(literal) => self.build_literal(literal),
            },
        }
    }

    fn build_literal(&mut self, literal: &Literal) -> ValueId {
        match literal {
            Literal::Int(int) => {
                let int_value = ConstInt {
                    ty: BuiltinType::Int.into(),
                    value: *int,
                };
                self.module.values.alloc(int_value.into())
            }
            Literal::Float(float) => {
                let float_value = ConstFloat {
                    ty: BuiltinType::Float.into(),
                    value: *float,
                };
                self.module.values.alloc(float_value.into())
            }
            Literal::Bool(bool) => {
                let bool_value = ConstInt {
                    ty: BuiltinType::Bool.into(),
                    value: *bool as i64,
                };
                self.module.values.alloc(bool_value.into())
            }
            Literal::String(_string) => {
                todo!("build string literal")
            }
            Literal::Char(_) => todo!(),
        }
    }
}
