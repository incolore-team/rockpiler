use std::collections::VecDeque;

use log::trace;

use crate::{ast::*, infer_eval::InferEvaluator, ir::*, scope::*};

pub fn build(ast: &mut TransUnit, syms: SymbolTable) -> Module {
    let mut builder = Builder::new(syms);
    builder.build_module(ast);
    builder.module
}

struct Builder {
    module: Module,
    loop_stack: Vec<(ValueId, ValueId)>, // (break target bb, continue target bb)
}

impl Builder {
    pub fn new(syms: SymbolTable) -> Self {
        Self {
            module: Module::new(syms),
            loop_stack: Vec::new(),
        }
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
        let is_external = func_decl.is_external();
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

        let is_variadic = false;
        let mut cur_func = FunctionValue::new(name, params, ret_ty, is_external, is_variadic);

        if !is_external {
            let entry_bb = BasicBlockValue::new("entry".to_string());

            let entry_bb_id = self.module.alloc_value(Value::BasicBlock(entry_bb));
            cur_func
                .bbs
                .append_with_name(entry_bb_id, "entry".to_string());

            self.module.set_insert_point(entry_bb_id);
        }

        let function_id = self.module.alloc_value(Value::Function(cur_func));
        self.module
            .functions
            .insert(func_decl.name.clone(), function_id);

        self.module.set_cur_func(function_id);
        if !is_external {
            let mut index = 0;
            for param in &func_decl.params {
                let alloca_id = self
                    .module
                    .spawn_alloca_inst(param.name.clone(), param.type_.clone());
                self.module
                    .sym2def
                    .insert(param.sema_ref.as_ref().unwrap().symbol_id, alloca_id);

                // store param to allocated mem
                let param_value = self.module.cur_func().params[index];
                self.module.spawn_store_inst(alloca_id, param_value);
                index += 1;
            }
            if let Some(body) = &func_decl.body {
                self.build_block_statement(body);
            }
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
            .map(|init_val| self.build_global_init_val(init_val, &ty));
        let initializer_id = initializer.map(|cv| self.module.alloc_value(cv.into()));

        let global_var = GlobalVariableValue {
            name: name.clone(),
            ty,
            initializer: initializer_id,
            is_const: var_decl.is_const,
        };

        let global_var_id = self.module.alloc_value(Value::GlobalVariable(global_var));
        self.module.global_variables.insert(name, global_var_id);
        self.module
            .sym2def
            .insert(var_decl.sema_ref.as_ref().unwrap().symbol_id, global_var_id);
    }

    pub fn build_i32_val(&mut self, val: i32) -> ValueId {
        let value = Value::Const(ConstValue::Int(ConstInt {
            ty: Type::Builtin(BuiltinType::Int),
            value: val as i64,
        }));
        self.module.alloc_value(value)
    }

    // TODO: 对alloca后的数组memset
    pub fn build_array_init_val(
        &mut self,
        ptr: ValueId,
        // array_init_val: &ArrayInitVal,
        deque: &mut VecDeque<InitVal>,
        type_: &ConstantArrayType,
    ) {
        // let mut queue = VecDeque::from(array_init_val.0.clone());
        let i32_zero_id = self.build_i32_val(0);

        let dim = type_.size;
        let elem_type = type_.element_type.as_ref();
        match elem_type {
            Type::Array(at) => {
                for i in 0..dim {
                    let i32_i_id = self.build_i32_val(i as i32);
                    if let ArrayType::Constant(const_at) = at {
                        // let iv_ = array_init_val.0.get(i);
                        let iv_ = deque.front().cloned();
                        if let Some(iv) = iv_ {
                            let ty = Type::Array(ArrayType::Constant(type_.clone())).into();
                            let gep_id = self.module.spawn_gep_inst(
                                ty,
                                elem_type.clone(),
                                ptr,
                                vec![i32_zero_id, i32_i_id],
                            );
                            match iv {
                                InitVal::Array(array_iv) => {
                                    // let mut queue = VecDeque::from(array_iv.0.clone());
                                    deque.pop_front();
                                    let mut sub_deque = VecDeque::from(array_iv.0.clone());
                                    self.build_array_init_val(gep_id, &mut sub_deque, const_at);
                                }
                                InitVal::Expr(expr) => {
                                    self.build_array_init_val(gep_id, deque, const_at);
                                }
                            }
                        }
                    }
                }
            }
            _ => {
                for i in 0..dim {
                    let i32_i_id = self.build_i32_val(i as i32);
                    // let iv = array_init_val.0.get(i);
                    let iv_ = deque.pop_front();
                    if let Some(iv) = iv_ {
                        let init_val = self.build_init_val(&iv, elem_type);
                        let ty = Type::Array(ArrayType::Constant(type_.clone())).into();
                        let gep_id = self.module.spawn_gep_inst(
                            ty,
                            elem_type.clone(),
                            ptr,
                            vec![i32_zero_id, i32_i_id],
                        );
                        self.module.spawn_store_inst(gep_id, init_val);
                    }
                }
            }
        }
    }

    pub fn build_global_array_init_val(
        &mut self,
        deque: &mut VecDeque<InitVal>,
        type_: &ConstantArrayType,
    ) -> ConstValue {
        let dim = type_.size;
        let elem_type = type_.element_type.as_ref();
        let mut values = vec![];
        match elem_type {
            Type::Array(at) => {
                for _ in 0..dim {
                    if let ArrayType::Constant(const_at) = at {
                        let iv_ = deque.front().cloned();
                        if let Some(iv) = iv_ {
                            values.push(match iv {
                                InitVal::Array(array_iv) => {
                                    deque.pop_front();
                                    let mut sub_deque = VecDeque::from(array_iv.0.clone());
                                    self.build_global_array_init_val(&mut sub_deque, const_at)
                                }
                                InitVal::Expr(expr) => {
                                    self.build_global_array_init_val(deque, const_at)
                                }
                            })
                        }
                    }
                }
            }
            _ => {
                for _ in 0..dim {
                    let iv_ = deque.pop_front();
                    if let Some(iv) = iv_ {
                        let init_val = self.build_global_init_val(&iv, elem_type);
                        values.push(init_val);
                    }
                }
            }
        }
        ConstValue::Array(ConstArray {
            ty: Type::Array(ArrayType::Constant(type_.clone())),
            values: values,
        })
    }

    pub fn build_global_init_val(&mut self, init_val: &InitVal, type_: &Type) -> ConstValue {
        match init_val {
            InitVal::Expr(expr) => {
                let literal = expr.eval_literal(&self.module.syms).unwrap();
                match literal {
                    Literal::Int(val) => ConstValue::Int(ConstInt {
                        ty: type_.clone(),
                        value: val,
                    }),
                    _ => todo!(),
                }
            }
            InitVal::Array(array_init_val) => {
                if let Type::Array(ArrayType::Constant(const_at)) = type_ {
                    let mut deque = VecDeque::from(array_init_val.0.clone());
                    self.build_global_array_init_val(&mut deque, const_at)
                    // let elem_type = const_at.element_type.as_ref();
                    // let mut values = vec![];
                    // for iv in &array_init_val.0 {
                    //     let cv = self.build_global_init_val(&iv, &elem_type);
                    //     values.push(cv);
                    // }
                    // let const_array = ConstArray {
                    //     ty: type_.clone(),
                    //     values,
                    // };
                    // ConstValue::Array(const_array)
                } else {
                    todo!()
                }
            }
        }
    }

    pub fn build_init_val(&mut self, init_val: &InitVal, type_: &Type) -> ValueId {
        match init_val {
            InitVal::Expr(expr) => self.build_expr(expr, false),
            InitVal::Array(array_init_val) => {
                unimplemented!()
                // let ty = Type::Array(ArrayType::Constant(ConstantArrayType {
                //     element_type: Box::new(type_.clone()), // Replace with the correct element type
                //     size: array_init_val.0.len() as usize,
                //     size_info: None,
                // }));
                // let values = array_init_val
                //     .0
                //     .iter()
                //     .map(|init_val| self.build_init_val(init_val, type_))
                //     .collect();

                // let const_array = ConstArray { ty, values };
                // let const_id = self
                //     .module
                //     .alloc_value(Value::Const(ConstValue::Array(const_array)));
                // const_id
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
            Stmt::Block(block_stmt) => {
                self.build_block_statement(block_stmt);
            }
            Stmt::Break => self.build_break_statement(),
            Stmt::DoWhile(do_while_stmt) => self.build_do_while_statement(do_while_stmt),
            Stmt::Continue => self.build_continue_statement(),
        }
    }

    /*
    int main() {
        int i = 0;
        do {
            i++;
        } while (i < 10)
        return 0;
    }

    ; LLVM IR 代码
    define dso_local i32 @main() {
    entry:
      %i = alloca i32, align 4
      store i32 0, i32* %i, align 4
      br label %do.body

    do.body:                                       ; preds = %while.cond
      %0 = load i32, ptr %i, align 4
      %inc = add nsw i32 %0, 1
      store i32 %inc, ptr %i, align 4
      br label %do.cond

    do.cond:                                       ; preds = %while.body, %entry
      %1 = load i32, ptr %i, align 4
      %cmp = icmp slt i32 %1, 10
      br i1 %cmp, label %do.body, label %do.end

    do.end:                                        ; preds = %while.cond
      ret i32 0
    }
     */
    pub fn build_do_while_statement(&mut self, do_while_stmt: &DoWhileStmt) {
        let body_bb = self.module.spawn_basic_block();
        let cond_bb = self.module.spawn_basic_block();
        let end_bb = self.module.alloc_basic_block();

        self.loop_stack.push((end_bb, body_bb));
        {
            self.module.spawn_jump_inst(body_bb);

            self.module.set_insert_point(body_bb);
            self.build_statement(&do_while_stmt.stmt);
            self.module.spawn_jump_inst(cond_bb);

            self.module.set_insert_point(cond_bb);
            let cond_value = self.build_expr(&do_while_stmt.cond, false);
            self.module.spawn_br_inst(cond_value, body_bb, end_bb);

            self.module.cur_func_mut().bbs.append(end_bb.clone());

            self.module.set_insert_point(end_bb);
        }
        self.loop_stack.pop();
    }

    pub fn build_break_statement(&mut self) {
        if self.loop_stack.is_empty() {
            panic!("break statement not in loop");
        }

        let (break_bb, _) = self.loop_stack.last().unwrap().clone();
        self.module.spawn_jump_inst(break_bb);
    }

    pub fn build_continue_statement(&mut self) {
        if self.loop_stack.is_empty() {
            panic!("continue statement not in loop");
        }

        let (_, cont_bb) = self.loop_stack.last().unwrap().clone();
        self.module.spawn_jump_inst(cont_bb);
    }

    pub fn build_block_statement(&mut self, block_stmt: &Block) {
        for stmt in &block_stmt.stmts {
            self.build_statement(stmt);
        }
    }

    pub fn build_var_decls_statement(&mut self, var_decls_stmt: &VarDecls) {
        for decl in &var_decls_stmt.decls {
            let alloca_id = self
                .module
                .spawn_alloca_inst(decl.name.clone(), decl.type_.clone());
            let decl_ty = decl.type_.clone();
            match &decl.init {
                Some(init_val) => {
                    match decl_ty {
                        Type::Array(at) => {
                            if let ArrayType::Constant(const_at) = at {
                                if let InitVal::Array(array_init_val) = init_val {
                                    let mut deque = VecDeque::from(array_init_val.0.clone());
                                    // self.build_array_init_val(alloca_id, array_init_val, &const_at);
                                    self.build_array_init_val(alloca_id, &mut deque, &const_at);
                                    self.module.sym2def.insert(
                                        decl.sema_ref.as_ref().unwrap().symbol_id,
                                        alloca_id,
                                    );
                                }
                            }
                        }
                        _ => {
                            let init_val_id = self.build_init_val(init_val, &decl.type_);
                            let store_id = self.module.spawn_store_inst(alloca_id, init_val_id);
                            self.module
                                .sym2def
                                .insert(decl.sema_ref.as_ref().unwrap().symbol_id, alloca_id);
                        }
                    }
                }
                None => {
                    self.module
                        .sym2def
                        .insert(decl.sema_ref.as_ref().unwrap().symbol_id, alloca_id);
                }
            }
        }
    }

    pub fn visit_cond_expr(&mut self, cond_expr: &Box<Expr>, true_bb: ValueId, false_bb: ValueId) {
        let mut is_short_circuit_eval = false;
        let expr = &**cond_expr;

        if let Expr::Infix(infix_expr) = expr {
            is_short_circuit_eval =
                infix_expr.op == InfixOp::LogicAnd || infix_expr.op == InfixOp::LogicOr;
        }

        if is_short_circuit_eval {
            let next_bb;
            let infix_expr = match expr {
                Expr::Infix(infix_expr) => infix_expr,
                _ => unreachable!(),
            };

            if infix_expr.op == InfixOp::LogicAnd {
                next_bb = self.module.spawn_basic_block();
                self.visit_cond_expr(&infix_expr.lhs, next_bb.clone(), false_bb);
            } else if infix_expr.op == InfixOp::LogicOr {
                next_bb = self.module.spawn_basic_block();
                self.visit_cond_expr(&infix_expr.lhs, true_bb, next_bb.clone());
            } else {
                unreachable!();
            }

            self.module.set_insert_point(next_bb);
            self.visit_cond_expr(&infix_expr.rhs, true_bb, false_bb);
        } else {
            let cond_val_id = self.build_expr(&cond_expr, false);
            println!("cond_val_id: {}", self.module.inspect_value(cond_val_id));
            // 如果不是 bool(i1) 类型，则生成一个比较指令
            let ty = self.module.get_value(cond_val_id).ty();
            let cond_val_id = if ty != BuiltinType::Bool.into() {
                let zero_val_id = self.module.spawn_zero_value(ty.clone());
                self.module
                    .spawn_binop_inst(ty, InfixOp::Ne, cond_val_id, zero_val_id)
            } else {
                cond_val_id
            };
            self.module.spawn_br_inst(cond_val_id, true_bb, false_bb);
        }
    }

    pub fn build_if_statement(&mut self, if_stmt: &IfElseStmt) {
        let true_bb = self.module.spawn_basic_block();
        let exit_bb = self.module.alloc_basic_block();
        let false_bb = if if_stmt.else_stmt.is_some() {
            self.module.spawn_basic_block()
        } else {
            exit_bb.clone()
        };

        self.visit_cond_expr(&if_stmt.cond, true_bb.clone(), false_bb.clone());

        self.module.set_insert_point(true_bb);
        self.build_statement(&if_stmt.then_stmt);
        self.module.spawn_jump_inst(exit_bb.clone());

        if let Some(else_stmt) = &if_stmt.else_stmt {
            self.module.set_insert_point(false_bb);
            self.build_statement(else_stmt);
            self.module.spawn_jump_inst(exit_bb.clone());
        }
        self.module.cur_func_mut().bbs.append(exit_bb.clone());
        self.module.set_insert_point(exit_bb);
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
        let cond_bb = self.module.spawn_basic_block();
        let body_bb = self.module.spawn_basic_block();
        let end_bb = self.module.alloc_basic_block();

        self.loop_stack.push((end_bb, cond_bb));
        {
            self.module.set_insert_point(cond_bb);
            let cond_value = self.build_expr(&while_stmt.cond, false);
            self.module.spawn_br_inst(cond_value, body_bb, end_bb);

            self.module.set_insert_point(body_bb);
            self.build_statement(&while_stmt.body);
            self.module.spawn_jump_inst(cond_bb);

            self.module.cur_func_mut().bbs.append(end_bb.clone());

            self.module.set_insert_point(end_bb);
        }
        self.loop_stack.pop();
    }

    pub fn get_value(&self, value_id: ValueId) -> &Value {
        &self.module.values[value_id]
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
        let init_value = for_stmt
            .init
            .as_ref()
            .map(|init| self.build_expr(init, false));

        let cond_bb = self.module.spawn_basic_block();
        let body_bb = self.module.spawn_basic_block();
        let end_bb = self.module.spawn_basic_block();

        self.module.set_insert_point(cond_bb);
        let cond_value = for_stmt
            .cond
            .as_ref()
            .map(|cond| self.build_expr(cond, false));
        if let Some(cond_value) = cond_value {
            self.module.spawn_br_inst(cond_value, body_bb, end_bb);
        } else {
            self.module.spawn_jump_inst(body_bb);
        }

        self.module.set_insert_point(body_bb);
        self.build_statement(&for_stmt.body);

        let _ = for_stmt
            .update
            .as_ref()
            .map(|update| self.build_expr(update, false));

        self.module.spawn_jump_inst(cond_bb);
    }

    pub fn build_return_statement(&mut self, return_stmt: &ReturnStmt) {
        let value = return_stmt
            .expr
            .as_ref()
            .map(|expr| self.build_expr(expr, false));
        self.module.spawn_return_inst(value);
    }

    pub fn build_expression_statement(&mut self, expr_stmt: &ExprStmt) {
        if let Some(expr) = &expr_stmt.expr {
            self.build_expr(expr, false);
        }
    }

    // is_lval 表示是否是左值表达式，如果是，则不需要生成 LoadInst
    pub fn build_expr(&mut self, expr: &Box<Expr>, is_lval: bool) -> ValueId {
        let expr = &**expr;
        match expr {
            Expr::Infix(infix_expr) => {
                let is_assign = infix_expr.op == InfixOp::Assign;
                let lhs = self.build_expr(&infix_expr.lhs, is_assign);
                let rhs = self.build_expr(&infix_expr.rhs, false);

                if is_assign {
                    return self.module.spawn_store_inst(lhs, rhs);
                }
                let ty = infix_expr.infer_ty.as_ref().unwrap().clone();
                let op = infix_expr.op.clone();
                self.module.spawn_binop_inst(ty, op, lhs, rhs)
            }
            Expr::Prefix(prefix_expr) => {
                let rhs = self.build_expr(&prefix_expr.rhs, false);
                let ty = prefix_expr.infer_ty.as_ref().unwrap().clone();
                match prefix_expr.op {
                    PrefixOp::Incr => {
                        let converted_infix_op = InfixOp::Add;
                        let one = ConstValue::Int(ConstInt {
                            ty: ty.clone(),
                            value: 1,
                        });
                        let one_id = self.module.alloc_value(one.into());
                        self.module
                            .spawn_binop_inst(ty, converted_infix_op, one_id, rhs)
                    }
                    PrefixOp::Decr => {
                        let converted_infix_op = InfixOp::Sub;
                        let one = ConstValue::Int(ConstInt {
                            ty: ty.clone(),
                            value: 1,
                        });
                        let one_id = self.module.alloc_value(one.into());
                        self.module
                            .spawn_binop_inst(ty, converted_infix_op, one_id, rhs)
                    }
                    PrefixOp::Not => todo!(),
                    PrefixOp::BitNot => todo!(),
                    PrefixOp::Pos => {
                        let converted_infix_op = InfixOp::Add;
                        let zero = ConstValue::zero_of(ty.clone());
                        let zero_id = self.module.alloc_value(zero.into());
                        self.module
                            .spawn_binop_inst(ty, converted_infix_op, zero_id, rhs)
                    }
                    PrefixOp::Neg => {
                        let converted_infix_op = InfixOp::Sub;
                        let zero = ConstValue::zero_of(ty.clone());
                        let zero_id = self.module.alloc_value(zero.into());
                        self.module
                            .spawn_binop_inst(ty, converted_infix_op, zero_id, rhs)
                    }
                }
            }
            Expr::Postfix(postfix_expr) => {
                let _lhs = self.build_expr(&postfix_expr.lhs, true);
                match &postfix_expr.op {
                    PostfixOp::Incr => {
                        let _op = InfixOp::Add;
                        let ty = postfix_expr.infer_ty.as_ref().unwrap().clone();
                        let one = ConstValue::Int(ConstInt {
                            ty: ty.clone(),
                            value: 1,
                        });
                        let one_id = self.module.alloc_value(one.into());
                        self.module.spawn_binop_inst(ty, _op, one_id, _lhs);
                        _lhs
                    }
                    PostfixOp::Decr => {
                        let _op = InfixOp::Sub;
                        let ty = postfix_expr.infer_ty.as_ref().unwrap().clone();
                        let one = ConstValue::Int(ConstInt {
                            ty: ty.clone(),
                            value: 1,
                        });
                        let one_id = self.module.alloc_value(one.into());
                        self.module.spawn_binop_inst(ty, _op, one_id, _lhs);
                        _lhs
                    }
                    PostfixOp::IndexAccess(ia) => {
                        let index = self.build_expr(&ia.index, false);
                        let i32_zero_id = self.build_i32_val(0);
                        let lhs = self.get_value(_lhs);
                        // trace!("lhs: {:?}", lhs);
                        let ty_ = match lhs {
                            Value::Instruction(iv) => iv.ty(),
                            Value::GlobalVariable(gv) => gv.ty.clone(),
                            _ => {
                                trace!("ty_: {:?}", lhs);
                                unreachable!()
                            }
                        };
                        let infer_ty = &postfix_expr.infer_ty;
                        let gep_inst_id = self.module.spawn_gep_inst(
                            ty_,
                            infer_ty.clone().unwrap(),
                            _lhs,
                            vec![i32_zero_id, index],
                        );
                        if let Some(Type::Builtin(_)) = infer_ty {
                            let load_inst_id = self.module.spawn_load_inst(gep_inst_id);
                            return load_inst_id;
                        }
                        gep_inst_id
                    }
                    _ => todo!(),
                }
            }
            Expr::Primary(primary_expr) => match primary_expr {
                PrimaryExpr::Group(expr) => self.build_expr(expr, false),
                PrimaryExpr::Call(call_expr) => {
                    let func_id = self.module.functions.get(&call_expr.id).unwrap().clone();
                    let args = call_expr
                        .args
                        .iter()
                        .map(|arg| arg.clone()) // 克隆 arg，以便在迭代之外使用
                        .collect::<Vec<_>>(); // 将结果收集到一个临时的 Vec 中

                    let args = args.into_iter().map(|arg| self.build_expr(&arg, false)); // 使用临时 Vec 构建表达式，避免多次借用 self
                    let args = args.collect::<Vec<_>>(); // 将结果收集到一个临时的 Vec 中
                    self.module.spawn_call_inst(func_id.clone(), args)
                }
                PrimaryExpr::Ident(ident_expr) => {
                    /*
                    为了区分不同作用域中的相同名称的变量，可以在构建 IR 时为每个作用域的变量创建不同的名称。
                     */
                    let sym_id = ident_expr.sema_ref.as_ref().unwrap().symbol_id;
                    if self.module.sym2def.get(&sym_id).is_none() {
                        panic!(
                            "missing symbol definition for {} symbol id: {:?}, symbol: {:?}",
                            ident_expr.id,
                            sym_id,
                            self.module.syms.resolve_symbol_by_id(sym_id)
                        );
                    }
                    let _symbol = self.module.syms.resolve_symbol_by_id(sym_id);
                    // log::debug!("sym_id: {:?}, _symbol: {:?}", sym_id, _symbol);
                    // log::debug!("sym2def: {:?}", self.module.sym2def);
                    let var_val_id = self.module.sym2def.get(&sym_id).unwrap().clone();
                    // 例如接下来要对变量进行赋值，那么就不需要 load。
                    // 如果接下来要使用变量进行运算等，则需要 load。
                    if is_lval {
                        var_val_id
                    } else {
                        self.module.spawn_load_inst(var_val_id)
                    }
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
                self.module.alloc_value(int_value.into())
            }
            Literal::Float(float) => {
                let float_value = ConstFloat {
                    ty: BuiltinType::Float.into(),
                    value: *float,
                };
                self.module.alloc_value(float_value.into())
            }
            Literal::Bool(bool) => {
                let bool_value = ConstInt {
                    ty: BuiltinType::Bool.into(),
                    value: *bool as i64,
                };
                self.module.alloc_value(bool_value.into())
            }
            Literal::String(_string) => {
                todo!("build string literal")
            }
            Literal::Char(_) => todo!(),
        }
    }
}
