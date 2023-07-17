use crate::ast::*;
use crate::scope::SymbolTable;
use crate::symbol::Symbol;

pub trait InferEvaluator {
    // 类型推导
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type>;
    // 常量表达式求值
    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal>;
}

impl InferEvaluator for Expr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        match self {
            Expr::Infix(expr) => expr.infer_type(syms),
            Expr::Prefix(expr) => expr.infer_type(syms),
            Expr::Postfix(expr) => expr.infer_type(syms),
            Expr::Primary(expr) => expr.infer_type(syms),
        }
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        match self {
            Expr::Infix(expr) => expr.eval_literal(syms),
            Expr::Prefix(expr) => expr.eval_literal(syms),
            Expr::Postfix(expr) => expr.eval_literal(syms),
            Expr::Primary(expr) => expr.eval_literal(syms),
        }
    }
}

impl InferEvaluator for PrimaryExpr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        match self {
            PrimaryExpr::Group(expr) => expr.infer_type(syms),
            PrimaryExpr::Call(expr) => expr.infer_type(syms),
            PrimaryExpr::Ident(expr) => expr.infer_type(syms),
            PrimaryExpr::Literal(literal) => Some(literal.infer_type(syms)),
        }
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        match self {
            PrimaryExpr::Group(expr) => expr.eval_literal(syms),
            PrimaryExpr::Call(expr) => expr.eval_literal(syms),
            PrimaryExpr::Ident(expr) => expr.eval_literal(syms),
            PrimaryExpr::Literal(literal) => Some(literal.clone()),
        }
    }
}

impl Literal {
    fn infer_type(&self, _: &SymbolTable) -> Type {
        match self {
            Literal::Int(_) => Type::Builtin(BuiltinType::Int),
            Literal::Char(_) => Type::Builtin(BuiltinType::Char),
            Literal::Float(_) => Type::Builtin(BuiltinType::Double),
            Literal::Bool(_) => Type::Builtin(BuiltinType::Bool),
            Literal::String(_) => Type::Pointer(PointerType::new(Type::Builtin(BuiltinType::Char))),
        }
    }
}

impl InferEvaluator for InfixExpr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        let lhs_type = self.lhs.infer_type(syms)?;
        let rhs_type = self.rhs.infer_type(syms)?;

        // 对于不同类型的运算，C 语言规定需要进行类型转换
        match self.op {
            InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div | InfixOp::Mod => {
                // 如果左右都是算数类型，则返回右侧值的类型
                if lhs_type.is_arithmetic() && rhs_type.is_arithmetic() {
                    return Some(rhs_type);
                }
            }
            InfixOp::BitAnd | InfixOp::BitOr | InfixOp::BitXor => {
                // 如果左右都是整数类型，则返回右侧值的类型
                if lhs_type.is_integer() && rhs_type.is_integer() {
                    return Some(rhs_type);
                }
            }
            InfixOp::BitShl | InfixOp::BitShr => {
                // 如果左值是整数类型，则返回左值的类型
                if lhs_type.is_integer() {
                    return Some(lhs_type);
                }
            }
            InfixOp::Eq | InfixOp::Ne | InfixOp::Lt | InfixOp::Gt | InfixOp::Le | InfixOp::Ge => {
                // 返回 bool 类型
                return Some(Type::Builtin(BuiltinType::Bool));
            }
            InfixOp::LogicAnd | InfixOp::LogicOr => {
                // 返回 bool 类型
                return Some(Type::Builtin(BuiltinType::Bool));
            }
            InfixOp::Assign | InfixOp::Rem => {
                // 如果左右类型一致，则返回该类型
                if lhs_type == rhs_type {
                    return Some(lhs_type);
                }
            }
        }

        // // 如果左右类型一致，则返回该类型
        // if lhs_type == rhs_type {
        //     return Some(lhs_type);
        // }

        // 无法进行类型推断
        None
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        let lhs_val = self.lhs.eval_literal(syms)?;
        let rhs_val = self.rhs.eval_literal(syms)?;

        // 如果左右表达式的值都可以求出，则进行常量表达式求值
        match self.op {
            InfixOp::Add => lhs_val.add(&rhs_val),
            InfixOp::Sub => lhs_val.sub(&rhs_val),
            InfixOp::Mul => lhs_val.mul(&rhs_val),
            InfixOp::Div => lhs_val.div(&rhs_val),
            InfixOp::Mod => lhs_val.rem(&rhs_val),
            InfixOp::BitAnd => lhs_val.bitand(&rhs_val),
            InfixOp::BitOr => lhs_val.bitor(&rhs_val),
            InfixOp::BitXor => lhs_val.bitxor(&rhs_val),
            InfixOp::BitShl => lhs_val.shl(&rhs_val),
            InfixOp::BitShr => lhs_val.shr(&rhs_val),
            InfixOp::Eq => lhs_val.eq(&rhs_val),
            InfixOp::Ne => lhs_val.ne(&rhs_val),
            InfixOp::Lt => lhs_val.lt(&rhs_val),
            InfixOp::Gt => lhs_val.gt(&rhs_val),
            InfixOp::Le => lhs_val.le(&rhs_val),
            InfixOp::Ge => lhs_val.ge(&rhs_val),
            InfixOp::LogicAnd => lhs_val.log_and(&rhs_val),
            InfixOp::LogicOr => lhs_val.log_or(&rhs_val),
            InfixOp::Assign => unreachable!(),
            InfixOp::Rem => unimplemented!(),
        }
    }
}

impl InferEvaluator for PrefixExpr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        let rhs_type = self.rhs.infer_type(syms)?;
        match self.op {
            PrefixOp::Incr | PrefixOp::Decr => {
                if rhs_type.is_arithmetic() {
                    Some(rhs_type)
                } else {
                    None
                }
            }
            PrefixOp::Pos | PrefixOp::Neg => {
                if rhs_type.is_arithmetic() {
                    Some(rhs_type)
                } else {
                    None
                }
            }
            PrefixOp::Not | PrefixOp::BitNot => {
                // 取反运算符只能应用于整数或布尔类型，返回 bool 类型
                if rhs_type.is_integer() || rhs_type == Type::Builtin(BuiltinType::Bool) {
                    Some(Type::Builtin(BuiltinType::Bool))
                } else {
                    None
                }
            }
        }
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        let rhs_val = self.rhs.eval_literal(syms)?;

        match self.op {
            PrefixOp::Incr => rhs_val.add(&Literal::Int(1)),
            PrefixOp::Decr => rhs_val.sub(&Literal::Int(1)),
            PrefixOp::Pos => Some(rhs_val),
            PrefixOp::Neg => rhs_val.neg(),
            PrefixOp::Not => rhs_val.not(),
            PrefixOp::BitNot => rhs_val.bit_not(),
        }
    }
}
impl InferEvaluator for PostfixExpr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        let lhs_type = self.lhs.infer_type(syms)?;

        match &self.op {
            PostfixOp::Incr | PostfixOp::Decr => self.infer_type_incr_decr(&lhs_type),
            PostfixOp::CallAccess(call) => self.infer_type_call_access(&lhs_type, &call),
            PostfixOp::DotAccess(dot) => self.infer_type_dot_access(&lhs_type, &dot),
            PostfixOp::IndexAccess(IndexAccess { index }) => {
                self.infer_type_index_access(&lhs_type, &index)
            }
        }
    }

    fn eval_literal(&self, _: &SymbolTable) -> Option<Literal> {
        todo!()
    }
}

impl PostfixExpr {
    fn infer_type_incr_decr(&self, lhs_type: &Type) -> Option<Type> {
        Some(lhs_type.clone())
    }

    fn infer_type_call_access(&self, _lhs_type: &Type, _call: &CallAccess) -> Option<Type> {
        // 如果左值表达式为函数名，则返回函数返回值类型
        // if let Expr::Primary(PrimaryExpr::Ident(IdentExpr { id, .. })) = &*self.lhs {
        //     if let Some(SemaRef::Function(func)) = self.resolve(id) {
        //         if func.params.len() != call.args.len() {
        //             return None;
        //         }

        //         // 检查参数类型
        //         for (param, arg) in func.params.iter().zip(call.args.iter()) {
        //             let param_ty = &param.type_;
        //             let arg_ty = arg.infer_type(syms)?;
        //             if !param_ty.can_assign_from(&arg_ty) {
        //                 return None;
        //             }
        //         }

        //         Some(func.ret_ty.clone())
        //     } else {
        //         None
        //     }
        // } else {
        //     None
        // }
        todo!()
    }

    fn infer_type_dot_access(&self, _lhs_type: &Type, _dot: &DotAccess) -> Option<Type> {
        unimplemented!()
    }

    fn infer_type_index_access(&self, _lhs_type: &Type, _index: &Box<Expr>) -> Option<Type> {
        // 如果对一个类型进行索引访问，则返回该类型的元素类型
        if let Type::Array(arr_ty) = _lhs_type {
            Some(arr_ty.element_type().clone())
        } else {
            panic!("index access on non-array type")
        }
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        let lhs_val = self.lhs.eval_literal(syms)?;

        match &self.op {
            PostfixOp::Incr => lhs_val.add(&Literal::Int(1)),
            PostfixOp::Decr => lhs_val.sub(&Literal::Int(1)),
            PostfixOp::CallAccess(_call) => {
                unreachable!()
            }
            PostfixOp::DotAccess(_dot) => {
                unreachable!()
            }
            PostfixOp::IndexAccess(IndexAccess { index: _ }) => {
                unreachable!()
            }
        }
    }
}

impl InferEvaluator for CallExpr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        // 如果调用的是函数，则返回函数返回值类型
        if let Some(Symbol::Func(func_sym)) = syms.resolve_symbol(&self.id) {
            let func = func_sym.func;
            if func.params.len() != self.args.len() {
                return None;
            }

            // 检查参数类型
            for (param, arg) in func.params.iter().zip(self.args.iter()) {
                let param_ty = &param.type_;
                let arg_ty = arg.infer_type(syms)?;
                if !param_ty.can_assign_from(&arg_ty) {
                    return None;
                }
            }

            Some(func.ret_ty.clone())
        } else {
            None
        }
    }

    fn eval_literal(&self, _syms: &SymbolTable) -> Option<Literal> {
        // 如果调用的是内置函数，则直接调用对应的函数
        // if let Some(func) = get_builtin_function(&self.id) {
        //     let mut args = Vec::new();
        //     for arg in self.args.iter() {
        //         let arg_val = arg.eval_literal(syms)?;
        //         args.push(arg_val);
        //     }
        //     func.call(args)
        // } else {
        //     None
        // }
        None
    }
}

impl InferEvaluator for IdentExpr {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        if let Some(Symbol::Var(var_sym)) = syms.resolve_symbol(&self.id) {
            Some(var_sym.var.type_.clone())
        } else {
            None
        }
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        if let Some(Symbol::Var(var_sym)) = syms.resolve_symbol(&self.id) {
            if let Some(init) = &var_sym.var.init {
                init.eval_literal(syms)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl InferEvaluator for InitVal {
    fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
        match self {
            InitVal::Expr(expr) => expr.infer_type(syms),
            InitVal::Array(_array) => None,
        }
    }

    fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
        match self {
            InitVal::Expr(expr) => expr.eval_literal(syms),
            InitVal::Array(_array) => None,
        }
    }
}

// impl InferEvaluator for DerefExpr {
//     fn infer_type(&self, syms: &SymbolTable) -> Option<Type> {
//         let base_type = self.base.infer_type(syms)?;

//         // 确认被访问的对象是否为指针类型
//         match base_type {
//             Type::Pointer(inner_type) => Some(inner_type.clone()),
//             _ => None,
//         }
//     }

//     fn eval_literal(&self, syms: &SymbolTable) -> Option<Literal> {
//         None
//     }
// }

impl InferEvaluator for Literal {
    fn infer_type(&self, _syms: &SymbolTable) -> Option<Type> {
        Some(match self {
            Literal::Bool(_) => Type::Builtin(BuiltinType::Bool),
            Literal::Char(_) => Type::Builtin(BuiltinType::Char),
            Literal::Int(_) => Type::Builtin(BuiltinType::Int),
            Literal::Float(_) => Type::Builtin(BuiltinType::Float),
            Literal::String(_) => Type::Pointer(PointerType {
                type_: Box::new(Type::Builtin(BuiltinType::Char)),
            }),
        })
    }

    fn eval_literal(&self, _syms: &SymbolTable) -> Option<Literal> {
        Some(self.clone())
    }
}

use crate::ast::Literal;

impl Literal {
    pub fn add(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l + r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Float(l + r)),
            _ => None,
        }
    }

    pub fn sub(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l - r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Float(l - r)),
            _ => None,
        }
    }

    pub fn mul(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l * r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Float(l * r)),
            _ => None,
        }
    }

    pub fn div(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l / r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Float(l / r)),
            _ => None,
        }
    }

    pub fn rem(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l % r)),
            _ => None,
        }
    }

    pub fn bitand(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l & r)),
            _ => None,
        }
    }

    pub fn bitor(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l | r)),
            _ => None,
        }
    }

    pub fn bitxor(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l ^ r)),
            _ => None,
        }
    }

    pub fn shl(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l << r)),
            _ => None,
        }
    }

    pub fn shr(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Int(l >> r)),
            _ => None,
        }
    }

    pub fn eq(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Bool(l == r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Bool(l == r)),
            (Literal::Char(l), Literal::Char(r)) => Some(Literal::Bool(*l == *r)),
            (Literal::String(l), Literal::String(r)) => Some(Literal::Bool(l == r)),
            (Literal::Bool(l), Literal::Bool(r)) => Some(Literal::Bool(l == r)),
            _ => None,
        }
    }

    pub fn ne(&self, other: &Literal) -> Option<Literal> {
        match self.eq(other) {
            Some(Literal::Bool(b)) => Some(Literal::Bool(!b)),
            _ => None,
        }
    }

    pub fn lt(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Bool(l < r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Bool(l < r)),
            (Literal::Char(l), Literal::Char(r)) => Some(Literal::Bool(*l < *r)),
            (Literal::String(l), Literal::String(r)) => Some(Literal::Bool(l < r)),
            _ => None,
        }
    }

    pub fn gt(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Int(l), Literal::Int(r)) => Some(Literal::Bool(l > r)),
            (Literal::Float(l), Literal::Float(r)) => Some(Literal::Bool(l > r)),
            (Literal::Char(l), Literal::Char(r)) => Some(Literal::Bool(*l > *r)),
            (Literal::String(l), Literal::String(r)) => Some(Literal::Bool(l > r)),
            _ => None,
        }
    }

    pub fn le(&self, other: &Literal) -> Option<Literal> {
        match self.lt(other) {
            Some(Literal::Bool(b)) => Some(Literal::Bool(
                b || self.eq(other).unwrap() == Literal::Bool(true),
            )),
            _ => None,
        }
    }

    pub fn ge(&self, other: &Literal) -> Option<Literal> {
        match self.gt(other) {
            Some(Literal::Bool(b)) => Some(Literal::Bool(
                b || self.eq(other).unwrap() == Literal::Bool(true),
            )),
            _ => None,
        }
    }

    pub fn log_and(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Bool(l), Literal::Bool(r)) => Some(Literal::Bool(*l && *r)),
            _ => None,
        }
    }

    pub fn log_or(&self, other: &Literal) -> Option<Literal> {
        match (self, other) {
            (Literal::Bool(l), Literal::Bool(r)) => Some(Literal::Bool(*l || *r)),
            _ => None,
        }
    }

    pub fn neg(&self) -> Option<Literal> {
        match self {
            Literal::Int(n) => Some(Literal::Int(-n)),
            Literal::Float(n) => Some(Literal::Float(-n)),
            _ => None,
        }
    }

    pub fn not(&self) -> Option<Literal> {
        match self {
            Literal::Bool(b) => Some(Literal::Bool(!b)),
            _ => None,
        }
    }

    pub fn bit_not(&self) -> Option<Literal> {
        match self {
            Literal::Int(n) => Some(Literal::Int(!n)),
            _ => None,
        }
    }
}
