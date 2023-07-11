use std::{any::Any, fmt::format};

use clap::ValueEnum;

use crate::{ast::*, ir::*};

pub fn print(module: &mut Module) {
    let mut printer = Printer { module: module };
    printer.print_module();
}

struct Printer<'a> {
    module: &'a Module,
}

impl<'a> Printer<'a> {
    pub fn print_module(&mut self) {
        for (name, var_val_id) in &self.module.global_variables {
            self.print_global_variable(name, var_val_id.to_owned());
        }

        for (name, func_val_id) in &self.module.functions {
            self.print_function(name, func_val_id.to_owned());
        }
    }

    pub fn print_global_variable(&mut self, name: &str, val_id: ValueId) {
        let var = self.module.get_global_var(val_id);
        let literal = match &var.initializer {
            Some(val_id) => {
                let val = Value::resolve(*val_id, self.module);
                self.format_value(val_id, val)
            }
            None => "zeroinitializer".to_string(),
        };
        println!(
            "@{} = global {} {}",
            name,
            self.format_type(&var.ty),
            literal
        );
    }

    pub fn print_function(&mut self, name: &str, func_val_id: ValueId) {
        let func = self.module.get_func(func_val_id);
        println!("define {} @{}(", self.format_type(&func.ret_ty), name);
        for (i, arg_value_id) in func.params.iter().enumerate() {
            let arg = FunctionValue::resolve_param(*arg_value_id, self.module);
            if i != 0 {
                println!(", ");
            }
            print!("{} {}", self.format_type(&arg.ty), arg.name);
        }
        println!(") {{");
        for (bb_name, bb_val_id) in &func.bbs.bbs {
            self.print_block(bb_name, *bb_val_id);
        }
        println!("}}");
    }

    pub fn print_block(&mut self, name: &str, bb_val_id: ValueId) {
        let bb = self.module.get_bb(bb_val_id);
        println!("{}:", name);
        for inst_val_id in &bb.insts {
            let inst = BasicBlockValue::resolve_inst(*inst_val_id, self.module);
            self.print_inst(inst_val_id, inst);
        }
    }

    pub fn print_inst(&mut self, val_id: &ValueId, inst_val: &InstValue) {
        match inst_val {
            InstValue::InfixOp(_) => self.print_infix_op_inst(val_id, inst_val),
            InstValue::Load(inst) => self.print_load_inst(val_id, inst),
            InstValue::Store(inst) => self.print_store_inst(inst),
            InstValue::Alloca(inst) => self.print_alloca_inst(val_id, inst),
            InstValue::Branch(_) => todo!(),
            InstValue::Jump(_) => todo!(),
            InstValue::Gep(_) => todo!(),
            InstValue::Return(inst) => self.print_ret_inst(val_id, inst),
            InstValue::Call(_) => todo!(),
            InstValue::Phi(_) => todo!(),
            InstValue::Cast(_) => todo!(),
        }
    }

    pub fn print_store_inst(&mut self, inst: &StoreInst) {
        let src_val = Value::resolve(inst.value, self.module);
        let dst_val = Value::resolve(inst.ptr, self.module);
        let ty = Value::ty(dst_val);
        print!(
            "store {} {}, ptr {}",
            self.format_type(&ty),
            self.format_value(&inst.value, src_val),
            self.format_value(&inst.ptr, dst_val)
        );
        println!();
    }
    pub fn print_load_inst(&mut self, val_id: &ValueId, inst: &LoadInst) {
        let src_val = Value::resolve(inst.src, self.module);
        let ty = Value::ty(src_val);
        print!(
            "{} = load {}, ptr {}",
            self.resolve_name(&val_id),
            self.format_type(&ty),
            self.resolve_name(&inst.src)
        );
        println!();
    }
    pub fn print_infix_op_inst(&mut self, val_id: &ValueId, inst: &InstValue) {
        let inst = match inst {
            InstValue::InfixOp(inst) => inst,
            _ => unreachable!(),
        };
        let lhs_val = Value::resolve(inst.lhs, self.module);
        let rhs_val = Value::resolve(inst.rhs, self.module);
        let ty = Value::ty(lhs_val);
        print!(
            "{} = {} {} {}, {}",
            self.resolve_name(val_id),
            self.format_infix_op(&inst.op),
            self.format_type(&ty),
            self.format_value(&inst.lhs, lhs_val),
            self.format_value(&inst.rhs, rhs_val)
        );
        println!();
    }

    pub fn format_infix_op(&mut self, op: &InfixOp) -> String {
        match op {
            InfixOp::Add => "add".to_string(),
            InfixOp::Sub => "sub".to_string(),
            InfixOp::Mul => "mul".to_string(),
            InfixOp::Div => "sdiv".to_string(),
            InfixOp::Rem => "srem".to_string(),
            InfixOp::Eq => "icmp eq".to_string(),
            InfixOp::Ne => "icmp ne".to_string(),
            InfixOp::Lt => "icmp slt".to_string(),
            InfixOp::Le => "icmp sle".to_string(),
            InfixOp::Gt => "icmp sgt".to_string(),
            InfixOp::Ge => "icmp sge".to_string(),
            InfixOp::BitAnd => todo!(),
            InfixOp::BitOr => todo!(),
            InfixOp::BitXor => todo!(),
            InfixOp::BitShl => todo!(),
            InfixOp::BitShr => todo!(),
            InfixOp::LogicAnd => todo!(),
            InfixOp::LogicOr => todo!(),
            InfixOp::Mod => todo!(),
            InfixOp::Assign => unreachable!("assign should be built as a StoreInst"),
        }
    }

    pub fn print_alloca_inst(&self, val_id: &ValueId, inst: &AllocaInst) {
        print!(
            "{} = alloca {}",
            self.resolve_name(val_id),
            self.format_type(&inst.ty)
        );
        println!();
    }

    pub fn print_ret_inst(&mut self, val_id: &ValueId, inst: &ReturnInst) {
        if let Some(val_id) = &inst.value {
            let val = Value::resolve(*val_id, self.module);
            print!("ret {} {}", "i32", self.format_value(val_id, val));
        }
        println!();
    }

    pub fn format_value(&mut self, val_id: &ValueId, val: &Value) -> String {
        match val {
            Value::GlobalVariable(_) => self.resolve_name(val_id),
            Value::Function(_) => todo!(),
            Value::BasicBlock(_) => todo!(),
            // Value::Instruction(inst) => self.format_inst(val_id, inst),
            Value::Instruction(inst) => self.resolve_name(val_id),
            Value::Const(c) => self.format_const(c),
            Value::VariableValue(_) => todo!(),
        }
    }

    // pub fn format_global_variable(&mut self, g_val: &GlobalVariableValue) -> String {
    //     let ret = self.module.value_name(g_val.id);
    //     let name = format!("@{}", g_val.name);
    //     print!(
    //         "{} = load {}, ptr {}",
    //         ret,
    //         self.format_type(&g_val.ty),
    //         name
    //     );
    //     println!();
    //     ret
    // }

    pub fn format_inst(&mut self, val_id: &ValueId, inst: &InstValue) -> String {
        match inst {
            InstValue::Alloca(alloca) => self.format_alloca_inst(val_id, alloca),
            InstValue::Branch(_) => todo!(),
            InstValue::Call(_) => todo!(),
            InstValue::Cast(_) => todo!(),
            InstValue::Gep(_) => todo!(),
            InstValue::InfixOp(bo) => self.format_binary_op(val_id, bo),
            InstValue::Jump(_) => todo!(),
            InstValue::Load(_) => todo!(),
            InstValue::Phi(_) => todo!(),
            InstValue::Return(_) => todo!(),
            InstValue::Store(store) => self.format_store_inst(val_id, store),
        }
    }

    pub fn format_alloca_inst(&mut self, val_id: &ValueId, inst: &AllocaInst) -> String {
        let ret = self.resolve_name(val_id);
        print!("{} = alloca {}", ret, self.format_type(&inst.ty));
        println!();
        ret
    }

    pub fn format_store_inst(&mut self, val_id: &ValueId, inst: &StoreInst) -> String {
        let ret = self.resolve_name(val_id);
        let src_val = Value::resolve(inst.value, self.module);
        let dst_val = Value::resolve(inst.ptr, self.module);
        print!(
            "{} = store {} {}, {}",
            ret,
            self.format_type(&Value::ty(src_val)),
            self.format_value(&inst.value, src_val),
            self.format_value(&inst.ptr, dst_val)
        );
        println!();
        ret
    }

    pub fn resolve_name(&self, val_id: &ValueId) -> String {
        let name = self.module.value_name.get(val_id);
        if let Some(name) = name {
            return name.clone();
        }
        panic!("no name for value: {:?}", val_id)
    }

    pub fn format_binary_op(&mut self, val_id: &ValueId, bo: &BinaryOperator) -> String {
        let lhs_val = Value::resolve(bo.lhs, self.module);
        let rhs_val = Value::resolve(bo.rhs, self.module);
        let infix_op = match bo.op {
            InfixOp::Add => "add",
            _ => todo!(),
        }
        .to_string();
        let lhs = self.format_value(&bo.lhs, lhs_val);
        let rhs = self.format_value(&bo.rhs, rhs_val);
        let ret = self.resolve_name(val_id);
        print!(
            "{} = {} {} {}, {}",
            ret,
            infix_op,
            self.format_type(&bo.ty),
            lhs,
            rhs
        );
        println!();
        ret
    }

    pub fn format_const(&self, val: &ConstValue) -> String {
        match val {
            ConstValue::Int(i) => {
                format!("{}", i.value)
            }
            ConstValue::Float(_) => todo!(),
            ConstValue::Array(_) => todo!(),
        }
    }

    pub fn format_type(&self, ty: &Type) -> String {
        match ty {
            Type::Builtin(t) => self.format_builtin_type(t),
            Type::Pointer(_) => todo!(),
            Type::Array(t) => self.format_array_type(t),
            Type::Record(_) => todo!(),
            Type::Function(_) => todo!(),
        }
    }

    pub fn format_array_type(&self, ty: &ArrayType) -> String {
        match ty {
            ArrayType::Constant(t) => self.format_const_array_type(t),
            _ => todo!(),
        }
    }

    pub fn format_const_array_type(&self, ty: &ConstantArrayType) -> String {
        format!(
            "[{} x {}]",
            ty.size,
            self.format_type(ty.element_type.as_ref())
        )
    }

    pub fn format_builtin_type(&self, ty: &BuiltinType) -> String {
        match ty {
            BuiltinType::Void => "void".to_string(),
            BuiltinType::Bool => "i1".to_string(),
            BuiltinType::UChar => "i8".to_string(),
            BuiltinType::Char => "i8".to_string(),
            BuiltinType::UShort => "i16".to_string(),
            BuiltinType::Short => "i16".to_string(),
            BuiltinType::UInt => "i32".to_string(),
            BuiltinType::Int => "i32".to_string(),
            BuiltinType::UInt64 => "i64".to_string(),
            BuiltinType::Int64 => "i64".to_string(),
            BuiltinType::Float => "float".to_string(),
            BuiltinType::Double => "double".to_string(),
        }
    }
}
