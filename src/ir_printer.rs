use crate::{ast::*, ir::*};

pub fn print(module: &Module) {
    let printer = Printer { module };
    printer.print_module();
}

struct Printer<'a> {
    module: &'a Module,
}

impl<'a> Printer<'a> {
    pub fn print_module(&self) {
        for (name, func_val_id) in &self.module.functions {
            self.print_function(name, func_val_id.to_owned());
        }
    }

    pub fn print_function(&self, name: &str, func_val_id: ValueId) {
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

    pub fn print_block(&self, name: &str, bb_val_id: ValueId) {
        let bb = self.module.get_bb(bb_val_id);
        println!("{}:", name);
        for inst_val_id in &bb.insts {
            let inst = BasicBlockValue::resolve_inst(*inst_val_id, self.module);
            self.print_inst(inst);
        }
    }

    pub fn print_inst(&self, inst_val: &InstValue) {
        match inst_val {
            InstValue::InfixOp(_) => todo!(),
            InstValue::Load(_) => todo!(),
            InstValue::Store(_) => todo!(),
            InstValue::Alloca(_) => todo!(),
            InstValue::Branch(_) => todo!(),
            InstValue::Jump(_) => todo!(),
            InstValue::Gep(_) => todo!(),
            InstValue::Return(inst) => self.print_ret_inst(inst),
            InstValue::Call(_) => todo!(),
            InstValue::Phi(_) => todo!(),
            InstValue::Cast(_) => todo!(),
        }
    }

    pub fn print_ret_inst(&self, inst: &ReturnInst) {
        print!("ret ");
        if let Some(val_id) = &inst.value {
            let val = Value::resolve(*val_id, self.module);
            print!("{}", self.format_value(val));
        }
        println!();
    }

    pub fn format_value(&self, val: &Value) -> String {
        match val {
            Value::GlobalVariable(_) => todo!(),
            Value::Function(_) => todo!(),
            Value::BasicBlock(_) => todo!(),
            Value::Instruction(_) => todo!(),
            Value::Const(c) => self.format_const(c),
            Value::ParameterValue(_) => todo!(),
        }
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
            Type::Array(_) => todo!(),
            Type::Record(_) => todo!(),
            Type::Function(_) => todo!(),
        }
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
