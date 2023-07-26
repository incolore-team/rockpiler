use std::collections::HashMap;

use crate::{
    ast::{Param, Type},
    ir::*,
    mc::*,
    mc_inst::VLDRInst,
};

pub fn build(module: &mut Module) -> AsmModule {
    let mut builder = McBuilder::new(module);
    builder.build_module();
    builder.module
}

struct McBuilder<'a> {
    ir_module: &'a Module,
    module: AsmModule,
    // ir func -> call conv
    vfp_callconv_map: HashMap<ValueId, VfpCallConv>,
    // ir bb -> asm bb
    bb_map: HashMap<ValueId, AsmValueId>,
    // ir func -> asm func
    func_map: HashMap<ValueId, AsmValueId>,
    // ir gv -> asm gv
    gv_map: HashMap<ValueId, AsmValueId>,
    // ir value -> vreg
    vreg_map: HashMap<ValueId, AsmOperand>,
}

impl Into<AsmGlobalVariable> for GlobalVariableValue {
    fn into(self) -> AsmGlobalVariable {
        todo!()
    }
}

impl Into<AsmFunction> for FunctionValue {
    fn into(self) -> AsmFunction {
        todo!()
    }
}

impl Into<AsmBlock> for BasicBlockValue {
    fn into(self) -> AsmBlock {
        todo!()
    }
}

impl From<ConstValue> for Imm {
    fn from(value: ConstValue) -> Self {
        match value {
            ConstValue::Int(i) => Imm::IntImm(IntImm {
                value: i.value as u32,
            }),
            ConstValue::Float(f) => Imm::FloatImm(FloatImm {
                value: f.value as f32,
                is_float: true,
            }),
            _ => unimplemented!(),
        }
    }
}

impl From<Type> for AsmTypeTag {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Builtin(b) => match b {
                crate::ast::BuiltinType::Void => AsmTypeTag::VOID,
                crate::ast::BuiltinType::Bool => AsmTypeTag::BOOL,
                crate::ast::BuiltinType::UChar => AsmTypeTag::CHAR,
                crate::ast::BuiltinType::Char => AsmTypeTag::CHAR,
                crate::ast::BuiltinType::UShort => todo!(),
                crate::ast::BuiltinType::Short => todo!(),
                crate::ast::BuiltinType::UInt => AsmTypeTag::INT32,
                crate::ast::BuiltinType::Int => AsmTypeTag::INT32,
                crate::ast::BuiltinType::UInt64 => todo!(),
                crate::ast::BuiltinType::Int64 => todo!(),
                crate::ast::BuiltinType::Float => AsmTypeTag::FLOAT,
                crate::ast::BuiltinType::Double => AsmTypeTag::DOUBLE,
            },
            Type::Pointer(_) => todo!(),
            Type::Array(_) => todo!(),
            Type::Record(_) => todo!(),
            Type::Function(_) => todo!(),
        }
    }
}

impl McBuilder<'_> {
    fn new<'a>(ir_module: &'a Module) -> McBuilder<'a> {
        McBuilder {
            ir_module,
            module: AsmModule::new(),
            vfp_callconv_map: HashMap::new(),
            bb_map: HashMap::new(),
            func_map: HashMap::new(),
            gv_map: HashMap::new(),
            vreg_map: HashMap::new(),
        }
    }

    fn build_module(&mut self) {
        self.build_global_variables();
        self.build_functions();
    }

    fn build_global_variables(&mut self) {
        for (name, id) in &self.ir_module.global_variables.clone() {
            let global = self.ir_module.get_global_var(*id);
            let val = AsmGlobalVariable::from(global.clone().into());
            let val_id = self.module.alloc_value(AsmValue::GlobalVariable(val));
            if global.initializer.is_some() {
                self.module.bss_globals.push(val_id);
            } else {
                self.module.globals.push(val_id);
            }
        }
    }

    fn build_functions(&mut self) {
        for (_, func_id) in &self.ir_module.functions {
            self.build_function(func_id);
        }
    }

    fn build_function(&mut self, func_id: &ValueId) {
        let func = self.ir_module.get_func(*func_id);
        let asm_func = AsmFunction::from(func.clone().into());
        let asm_func_id = self.module.alloc_value(AsmValue::Function(asm_func));
        self.module.functions.push(asm_func_id);

        for (name, block_id) in &func.bbs.bbs.clone() {
            let block = self.ir_module.get_bb(*block_id);
            let asm_block = AsmBlock::from(block.clone().into());
            let asm_block_id = self.module.alloc_value(AsmValue::Block(asm_block));
            self.module.cur_func_mut().bbs.push(asm_block_id);
        }
        // TODO: prologue
        // TODO: handle callling convention
    }
    fn get_cc(&mut self, func_id: &ValueId) -> VfpCallConv {
        let f = self.ir_module.get_func(*func_id);
        if self.vfp_callconv_map.contains_key(func_id) {
            return self.vfp_callconv_map.get(func_id).unwrap().clone();
        }

        let ret: VfpCallConv;
        let params: Vec<ParamInfo> = f
            .params
            .clone()
            .iter()
            .map(|v_id| {
                let param_val = FunctionValue::resolve_param(*v_id, self.ir_module);
                ParamInfo {
                    base_type: AsmTypeTag::from(*param_val.ty.base_type()),
                    is_pointer: param_val.ty.is_pointer(false),
                }
            })
            .collect();
        if f.is_variadic {
            // 变参函数需要在调用处临时计算CallCVfpCallConv
            // ret = BaseCallCVfpCallConv::new().resolve(&params, f.ret_type);
            unimplemented!();
        } else {
            ret = VfpCallConv::new().resolve(&params, AsmTypeTag::from(f.ret_ty));
        }

        self.vfp_callconv_map.insert(*func_id, ret.clone());
        ret
    }

    // 仅当处理phi指令的时候的部分情况需要 before_jump = true
    fn convert_value(&mut self, valud_id: ValueId) -> AsmOperand {
        self.convert_value_with_before_jump(valud_id, false)
    }

    // 对指令返回的值分配Vreg
    // 对其他Value进行转换
    // 该函数还需要为内存中的参数生成必要的Load指令
    fn convert_value_with_before_jump(
        &mut self,
        valud_id: ValueId,
        before_jump: bool,
    ) -> AsmOperand {
        let asm_func_id = self.module.cur_func_value_id();
        let ssa_func_id = self.ir_module.cur_func_value_id();
        let asm_bb_id = self.module.cur_bb_value_id();
        let ssa_bb_id = self.ir_module.cur_bb_value_id();
        let ssa_func = self.ir_module.get_func(ssa_func_id);

        if self.vreg_map.contains_key(&valud_id) {
            return self.vreg_map.get(&valud_id).unwrap().clone();
        }

        // BasicBlovkValue，FuncValue，在对应的指令预先判断处理。
        let v = self.ir_module.get_value(valud_id);
        if let Value::BasicBlock(_) | Value::Function(_) = v {
            unimplemented!();
        }

        // 如果是ConstantValue则需要转为Imm
        if let Value::Const(cv) = v {
            assert!(!cv.is_array());
            return AsmOperand::Imm(Imm::from(*cv));
        }

        // IR那边GlobalVariable直接引用也代表地址，所以不用Load
        if let Value::GlobalVariable(gv) = v {
            return self.gv_map.get(&valud_id).unwrap().imm.clone();
        }

        let mut ret = VirtReg::new(self.vreg_ind, v.type_().is_base_float());
        if v.type_().is_base_float() {
            ret.is_float = true;
        }
        self.vreg_map.insert(v.clone(), ret.clone());

        // 如果是参数且在内存中，则生成load指令
        // 使用CallCVfpCallConv的解析结果。
        if let Value::VariableValue(pv) = v {
            let index = ssa_func
                .params
                .iter()
                .position(|arg_id| *arg_id == valud_id)
                .unwrap();
            let cc = self.get_cc(&ssa_func_id); // 有函数体的必然不是vararg的。
            let loc = cc.self_arg[index].clone();
            assert!(loc.is_stack_operand()); // 其他的应该在前面就取到了vreg。
            if let AsmOperand::StackOperand(stack_op) = loc {
                // 生成Load指令加载内存里的值到虚拟寄存器里。
                // let load: Box<dyn Inst> = if pv.type_().is_base_float() {
                //     Box::new(VLDRInst::new(abb.clone(), ret.clone(), loc))
                // } else {
                //     Box::new(LoadInst::new(abb.clone(), ret.clone(), loc))
                // };

                // if before_jump {
                //     abb.add_all_before_jump(expand_stack_operand_load_store(load));
                // } else {
                //     abb.insts.extend(expand_stack_operand_load_store(load));
                // }
            }
        }

        // 增加注释便于Debug
        ret.comment = v.name().to_string();
        AsmOperand::VirtReg(ret)
    }

    // 使用临时寄存器的场景
    fn get_vreg(&mut self, is_float: bool) -> VirtReg {
        let ret = VirtReg::new(self.vreg_ind, is_float);
        self.vreg_ind += 1;
        ret
    }
    // fn build_basic_blocks(&mut self, blocks: &Vec<BasicBlock>) -> Vec<asmBasicBlock> {
    //     let mut asm_blocks = Vec::new();
    //     for block in blocks {
    //         asm_blocks.push(self.build_basic_block(block));
    //     }
    //     asm_blocks
    // }

    // fn build_basic_block(&mut self, block: &BasicBlock) -> asmBasicBlock {
    //     let mut asm_block = asmBasicBlock::new();
    //     asm_block.name = block.name.clone();
    //     asm_block.instructions = self.build_instructions(&block.instructions);
    //     asm_block
    // }

    // fn build_instructions(&mut self, instructions: &Vec<Instruction>) -> Vec<asmInstruction> {
    //     let mut asm_instructions = Vec::new();
    //     for instruction in instructions {
    //         asm_instructions.push(self.build_instruction(instruction));
    //     }
    //     asm_instructions
    // }

    // fn build_instruction(&mut self, instruction: &Instruction) -> asmInstruction {
    //     let mut asm_instruction = asmInstruction::new();
    //     asm_instruction.opcode = instruction.opcode.clone();
    //     asm_instruction.operands = self.build_operands(&instruction.operands);
    //     asm_instruction
    // }

    // fn build_operands(&mut self, operands: &Vec<Operand>) -> Vec<asmOperand> {
    //     let mut asm_operands = Vec::new();
    //     for operand in operands {
    //         asm_operands.push(self.build_operand(operand));
    //     }
    //     asm_operands
    // }

    // fn build_operand(&mut self, operand: &Operand) -> asmOperand {
    //     todo!()
    // }
}
