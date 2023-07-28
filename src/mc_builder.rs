use std::collections::HashMap;

use crate::{
    ast::{Param, Type},
    ir::*,
    mc::*,
    mc_inst::{self, AsmInst, AsmInstTrait, BinOpInst, PrologueInst, StackOpInstTrait, VLDRInst},
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

    vreg_idx: i32,
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
            ConstValue::Int(i) => Imm::Int(IntImm {
                value: i.value as u32,
            }),
            ConstValue::Float(f) => Imm::Float(FloatImm {
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
            vreg_idx: 0,
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

            self.bb_map.insert(block_id.clone(), asm_block_id.clone());
        }

        let prologue = PrologueInst::new(asm_func_id.clone());
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
                    base_type: AsmTypeTag::from(param_val.ty.base_type().clone()),
                    is_pointer: param_val.ty.is_pointer(false),
                }
            })
            .collect();
        if f.is_variadic {
            // 变参函数需要在调用处临时计算CallCVfpCallConv
            // ret = BaseCallCVfpCallConv::new().resolve(&params, f.ret_type);
            unimplemented!();
        } else {
            ret = VfpCallConv::new().resolve(&params, AsmTypeTag::from(f.ret_ty.clone()));
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
            return AsmOperand::Imm(Imm::from(cv.clone()));
        }

        // IR那边GlobalVariable直接引用也代表地址，所以不用Load
        if let Value::GlobalVariable(gv) = v {
            let asmgv_id = self.gv_map.get(&valud_id).unwrap();
            let asmgv = self.module.get_global_variable(*asmgv_id);
            return AsmOperand::Imm(Imm::Label(asmgv.imm.clone()));
        }

        let ret = VirtReg {
            index: self.vreg_idx,
            is_float: *v.ty().base_type() == crate::ast::BuiltinType::Float.into(),
        };
        self.vreg_map.insert(valud_id, ret.clone().into());

        // 如果是参数且在内存中，则生成load指令
        // 使用CallCVfpCallConv的解析结果。
        if let Value::VariableValue(pv) = v {
            let arg_idx = ssa_func
                .params
                .iter()
                .position(|arg_id| *arg_id == valud_id)
                .unwrap();
            let cc = self.get_cc(&ssa_func_id); // 有函数体的必然不是vararg的。
            let loc = cc.self_arg[arg_idx].clone();
            assert!(loc.is_stack_operand()); // 其他的应该在前面就取到了vreg。
            if let AsmOperand::StackOperand(_) = loc {
                // 生成Load指令加载内存里的值到虚拟寄存器里。
                let load = if *pv.ty.base_type() == Type::Builtin(crate::ast::BuiltinType::Float) {
                    self.module
                        .alloc_value(AsmValue::Inst(mc_inst::AsmInst::VLDR(
                            mc_inst::VLDRInst::new(ret.clone().into(), loc),
                        )))
                } else {
                    self.module
                        .alloc_value(AsmValue::Inst(mc_inst::AsmInst::LDR(
                            mc_inst::LDRInst::new(ret.clone().into(), loc),
                        )))
                };
                if before_jump {
                    let new_insts = self.expand_stack_operand_load_store(load);
                    self.module.add_all_before_branch(asm_bb_id, new_insts);
                } else {
                    let new_insts = self.expand_stack_operand_load_store(load);
                    let abb = self.module.get_bb_mut(asm_bb_id);
                    abb.insts.extend(new_insts);
                }
            }
        }

        // 增加注释便于Debug
        // ret.comment = v.name().to_string();
        AsmOperand::VirtReg(ret)
    }
    // Check whether the second argument StackOperand meets the requirements, if not, expand it into multiple instructions
    // Load dst, addr
    // Store val, addr
    fn expand_stack_operand_load_store(&mut self, inst_id: AsmValueId) -> Vec<AsmValueId> {
        let mut ret = Vec::new();
        let mut newuse = Vec::new();

        let mut inst = self.module.get_inst(inst_id).clone();
        assert!(inst.is_ldr() || inst.is_str() || inst.is_vstr() || inst.is_vldr());

        if inst.is_str() || inst.is_vstr() {
            newuse.push(inst.get_uses()[0].clone());
            self.expand_stack_operand(inst_id, &inst.get_uses()[1], &mut newuse, &mut ret);
        } else if inst.is_ldr() || inst.is_vldr() {
            self.expand_stack_operand(inst_id, &inst.get_uses()[0], &mut newuse, &mut ret);
        }
        inst.set_uses(newuse);
        self.module.set_inst(inst_id, inst);
        ret.push(inst_id);
        ret
    }

    fn expand_cmp_imm(&mut self, inst_id: AsmValueId) -> Vec<AsmValueId> {
        let mut inst = self.module.get_inst_mut(inst_id).clone();
        let mut ret = Vec::new();
        let mut newuse = Vec::new();
        self.expand_imm(&inst.get_uses()[0], &mut newuse, &mut ret);
        self.expand_operand2(&inst.get_uses()[1], &mut newuse, &mut ret);
        inst.set_uses(newuse);
        self.module.set_inst(inst_id, inst);
        ret.push(inst_id);
        ret
    }

    fn expand_inst_imm(&mut self, inst_id: AsmValueId) -> Vec<AsmValueId> {
        let mut inst = self.module.get_inst_mut(inst_id).clone();
        let mut ret = Vec::new();
        let mut newuse = Vec::new();
        for op in &inst.get_uses() {
            self.expand_imm(op, &mut newuse, &mut ret);
        }
        inst.set_uses(newuse);
        self.module.set_inst(inst_id, inst);
        ret.push(inst_id);
        ret
    }

    fn expand_imm(
        &mut self,
        op: &AsmOperand,
        new_ops: &mut Vec<AsmOperand>,
        insts: &mut Vec<AsmValueId>,
    ) {
        if let AsmOperand::Imm(imm) = op {
            let tmp = self.get_vreg(op.is_float());
            insts.extend(self.module.load_imm(tmp.clone().into(), &imm));
            new_ops.push(tmp.into());
        } else {
            new_ops.push((*op).clone());
        }
    }

    fn expand_stack_operand(
        &mut self,
        inst_id: AsmValueId,
        op: &AsmOperand,
        new_ops: &mut Vec<AsmOperand>,
        insts: &mut Vec<AsmValueId>,
    ) {
        if let AsmOperand::StackOperand(so) = op {
            let inst = self.module.get_inst(inst_id);
            if inst.is_imm_fit(so) {
                new_ops.push(AsmOperand::StackOperand(so.clone()));
                return;
            }
            let tmp = AsmOperand::VirtReg(self.get_vreg(false));
            let tmp2 = AsmOperand::VirtReg(self.get_vreg(false));
            assert!(so.ty != StackOperandType::Spill);
            let id = match so.ty {
                StackOperandType::SelfArg => {
                    insts.extend(
                        self.module
                            .load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                    );
                    let binop = mc_inst::BinOpInst::new(
                        mc_inst::BinaryOp::Add,
                        tmp2.clone(),
                        IntReg::new(RegType::Fp).into(),
                        tmp,
                    );
                    let new_inst = AsmValue::Inst(mc_inst::AsmInst::BinOp(binop));
                    self.module.alloc_value(new_inst)
                }
                StackOperandType::Local => {
                    insts.extend(
                        self.module
                            .load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                    );
                    let binop = mc_inst::BinOpInst::new(
                        mc_inst::BinaryOp::Sub,
                        tmp2.clone(),
                        IntReg::new(RegType::Fp).into(),
                        tmp,
                    );
                    let new_inst = AsmValue::Inst(mc_inst::AsmInst::BinOp(binop));
                    self.module.alloc_value(new_inst)
                }
                StackOperandType::CallParam => {
                    insts.extend(
                        self.module
                            .load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                    );
                    let binop = mc_inst::BinOpInst::new(
                        mc_inst::BinaryOp::Add,
                        tmp2.clone(),
                        IntReg::new(RegType::Sp).into(),
                        tmp,
                    );
                    let new_inst = AsmValue::Inst(mc_inst::AsmInst::BinOp(binop));
                    self.module.alloc_value(new_inst)
                }
                _ => panic!("Unsupported operation"),
            };
            insts.push(id);
            new_ops.push(tmp2);
        } else {
            new_ops.push((*op).clone());
        }
    }
    // Flexible Operand 2 can be Imm8m
    fn expand_operand2(
        &mut self,
        op: &AsmOperand,
        new_ops: &mut Vec<AsmOperand>,
        insts: &mut Vec<AsmValueId>,
    ) {
        match op {
            AsmOperand::Imm(immop) => {
                if immop.highest_one_bit() < 255 {
                    new_ops.push(AsmOperand::Imm(immop.clone()));
                    return;
                }
                let tmp = AsmOperand::VirtReg(self.get_vreg(false));
                insts.extend(self.module.load_imm(tmp.clone(), immop));
                new_ops.push(tmp);
            }
            _ => {
                new_ops.push((*op).clone());
            }
        }
    }

    pub fn expand_bin_op(&mut self, bin_id: AsmValueId) -> Vec<AsmValueId> {
        let mut bin_inst = self
            .module
            .get_inst_mut(bin_id)
            .as_bin_op()
            .unwrap()
            .clone();
        let mut ret = Vec::new();
        let mut op1 = bin_inst.get_uses()[0].clone();
        let mut op2 = bin_inst.get_uses()[1].clone();
        if let AsmOperand::Imm(imm) = op1.clone() {
            let tmp = AsmOperand::VirtReg(self.get_vreg(op1.is_float()));
            ret.extend(self.module.load_imm(tmp.clone(), &imm));
            op1 = tmp;
        }
        match op2.clone() {
            AsmOperand::Imm(imm) => {
                if !matches!(bin_inst.op, mc_inst::BinaryOp::Add | mc_inst::BinaryOp::Sub)
                    || imm.highest_one_bit() >= 255
                {
                    let tmp = AsmOperand::VirtReg(self.get_vreg(op2.is_float()));
                    ret.extend(self.module.load_imm(tmp.clone(), &imm));
                    op2 = tmp;
                }
            }
            _ => (),
        }
        bin_inst.set_uses(vec![op1, op2]);
        self.module.set_inst(bin_id, bin_inst.into());
        ret.push(bin_id);
        ret
    }

    // 检查第二个参数StackOperand是否满足要求，不满足则展开为多个指令
    // 给寄存器分配使用的公开版本
    pub fn expand_stack_operand_load_store_ip(&mut self, inst_id: AsmValueId) -> Vec<AsmValueId> {
        let inst = self.module.get_inst_mut(inst_id).clone();
        let mut ret = Vec::<AsmValueId>::new();
        let mut newuse = Vec::new();
        assert!(matches!(
            inst,
            AsmInst::LDR(_) | AsmInst::STR(_) | AsmInst::VSTR(_) | AsmInst::VLDR(_)
        ));
        match inst {
            AsmInst::STR(_) | AsmInst::VSTR(_) => {
                newuse.push(inst.get_uses()[0].clone());
                self.expand_stack_operand_ip(
                    inst_id,
                    &inst.get_uses()[1],
                    &inst.get_uses()[0],
                    &mut newuse,
                    &mut ret,
                );
            }
            AsmInst::LDR(_) | AsmInst::VLDR(_) => {
                self.expand_stack_operand_ip(
                    inst_id,
                    &inst.get_uses()[0],
                    &inst.get_defs()[0],
                    &mut newuse,
                    &mut ret,
                );
            }
            _ => (),
        }
        // in case of using a old value
        let mut inst = self.module.get_inst_mut(inst_id).clone();
        inst.set_uses(newuse);
        self.module.set_inst(inst_id, inst);
        ret.push(inst_id);
        ret
    }

    pub fn expand_stack_operand_ip(
        &mut self,
        inst_id: AsmValueId,
        op: &AsmOperand,
        target: &AsmOperand,
        new_ops: &mut Vec<AsmOperand>,
        insts: &mut Vec<AsmValueId>,
    ) {
        let inst = self.module.get_inst(inst_id).clone();
        match op {
            AsmOperand::StackOperand(so) => {
                if inst.is_imm_fit(so) {
                    new_ops.push(AsmOperand::StackOperand(so.clone()));
                    return;
                }
                assert!(matches!(
                    target,
                    AsmOperand::IntReg(_) | AsmOperand::VfpReg(_)
                ));
                let tmp = AsmOperand::IntReg(IntReg::new(RegType::Ip));
                let tmp2 = AsmOperand::IntReg(IntReg::new(RegType::Ip));
                match so.ty {
                    StackOperandType::SelfArg => {
                        insts.extend(
                            self.module
                                .load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Add,
                            tmp2.clone(),
                            AsmOperand::IntReg(IntReg::new(RegType::Fp)),
                            tmp.clone(),
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.module.alloc_value(inst);

                        insts.push(id);
                    }
                    StackOperandType::Local | StackOperandType::Spill => {
                        insts.extend(
                            self.module
                                .load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Sub,
                            tmp2.clone(),
                            AsmOperand::IntReg(IntReg::new(RegType::Fp)),
                            tmp.clone(),
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.module.alloc_value(inst);

                        insts.push(id);
                    }
                    StackOperandType::CallParam => {
                        insts.extend(
                            self.module
                                .load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Add,
                            tmp2.clone(),
                            AsmOperand::IntReg(IntReg::new(RegType::Sp)),
                            tmp.clone(),
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.module.alloc_value(inst);

                        insts.push(id);
                    }
                    _ => panic!("Unsupported operation"),
                }
                new_ops.push(tmp2);
            }
            _ => {
                new_ops.push((*op).clone());
            }
        }
    }
    // 使用临时寄存器的场景
    fn get_vreg(&mut self, is_float: bool) -> VirtReg {
        let ret = VirtReg::new(self.vreg_idx, is_float);
        self.vreg_idx += 1;
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
