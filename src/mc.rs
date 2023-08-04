pub enum AsmValue {
    Block(AsmBlock),
    GlobalVariable(AsmGlobalVariable),
    Function(AsmFunction),
    Inst(AsmInst),
}

pub type AsmValueId = id_arena::Id<AsmValue>;
/// PhyReg -> VirtReg (General or VFP)
pub type RegConstraintMap = std::collections::HashMap<AsmOperand, VirtReg>;

pub struct AsmModule {
    values: id_arena::Arena<AsmValue>,
    pub globals: Vec<AsmValueId>,
    pub bss_globals: Vec<AsmValueId>, // uninitialized globals
    pub funcs: Vec<AsmValueId>,

    cur_func: Option<AsmValueId>,
    cur_bb: Option<AsmValueId>,
}

impl AsmModule {
    pub fn set_cur_func(&mut self, func_id: AsmValueId) {
        self.cur_func = Some(func_id);
    }

    pub fn set_cur_bb(&mut self, bb_id: AsmValueId) {
        self.cur_bb = Some(bb_id);
    }

    pub fn cur_func_mut(&mut self) -> &mut AsmFunction {
        if let AsmValue::Function(func) = self.values.get_mut(self.cur_func.unwrap()).unwrap() {
            func
        } else {
            panic!("current value is not a function");
        }
    }

    pub fn cur_func(&self) -> &AsmFunction {
        if let AsmValue::Function(func) = self.values.get(self.cur_func.unwrap()).unwrap() {
            func
        } else {
            panic!("current value is not a function");
        }
    }

    pub fn cur_bb_mut(&mut self) -> &mut AsmBlock {
        let cur_bb = self.cur_bb.unwrap();
        if let AsmValue::Block(bb) = self.values.get_mut(cur_bb).unwrap() {
            bb
        } else {
            panic!("current value is not a block");
        }
    }

    pub fn cur_func_value_id(&self) -> AsmValueId {
        self.cur_func.unwrap()
    }

    pub fn cur_bb_value_id(&self) -> AsmValueId {
        self.cur_bb.unwrap()
    }

    pub fn cur_bb(&self) -> &AsmBlock {
        let cur_bb = self.cur_bb.unwrap();
        if let AsmValue::Block(bb) = self.values.get(cur_bb).unwrap() {
            bb
        } else {
            panic!("current value is not a block");
        }
    }

    pub fn get_global_variable(&self, id: AsmValueId) -> &AsmGlobalVariable {
        if let AsmValue::GlobalVariable(global) = self.values.get(id).unwrap() {
            global
        } else {
            panic!("value is not a global variable");
        }
    }

    pub fn get_bb_mut(&mut self, id: AsmValueId) -> &mut AsmBlock {
        if let AsmValue::Block(bb) = self.values.get_mut(id).unwrap() {
            bb
        } else {
            panic!("value is not a block");
        }
    }

    pub fn get_bb(&self, id: AsmValueId) -> &AsmBlock {
        if let AsmValue::Block(bb) = self.values.get(id).unwrap() {
            bb
        } else {
            panic!("value is not a block");
        }
    }

    pub fn get_inst(&self, id: AsmValueId) -> &AsmInst {
        if let AsmValue::Inst(inst) = self.values.get(id).unwrap() {
            inst
        } else {
            panic!("value is not an instruction");
        }
    }

    pub fn get_inst_mut(&mut self, id: AsmValueId) -> &mut AsmInst {
        if let AsmValue::Inst(inst) = self.values.get_mut(id).unwrap() {
            inst
        } else {
            panic!("value is not an instruction");
        }
    }

    pub fn set_inst(&mut self, id: AsmValueId, new_inst: AsmInst) {
        if let AsmValue::Inst(inst) = self.values.get_mut(id).unwrap() {
            *inst = new_inst;
        } else {
            panic!("value is not an instruction");
        }
    }

    pub fn get_func(&self, id: AsmValueId) -> &AsmFunction {
        if let AsmValue::Function(func) = self.values.get(id).unwrap() {
            func
        } else {
            panic!("value is not a function");
        }
    }

    pub fn get_func_mut(&mut self, id: AsmValueId) -> &mut AsmFunction {
        if let AsmValue::Function(func) = self.values.get_mut(id).unwrap() {
            func
        } else {
            panic!("value is not a function");
        }
    }

    pub fn load_imm(&mut self, reg: AsmOperand, imm: &Imm) -> Vec<AsmValueId> {
        let mut ret = Vec::new();
        if let Imm::Float(fimm) = imm {
            let tmp = Reg::new(RegType::Ip);
            ret.extend(self.load_imm(tmp.clone().into(), &Imm::Int(fimm.cast_to_raw_int().into())));
            let inst: AsmInst = VMovInst::new(VMovType::A2S, reg, tmp.into()).into();
            let inst_id = self.values.alloc(AsmValue::Inst(inst));
            ret.push(inst_id);
        } else if let Imm::Label(_) | Imm::Int(_) = imm {
            if imm.highest_one_bit() < 65535 {
                // ret.push(MovInst::new(MovType::Movw, reg.clone(), imm.clone().into()).into());
                let inst: AsmInst =
                    MovInst::new(MovType::Movw, reg, imm.clone().into(), None).into();
                let inst_id = self.values.alloc(AsmValue::Inst(inst));
                ret.push(inst_id);
            } else {
                // ret.push(MovInst::new(MovType::Movw, reg.clone(), imm.lowest_word().into()).into());
                let inst: AsmInst =
                    MovInst::new(MovType::Movw, reg.clone(), imm.lowest_word().into(), None).into();
                let inst_id = self.values.alloc(AsmValue::Inst(inst));
                ret.push(inst_id);
                // ret.push(MovInst::new(MovType::Movt, reg.clone(), imm.highest_word().into()).into(),);
                let inst: AsmInst =
                    MovInst::new(MovType::Movt, reg, imm.highest_word().into(), None).into();
                let inst_id = self.values.alloc(AsmValue::Inst(inst));
                ret.push(inst_id);
            }
        } else {
            panic!("Unsupported immediate type.");
        }
        ret
    }

    pub fn add_all_before_branch(&mut self, bb_id: AsmValueId, insts: Vec<AsmValueId>) {
        let mut i = 0;
        let bb_cloned = self.get_bb(bb_id).clone();
        while i < bb_cloned.insts.len() {
            let vid = bb_cloned.insts[i];
            let v = self.values.get(vid).unwrap();
            if let AsmValue::Inst(inst) = v {
                if let AsmInst::Br(_) = inst {
                    break;
                }
            }
            i += 1;
        }
        let mut cloned_old = bb_cloned.insts.clone();
        cloned_old.splice(i..i, insts);

        let bb = self.get_bb_mut(bb_id);
        bb.insts = cloned_old;
    }

    pub fn expand_bin_op_ip(&mut self, bin_id: AsmValueId) -> Vec<AsmValueId> {
        let mut bin_inst = self.get_inst_mut(bin_id).as_bin_op().unwrap().clone();
        let mut ret = Vec::new();
        let mut op1 = bin_inst.get_uses()[0].clone();
        let mut op2 = bin_inst.get_uses()[1].clone();
        let mut ip_used = false;
        if let AsmOperand::Imm(imm) = op1.clone() {
            assert!(!ip_used);
            ip_used = true;
            let tmp = AsmOperand::IntReg(Reg::new(RegType::Ip));
            ret.extend(self.load_imm(tmp.clone(), &imm));
            op1 = tmp;
        }
        match op2.clone() {
            AsmOperand::Imm(imm) => {
                if !matches!(bin_inst.op, mc_inst::BinaryOp::Add | mc_inst::BinaryOp::Sub)
                    || imm.highest_one_bit() >= 255
                {
                    assert!(!ip_used);
                    // ip_used = true;
                    let tmp = AsmOperand::IntReg(Reg::new(RegType::Ip));
                    ret.extend(self.load_imm(tmp.clone(), &imm));
                    op2 = tmp;
                }
            }
            _ => (),
        }
        bin_inst.set_uses(vec![op1, op2]);
        self.set_inst(bin_id, bin_inst.into());
        ret.push(bin_id);
        ret
    }

    // 检查第二个参数StackOperand是否满足要求，不满足则展开为多个指令
    // 给寄存器分配使用的公开版本
    pub fn expand_stack_operand_load_store_ip(&mut self, inst_id: AsmValueId) -> Vec<AsmValueId> {
        let inst = self.get_inst_mut(inst_id).clone();
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
        let mut inst = self.get_inst_mut(inst_id).clone();
        inst.set_uses(newuse);
        self.set_inst(inst_id, inst);
        ret.push(inst_id);
        ret
    }

    pub fn expand_stack_operand_load_store_tmp(
        &mut self,
        inst: AsmInst,
        tmp: AsmOperand,
    ) -> VecDeque<AsmInst> {
        let mut ret = VecDeque::new();
        let mut new_use = Vec::new();
        assert!(matches!(
            inst,
            AsmInst::LDR(_) | AsmInst::STR(_) | AsmInst::VSTR(_) | AsmInst::VLDR(_)
        ));
        match inst {
            AsmInst::STR(_) | AsmInst::VSTR(_) => {
                new_use.push(inst.get_uses()[0].clone());
                // For StoreInst and VSTRInst, value register can't be occupied, a temporary register must be used if situation occurs
                self.expand_stack_operand_tmp(
                    &mut inst,
                    &inst.get_uses()[1], // possible StackOperand
                    &inst.get_uses()[0],
                    &tmp,
                    &mut new_use,
                    &mut ret,
                    &inst.parent,
                );
            }
            AsmInst::LDR(_) | AsmInst::VLDR(_) => {
                // For LoadInst and VLDRInst, no additional register is needed, as long as there is a target register
                self.expand_stack_operand_tmp(
                    &mut inst,
                    &inst.uses[0],
                    &inst.defs[0],
                    &tmp,
                    &mut new_use,
                    &mut ret,
                    &inst.parent,
                );
            }
            _ => (),
        }
        inst.uses = new_use;
        ret.push_back(inst);
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
        let inst = self.get_inst(inst_id).clone();
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
                let tmp = AsmOperand::IntReg(Reg::new(RegType::Ip));
                let tmp2 = AsmOperand::IntReg(Reg::new(RegType::Ip));
                match so.ty {
                    StackOperandType::SelfArg => {
                        insts.extend(
                            self.load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Add,
                            tmp2.clone(),
                            AsmOperand::IntReg(Reg::new(RegType::Fp)),
                            tmp,
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.alloc_value(inst);

                        insts.push(id);
                    }
                    StackOperandType::Local | StackOperandType::Spill => {
                        insts.extend(
                            self.load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Sub,
                            tmp2.clone(),
                            AsmOperand::IntReg(Reg::new(RegType::Fp)),
                            tmp,
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.alloc_value(inst);

                        insts.push(id);
                    }
                    StackOperandType::CallParam => {
                        insts.extend(
                            self.load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Add,
                            tmp2.clone(),
                            AsmOperand::IntReg(Reg::new(RegType::Sp)),
                            tmp,
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.alloc_value(inst);

                        insts.push(id);
                    }
                }
                new_ops.push(tmp2);
            }
            _ => {
                new_ops.push((*op).clone());
            }
        }
    }

    // 指定临时寄存器
    // public static void expandStackOperandTmp(StackOpInst inst, AsmOperand op, AsmOperand target, AsmOperand tmp, List<AsmOperand> newOps, List<AsmInst> insts,
    //                                     AsmBlock p) {
    //     if (op instanceof StackOperand) {
    //         var so = (StackOperand) op;
    //         // – 4095 to +4095 then OK。但由于后面要加减一些？，所以范围放窄一些？TODO
    //         if (inst.isImmFit(so)) {
    //             newOps.add(so);
    //             return;
    //         }
    //         assert target instanceof Reg || target instanceof VfpReg;
    //         // AsmOperand tmp= new Reg(Reg.Type.ip);
    //         assert tmp != null;
    //         var tmp2 = tmp;
    //         if (so.type == StackOperand.Type.SELF_ARG) {
    //             insts.addAll(MovInst.loadImm(p, tmp, new IntImm(Math.toIntExact(so.offset))));
    //             insts.add(new BinOpInst(p, BinaryOp.ADD, tmp2, new Reg(Reg.Type.fp), tmp));
    //         } else if (so.type == StackOperand.Type.LOCAL || so.type == StackOperand.Type.SPILL) {
    //             insts.addAll(MovInst.loadImm(p, tmp, new IntImm(Math.toIntExact(so.offset))));
    //             insts.add(new BinOpInst(p, BinaryOp.SUB, tmp2, new Reg(Reg.Type.fp), tmp));
    //         } else if (so.type == StackOperand.Type.CALL_PARAM) {
    //             insts.addAll(MovInst.loadImm(p, tmp, new IntImm(Math.toIntExact(so.offset))));
    //             insts.add(new BinOpInst(p, BinaryOp.ADD, tmp2, new Reg(Reg.Type.sp), tmp));
    //         } else {throw new UnsupportedOperationException();}

    //         newOps.add(tmp2);
    //     } else {
    //         // 不变
    //         newOps.add(op);
    //     }
    // }

    pub fn expand_stack_operand_tmp(
        &mut self,
        inst_id: AsmValueId,
        op: &AsmOperand,
        target: &AsmOperand,
        tmp: &AsmOperand,
        new_ops: &mut Vec<AsmOperand>,
        insts: &mut Vec<AsmValueId>,
    ) {
        let inst = self.get_inst(inst_id).clone();
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
                let tmp2 = tmp.clone();
                match so.ty {
                    StackOperandType::SelfArg => {
                        insts.extend(
                            self.load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Add,
                            tmp2.clone(),
                            AsmOperand::IntReg(Reg::new(RegType::Fp)),
                            tmp.clone(),
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.alloc_value(inst);

                        insts.push(id);
                    }
                    StackOperandType::Local | StackOperandType::Spill => {
                        insts.extend(
                            self.load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Sub,
                            tmp2.clone(),
                            AsmOperand::IntReg(Reg::new(RegType::Fp)),
                            tmp.clone(),
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.alloc_value(inst);

                        insts.push(id);
                    }
                    StackOperandType::CallParam => {
                        insts.extend(
                            self.load_imm(tmp.clone(), &Imm::Int(IntImm::from(so.offset as i32))),
                        );
                        let inst = mc_inst::BinOpInst::new(
                            mc_inst::BinaryOp::Add,
                            tmp2.clone(),
                            AsmOperand::IntReg(Reg::new(RegType::Sp)),
                            tmp.clone(),
                        );
                        let inst = AsmValue::Inst(AsmInst::BinOp(inst));
                        let id = self.alloc_value(inst);

                        insts.push(id);
                    }
                }
                new_ops.push(tmp2);
            }
            _ => {
                new_ops.push(op.clone());
            }
        }
    }
}

pub struct AsmFunction {
    pub name: String, // label
    pub entry: Option<AsmValueId>,
    pub bbs: Vec<AsmValueId>,
    pub stack_state: StackState,
}

pub struct AsmGlobalVariable {
    pub base: AsmTypeTag,
    // 用于填充导出的链接器符号的大小，和bss段时占用空间的大小。以字节为单位
    pub size: usize,
    pub imm: LabelImm,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AsmTypeTag {
    VOID,
    BOOL,
    CHAR,
    INT32,
    FLOAT,
    DOUBLE,
}

impl AsmTypeTag {
    pub fn size(&self) -> u32 {
        match self {
            AsmTypeTag::VOID => 0,
            AsmTypeTag::BOOL => 1,
            AsmTypeTag::CHAR => 1,
            AsmTypeTag::INT32 => 4,
            AsmTypeTag::FLOAT => 4,
            AsmTypeTag::DOUBLE => 8,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            AsmTypeTag::FLOAT | AsmTypeTag::DOUBLE => true,
            _ => false,
        }
    }
}
#[derive(Default)]
pub struct StackState {
    pub alloca_finished: bool,
    pub spill_size: i64,
    pub local_size: i64,
    max_arg_size: i64,
}

impl StackState {
    // Returns offset relative to BP, use as: bp-offset
    pub fn alloc_local(&mut self, size: i64) -> i64 {
        if self.alloca_finished {
            panic!("Cannot alloc alloca space after spill space")
        } else {
            self.local_size += size;
            self.local_size
        }
    }

    // Returns offset relative to BP, use as: bp-offset
    pub fn alloc_spill(&mut self, size: i64) -> i64 {
        self.alloca_finished = true;
        self.spill_size += size;
        self.spill_size + self.local_size
    }

    pub fn preserve_arg_size(&mut self, size: i64) {
        if size > self.max_arg_size {
            self.max_arg_size = size;
        }
    }

    pub fn total_stack_size(&self) -> i64 {
        let ret = self.local_size + self.spill_size + self.max_arg_size;
        // Round up to the nearest multiple of 8
        (ret + 7) / 8 * 8
    }
}

pub struct AsmBlock {
    pub prev: Option<AsmValueId>,
    pub next: Option<AsmValueId>,
    pub name: String,
    pub preds: Vec<AsmValueId>,
    pub succs: Vec<AsmValueId>,
    pub insts: Vec<AsmValueId>,
}

impl AsmModule {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            bss_globals: Vec::new(),
            funcs: Vec::new(),
            values: id_arena::Arena::new(),
            cur_func: None,
            cur_bb: None,
        }
    }

    pub fn alloc_value(&mut self, value: AsmValue) -> AsmValueId {
        self.values.alloc(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]

pub enum AsmOperand {
    Imm(Imm),
    StackOperand(StackOperand),
    VirtReg(VirtReg),
    IntReg(Reg),
    VfpDoubleReg(VfpDoubleReg),
    VfpReg(VfpReg),
}

impl AsmOperand {
    pub fn is_stack_operand(&self) -> bool {
        match self {
            AsmOperand::StackOperand(_) => true,
            _ => false,
        }
    }

    pub fn as_imm(&self) -> Option<&Imm> {
        match self {
            AsmOperand::Imm(imm) => Some(imm),
            _ => None,
        }
    }

    pub fn as_stack_operand(&self) -> Option<&StackOperand> {
        match self {
            AsmOperand::StackOperand(operand) => Some(operand),
            _ => None,
        }
    }

    pub fn as_virt_reg(&self) -> Option<&VirtReg> {
        match self {
            AsmOperand::VirtReg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn as_int_reg(&self) -> Option<&Reg> {
        match self {
            AsmOperand::IntReg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn as_vfp_double_reg(&self) -> Option<&VfpDoubleReg> {
        match self {
            AsmOperand::VfpDoubleReg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn as_vfp_reg(&self) -> Option<&VfpReg> {
        match self {
            AsmOperand::VfpReg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            AsmOperand::VfpDoubleReg(_) | AsmOperand::VfpReg(_) => true,
            AsmOperand::Imm(imm) => imm.is_float(),
            AsmOperand::VirtReg(reg) => reg.is_float,
            _ => false,
        }
    }
}

impl From<Imm> for AsmOperand {
    fn from(imm: Imm) -> Self {
        AsmOperand::Imm(imm)
    }
}

impl From<StackOperand> for AsmOperand {
    fn from(operand: StackOperand) -> Self {
        AsmOperand::StackOperand(operand)
    }
}

impl From<VirtReg> for AsmOperand {
    fn from(reg: VirtReg) -> Self {
        AsmOperand::VirtReg(reg)
    }
}

impl From<Reg> for AsmOperand {
    fn from(reg: Reg) -> Self {
        AsmOperand::IntReg(reg)
    }
}

impl From<VfpDoubleReg> for AsmOperand {
    fn from(reg: VfpDoubleReg) -> Self {
        AsmOperand::VfpDoubleReg(reg)
    }
}

impl From<VfpReg> for AsmOperand {
    fn from(reg: VfpReg) -> Self {
        AsmOperand::VfpReg(reg)
    }
}

impl From<IntImm> for AsmOperand {
    fn from(imm: IntImm) -> Self {
        AsmOperand::Imm(Imm::Int(imm))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]

pub enum Imm {
    Float(FloatImm),
    Int(IntImm),
    Label(LabelImm),
}

impl Imm {
    pub fn is_float(&self) -> bool {
        match self {
            Imm::Float(_) => true,
            Imm::Label(l) => l.is_float,
            _ => false,
        }
    }
}

impl ImmTrait for Imm {
    fn highest_one_bit(&self) -> u32 {
        match self {
            Imm::Float(imm) => imm.highest_one_bit(),
            Imm::Int(imm) => imm.highest_one_bit(),
            Imm::Label(imm) => imm.highest_one_bit(),
        }
    }

    fn lowest_dword(&self) -> Imm {
        match self {
            Imm::Float(imm) => imm.lowest_dword(),
            Imm::Int(imm) => imm.lowest_dword(),
            Imm::Label(imm) => imm.lowest_dword(),
        }
    }

    fn highest_dword(&self) -> Imm {
        match self {
            Imm::Float(imm) => imm.highest_dword(),
            Imm::Int(imm) => imm.highest_dword(),
            Imm::Label(imm) => imm.highest_dword(),
        }
    }

    fn lowest_word(&self) -> Imm {
        match self {
            Imm::Float(imm) => imm.lowest_word(),
            Imm::Int(imm) => imm.lowest_word(),
            Imm::Label(imm) => imm.lowest_word(),
        }
    }

    fn highest_word(&self) -> Imm {
        match self {
            Imm::Float(imm) => imm.highest_word(),
            Imm::Int(imm) => imm.highest_word(),
            Imm::Label(imm) => imm.highest_word(),
        }
    }
}

pub trait ImmTrait {
    fn highest_one_bit(&self) -> u32;
    fn lowest_dword(&self) -> Imm;
    fn highest_dword(&self) -> Imm;
    fn lowest_word(&self) -> Imm;
    fn highest_word(&self) -> Imm;
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StackOperand {
    pub ty: StackOperandType,
    pub offset: i64,
}

impl StackOperand {
    pub fn new(ty: StackOperandType, offset: i64) -> Self {
        Self { ty, offset }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum StackOperandType {
    Local,
    Spill,
    CallParam,
    SelfArg,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VirtReg {
    pub index: i32,
    pub is_float: bool,
}

impl VirtReg {
    pub fn new(index: i32, is_float: bool) -> Self {
        Self { index, is_float }
    }
}

impl From<i64> for VirtReg {
    fn from(val: i64) -> Self {
        assert!(val >= 0);
        Self {
            index: val as i32,
            is_float: false,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Reg {
    pub ty: RegType,
    pub is_float: bool,
}

impl Reg {
    pub fn is_callee_saved(index: i64) -> bool {
        RegType::is_callee_saved(index)
    }
}
impl Reg {
    pub fn new(ty: RegType) -> Self {
        Self {
            ty,
            is_float: false,
        }
    }
}

impl From<i64> for Reg {
    fn from(val: i64) -> Self {
        assert!((0..16).contains(&val));
        Self {
            ty: RegType::from(val),
            is_float: false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum RegType {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    Fp,
    Ip,
    Sp,
    Lr,
    Pc,
}

impl RegType {
    pub fn is_callee_saved(idx: i64) -> bool {
        (4..=10).contains(&idx)
    }
}
impl From<RegType> for i64 {
    fn from(reg: RegType) -> Self {
        match reg {
            RegType::R0 => 0,
            RegType::R1 => 1,
            RegType::R2 => 2,
            RegType::R3 => 3,
            RegType::R4 => 4,
            RegType::R5 => 5,
            RegType::R6 => 6,
            RegType::R7 => 7,
            RegType::R8 => 8,
            RegType::R9 => 9,
            RegType::R10 => 10,
            RegType::Fp => 11,
            RegType::Ip => 12,
            RegType::Sp => 13,
            RegType::Lr => 14,
            RegType::Pc => 15,
        }
    }
}

impl From<i64> for RegType {
    fn from(val: i64) -> Self {
        match val {
            0 => RegType::R0,
            1 => RegType::R1,
            2 => RegType::R2,
            3 => RegType::R3,
            4 => RegType::R4,
            5 => RegType::R5,
            6 => RegType::R6,
            7 => RegType::R7,
            8 => RegType::R8,
            9 => RegType::R9,
            10 => RegType::R10,
            11 => RegType::Fp,
            12 => RegType::Ip,
            13 => RegType::Sp,
            14 => RegType::Lr,
            15 => RegType::Pc,
            _ => panic!("invalid reg index"),
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct VfpDoubleReg {
    pub is_float: bool,
}

impl default::Default for VfpDoubleReg {
    fn default() -> Self {
        Self { is_float: true }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct VfpReg {
    pub index: i64,
    pub use_as_double: bool,
    pub is_float: bool,
}

impl VfpReg {
    pub fn is_callee_saved(index: i64) -> bool {
        index >= 16
    }
}

impl From<i64> for VfpReg {
    fn from(val: i64) -> Self {
        assert!((0..32).contains(&val));
        Self {
            index: val,
            use_as_double: false,
            is_float: true,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatImm {
    pub value: f32,
    pub is_float: bool,
}

impl FloatImm {
    pub fn cast_to_raw_int(&self) -> u32 {
        unsafe { std::mem::transmute(self.value) }
    }
}
impl cmp::Eq for FloatImm {}

impl hash::Hash for FloatImm {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.cast_to_raw_int().hash(state);
    }
}
impl From<f32> for FloatImm {
    fn from(val: f32) -> Self {
        Self {
            value: val,
            is_float: true,
        }
    }
}

impl ImmTrait for FloatImm {
    fn highest_one_bit(&self) -> u32 {
        let raw = self.cast_to_raw_int();
        let mut ret = 0;
        for i in 0..32 {
            if raw & (1 << i) != 0 {
                ret = i;
            }
        }
        ret
    }

    fn lowest_dword(&self) -> Imm {
        let raw = self.cast_to_raw_int();
        Imm::Int(IntImm { value: raw })
    }

    fn highest_dword(&self) -> Imm {
        let _raw = self.cast_to_raw_int();
        Imm::Int(IntImm { value: 0 })
    }

    fn lowest_word(&self) -> Imm {
        let raw = self.cast_to_raw_int();
        Imm::Int(IntImm {
            value: raw & 0xffff,
        })
    }

    fn highest_word(&self) -> Imm {
        let raw = self.cast_to_raw_int();
        Imm::Int(IntImm {
            value: (raw >> 16) & 0xffff,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct IntImm {
    pub value: u32,
}

impl IntImm {
    pub fn new(value: u32) -> Self {
        Self { value }
    }
}

impl From<u32> for IntImm {
    fn from(val: u32) -> Self {
        Self { value: val }
    }
}

impl From<i32> for IntImm {
    fn from(val: i32) -> Self {
        Self { value: val as u32 }
    }
}

impl ImmTrait for IntImm {
    fn highest_one_bit(&self) -> u32 {
        let mut ret = 0;
        for i in 0..32 {
            if self.value & (1 << i) != 0 {
                ret = i;
            }
        }
        ret
    }

    fn lowest_dword(&self) -> Imm {
        Imm::Int(IntImm { value: self.value })
    }

    fn highest_dword(&self) -> Imm {
        Imm::Int(IntImm { value: 0 })
    }

    fn lowest_word(&self) -> Imm {
        Imm::Int(IntImm {
            value: self.value & 0xffff,
        })
    }

    fn highest_word(&self) -> Imm {
        Imm::Int(IntImm {
            value: (self.value >> 16) & 0xffff,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LabelImm {
    pub state: LabelImmState,
    pub label: String,
    pub is_float: bool,
}

impl LabelImm {
    pub fn new(label: String) -> Self {
        Self {
            state: LabelImmState::Label,
            label,
            is_float: false,
        }
    }
}

impl ImmTrait for LabelImm {
    fn highest_one_bit(&self) -> u32 {
        1 << 31
    }

    fn lowest_dword(&self) -> Imm {
        Imm::Label(LabelImm {
            state: LabelImmState::Low,
            label: self.label.clone(),
            is_float: false,
        })
    }

    fn highest_dword(&self) -> Imm {
        Imm::Label(LabelImm {
            state: LabelImmState::High,
            label: self.label.clone(),
            is_float: false,
        })
    }

    fn lowest_word(&self) -> Imm {
        Imm::Label(LabelImm {
            state: LabelImmState::Low,
            label: self.label.clone(),
            is_float: false,
        })
    }

    fn highest_word(&self) -> Imm {
        Imm::Label(LabelImm {
            state: LabelImmState::High,
            label: self.label.clone(),
            is_float: false,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LabelImmState {
    Label,
    High,
    Low,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CallConv {
    BaseCallConv(BaseCallConv),
    VfpCallConv(VfpCallConv),
}

impl CallConv {
    pub fn add_param(&mut self, param: ParamInfo) -> AsmOperand {
        match self {
            CallConv::BaseCallConv(base_call_conv) => base_call_conv.add_param(param),
            CallConv::VfpCallConv(vfp_call_conv) => vfp_call_conv.add_param(param),
        }
    }

    pub fn get_stack_size(&self) -> i64 {
        match self {
            CallConv::BaseCallConv(base_call_conv) => base_call_conv.get_stack_size(),
            CallConv::VfpCallConv(vfp_call_conv) => vfp_call_conv.get_stack_size(),
        }
    }

    pub fn get_ret_reg(&self) -> AsmOperand {
        match self {
            CallConv::BaseCallConv(base_call_conv) => base_call_conv.get_ret_reg(),
            CallConv::VfpCallConv(vfp_call_conv) => vfp_call_conv.get_ret_reg(),
        }
    }
}

impl CallConv {
    pub fn as_base_call_conv(&self) -> &BaseCallConv {
        match self {
            CallConv::BaseCallConv(base_call_conv) => base_call_conv,
            _ => panic!("not base call conv"),
        }
    }

    pub fn as_vfp_call_conv(&self) -> &VfpCallConv {
        match self {
            CallConv::VfpCallConv(vfp_call_conv) => vfp_call_conv,
            _ => panic!("not vfp call conv"),
        }
    }
}

/// ARM ABI calling convention
/// https://learn.microsoft.com/zh-cn/cpp/build/overview-of-arm-abi-conventions?view=msvc-170
use std::{cmp, collections::VecDeque, default, hash};

use crate::{
    ast::Type,
    mc_inst::{
        self, AsmInst, AsmInstTrait, MovInst, MovType, StackOpInstTrait, VMovInst, VMovType,
    },
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BaseCallConv {
    pub call_params: Vec<AsmOperand>,
    pub ret_reg: AsmOperand,
    pub ncrn: i64,
    pub nsaa: i64,
}

trait CallConvTrait {
    fn get_stack_size(&self) -> i64;
    fn get_ret_reg(&self) -> AsmOperand;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParamInfo {
    pub is_pointer: bool,
    pub base_type: AsmTypeTag,
}

impl From<Type> for ParamInfo {
    fn from(ty: Type) -> Self {
        let bt = ty.base_type();
        Self {
            is_pointer: ty.is_pointer(true),
            base_type: AsmTypeTag::from(bt.clone()),
        }
    }
}

impl BaseCallConv {
    pub fn new() -> Self {
        Self {
            call_params: Vec::new(),
            ret_reg: AsmOperand::IntReg(Reg {
                ty: RegType::R0,
                is_float: false,
            }),
            ncrn: 0,
            nsaa: 0,
        }
    }

    pub fn resolve(mut self, params: &[ParamInfo], ret_ty: AsmTypeTag) -> Self {
        if ret_ty != AsmTypeTag::VOID {
            self.ret_reg = AsmOperand::IntReg(Reg {
                ty: RegType::R0,
                is_float: ret_ty.is_float(),
            });
        }
        for param in params {
            self.add_param(param.clone());
        }
        self
    }

    pub fn add_param(&mut self, param_info: ParamInfo) -> AsmOperand {
        let ret;
        let size = if !param_info.is_pointer && param_info.base_type == AsmTypeTag::DOUBLE {
            self.ncrn = (self.ncrn + 1) / 2 * 2;
            if self.ncrn >= 4 {
                self.nsaa = (self.nsaa + 7) / 8 * 8;
            }
            8
        } else {
            4
        };
        if self.ncrn + (size / 4) <= 4 {
            let ty = RegType::from(self.ncrn);
            ret = AsmOperand::IntReg(Reg {
                ty,
                is_float: false,
            });
            self.call_params.push(ret.clone());
            self.ncrn += size / 4;
        } else {
            assert_eq!(self.ncrn, 4);
            ret = AsmOperand::StackOperand(StackOperand {
                ty: StackOperandType::CallParam,
                offset: self.nsaa,
            });
            self.call_params.push(ret.clone());
            self.nsaa += size;
        }
        ret
    }
}

impl CallConvTrait for BaseCallConv {
    fn get_stack_size(&self) -> i64 {
        self.nsaa
    }

    fn get_ret_reg(&self) -> AsmOperand {
        self.ret_reg.clone()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VfpCallConv {
    // 结果有几种情况：1是r0-r3，2是在s0-s15，3是在内存里（StackOperand）。
    pub call_params: Vec<AsmOperand>,
    // 由于中间push了FP和LR，所以对于内存变量，访问的offset会有所不同
    pub self_args: Vec<AsmOperand>,
    pub ret_reg: AsmOperand, // s0或者r0
    pub ncrn: i64,           //  Next Core Register Number
    pub nsaa: i64,           // Next Stacked Argument Address 计算结束后也是需要占用的栈空间大小
    pub next_vfp: i64,
}

impl VfpCallConv {
    pub fn new() -> Self {
        Self {
            call_params: Vec::new(),
            self_args: Vec::new(),
            ret_reg: AsmOperand::IntReg(Reg {
                ty: RegType::R0,
                is_float: false,
            }),
            ncrn: 0,
            nsaa: 0,
            next_vfp: 0,
        }
    }

    pub fn resolve(mut self, params: &[ParamInfo], ret_ty: AsmTypeTag) -> Self {
        if ret_ty == AsmTypeTag::FLOAT {
            self.ret_reg = AsmOperand::VfpReg(VfpReg::from(0));
        } else if ret_ty == AsmTypeTag::INT32 {
            self.ret_reg = AsmOperand::IntReg(Reg::from(0));
        }

        for param in params {
            assert_ne!(param.base_type, AsmTypeTag::DOUBLE);
            let size = 4;
            if param.base_type == AsmTypeTag::FLOAT {
                // if is VFP CPRC (Co-processor Register Candidate)
                if self.next_vfp < 16 {
                    let result = AsmOperand::VfpReg(VfpReg::from(self.next_vfp));
                    self.call_params.push(result.clone());
                    self.self_args.push(result);
                    self.next_vfp += 1;
                } else {
                    assert_eq!(self.next_vfp, 16);
                    let result = AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::CallParam,
                        offset: self.nsaa,
                    });
                    self.call_params.push(result.clone());
                    //
                    self.self_args.push(AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::SelfArg,
                        offset: self.nsaa + 8,
                    }));
                    self.nsaa += size;
                }
            } else if (self.ncrn + (size / 4)) <= 4 {
                // 寄存器能分配下
                let result = AsmOperand::IntReg(Reg::from(self.ncrn));
                self.call_params.push(result.clone());
                self.self_args.push(result);
                self.ncrn += size / 4;
            } else {
                assert_eq!(self.ncrn, 4);
                self.call_params
                    .push(AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::CallParam,
                        offset: self.nsaa,
                    }));
                self.self_args.push(AsmOperand::StackOperand(StackOperand {
                    ty: StackOperandType::SelfArg,
                    offset: self.nsaa + 8,
                }));
                self.nsaa += size;
            }
        }
        self
    }

    pub fn add_param(&mut self, _param_info: ParamInfo) -> AsmOperand {
        unimplemented!()
    }
}

impl CallConvTrait for VfpCallConv {
    fn get_stack_size(&self) -> i64 {
        self.nsaa
    }

    fn get_ret_reg(&self) -> AsmOperand {
        self.ret_reg.clone()
    }
}
