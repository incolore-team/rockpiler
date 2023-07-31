use core::fmt;

use crate::{
    ast::InfixOp,
    mc::{
        AsmOperand, AsmValueId, CallConv, Imm, ImmTrait, IntReg, LabelImm, RegConstraintMap,
        RegType, StackOperand, VirtReg,
    },
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConstraintsComponent {
    pub in_constraints: RegConstraintMap,
    pub out_constraints: RegConstraintMap,
}

impl Default for ConstraintsComponent {
    fn default() -> Self {
        Self {
            in_constraints: RegConstraintMap::new(),
            out_constraints: RegConstraintMap::new(),
        }
    }
}

impl ConstraintsComponent {
    pub fn new(in_constraints: RegConstraintMap, out_constraints: RegConstraintMap) -> Self {
        Self {
            in_constraints,
            out_constraints,
        }
    }
}
pub trait ConstraintsTrait {
    fn get_in_constraints(&self) -> &RegConstraintMap;
    fn get_out_constraints(&self) -> &RegConstraintMap;
    fn get_in_constraints_mut(&mut self) -> &mut RegConstraintMap;
    fn get_out_constraints_mut(&mut self) -> &mut RegConstraintMap;
    fn set_in_constraint(&mut self, key: VirtReg, value: AsmOperand);
    fn set_out_constraint(&mut self, key: VirtReg, value: AsmOperand);
}

macro_rules! impl_constraints_trait {
    ($t:ty) => {
        impl ConstraintsTrait for $t {
            fn get_in_constraints(&self) -> &RegConstraintMap {
                &self.constraints.in_constraints
            }

            fn get_out_constraints(&self) -> &RegConstraintMap {
                &self.constraints.out_constraints
            }

            fn get_in_constraints_mut(&mut self) -> &mut RegConstraintMap {
                &mut self.constraints.in_constraints
            }

            fn get_out_constraints_mut(&mut self) -> &mut RegConstraintMap {
                &mut self.constraints.out_constraints
            }

            fn set_in_constraint(&mut self, key: VirtReg, value: AsmOperand) {
                self.constraints.in_constraints.insert(key, value);
            }

            fn set_out_constraint(&mut self, key: VirtReg, value: AsmOperand) {
                self.constraints.out_constraints.insert(key, value);
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AsmOperandComponent {
    defs: Vec<AsmOperand>,
    uses: Vec<AsmOperand>,
}

impl AsmOperandComponent {
    pub fn new(defs: Vec<AsmOperand>, uses: Vec<AsmOperand>) -> AsmOperandComponent {
        AsmOperandComponent { defs, uses }
    }
}

pub trait AsmInstTrait {
    fn get_defs(&self) -> Vec<AsmOperand>;
    fn get_uses(&self) -> Vec<AsmOperand>;
    fn get_uses_mut(&mut self) -> &mut Vec<AsmOperand>;
    fn get_defs_mut(&mut self) -> &mut Vec<AsmOperand>;
    fn set_uses(&mut self, uses: Vec<AsmOperand>);
    fn set_defs(&mut self, defs: Vec<AsmOperand>);
}

macro_rules! impl_asm_inst_trait_no_oprs {
    ($t:ty) => {
        impl AsmInstTrait for $t {
            fn get_defs(&self) -> Vec<AsmOperand> {
                unimplemented!()
            }

            fn get_uses(&self) -> Vec<AsmOperand> {
                unimplemented!()
            }

            fn get_uses_mut(&mut self) -> &mut Vec<AsmOperand> {
                unimplemented!()
            }

            fn get_defs_mut(&mut self) -> &mut Vec<AsmOperand> {
                unimplemented!()
            }

            fn set_uses(&mut self, uses: Vec<AsmOperand>) {
                unimplemented!()
            }

            fn set_defs(&mut self, defs: Vec<AsmOperand>) {
                unimplemented!()
            }
        }
    };
}
macro_rules! impl_asm_inst_trait {
    ($t:ty) => {
        impl AsmInstTrait for $t {
            fn get_defs(&self) -> Vec<AsmOperand> {
                self.oprs.defs.clone()
            }

            fn get_uses(&self) -> Vec<AsmOperand> {
                self.oprs.uses.clone()
            }

            fn get_uses_mut(&mut self) -> &mut Vec<AsmOperand> {
                &mut self.oprs.uses
            }

            fn get_defs_mut(&mut self) -> &mut Vec<AsmOperand> {
                &mut self.oprs.defs
            }

            fn set_uses(&mut self, uses: Vec<AsmOperand>) {
                self.oprs.uses = uses;
            }

            fn set_defs(&mut self, defs: Vec<AsmOperand>) {
                self.oprs.defs = defs;
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub enum AsmInst {
    BinOp(BinOpInst),
    Br(BrInst),
    BX(BXInst),
    Call(CallInst),
    TailCall(TailCallInst),
    CMP(CMPInst),
    FBinOp(FBinOpInst),
    FCMP(FCMPInst),
    LDR(LDRInst),
    Mov(MovInst),
    STR(STRInst),
    VCVT(VCVTInst),
    VLDR(VLDRInst),
    VMov(VMovInst),
    VMRS(VMRSInst),
    VSTR(VSTRInst),
    Prologue(PrologueInst),
    RetInst(RetInst),
}

macro_rules! impl_stack_op_inst_trait {
    ($inst:ident) => {
        impl StackOpInstTrait for $inst {
            fn is_imm_fit(&self, so: &StackOperand) -> bool {
                $inst::is_imm_fit(so)
            }
        }
    };
}

impl StackOpInstTrait for AsmInst {
    fn is_imm_fit(&self, so: &StackOperand) -> bool {
        match self {
            AsmInst::LDR(inst) => inst.is_imm_fit(so),
            AsmInst::VLDR(inst) => inst.is_imm_fit(so),
            AsmInst::STR(inst) => inst.is_imm_fit(so),
            AsmInst::VSTR(inst) => inst.is_imm_fit(so),
            _ => unreachable!("is_imm_fit: not a stack op inst"),
        }
    }
}

impl_stack_op_inst_trait!(LDRInst);
impl_stack_op_inst_trait!(VLDRInst);
impl_stack_op_inst_trait!(STRInst);
impl_stack_op_inst_trait!(VSTRInst);

impl AsmInst {
    pub fn as_bin_op(&self) -> Option<&BinOpInst> {
        match self {
            AsmInst::BinOp(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_bin_op_mut(&mut self) -> Option<&mut BinOpInst> {
        match self {
            AsmInst::BinOp(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_br(&self) -> Option<&BrInst> {
        match self {
            AsmInst::Br(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_br_mut(&mut self) -> Option<&mut BrInst> {
        match self {
            AsmInst::Br(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_bx(&self) -> Option<&BXInst> {
        match self {
            AsmInst::BX(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_bx_mut(&mut self) -> Option<&mut BXInst> {
        match self {
            AsmInst::BX(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&CallInst> {
        match self {
            AsmInst::Call(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_call_mut(&mut self) -> Option<&mut CallInst> {
        match self {
            AsmInst::Call(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_tail_call(&self) -> Option<&TailCallInst> {
        match self {
            AsmInst::TailCall(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_tail_call_mut(&mut self) -> Option<&mut TailCallInst> {
        match self {
            AsmInst::TailCall(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_cmp(&self) -> Option<&CMPInst> {
        match self {
            AsmInst::CMP(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_cmp_mut(&mut self) -> Option<&mut CMPInst> {
        match self {
            AsmInst::CMP(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_fbin_op(&self) -> Option<&FBinOpInst> {
        match self {
            AsmInst::FBinOp(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_fbin_op_mut(&mut self) -> Option<&mut FBinOpInst> {
        match self {
            AsmInst::FBinOp(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_fcmp(&self) -> Option<&FCMPInst> {
        match self {
            AsmInst::FCMP(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_fcmp_mut(&mut self) -> Option<&mut FCMPInst> {
        match self {
            AsmInst::FCMP(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_ldr(&self) -> Option<&LDRInst> {
        match self {
            AsmInst::LDR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_ldr_mut(&mut self) -> Option<&mut LDRInst> {
        match self {
            AsmInst::LDR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_mov(&self) -> Option<&MovInst> {
        match self {
            AsmInst::Mov(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_mov_mut(&mut self) -> Option<&mut MovInst> {
        match self {
            AsmInst::Mov(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&STRInst> {
        match self {
            AsmInst::STR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_str_mut(&mut self) -> Option<&mut STRInst> {
        match self {
            AsmInst::STR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vcvt(&self) -> Option<&VCVTInst> {
        match self {
            AsmInst::VCVT(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vcvt_mut(&mut self) -> Option<&mut VCVTInst> {
        match self {
            AsmInst::VCVT(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vldr(&self) -> Option<&VLDRInst> {
        match self {
            AsmInst::VLDR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vldr_mut(&mut self) -> Option<&mut VLDRInst> {
        match self {
            AsmInst::VLDR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vmov(&self) -> Option<&VMovInst> {
        match self {
            AsmInst::VMov(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vmov_mut(&mut self) -> Option<&mut VMovInst> {
        match self {
            AsmInst::VMov(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vmrs(&self) -> Option<&VMRSInst> {
        match self {
            AsmInst::VMRS(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vmrs_mut(&mut self) -> Option<&mut VMRSInst> {
        match self {
            AsmInst::VMRS(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vstr(&self) -> Option<&VSTRInst> {
        match self {
            AsmInst::VSTR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_vstr_mut(&mut self) -> Option<&mut VSTRInst> {
        match self {
            AsmInst::VSTR(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_prologue(&self) -> Option<&PrologueInst> {
        match self {
            AsmInst::Prologue(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_prologue_mut(&mut self) -> Option<&mut PrologueInst> {
        match self {
            AsmInst::Prologue(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_ret(&self) -> Option<&RetInst> {
        match self {
            AsmInst::RetInst(inst) => Some(inst),
            _ => None,
        }
    }

    pub fn as_ret_mut(&mut self) -> Option<&mut RetInst> {
        match self {
            AsmInst::RetInst(inst) => Some(inst),
            _ => None,
        }
    }
}

impl AsmInstTrait for AsmInst {
    fn get_defs(&self) -> Vec<AsmOperand> {
        match self {
            AsmInst::BinOp(inst) => inst.get_defs(),
            AsmInst::Br(inst) => inst.get_defs(),
            AsmInst::BX(inst) => inst.get_defs(),
            AsmInst::Call(inst) => inst.get_defs(),
            AsmInst::CMP(inst) => inst.get_defs(),
            AsmInst::FBinOp(inst) => inst.get_defs(),
            AsmInst::FCMP(inst) => inst.get_defs(),
            AsmInst::LDR(inst) => inst.get_defs(),
            AsmInst::Mov(inst) => inst.get_defs(),
            AsmInst::STR(inst) => inst.get_defs(),
            AsmInst::VCVT(inst) => inst.get_defs(),
            AsmInst::VLDR(inst) => inst.get_defs(),
            AsmInst::VMov(inst) => inst.get_defs(),
            AsmInst::VMRS(inst) => inst.get_defs(),
            AsmInst::VSTR(inst) => inst.get_defs(),
            AsmInst::Prologue(inst) => inst.get_defs(),
            AsmInst::RetInst(inst) => inst.get_defs(),
            AsmInst::TailCall(inst) => inst.get_defs(),
        }
    }

    fn get_uses(&self) -> Vec<AsmOperand> {
        match self {
            AsmInst::BinOp(inst) => inst.get_uses(),
            AsmInst::Br(inst) => inst.get_uses(),
            AsmInst::BX(inst) => inst.get_uses(),
            AsmInst::Call(inst) => inst.get_uses(),
            AsmInst::CMP(inst) => inst.get_uses(),
            AsmInst::FBinOp(inst) => inst.get_uses(),
            AsmInst::FCMP(inst) => inst.get_uses(),
            AsmInst::LDR(inst) => inst.get_uses(),
            AsmInst::Mov(inst) => inst.get_uses(),
            AsmInst::STR(inst) => inst.get_uses(),
            AsmInst::VCVT(inst) => inst.get_uses(),
            AsmInst::VLDR(inst) => inst.get_uses(),
            AsmInst::VMov(inst) => inst.get_uses(),
            AsmInst::VMRS(inst) => inst.get_uses(),
            AsmInst::VSTR(inst) => inst.get_uses(),
            AsmInst::Prologue(inst) => inst.get_uses(),
            AsmInst::RetInst(inst) => inst.get_uses(),
            AsmInst::TailCall(inst) => inst.get_uses(),
        }
    }

    fn get_uses_mut(&mut self) -> &mut Vec<AsmOperand> {
        match self {
            AsmInst::BinOp(inst) => inst.get_uses_mut(),
            AsmInst::Br(inst) => inst.get_uses_mut(),
            AsmInst::BX(inst) => inst.get_uses_mut(),
            AsmInst::Call(inst) => inst.get_uses_mut(),
            AsmInst::CMP(inst) => inst.get_uses_mut(),
            AsmInst::FBinOp(inst) => inst.get_uses_mut(),
            AsmInst::FCMP(inst) => inst.get_uses_mut(),
            AsmInst::LDR(inst) => inst.get_uses_mut(),
            AsmInst::Mov(inst) => inst.get_uses_mut(),
            AsmInst::STR(inst) => inst.get_uses_mut(),
            AsmInst::VCVT(inst) => inst.get_uses_mut(),
            AsmInst::VLDR(inst) => inst.get_uses_mut(),
            AsmInst::VMov(inst) => inst.get_uses_mut(),
            AsmInst::VMRS(inst) => inst.get_uses_mut(),
            AsmInst::VSTR(inst) => inst.get_uses_mut(),
            AsmInst::Prologue(inst) => inst.get_uses_mut(),
            AsmInst::RetInst(inst) => inst.get_uses_mut(),
            AsmInst::TailCall(inst) => inst.get_uses_mut(),
        }
    }

    fn get_defs_mut(&mut self) -> &mut Vec<AsmOperand> {
        match self {
            AsmInst::BinOp(inst) => inst.get_defs_mut(),
            AsmInst::Br(inst) => inst.get_defs_mut(),
            AsmInst::BX(inst) => inst.get_defs_mut(),
            AsmInst::Call(inst) => inst.get_defs_mut(),
            AsmInst::CMP(inst) => inst.get_defs_mut(),
            AsmInst::FBinOp(inst) => inst.get_defs_mut(),
            AsmInst::FCMP(inst) => inst.get_defs_mut(),
            AsmInst::LDR(inst) => inst.get_defs_mut(),
            AsmInst::Mov(inst) => inst.get_defs_mut(),
            AsmInst::STR(inst) => inst.get_defs_mut(),
            AsmInst::VCVT(inst) => inst.get_defs_mut(),
            AsmInst::VLDR(inst) => inst.get_defs_mut(),
            AsmInst::VMov(inst) => inst.get_defs_mut(),
            AsmInst::VMRS(inst) => inst.get_defs_mut(),
            AsmInst::VSTR(inst) => inst.get_defs_mut(),
            AsmInst::Prologue(inst) => inst.get_defs_mut(),
            AsmInst::RetInst(inst) => inst.get_defs_mut(),
            AsmInst::TailCall(inst) => inst.get_defs_mut(),
        }
    }

    fn set_uses(&mut self, uses: Vec<AsmOperand>) {
        match self {
            AsmInst::BinOp(inst) => inst.set_uses(uses),
            AsmInst::Br(inst) => inst.set_uses(uses),
            AsmInst::BX(inst) => inst.set_uses(uses),
            AsmInst::Call(inst) => inst.set_uses(uses),
            AsmInst::CMP(inst) => inst.set_uses(uses),
            AsmInst::FBinOp(inst) => inst.set_uses(uses),
            AsmInst::FCMP(inst) => inst.set_uses(uses),
            AsmInst::LDR(inst) => inst.set_uses(uses),
            AsmInst::Mov(inst) => inst.set_uses(uses),
            AsmInst::STR(inst) => inst.set_uses(uses),
            AsmInst::VCVT(inst) => inst.set_uses(uses),
            AsmInst::VLDR(inst) => inst.set_uses(uses),
            AsmInst::VMov(inst) => inst.set_uses(uses),
            AsmInst::VMRS(inst) => inst.set_uses(uses),
            AsmInst::VSTR(inst) => inst.set_uses(uses),
            AsmInst::Prologue(inst) => inst.set_uses(uses),
            AsmInst::RetInst(inst) => inst.set_uses(uses),
            AsmInst::TailCall(inst) => inst.set_uses(uses),
        }
    }

    fn set_defs(&mut self, defs: Vec<AsmOperand>) {
        match self {
            AsmInst::BinOp(inst) => inst.set_defs(defs),
            AsmInst::Br(inst) => inst.set_defs(defs),
            AsmInst::BX(inst) => inst.set_defs(defs),
            AsmInst::Call(inst) => inst.set_defs(defs),
            AsmInst::CMP(inst) => inst.set_defs(defs),
            AsmInst::FBinOp(inst) => inst.set_defs(defs),
            AsmInst::FCMP(inst) => inst.set_defs(defs),
            AsmInst::LDR(inst) => inst.set_defs(defs),
            AsmInst::Mov(inst) => inst.set_defs(defs),
            AsmInst::STR(inst) => inst.set_defs(defs),
            AsmInst::VCVT(inst) => inst.set_defs(defs),
            AsmInst::VLDR(inst) => inst.set_defs(defs),
            AsmInst::VMov(inst) => inst.set_defs(defs),
            AsmInst::VMRS(inst) => inst.set_defs(defs),
            AsmInst::VSTR(inst) => inst.set_defs(defs),
            AsmInst::Prologue(inst) => inst.set_defs(defs),
            AsmInst::RetInst(inst) => inst.set_defs(defs),
            AsmInst::TailCall(inst) => inst.set_defs(defs),
        }
    }
}

macro_rules! impl_asm_from_trait {
    ($field:ident, $t:ty) => {
        impl From<$t> for AsmInst {
            fn from(inst: $t) -> Self {
                AsmInst::$field(inst)
            }
        }
    };
}

impl_asm_from_trait!(BinOp, BinOpInst);
impl_asm_from_trait!(Br, BrInst);
impl_asm_from_trait!(BX, BXInst);
impl_asm_from_trait!(Call, CallInst);
impl_asm_from_trait!(Call, TailCallInst);
impl_asm_from_trait!(CMP, CMPInst);
impl_asm_from_trait!(FBinOp, FBinOpInst);
impl_asm_from_trait!(FCMP, FCMPInst);
impl_asm_from_trait!(LDR, LDRInst);
impl_asm_from_trait!(Mov, MovInst);
impl_asm_from_trait!(STR, STRInst);
impl_asm_from_trait!(VCVT, VCVTInst);
impl_asm_from_trait!(VLDR, VLDRInst);
impl_asm_from_trait!(VMov, VMovInst);
impl_asm_from_trait!(VMRS, VMRSInst);
impl_asm_from_trait!(VSTR, VSTRInst);
impl_asm_from_trait!(Prologue, PrologueInst);
impl_asm_from_trait!(RetInst, RetInst);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RetInst {
    pub func: AsmValueId,
    pub constraints: ConstraintsComponent,
}
impl_asm_inst_trait_no_oprs!(RetInst);
impl RetInst {
    pub fn new(func: AsmValueId) -> Self {
        Self {
            func,
            constraints: ConstraintsComponent::default(),
        }
    }
}

impl_constraints_trait!(RetInst);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrologueInst {
    pub func: AsmValueId,
    pub constraints: ConstraintsComponent,
}
impl_asm_inst_trait_no_oprs!(PrologueInst);
impl PrologueInst {
    pub fn new(func: AsmValueId) -> Self {
        Self {
            func,
            constraints: ConstraintsComponent::default(),
        }
    }
}

impl_constraints_trait!(PrologueInst);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VMRSInst {}
impl_asm_inst_trait_no_oprs!(VMRSInst);
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VMovType {
    CPY,
    A2S,
    S2A,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VMovInst {
    pub ty: VMovType,
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(VMovInst);
impl VMovInst {
    pub fn new(ty: VMovType, to: AsmOperand, from: AsmOperand) -> Self {
        Self {
            ty,
            oprs: AsmOperandComponent::new(vec![to], vec![from]),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VCVTType {
    F2I,
    I2F,
    F2D,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VCVTInst {
    pub ty: VCVTType,
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(VCVTInst);
impl VCVTInst {
    pub fn new(ty: VCVTType, to: AsmOperand, from: AsmOperand) -> Self {
        Self {
            ty,
            oprs: AsmOperandComponent::new(vec![to], vec![from]),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovType {
    Reg,
    Movw,
    Movt,
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct MovInst {
    pub ty: MovType,
    pub cond: Cond,
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(MovInst);
impl MovInst {
    pub fn new(ty: MovType, to: AsmOperand, from: AsmOperand, cond: Option<Cond>) -> Self {
        Self {
            ty,
            oprs: AsmOperandComponent::new(vec![to], vec![from]),
            cond: if let Some(cond) = cond {
                cond
            } else {
                Cond::AL
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FCMPInst {
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(FCMPInst);
impl FCMPInst {
    pub fn new(op1: AsmOperand, op2: AsmOperand) -> Self {
        Self {
            oprs: AsmOperandComponent::new(vec![], vec![op1, op2]),
        }
    }
}
/**
* 1. fadd - vadd.f32 Fd, Fn, Fm 不支持立即数
* 2. fsub - vsub.f32同上
* 3. fmul - vmul.f32
* 4. fdiv - vdiv.f32
* 5. 浮点数好像不支持取模
 */
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FBinOpInst {
    pub op: BinaryOp,
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(FBinOpInst);
impl FBinOpInst {
    pub fn new(op: BinaryOp, to: AsmOperand, op1: AsmOperand, op2: AsmOperand) -> Self {
        Self {
            op,
            oprs: AsmOperandComponent::new(vec![to], vec![op1, op2]),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CMPInst {
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(CMPInst);
impl CMPInst {
    pub fn new(op1: AsmOperand, op2: AsmOperand) -> Self {
        Self {
            oprs: AsmOperandComponent::new(vec![], vec![op1, op2]),
        }
    }
}

pub trait StackOpInstTrait {
    fn is_imm_fit(&self, so: &StackOperand) -> bool;
}

impl AsmInst {
    pub fn is_call(&self) -> bool {
        matches!(self, AsmInst::Call(_))
    }

    pub fn is_ldr(&self) -> bool {
        matches!(self, AsmInst::LDR(_))
    }

    pub fn is_vldr(&self) -> bool {
        matches!(self, AsmInst::VLDR(_))
    }

    pub fn is_binop(&self) -> bool {
        matches!(self, AsmInst::BinOp(_))
    }

    pub fn is_bx(&self) -> bool {
        matches!(self, AsmInst::BX(_))
    }

    pub fn is_br(&self) -> bool {
        matches!(self, AsmInst::Br(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, AsmInst::STR(_))
    }

    pub fn is_vstr(&self) -> bool {
        matches!(self, AsmInst::VSTR(_))
    }

    pub fn is_stack_op(&self) -> bool {
        matches!(self, AsmInst::LDR(_) | AsmInst::VLDR(_) | AsmInst::STR(_))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct CallInst {
    /// jump target
    pub label: LabelImm,
    pub oprs: AsmOperandComponent,
    pub cc: CallConv,
    /// used to bind args to calling convention registers
    pub constraints: ConstraintsComponent,
}

impl_asm_inst_trait!(CallInst);
impl_asm_inst_trait!(TailCallInst);

impl CallInst {
    pub fn new(label: LabelImm, cc: CallConv) -> Self {
        Self {
            label,
            cc,
            oprs: AsmOperandComponent::new(vec![], vec![]),
            constraints: ConstraintsComponent::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TailCallInst {
    /// jump target
    pub label: LabelImm,
    pub oprs: AsmOperandComponent,
    pub cc: CallConv,
    /// used to bind args to calling convention registers
    pub constraints: ConstraintsComponent,
}

impl TailCallInst {
    pub fn new(label: LabelImm, cc: CallConv) -> Self {
        Self {
            label,
            cc,
            oprs: AsmOperandComponent::new(vec![], vec![]),
            constraints: ConstraintsComponent::default(),
        }
    }
}

impl_constraints_trait!(CallInst);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd,
    LogOr,
    LogEq,
    LogNeq,
    LogLt,
    LogGt,
    LogLe,
    LogGe,
}

impl From<InfixOp> for BinaryOp {
    fn from(op: InfixOp) -> Self {
        match op {
            InfixOp::Add => BinaryOp::Add,
            InfixOp::Sub => BinaryOp::Sub,
            InfixOp::Mul => BinaryOp::Mul,
            InfixOp::Div => BinaryOp::Div,
            InfixOp::Mod => BinaryOp::Mod,
            InfixOp::BitAnd => todo!(),
            InfixOp::BitOr => todo!(),
            InfixOp::BitXor => todo!(),
            InfixOp::BitShl => todo!(),
            InfixOp::BitShr => todo!(),
            InfixOp::LogicAnd => BinaryOp::LogAnd,
            InfixOp::LogicOr => BinaryOp::LogOr,
            InfixOp::Rem => todo!(),
            InfixOp::Eq => BinaryOp::LogEq,
            InfixOp::Ne => BinaryOp::LogNeq,
            InfixOp::Lt => BinaryOp::LogLt,
            InfixOp::Gt => BinaryOp::LogGt,
            InfixOp::Le => BinaryOp::LogLe,
            InfixOp::Ge => BinaryOp::LogGe,
            InfixOp::Assign => todo!(),
        }
    }
}

/// https://developer.arm.com/documentation/dui0473/m/vfp-instructions/vstr--floating-point-
/// VSTR{C} Fd, [Rn{, #<immed>}] imm范围0-1020
#[derive(Debug, PartialEq, Eq, Clone)]

pub struct VLDRInst {
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(VLDRInst);
impl VLDRInst {
    pub fn new(dest: AsmOperand, addr: AsmOperand) -> VLDRInst {
        let oprs = AsmOperandComponent::new(vec![dest], vec![addr]);
        VLDRInst { oprs }
    }

    pub fn is_imm_fit(so: &StackOperand) -> bool {
        assert!(so.offset % 4 == 0);
        so.offset >= 0 && so.offset <= 1020
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]

pub struct LDRInst {
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(LDRInst);
impl LDRInst {
    pub fn new(dest: AsmOperand, addr: AsmOperand) -> LDRInst {
        let oprs = AsmOperandComponent::new(vec![dest], vec![addr]);
        LDRInst { oprs }
    }

    pub fn is_imm_fit(so: &StackOperand) -> bool {
        assert!(so.offset % 4 == 0);
        so.offset >= -4070 && so.offset <= 4070
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct STRInst {
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(STRInst);
impl STRInst {
    pub fn new(val: AsmOperand, addr: AsmOperand) -> STRInst {
        let oprs = AsmOperandComponent::new(vec![], vec![val, addr]);
        STRInst { oprs }
    }

    pub fn is_imm_fit(so: &StackOperand) -> bool {
        assert!(so.offset % 4 == 0);
        so.offset >= -4070 && so.offset <= 4070
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VSTRInst {
    pub oprs: AsmOperandComponent,
}
impl_asm_inst_trait!(VSTRInst);
impl VSTRInst {
    pub fn new(val: AsmOperand, addr: AsmOperand) -> VSTRInst {
        let oprs = AsmOperandComponent::new(vec![], vec![val, addr]);
        VSTRInst { oprs }
    }

    pub fn is_imm_fit(so: &StackOperand) -> bool {
        assert!(so.offset % 4 == 0);
        so.offset >= 0 && so.offset <= 1020
    }
}

/**
 * 存放结果的to必须是寄存器，不能是常量
 *
 * 1. ADD Rd, Rn, #<imm12> 立即数范围是 0-4095 否则就用ADD Rd, Rn, Rm
 * 2. SUB Rd, Rn, #<imm12> 同上
 * 3. MUL Rd, Rm, Rs 无法使用立即数，必须要转换了
 * 4. SDIV Rd, Rn, Rm 有符号除法，同上
 * 5. 取模：不支持，在IR那边转换为调用相关eabi函数
 */
#[derive(Debug, PartialEq, Eq, Clone)]

pub struct BinOpInst {
    pub op: BinaryOp,
    pub oprs: AsmOperandComponent,
}

impl_asm_inst_trait!(BinOpInst);

impl BinOpInst {
    pub fn new(op: BinaryOp, to: AsmOperand, op1: AsmOperand, op2: AsmOperand) -> BinOpInst {
        let oprs = AsmOperandComponent::new(vec![to], vec![op1, op2]);
        BinOpInst { op, oprs }
    }

    fn op_to_string(op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "ADD",
            BinaryOp::Sub => "SUB",
            BinaryOp::Mul => "MUL",
            BinaryOp::Div => "SDIV",
            _ => unreachable!(),
        }
    }

    fn is_imm_fit(&self, m: &Imm) -> bool {
        match self.op {
            BinaryOp::Add | BinaryOp::Sub => Operand2::is_imm_fit(m),
            _ => false,
        }
    }
}

pub struct Operand2;
// Flexible Operand 2 目前仅当作8bit常量使用
impl Operand2 {
    pub fn is_imm_fit(m: &Imm) -> bool {
        m.highest_one_bit() < 255
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BrInst {
    cond: Cond,
    target: AsmValueId, // AsmBlock
}

impl_asm_inst_trait_no_oprs!(BrInst);

impl BrInst {
    pub fn new(cond: Cond, target: AsmValueId) -> BrInst {
        BrInst { cond, target }
    }
}

pub struct BrInstBuilder {
    inst: BrInst,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Cond {
    AL,
    EQ,
    NE,
    GE,
    GT,
    LE,
    LT,
}

impl Cond {
    pub fn neg(&self) -> Cond {
        match self {
            Cond::AL => Cond::AL,
            Cond::EQ => Cond::NE,
            Cond::NE => Cond::EQ,
            Cond::GE => Cond::LT,
            Cond::GT => Cond::LE,
            Cond::LE => Cond::GT,
            Cond::LT => Cond::GE,
        }
    }
}

impl From<InfixOp> for Cond {
    fn from(op: InfixOp) -> Cond {
        match op {
            InfixOp::Eq => Cond::EQ,
            InfixOp::Ne => Cond::NE,
            InfixOp::Ge => Cond::GE,
            InfixOp::Gt => Cond::GT,
            InfixOp::Le => Cond::LE,
            InfixOp::Lt => Cond::LT,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Cond::AL => write!(f, ""),
            Cond::EQ => write!(f, "EQ"),
            Cond::NE => write!(f, "NE"),
            Cond::GE => write!(f, "GE"),
            Cond::GT => write!(f, "GT"),
            Cond::LE => write!(f, "LE"),
            Cond::LT => write!(f, "LT"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BXInst {
    pub oprs: AsmOperandComponent,
}

impl_asm_inst_trait!(BXInst);

impl BXInst {
    pub fn new(opr: AsmOperand) -> BXInst {
        BXInst {
            oprs: AsmOperandComponent::new(vec![], vec![opr]),
        }
    }
}
