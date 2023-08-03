use std::fmt::format;

use crate::{mc::*, mc_inst::*};

pub fn print(module: &mut AsmModule) {
    let mut printer = Printer { module };
    printer.print_module();
}

struct Printer<'a> {
    module: &'a mut AsmModule,
}

impl<'a> Printer<'a> {
    pub fn print_module(&mut self) {
        for func_id in self.module.funcs.clone() {
            self.print_func(func_id);
        }
    }

    pub fn print_func(&mut self, func_id: AsmValueId) {
        let func = self.module.get_func(func_id);
        for bb_id in func.bbs.clone() {
            self.print_bb(bb_id);
        }
    }

    pub fn print_bb(&mut self, bb_id: AsmValueId) {
        let bb = self.module.get_bb(bb_id);
        println!("{}:", bb.name);
        for inst_id in bb.insts.clone() {
            self.print_inst(inst_id);
        }
    }

    pub fn print_inst(&mut self, inst_id: AsmValueId) {
        let inst = self.module.get_inst(inst_id).clone();
        println!("    {}", inst.to_arm(&mut self.module));
    }
}

pub trait ToArm {
    fn to_arm(&self, module: &mut AsmModule) -> String;
}

impl ToArm for AsmInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self {
            AsmInst::BinOp(i) => i.to_arm(module),
            AsmInst::Br(i) => i.to_arm(module),
            AsmInst::BX(i) => i.to_arm(module),
            AsmInst::Call(i) => i.to_arm(module),
            AsmInst::CMP(i) => i.to_arm(module),
            AsmInst::FBinOp(i) => i.to_arm(module),
            AsmInst::FCMP(i) => i.to_arm(module),
            AsmInst::LDR(i) => i.to_arm(module),
            AsmInst::Mov(i) => i.to_arm(module),
            AsmInst::STR(i) => i.to_arm(module),
            AsmInst::VCVT(i) => i.to_arm(module),
            AsmInst::VLDR(i) => i.to_arm(module),
            AsmInst::VMov(i) => i.to_arm(module),
            AsmInst::VMRS(i) => i.to_arm(module),
            AsmInst::VSTR(i) => i.to_arm(module),
            AsmInst::Prologue(i) => i.to_arm(module),
            AsmInst::Ret(i) => i.to_arm(module),
        }
    }
}

impl ToArm for BinOpInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "{}\t{}, {}, {}",
            self.op.to_arm(module),
            self.get_defs()[0].to_arm(module),
            self.get_uses()[0].to_arm(module),
            self.get_uses()[1].to_arm(module),
        )
    }
}

impl ToArm for BinaryOp {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self {
            BinaryOp::Add => "ADD".to_string(),
            BinaryOp::Sub => "SUB".to_string(),
            BinaryOp::Mul => "MUL".to_string(),
            BinaryOp::Div => "SDIV".to_string(),
            BinaryOp::Mod => "ERR".to_string(),
            BinaryOp::LogAnd => "ERR".to_string(),
            BinaryOp::LogOr => "ERR".to_string(),
            BinaryOp::LogEq => "ERR".to_string(),
            BinaryOp::LogNeq => "ERR".to_string(),
            BinaryOp::LogLt => "ERR".to_string(),
            BinaryOp::LogGt => "ERR".to_string(),
            BinaryOp::LogLe => "ERR".to_string(),
            BinaryOp::LogGe => "ERR".to_string(),
        }
    }
}
impl ToArm for AsmOperand {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self {
            AsmOperand::Imm(opr) => opr.to_arm(module),
            AsmOperand::StackOperand(opr) => opr.to_arm(module),
            AsmOperand::VirtReg(opr) => opr.to_arm(module),
            AsmOperand::IntReg(opr) => opr.to_arm(module),
            AsmOperand::VfpDoubleReg(opr) => opr.to_arm(module),
            AsmOperand::VfpReg(opr) => opr.to_arm(module),
        }
    }
}

impl ToArm for Imm {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self {
            Imm::Float(i) => format!("#0x{:x}", i.cast_to_raw_int()),
            Imm::Int(i) => format!("#0x{:x}", i.value as u64),
            Imm::Label(i) => i.to_arm(module),
        }
    }
}

impl ToArm for IntImm {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!("#0x{:x}", self.value as u64)
    }
}

impl ToArm for LabelImm {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self.state {
            LabelImmState::High => format!("#:upper16:{}", self.label),
            LabelImmState::Label => self.label.to_string(),
            LabelImmState::Low => format!("#:lower16:{}", self.label),
            _ => String::new(),
        }
    }
}
impl ToArm for StackOperand {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        let mut offset = self.offset;
        let base;
        match self.ty {
            StackOperandType::Spill | StackOperandType::Local => {
                offset = -offset;
                base = RegType::Fp;
            }
            StackOperandType::CallParam => {
                base = RegType::Sp;
            }
            StackOperandType::SelfArg => {
                base = RegType::Fp;
            }
            _ => {
                panic!("Unsupported operation");
            }
        }
        format!("{}, #{}", base.to_arm(module), to_signed_hex_string(offset))
    }
}

pub fn to_signed_hex_string(offset: i64) -> String {
    let mut offset = offset.clone();
    let mut result = String::new();
    if offset < 0 {
        result.push('-');
        offset = -offset;
    }
    result.push_str("0x");
    result.push_str(&offset.to_string());
    result
}

impl ToArm for RegType {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self {
            RegType::R0 => "r0".to_string(),
            RegType::R1 => "r1".to_string(),
            RegType::R2 => "r2".to_string(),
            RegType::R3 => "r3".to_string(),
            RegType::R4 => "r4".to_string(),
            RegType::R5 => "r5".to_string(),
            RegType::R6 => "r6".to_string(),
            RegType::R7 => "r7".to_string(),
            RegType::R8 => "r8".to_string(),
            RegType::R9 => "r9".to_string(),
            RegType::R10 => "R10".to_string(),
            RegType::Fp => "fp".to_string(),
            RegType::Ip => "ip".to_string(),
            RegType::Sp => "sp".to_string(),
            RegType::Lr => "lr".to_string(),
            RegType::Pc => "pc".to_string(),
        }
    }
}
impl ToArm for VirtReg {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        return format!("vr{}", self.index);
    }
}
impl ToArm for IntReg {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        return self.ty.to_arm(module);
    }
}
impl ToArm for VfpDoubleReg {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        return "d16".to_string();
    }
}
impl ToArm for VfpReg {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!("s{}", self.index)
    }
}
impl ToArm for BrInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "B{}\t{}",
            self.cond.to_string(),
            self.target_label.clone().unwrap()
        )
    }
}
impl ToArm for BXInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        unimplemented!("BXInst")
    }
}
impl ToArm for CallInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!("BL\t{}", self.label.label)
    }
}
impl ToArm for CMPInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "CMP\t{},{}",
            self.get_uses()[0].to_arm(module),
            self.get_uses()[1].to_arm(module),
        )
    }
}
impl ToArm for FBinOpInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "{}\t{},{}",
            self.op.to_arm(module),
            self.get_uses()[0].to_arm(module),
            self.get_uses()[1].to_arm(module),
        )
    }
}

impl ToArm for FBinaryOp {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        match self.0 {
            BinaryOp::Add => "VADD.F32".to_string(),
            BinaryOp::Div => "VDIV.F32".to_string(),
            BinaryOp::Mul => "VMUL.F32".to_string(),
            BinaryOp::Sub => "VSUB.F32".to_string(),
            BinaryOp::Mod => todo!(),
            BinaryOp::LogAnd => todo!(),
            BinaryOp::LogOr => todo!(),
            BinaryOp::LogEq => todo!(),
            BinaryOp::LogNeq => todo!(),
            BinaryOp::LogLt => todo!(),
            BinaryOp::LogGt => todo!(),
            BinaryOp::LogLe => todo!(),
            BinaryOp::LogGe => todo!(),
        }
    }
}

impl ToArm for FCMPInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "VCMP.F32\t{},{}",
            self.get_uses()[0].to_arm(module),
            self.get_uses()[1].to_arm(module),
        )
    }
}
impl ToArm for LDRInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "LDR\t{},[{}]",
            self.get_defs()[0].to_arm(module),
            self.get_uses()[0].to_arm(module),
        )
    }
}
impl ToArm for MovInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "{}\t{},{}",
            self.ty.to_string(),
            self.get_defs()[0].to_arm(module),
            self.get_uses()[0].to_arm(module),
        )
    }
}
impl ToArm for STRInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "STR\t{},[{}]",
            self.get_uses()[0].to_arm(module),
            self.get_uses()[1].to_arm(module),
        )
    }
}
impl ToArm for VCVTInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "{}\t{},{}",
            self.ty.to_string(),
            self.get_defs()[0].to_arm(module),
            self.get_uses()[0].to_arm(module),
        )
    }
}
impl ToArm for VLDRInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "VLDR\t{},[{}]",
            self.get_defs()[0].to_arm(module),
            self.get_uses()[0].to_arm(module),
        )
    }
}
impl ToArm for VMovInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "VMOV\t{},{}",
            self.get_defs()[0].to_arm(module),
            self.get_uses()[0].to_arm(module),
        )
    }
}
impl ToArm for VMRSInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!("vmrs\tAPSR_nzcv, FPSCR")
    }
}
impl ToArm for VSTRInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        format!(
            "VSTR\t{},[{}]",
            self.get_uses()[0].to_arm(module),
            self.get_uses()[1].to_arm(module),
        )
    }
}
impl PrologueInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        static FORMAT: &str = "push\t{fp, lr}\n\tmov\tfp, sp\n";
        let mut sb = String::from(FORMAT);
        let func_id = self.func;
        let func = module.get_func(func_id);

        let stack_size = func.stack_state.total_stack_size() as i32;
        let binop = BinOpInst::new(
            BinaryOp::Sub,
            IntReg::new(RegType::Sp).into(),
            IntReg::new(RegType::Sp).into(),
            IntImm::new(stack_size as u32).into(),
        );
        let binop_id = module.alloc_value(AsmValue::Inst(AsmInst::BinOp(binop)));
        let subs = module.expand_bin_op_ip(binop_id);
        subs.iter().for_each(|inst_id| {
            let inst = module.get_inst(*inst_id).clone();
            sb.push_str(&format!("\t{}\n", inst.to_arm(module)));
        });
        sb
    }
}
impl ToArm for RetInst {
    fn to_arm(&self, module: &mut AsmModule) -> String {
        let comment = if !self.get_uses().is_empty() {
            format!("ret {}", self.get_uses()[0].to_arm(module)) // assuming YourType implements ToString
        } else {
            String::from("ret void")
        };
        format!("mov\tsp, fp\t@ {}\n\tpop\t{{fp, lr}}\n\tbx\tlr", comment)
    }
}
