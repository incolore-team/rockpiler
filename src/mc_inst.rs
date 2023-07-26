/*

rewrite in Rust. But Don't save parent in any Inst struct. Use enum instead of trait

// VSTR{C} Fd, [Rn{, #<immed>}] imm范围0-1020
public class VLDRInst extends AsmInst implements StackOpInst {
    public VLDRInst(AsmBlock p, AsmOperand dest, AsmOperand addr) {
        parent = p;
        defs.add(dest);
        uses.add(addr);
    }

    @Override
    public String toString() {
        return String.format("VLDR\t%s, [%s]",
            defs.get(0).toString(), uses.get(0).toString());
    }

    @Override
    public boolean isImmFit(StackOperand so) {
        return VLDRInst.isImmFitStatic(so);
    }

    // `VSTR/VLDR Fd, [Rn{, #<immed>}]`  Immediate range 0-1020, multiple of 4.
    public static boolean isImmFitStatic(StackOperand so) {
        assert (so.offset % 4) == 0;
        if (so.offset >= 0 && so.offset <= 1020) {
            return true;
        }
        return false;
    }
}

public class BinOpInst extends AsmInst implements ImmOpInst {
    public BinaryOp op;

    public BinOpInst(AsmBlock p, BinaryOp op, AsmOperand to, AsmOperand op1, AsmOperand op2) {
        parent = p;
        this.op = op;
        defs.add(to);
        uses.add(op1);
        uses.add(op2);
    }

    public static String opToString(BinaryOp op) {
        switch(op) {
            case ADD: return "ADD";
            case DIV: return "SDIV";
            case MUL: return "MUL";
            case SUB: return "SUB";
            default: return null;
        }
    }

    @Override
    public String toString() {
        return String.format("%s\t%s, %s, %s", opToString(op), defs.get(0).toString(),
                                    uses.get(0).toString(), uses.get(1).toString());
    }

    public boolean isImmFit(Imm m) {
        if (op == BinaryOp.ADD || op == BinaryOp.SUB) {
            return Operand2.isImmFit(m);
        }
        return false;
    }
}


public class BrInst extends AsmInst {
    public Cond cond;
    public AsmBlock target;

    public static class Builder {
        private BrInst inst;

        public Builder(AsmBlock parent, AsmBlock target) {
            inst = new BrInst();
            inst.parent = parent;
            inst.target = target;
            inst.cond = Cond.AL;
        }

        public Builder addCond(Cond c) {
            inst.cond = c;
            return this;
        }

        public Builder addComment(String c) {
            inst.comment = c;
            return this;
        }

        public BrInst build() {
            return inst;
        }
    }

    @Override
    public String toString() {
        return String.format("B%s\t%s", cond.toString(), target.label);
    }
}


public class BXInst extends AsmInst {
    public BXInst(AsmBlock p, AsmOperand op) {
        parent = p;
        uses.add(op);
    }
}


public class CallInst extends ConstrainRegInst {
    public LabelImm target;
    public CallingConvention cc;

    public CallInst(AsmBlock p, LabelImm f, CallingConvention cc) {
        parent = p;
        target = f;
        this.cc = cc;
    }

    @Override
    public String toString() {
        return String.format("BL\t%s", target.label);
    }

    // Call 指令设置约束使用更下面两个方法
    @Override
    public void setConstraint(VirtReg reg, Reg phyReg) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setConstraint(VirtReg reg, VfpReg phyReg) {
        throw new UnsupportedOperationException();
    }

    public void setConstraint(VirtReg reg, Reg phyReg, boolean argOrRet) {
        if (argOrRet) {
            inConstraints.put(phyReg, reg);
        } else {
            outConstraints.put(phyReg, reg);
        }
    }

    public void setConstraint(VirtReg reg, VfpReg phyReg, boolean argOrRet) {
        if (argOrRet) {
            inConstraints.put(phyReg, reg);
        } else {
            outConstraints.put(phyReg, reg);
        }
    }

}


public class CMPInst extends AsmInst implements ImmOpInst {
    public CMPInst(AsmBlock p, AsmOperand op1, AsmOperand op2) {
        parent = p;
        uses.add(op1);
        uses.add(op2);
    }

    @Override
    public String toString() {
        return String.format("CMP\t%s, %s",
                    uses.get(0).toString(), uses.get(1).toString());
    }

    public boolean isImmFit(Imm m) {
        return Operand2.isImmFit(m);
    }
}


/**
* 1. fadd - vadd.f32 Fd, Fn, Fm 不支持立即数
* 2. fsub - vsub.f32同上
* 3. fmul - vmul.f32
* 4. fdiv - vdiv.f32
* 5. 浮点数好像不支持取模
 */
public class FBinOpInst extends AsmInst {
    public BinaryOp op;

    public FBinOpInst(AsmBlock p, BinaryOp op, AsmOperand to, AsmOperand op1, AsmOperand op2) {
        parent = p;
        this.op = op;
        assert to.isFloat && op1.isFloat && op2.isFloat;
        defs.add(to);
        uses.add(op1);
        uses.add(op2);
    }

    public static String opToString(BinaryOp op) {
        switch(op) {
            case ADD: return "VADD.F32";
            case DIV: return "VDIV.F32";
            case MUL: return "VMUL.F32";
            case SUB: return "VSUB.F32";
            default: return null;
        }
    }

    @Override
    public String toString() {
        return String.format("%s\t%s, %s, %s", opToString(op), defs.get(0).toString(),
                                    uses.get(0).toString(), uses.get(1).toString());
    }
}


public class FCMPInst extends AsmInst {
    public FCMPInst(AsmBlock p, AsmOperand op1, AsmOperand op2) {
        parent = p;
        uses.add(op1);
        uses.add(op2);
    }

    @Override
    public String toString() {
        return String.format("VCMP.F32\t%s, %s",
                    uses.get(0).toString(), uses.get(1).toString());
    }
}


/**
 * 同时表示
 * 1. 普通的基于寄存器的加载
 * 3. 基于sp/bp带偏移的加载
 * dest必须是寄存器，addr可以是寄存器，StackOperand，Imm立即数。
 */
public class LoadInst extends AsmInst implements StackOpInst {
    public LoadInst(AsmBlock p, AsmOperand dest, AsmOperand addr) {
        parent = p;
        defs.add(dest);
        uses.add(addr);
    }

    @Override
    public String toString() {
        return String.format("LDR\t%s, [%s]",
            defs.get(0).toString(), uses.get(0).toString());
    }

    public boolean isImmFit(StackOperand so) {
        return LoadInst.isImmFitStatic(so);
    }

    // `LDR/STR Rd, [Rn {, #<offset>}]` -4095 to +4095
    public static boolean isImmFitStatic(StackOperand so) {
        // 范围暂时放窄一些，等测例都过了再改回来
        if (so.offset <= 4070 && so.offset >= -4070) {
            return true;
        }
        return false;
    }
}


public class MovInst extends AsmInst {
    public enum Ty {
        REG,
        MOVW,
        MOVT;

        @Override
        public String toString() {
            switch (this) {
                case MOVT: return "MOVT";
                case MOVW: return "MOVW";
                case REG: return "MOV";
            }
            return null;
        }
    }
    public Ty ty;
    public Cond cond;

    public MovInst(AsmBlock p, Ty ty, AsmOperand to, AsmOperand from) {
        parent = p;
        this.ty = ty;
        defs.add(to);
        uses.add(from);
        cond = Cond.AL;
    }

    public static List<AsmInst> loadImm(AsmBlock p, AsmOperand reg, Imm imm) {
        var ret = new ArrayList<AsmInst>();
        if (imm instanceof FloatImm) {
            var fimm = (FloatImm) imm;
            assert reg.isFloat == true;
            // 先借助保留的IP加载进来
            var tmp = new Reg(Reg.Type.ip);
            ret.addAll(loadImm(p, tmp, new IntImm(fimm.bitcastInt())));
            // 从IP移到目标寄存器里。
            ret.add(new VMovInst(p, VMovInst.Ty.A2S, reg, tmp));
        } else if (imm instanceof LabelImm || imm instanceof IntImm) { // 简单情况
            // MOVW会自动清空高16bit，所以立即数小于16bit范围的可以直接MOVW
            if (imm.highestOneBit() < 65535) {
                ret.add(new MovInst(p, Ty.MOVW, reg, imm));
            } else {
                ret.add(new MovInst(p, Ty.MOVW, reg, imm.getLow16()));
                ret.add(new MovInst(p, Ty.MOVT, reg, imm.getHigh16()));
            }
        } else {throw new UnsupportedOperationException();}
        return ret;
    }

    @Override
    public String toString() {
        return ty.toString()+cond.toString()+"\t"+defs.get(0).toString()+", "+uses.get(0).toString();
    }
}


public class Prologue extends ConstrainRegInst {
    // 寄存器分配的起始约束 - 参数

    // 所在函数
    AsmFunc func;

    public Prologue(AsmBlock p, AsmFunc f) {
        func = f;
        parent = p;
    }

    public static String format = "push\t{fp, lr}\n"
                                + "\tmov\tfp, sp\n";
    @Override
    public String toString() {
        var sb = new StringBuilder(format);
        var stackSize = Math.toIntExact(func.sm.totalStackSize());
        var subs = Generator.expandBinOpIP(new BinOpInst(null, BinaryOp.SUB, new Reg(Reg.Type.sp), new Reg(Reg.Type.sp), new IntImm(stackSize)));
        subs.forEach(inst -> {sb.append("\t").append(inst.toString()).append("\n");});
        return sb.toString();
    }

    @Override
    public void setConstraint(VirtReg reg, Reg phyReg) {
        assert !reg.isFloat;
        outConstraints.put(phyReg, reg);
    }

    @Override
    public void setConstraint(VirtReg reg, VfpReg phyReg) {
        assert reg.isFloat;
        outConstraints.put(phyReg, reg);
    }
}

// 代表了需要插入epilogue和返回的抽象指令
// use可能为空，代表空的return，也可能返回一个值
public class RetInst extends ConstrainRegInst {
    // 寄存器分配的约束-返回值

    // 所在函数
    AsmFunc func;

    public RetInst(AsmBlock p, AsmFunc f) {
        func = f;
        parent = p;
    }

    public static String format = "mov\tsp, fp\t@ %s\n"
                                    + "\tpop\t{fp, lr}\n"
                                    + "\tbx\tlr";

    @Override
    public String toString() {
        String comment;
        if (uses.size() > 0) {
            comment = "ret " + uses.get(0).toString();
        } else {
            comment = "ret void";
        }
        return String.format(format, comment);
    }

    @Override
    public void setConstraint(VirtReg reg, Reg phyReg) {
        assert !reg.isFloat;
        inConstraints.put(phyReg, reg);
    }

    @Override
    public void setConstraint(VirtReg reg, VfpReg phyReg) {
        assert reg.isFloat;
        inConstraints.put(phyReg, reg);
    }
}

public class StoreInst extends AsmInst implements StackOpInst {
    public StoreInst(AsmBlock p, AsmOperand val, AsmOperand addr) {
        parent = p;
        uses.add(val);
        uses.add(addr);
    }

    @Override
    public String toString() {
        return String.format("STR\t%s, [%s]",
            uses.get(0).toString(), uses.get(1).toString());
    }

    public boolean isImmFit(StackOperand so) {
        return LoadInst.isImmFitStatic(so);
    }
}

/**
 * label的名字按照AsmFunc.tailCallLabel的格式设置
 */
public class TailCallInst extends CallInst {

    public TailCallInst(AsmBlock p, LabelImm f, CallingConvention cc) {
        super(p, f, cc);
    }

    @Override
    public String toString() {
        return String.format("B\t%s", target.label);
    }
}

public class VCVTInst extends AsmInst {
    // 根据quick reference card上的条目分类
    public enum Ty {
        F2I,
        I2F,
        F2D;

        @Override
        public String toString() {
            switch (this) {
                case F2I: return "VCVT.S32.F32";
                case I2F: return "VCVT.F32.S32";
                case F2D: return "vcvt.f64.f32";
            }
            return null;
        }
    }
    Ty ty;

    public VCVTInst(AsmBlock p, Ty ty, AsmOperand to, AsmOperand from) {
        parent = p;
        this.ty = ty;
        defs.add(to);
        uses.add(from);
    }

    @Override
    public String toString() {
        return ty.toString()+"\t"+defs.get(0).toString()+", "+uses.get(0).toString();
    }
}
// VSTR{C} Fd, [Rn{, #<immed>}] imm范围0-1020
public class VLDRInst extends AsmInst implements StackOpInst {
    public VLDRInst(AsmBlock p, AsmOperand dest, AsmOperand addr) {
        parent = p;
        defs.add(dest);
        uses.add(addr);
    }

    @Override
    public String toString() {
        return String.format("VLDR\t%s, [%s]",
            defs.get(0).toString(), uses.get(0).toString());
    }

    @Override
    public boolean isImmFit(StackOperand so) {
        return VLDRInst.isImmFitStatic(so);
    }

    // `VSTR/VLDR Fd, [Rn{, #<immed>}]`  Immediate range 0-1020, multiple of 4.
    public static boolean isImmFitStatic(StackOperand so) {
        assert (so.offset % 4) == 0;
        if (so.offset >= 0 && so.offset <= 1020) {
            return true;
        }
        return false;
    }
}
// 未来需要兼顾移动两个寄存器的情况
public class VMovInst extends AsmInst {
    // 根据quick reference card上的条目分类
    public enum Ty {
        CPY,
        A2S,
        S2A;

        @Override
        public String toString() {
            switch (this) {
                case CPY:
                case A2S:
                case S2A: return "VMOV";
            }
            return null;
        }
    }
    Ty ty;
    // public Cond cond; // TODO FPSCR的cond是否需要单独的类

    public VMovInst(AsmBlock p, Ty ty, AsmOperand to, AsmOperand from) {
        parent = p;
        this.ty = ty;
        defs.add(to);
        uses.add(from);
        // cond = Cond.AL;
    }

    @Override
    public String toString() {
        var sj = new StringJoiner(", ");
        defs.forEach(d -> sj.add(d.toString()));
        uses.forEach(u -> sj.add(u.toString()));
        return ty.toString()+"\t"+sj.toString();
    }
}
public class VMRS extends AsmInst {

    public VMRS(AsmBlock p) {
        parent = p;
    }

    @Override
    public String toString() {
        return "vmrs\tAPSR_nzcv, FPSCR";
    }
}
public class VSTRInst extends AsmInst implements StackOpInst {
    public VSTRInst(AsmBlock p, AsmOperand val, AsmOperand addr) {
        parent = p;
        uses.add(val);
        uses.add(addr);
    }

    @Override
    public String toString() {
        return String.format("VSTR\t%s, [%s]",
            uses.get(0).toString(), uses.get(1).toString());
    }

    @Override
    public boolean isImmFit(StackOperand so) {
        return VLDRInst.isImmFitStatic(so);
    }
}

*/

use core::fmt;

use crate::mc::{AsmOperand, AsmValueId, CallConv, Imm, ImmTrait, RegConstraintMap, StackOperand};

#[derive(Debug, PartialEq, Clone)]

pub enum AsmInst {
    Call(CallInst),
}
#[derive(Debug, PartialEq, Clone)]

pub struct CallInst {
    /// jump target
    pub label: AsmValueId,
    /// used to bind args to calling convention registers
    pub in_constraints: RegConstraintMap,
    pub out_constraints: RegConstraintMap,
}
#[derive(Debug, PartialEq, Clone)]

pub struct Prologue {
    pub out_constraints: RegConstraintMap,
}

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

/// https://developer.arm.com/documentation/dui0473/m/vfp-instructions/vstr--floating-point-
/// VSTR{C} Fd, [Rn{, #<immed>}] imm范围0-1020

pub struct VLDRInst {
    pub dest: AsmOperand,
    pub addr: AsmOperand,
}

impl VLDRInst {
    fn new(dest: AsmOperand, addr: AsmOperand) -> VLDRInst {
        VLDRInst { dest, addr }
    }

    fn is_imm_fit(so: &StackOperand) -> bool {
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
struct BinOpInst {
    op: BinaryOp,
    to: AsmOperand,
    op1: AsmOperand,
    op2: AsmOperand,
}

pub struct Operand2;
// Flexible Operand 2 目前仅当作8bit常量使用
impl Operand2 {
    pub fn is_imm_fit(m: &Imm) -> bool {
        m.highest_one_bit() < 255
    }
}

impl BinOpInst {
    fn new(op: BinaryOp, to: AsmOperand, op1: AsmOperand, op2: AsmOperand) -> BinOpInst {
        BinOpInst { op, to, op1, op2 }
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

struct BrInst {
    cond: Cond,
    target: AsmValueId, // AsmBlock
}

struct BrInstBuilder {
    inst: BrInst,
}

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

pub struct BXInst {
    op: AsmOperand,
}
