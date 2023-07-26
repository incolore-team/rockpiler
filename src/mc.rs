pub enum AsmValue {
    Block(AsmBlock),
    GlobalVariable(AsmGlobalVariable),
    Function(AsmFunction),
    Instruction(AsmInst),
}

type AsmValueId = id_arena::Id<AsmValue>;
/// VirtReg -> PhyReg
type RegConstraintMap = std::collections::HashMap<AsmValueId, IntReg>;

pub struct AsmModule {
    values: id_arena::Arena<AsmValue>,
    pub globals: Vec<AsmValueId>,
    pub bss_globals: Vec<AsmValueId>, // uninitialized globals
    pub functions: Vec<AsmValueId>,

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

    pub fn cur_bb(&self) -> &AsmBlock {
        let cur_bb = self.cur_bb.unwrap();
        if let AsmValue::Block(bb) = self.values.get(cur_bb).unwrap() {
            bb
        } else {
            panic!("current value is not a block");
        }
    }
}

pub struct AsmFunction {
    pub name: String, // label
    pub entry: AsmValueId,
    pub bbs: Vec<AsmValueId>,
    pub stack_state: StackState,
}

pub struct AsmGlobalVariable {
    pub type_tag: AsmTypeTag,
}
#[derive(Debug, PartialEq, Clone)]
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

pub struct StackState {
    pub spill_done: bool,
    pub spill_size: u32,
    pub local_size: u32,
    max_arg_size: u32,
}

pub struct AsmBlock {
    pub prev: Option<AsmValueId>,
    pub next: Option<AsmValueId>,
    pub preds: Vec<AsmValueId>,
    pub succs: Vec<AsmValueId>,
}

impl AsmModule {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            bss_globals: Vec::new(),
            functions: Vec::new(),
            values: id_arena::Arena::new(),
            cur_func: None,
            cur_bb: None,
        }
    }

    pub fn alloc_value(&mut self, value: AsmValue) -> AsmValueId {
        self.values.alloc(value)
    }
}
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
#[derive(Debug, PartialEq, Clone)]

pub enum AsmOperand {
    Imm(Imm),
    StackOperand(StackOperand),
    VirtReg(VirtReg),
    IntReg(IntReg),
    VfpDoubleReg(VfpDoubleReg),
    VfpReg(VfpReg),
}
#[derive(Debug, PartialEq, Clone)]

pub enum Imm {
    FloatImm(FloatImm),
    IntImm(IntImm),
    LabelImm(LabelImm),
}

pub trait ImmTrait {
    fn highest_one_bit(&self) -> u32;
    fn lowest_dword(&self) -> Imm;
    fn highest_dword(&self) -> Imm;
    fn lowest_word(&self) -> Imm;
    fn highest_word(&self) -> Imm;
}

#[derive(Debug, PartialEq, Clone)]
pub struct StackOperand {
    pub ty: StackOperandType,
    pub offset: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StackOperandType {
    Local,
    Spill,
    CallParam,
    SelfArg,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VirtReg {
    pub index: i32,
    pub is_float: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntReg {
    pub ty: RegType,
    pub is_float: bool,
}

impl From<i64> for IntReg {
    fn from(val: i64) -> Self {
        assert!(val >= 0 && val < 16);
        Self {
            ty: RegType::from(val),
            is_float: false,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
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
    pub fn is_callee_saved(&self) -> bool {
        let idx: i64 = (*self).into();
        idx >= 4 && idx <= 10
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
#[derive(Debug, PartialEq, Clone)]
pub struct VfpDoubleReg {
    pub is_float: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VfpReg {
    pub index: i64,
    pub use_as_double: bool,
    pub is_float: bool,
}

impl VfpReg {
    pub fn is_callee_saved(&self) -> bool {
        self.index >= 16
    }
}

impl From<i64> for VfpReg {
    fn from(val: i64) -> Self {
        assert!(val >= 0 && val < 32);
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
        Imm::IntImm(IntImm {
            value: raw & 0xffff_ffff,
        })
    }

    fn highest_dword(&self) -> Imm {
        let raw = self.cast_to_raw_int();
        Imm::IntImm(IntImm { value: raw >> 32 })
    }

    fn lowest_word(&self) -> Imm {
        let raw = self.cast_to_raw_int();
        Imm::IntImm(IntImm {
            value: raw & 0xffff,
        })
    }

    fn highest_word(&self) -> Imm {
        let raw = self.cast_to_raw_int();
        Imm::IntImm(IntImm {
            value: (raw >> 16) & 0xffff,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntImm {
    pub value: u32,
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
        Imm::IntImm(IntImm {
            value: self.value & 0xffff_ffff,
        })
    }

    fn highest_dword(&self) -> Imm {
        Imm::IntImm(IntImm {
            value: self.value >> 32,
        })
    }

    fn lowest_word(&self) -> Imm {
        Imm::IntImm(IntImm {
            value: self.value & 0xffff,
        })
    }

    fn highest_word(&self) -> Imm {
        Imm::IntImm(IntImm {
            value: (self.value >> 16) & 0xffff,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
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
        return 1 << 31;
    }

    fn lowest_dword(&self) -> Imm {
        return Imm::LabelImm(LabelImm {
            state: LabelImmState::Low,
            label: self.label.clone(),
            is_float: false,
        });
    }

    fn highest_dword(&self) -> Imm {
        return Imm::LabelImm(LabelImm {
            state: LabelImmState::High,
            label: self.label.clone(),
            is_float: false,
        });
    }

    fn lowest_word(&self) -> Imm {
        return Imm::LabelImm(LabelImm {
            state: LabelImmState::Low,
            label: self.label.clone(),
            is_float: false,
        });
    }

    fn highest_word(&self) -> Imm {
        return Imm::LabelImm(LabelImm {
            state: LabelImmState::High,
            label: self.label.clone(),
            is_float: false,
        });
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LabelImmState {
    Label,
    High,
    Low,
}

pub enum CallConv {
    BaseCallConv(BaseCallConv),
    VfpCallConv(VfpCallConv),
}
/// ARM ABI calling convention
/// https://learn.microsoft.com/zh-cn/cpp/build/overview-of-arm-abi-conventions?view=msvc-170
use std::collections::LinkedList;

pub struct BaseCallConv {
    call_param: LinkedList<AsmOperand>,
    ret_reg: AsmOperand,
    ncrn: i64,
    nsaa: i64,
}

trait CallConvTrait {
    fn get_stack_size(&self) -> i64;
    fn get_ret_reg(&self) -> AsmOperand;
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParamInfo {
    pub is_pointer: bool,
    pub base_type: AsmTypeTag,
}

impl BaseCallConv {
    pub fn new() -> Self {
        Self {
            call_param: LinkedList::new(),
            ret_reg: AsmOperand::IntReg(IntReg {
                ty: RegType::R0,
                is_float: false,
            }),
            ncrn: 0,
            nsaa: 0,
        }
    }

    pub fn resolve(mut self, params: &[ParamInfo], ret_ty: AsmTypeTag) -> Self {
        if ret_ty != AsmTypeTag::VOID {
            self.ret_reg = AsmOperand::IntReg(IntReg {
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
        let mut ret;
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
            ret = AsmOperand::IntReg(IntReg {
                ty,
                is_float: false,
            });
            self.call_param.push_back(ret.clone());
            self.ncrn += size / 4;
        } else {
            assert_eq!(self.ncrn, 4);
            ret = AsmOperand::StackOperand(StackOperand {
                ty: StackOperandType::CallParam,
                offset: self.nsaa,
            });
            self.call_param.push_back(ret.clone());
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

pub struct VfpCallConv {
    // 结果有几种情况：1是r0-r3，2是在s0-s15，3是在内存里（StackOperand）。
    pub call_param: Vec<AsmOperand>,
    // 由于中间push了FP和LR，所以对于内存变量，访问的offset会有所不同
    pub self_arg: Vec<AsmOperand>,
    pub ret_reg: AsmOperand, // s0或者r0
    pub ncrn: i64,           //  Next Core Register Number
    pub nsaa: i64,           // Next Stacked Argument Address 计算结束后也是需要占用的栈空间大小
    pub next_vfp: i64,
}

impl VfpCallConv {
    pub fn new() -> Self {
        Self {
            call_param: Vec::new(),
            self_arg: Vec::new(),
            ret_reg: AsmOperand::IntReg(IntReg {
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
            self.ret_reg = AsmOperand::IntReg(IntReg::from(0));
        }

        for param in params {
            assert_ne!(param.base_type, AsmTypeTag::DOUBLE);
            let size = 4;
            if param.base_type == AsmTypeTag::FLOAT {
                // if is VFP CPRC (Co-processor Register Candidate)
                if self.next_vfp < 16 {
                    let result = AsmOperand::VfpReg(VfpReg::from(self.next_vfp));
                    self.call_param.push(result.clone());
                    self.self_arg.push(result);
                    self.next_vfp += 1;
                } else {
                    assert_eq!(self.next_vfp, 16);
                    let result = AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::CallParam,
                        offset: self.nsaa,
                    });
                    self.call_param.push(result.clone());
                    //
                    self.self_arg.push(AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::SelfArg,
                        offset: self.nsaa + 8,
                    }));
                    self.nsaa += size;
                }
            } else {
                if (self.ncrn + (size / 4)) <= 4 {
                    // 寄存器能分配下
                    let result = AsmOperand::IntReg(IntReg::from(self.ncrn));
                    self.call_param.push(result.clone());
                    self.self_arg.push(result);
                    self.ncrn = self.ncrn + (size / 4);
                } else {
                    assert_eq!(self.ncrn, 4);
                    self.call_param.push(AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::CallParam,
                        offset: self.nsaa,
                    }));
                    self.self_arg.push(AsmOperand::StackOperand(StackOperand {
                        ty: StackOperandType::SelfArg,
                        offset: self.nsaa + 8,
                    }));
                    self.nsaa += size;
                }
            }
        }
        self
    }

    pub fn add_param(&mut self, param_info: ParamInfo) -> AsmOperand {
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
