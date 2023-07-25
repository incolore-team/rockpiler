pub enum AsmValue {
    Block(AsmBlock),
    GlobalVariable(AsmGlobalVariable),
    Function(AsmFunction),
    Instruction(AsmInst),
}

type AsmValueId = id_arena::Id<AsmValue>;
/// VirtReg -> PhyReg
type RegConstraintMap = std::collections::HashMap<AsmValueId, PhyReg>;

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
            cur_bb: None
        }
    }

    pub fn alloc_value(&mut self, value: AsmValue) -> AsmValueId {
        self.values.alloc(value)
    }
}

pub enum AsmInst {
    Call(CallInst),
}

pub struct CallInst {
    /// jump target
    pub label: AsmValueId,
    /// used to bind args to calling convention registers
    pub in_constraints: RegConstraintMap,
    pub out_constraints: RegConstraintMap,
}

pub struct Prologue {
    pub out_constraints: RegConstraintMap,
}

pub enum AsmOperand {
    Imm(Imm),
    StackOperand(StackOperand),
    VirtReg(VirtReg),
    Reg(Reg),
    VfpDoubleReg(VfpDoubleReg),
    VfpReg(VfpReg),
}

pub enum Imm {
    FloatImm(FloatImm),
    IntImm(IntImm),
    LabelImm(LabelImm),
}

pub trait ImmTrait {
    fn highest_bit(&self) -> u32;
    fn lowest_dword(&self) -> Imm;
    fn highest_dword(&self) -> Imm;
    fn lowest_word(&self) -> Imm;
    fn highest_word(&self) -> Imm;
}

pub struct StackOperand {
    pub ty: StackOperandType,
    pub offset: i64,
}

pub enum StackOperandType {
    Local,
    Spill,
    CallParam,
    SelfArg,
}

pub struct VirtReg {
    pub index: i32,
    pub is_float: bool,
}

pub struct Reg {
    pub ty: RegType,
    pub is_float: bool,
}

pub enum RegType {
    R0, R1, R2, R3,
    R4, R5, R6, R7, R8, R9, R10, Fp, Ip, Sp, Lr, Pc,
}

impl RegType {
    pub fn is_callee_saved(&self) -> bool {
        let idx:i32 = Into::<i32>::into(*self);
        idx >= 4 && idx <= 10  
    }
}

impl Into<i32> for RegType {
    fn into(self) -> i32 {
        match self {
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

pub struct VfpDoubleReg {
    pub is_float: bool,
}

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
    fn highest_bit(&self) -> u32 {
        31
    }

    fn lowest_dword(&self) -> Imm {
        unimplemented!()
    }

    fn highest_dword(&self) -> Imm {
        unimplemented!()
    }

    fn lowest_word(&self) -> Imm {
        unimplemented!()
    }

    fn highest_word(&self) -> Imm {
        unimplemented!()
    }
}

pub struct IntImm {
    pub value: u32,
}

pub struct LabelImm {
    pub state: LabelImmState,
    pub label: String,
    pub is_float: bool,
}

pub enum LabelImmState {
    Label,
    High,
    Low,
}
/// ARM ABI calling convention
/// https://learn.microsoft.com/zh-cn/cpp/build/overview-of-arm-abi-conventions?view=msvc-170
pub struct VfpCallingConvention {
    // 结果有几种情况：1是r0-r3，2是在s0-s15，3是在内存里（StackOperand）。
    call_param: Vec<AsmValueId>,
    // 由于中间push了FP和LR，所以对于内存变量，访问的offset会有所不同
    self_arg: Vec<AsmValueId>,
    ret_reg: Option<AsmValueId>, // s0或者r0
    ncrn: u64,
    nsaa: u64, // 计算结束后也是需要占用的栈空间大小
    next_vfp: u64,
}

impl VfpCallingConvention {
    pub fn new() -> Self {
        VfpCallingConvention {
            call_param: Vec::new(),
            self_arg: Vec::new(),
            ret_reg: None,
            /// 下一个核心寄存器号
            ncrn: 0, 
            /// 下一个堆叠自变量地址
            nsaa: 0,
            next_vfp: 0,
        }
    }


    pub fn add_param(&mut self, _t: Type) -> AsmOperand {
        unimplemented!()
    }

    pub fn get_stack_size(&self) -> u64 {
        self.nsaa
    }

    pub fn get_ret_reg(&self) -> AsmOperand {
        self.ret_reg.clone()
    }
}