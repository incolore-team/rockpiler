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
        }
    }

    pub fn alloc_value(&mut self, value: AsmValue) -> AsmValueId {
        self.values.alloc(value)
    }
}

pub struct VirtReg {
    pub id: u32,
    pub is_float: bool,
}

pub enum PhyReg {
    Int(IntReg),
    Vfp(VfpReg),
}
// args and return value (caller saved)
// r0,
// r1,
// r2,
// r3,
// local variables (callee saved)
// r4,
// r5,
// r6,
// r7,
// r8,
// r9,
// r10,
// fp,
// special purposes
// ip,
// sp,
// lr,
// pc;
// some aliases
// fp = r11,  // frame pointer (omitted), allocatable
// ip = r12,  // ipc scratch register, used in some instructions (caller saved)
// sp = r13,  // stack pointer
// lr = r14,  // link register (caller saved)
// pc = r15,  // program counter
pub struct IntReg {
    pub idx: u32,
}

// vfp(vector floating point) registers
// idx \in [0, 31]
pub struct VfpReg {
    pub idx: u32,
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

pub enum Imm {
    Int32(i32),
    Float(f32),
    /// 表示一个跳转目标，因此 Label 是立即数
    Label(i32),
}
