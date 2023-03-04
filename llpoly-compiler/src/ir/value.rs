use std::{collections::HashMap, fmt::Display};

use linked_hash_map::LinkedHashMap;

use super::{ty::Ty, module::Module};


#[derive(Debug, Default, Eq, Hash, PartialEq, Clone, Copy)]
pub struct ValueId(usize);

impl ValueId {
    pub fn uninitialized() -> ValueId {
        ValueId(0)
    }
}

impl Display for ValueId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<usize> for ValueId {
    fn from(id: usize) -> ValueId {
        ValueId(id)
    }
}
impl From<ValueId> for usize {
    fn from(id: ValueId) -> usize {
        id.0
    }
}

#[derive(Debug, Default)]
pub struct ValueRegistry {
    pub vals: HashMap<ValueId, Value>,
}

impl ValueRegistry {
    pub fn new() -> ValueRegistry {
        ValueRegistry {
            vals: HashMap::new(),
        }
    }
    pub fn next_id(&self) -> ValueId {
        ValueId(self.vals.len())
    }
    pub fn insert(&mut self, val: Value) -> ValueId {
        let id = ValueId(self.vals.len());
        if val.id == ValueId::uninitialized() {
            let mut val = val;
            val.id = id;
            self.vals.insert(id, val);
            return id;
        } else {
            assert_eq!(val.id, id);
        }
        self.vals.insert(id, val);
        id
    }

    pub fn get(&self, id: ValueId) -> Option<&Value> {
        self.vals.get(&id)
    }

    pub fn get_mut(&mut self, id: ValueId) -> Option<&mut Value> {
        self.vals.get_mut(&id)
    }
}

#[derive(Debug, Default)]
pub struct Use {
    pub user: ValueId,
    pub value: ValueId,
}

#[derive(Debug, Default)]
pub struct Value {
    /// ! The print name of the value， e.g. `%0`, `@0`, `@.str`, `@.str.1`
    pub display_name: String,
    /// ! The key to the registry
    pub id: ValueId,
    /// ! The value's users.
    pub uses: Vec<ValueId>,
    /// ! The real value.
    pub proto: ValueProto,
}

impl Value {
    pub fn new(display_name: String, proto: ValueProto) -> Value {
        Value {
            display_name,
            proto,
            ..Default::default()
        }
    }
}

pub trait TypedValue {
    fn ty(&self, ctx: &Module) -> Ty;
}

impl TypedValue for Value {
    fn ty(&self, ctx: &Module) -> Ty {
        match &self.proto {
            ValueProto::None => todo!(),
            ValueProto::Var(v) => v.ty.clone(),
            ValueProto::ConstStr(_) => Ty::ptr(),
            ValueProto::Func(_v) => todo!(),
            ValueProto::BasicBlock(_) => todo!(),
            ValueProto::Inst(v) => v.ty(ctx),
            ValueProto::ImmVal(v) => match &v {
                ImmVal::None => todo!(),
                ImmVal::Int(v) => v.ty.clone(),
                ImmVal::Float(v) => v.ty.clone(),
            },
        }
    }
}

#[derive(Debug, Default)]
pub enum ValueProto {
    #[default]
    None,
    Var(Var),
    ConstStr(String),
    Func(Func),
    BasicBlock(BasicBlock),
    Inst(Inst),
    ImmVal(ImmVal),
}

#[derive(Debug, Default)]
pub enum ImmVal {
    #[default]
    None,
    Int(IntConstVal),
    Float(FloatConstVal),
}

impl ImmVal {
    pub fn ty(&self) -> Ty {
        match self {
            ImmVal::None => todo!(),
            ImmVal::Int(v) => v.ty.clone(),
            ImmVal::Float(v) => v.ty.clone(),
        }
    }

    pub fn int(val: i64, width: usize) -> ImmVal {
        ImmVal::Int(IntConstVal {
            val,
            ty: Ty::int(width),
        })
    }

    pub fn float(val: f64, width: usize) -> ImmVal {
        ImmVal::Float(FloatConstVal {
            val,
            ty: Ty::float(width),
        })
    }
}

#[derive(Debug, Default)]
pub struct IntConstVal {
    pub val: i64,
    pub ty: Ty,
}
#[derive(Debug, Default, Clone)]
pub struct FloatConstVal {
    pub val: f64,
    pub ty: Ty,
}

#[derive(Debug, Default)]
pub enum Inst {
    #[default]
    None,
    Call(CallInst),
    Ret(RetInst),
    Load(LoadInst),
}

impl TypedValue for Inst {
    fn ty(&self, ctx: &Module) -> Ty {
        match self {
            Inst::None => todo!(),
            Inst::Call(v) => {
                let func_id = v.func;
                let func = ctx.get_func(func_id);
                func.ret_ty.clone()
            }
            Inst::Ret(_) => unreachable!("ret inst should not be used as a input value"),
            Inst::Load(v) => v.ty.clone(),
        }
    }
}


#[derive(Debug, Default)]
pub struct CallInst {
    pub func: ValueId, // callee func
    pub args: Vec<ValueId>,
}

#[derive(Debug, Default)]
pub struct RetInst {
    pub val: Option<ValueId>,
}

#[derive(Debug, Default)]
pub struct BrInst {
    pub cond: Option<ValueId>,
    pub true_bb: ValueId,
    pub false_bb: Option<ValueId>,
}

#[derive(Debug, Default)]
pub struct PhiInst {
    pub ty: Ty,
    pub incoming: Vec<(ValueId, ValueId)>,
}

#[derive(Debug, Default)]
pub struct AllocaInst {
    pub ty: Ty,
}

#[derive(Debug, Default)]
pub struct LoadInst {
    pub ty: Ty,
    pub ptr: ValueId,
}

#[derive(Debug, Default)]
pub struct StoreInst {
    pub val: ValueId,
    pub ptr: ValueId,
}

#[derive(Debug, Default)]
pub struct GetElementPtrInst {
    pub ty: Ty,
    pub ptr: ValueId,
    pub indices: Vec<ValueId>,
}


#[derive(Debug, Default)]
pub struct BasicBlock {
    pub name: String,
    pub insts: Vec<ValueId>,
}

impl BasicBlock {
    pub fn new(name: String) -> BasicBlock {
        BasicBlock {
            name,
            insts: Vec::new(),
        }
    }
}

impl BasicBlock {
    pub fn append_inst(&mut self, inst: ValueId) {
        self.insts.push(inst);
    }
}


#[derive(Debug, Default)]
pub struct Func {
    pub name: String,
    pub is_external: bool,
    pub is_builtin: bool,
    pub bb_map: LinkedHashMap<String, ValueId>,
    pub entry_bb: Option<ValueId>, // only-symbol func has no entry_bb
    pub params: Vec<ValueId>,
    pub ret_ty: Ty,

    pub(crate) next_local_id: usize,
}

impl  Func {
    pub fn new_global(name: String, ret_ty: Ty, params: Vec<ValueId>) -> Func {
        Func {
            name,
            is_external: false,
            is_builtin: false,
            bb_map: LinkedHashMap::new(),
            entry_bb: None,
            params,
            ret_ty,
            next_local_id: 0,
        }
    }
}

#[derive(Debug, Default)]
pub struct Var {
    pub name: String,
    pub ty: Ty,
    pub is_global: bool,
    pub is_param: bool,
    pub is_const: bool,
    pub is_external: bool,
    pub is_bultin: bool,
    pub init: Option<ValueId>,
}

impl Var {
    pub fn new_global(name: String, ty: Ty, init: Option<ValueId>) -> Var {
        Var {
            name,
            ty,
            is_global: true,
            is_param: false,
            is_const: false,
            is_external: false,
            is_bultin: false,
            init,
        }
    }

    pub fn new_param(name: String, ty: Ty) -> Var {
        Var {
            name,
            ty,
            is_global: false,
            is_param: true,
            is_const: false,
            is_external: false,
            is_bultin: false,
            init: None,
        }
    }

    pub fn new_const(name: String, ty: Ty, init: ValueId) -> Var {
        Var {
            name,
            ty,
            is_global: false,
            is_param: false,
            is_const: true,
            is_external: false,
            is_bultin: false,
            init: Some(init),
        }
    }

    pub fn new_external(name: String, ty: Ty) -> Var {
        Var {
            name,
            ty,
            is_global: false,
            is_param: false,
            is_const: false,
            is_external: true,
            is_bultin: false,
            init: None,
        }
    }
}
