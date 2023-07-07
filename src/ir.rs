use std::collections::{HashMap, LinkedList};

use crate::{
    ast::*,
    scope::{SymbolId, SymbolTable},
};
use id_arena::Arena;
use linked_hash_map::LinkedHashMap;

pub type ValueId = id_arena::Id<Value>;
/// 一个模块，相当于编译单元的 IR 表示
/// 作为可以优化的 SSA IR，我们需要维护好 Use-Def 关系
/// 每个 Value 可能被其它 Value 使用，记录在 value_user 中
/// 每个 Value 可能使用其它 Value，反映在其结构体的 ValueId 类型的字段中
/// 考虑到优化时，我们可能会：
/// 1. 删除一个 Value a, 那么需要删除 “a 使用 Value b” 的记录
///    例如 a 是无用变量。（否则不能轻易删除一个 value）
/// 2. 替换一个 Value a 为 Value c, 那么需要
///     1. 对任意 a 的用户 z，将 “z 使用 Value a” 的记录改为 “z 使用 Value c”
#[derive(Debug)]
pub struct Module {
    // GlobalValue = GlobalVariable | Function
    pub values: Arena<Value>,
    pub types: Arena<Type>,

    pub global_variables: LinkedHashMap<String, ValueId>,
    pub syms: SymbolTable,
    pub sym2def: LinkedHashMap<SymbolId, ValueId>,
    pub functions: LinkedHashMap<String, ValueId>,
    pub builtins: LinkedHashMap<String, ValueId>,
    pub constants: LinkedHashMap<String, ValueId>,

    // value -> users of the value
    pub value_user: HashMap<ValueId, Vec<ValueId>>,
}

impl Module {
    pub fn new(syms: SymbolTable) -> Self {
        let module = Module {
            values: Arena::new(),
            types: Arena::new(),
            global_variables: LinkedHashMap::new(),
            syms,
            sym2def: LinkedHashMap::new(),
            functions: LinkedHashMap::new(),
            builtins: LinkedHashMap::new(),
            constants: LinkedHashMap::new(),
            value_user: HashMap::new(),
        };
        // module.add_builtin_types();
        module
    }

    pub fn get_bb_mut(&mut self, bb_id: ValueId) -> &mut BasicBlockValue {
        match &mut self.values[bb_id] {
            Value::BasicBlock(bb) => bb,
            _ => panic!("expect a basic block"),
        }
    }

    pub fn get_bb(&self, bb_id: ValueId) -> &BasicBlockValue {
        match &self.values[bb_id] {
            Value::BasicBlock(bb) => bb,
            _ => panic!("expect a basic block"),
        }
    }

    pub fn get_func_mut(&mut self, func_id: ValueId) -> &mut FunctionValue {
        match &mut self.values[func_id] {
            Value::Function(func) => func,
            _ => panic!("expect a function"),
        }
    }

    pub fn get_func(&self, func_id: ValueId) -> &FunctionValue {
        match &self.values[func_id] {
            Value::Function(func) => func,
            _ => panic!("expect a function"),
        }
    }

    pub fn get_inst_mut(&mut self, inst_id: ValueId) -> &mut InstValue {
        match &mut self.values[inst_id] {
            Value::Instruction(inst) => inst,
            _ => panic!("expect an instruction"),
        }
    }

    pub fn get_inst(&self, inst_id: ValueId) -> &InstValue {
        match &self.values[inst_id] {
            Value::Instruction(inst) => inst,
            _ => panic!("expect an instruction"),
        }
    }

    pub fn get_global_var(&self, var_id: ValueId) -> &GlobalVariableValue {
        match &self.values[var_id] {
            Value::GlobalVariable(gv) => gv,
            _ => panic!("expect a global variable"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    GlobalVariable(GlobalVariableValue),
    Function(FunctionValue),
    BasicBlock(BasicBlockValue),
    Instruction(InstValue),
    Const(ConstValue),
    VariableValue(VariableValue),
}

impl Value {
    pub fn as_global_variable(&self) -> Option<&GlobalVariableValue> {
        match self {
            Value::GlobalVariable(gv) => Some(gv),
            _ => None,
        }
    }

    pub fn resolve(id: ValueId, module: &Module) -> &Value {
        &module.values[id]
    }

    pub fn ty(&self) -> Type {
        match self {
            Value::GlobalVariable(gv) => gv.ty.clone(),
            Value::Function(f) => f.ret_ty.clone(),
            Value::BasicBlock(_) => Type::Builtin(BuiltinType::Void),
            Value::Instruction(inst) => inst.ty(),
            Value::Const(c) => c.ty(),
            Value::VariableValue(p) => p.ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVariableValue {
    pub name: String,
    pub ty: Type,
    pub initializer: Option<ValueId>,
}

#[derive(Debug, Clone)]
pub struct FunctionValue {
    pub name: String,
    pub params: Vec<ValueId>, // Parameters
    pub ret_ty: Type,
    pub bbs: BasicBlockList, // BasicBlocks
    pub is_var_arg: bool,
}

impl FunctionValue {
    pub fn new(name: String, params: Vec<ValueId>, ret_ty: Type, is_var_arg: bool) -> Self {
        FunctionValue {
            name,
            params,
            ret_ty,
            bbs: BasicBlockList::default(),
            is_var_arg,
        }
    }

    pub fn resolve_param(param_value_id: ValueId, module: &Module) -> &VariableValue {
        match &module.values[param_value_id] {
            Value::VariableValue(p) => p,
            _ => panic!("expect a parameter"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BasicBlockList {
    pub bbs: LinkedHashMap<String, ValueId>,
    pub next_id: usize, // next id of basic block
}

impl BasicBlockList {
    pub fn append(&mut self, bb_id: ValueId) -> String {
        let name = format!("bb_{}", self.next_id);
        self.next_id += 1;
        self.bbs.insert(name.clone(), bb_id);
        name
    }

    pub fn append_with_name(&mut self, bb_id: ValueId, name: String) {
        self.bbs.insert(name, bb_id);
    }
}

impl FunctionValue {}

#[derive(Debug, Clone, Default)]
pub struct BasicBlockValue {
    pub name: String,
    pub insts: LinkedList<ValueId>,
    pub terminator: Option<ValueId>,
}

impl BasicBlockValue {
    pub fn new(name: String) -> Self {
        BasicBlockValue {
            name,
            insts: LinkedList::new(),
            terminator: None,
        }
    }

    pub fn resolve_inst(inst_value_id: ValueId, module: &Module) -> &InstValue {
        match &module.values[inst_value_id] {
            Value::Instruction(inst) => inst,
            _ => panic!("expect an instruction"),
        }
    }
}

/// 在 LLVM 叫 Argument
#[derive(Debug, Clone)]
pub struct VariableValue {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum InstValue {
    InfixOp(BinaryOperator),
    Load(LoadInst),
    Store(StoreInst),
    Alloca(AllocaInst),
    Branch(BranchInst),
    Jump(JumpInst),
    Gep(GetElementPtrInst),
    Return(ReturnInst),
    Call(CallInst),
    Phi(PhiInst),
    Cast(CastInst),
}

impl InstValue {
    pub fn ty(&self) -> Type {
        match self {
            InstValue::InfixOp(op) => op.ty.clone(),
            InstValue::Load(inst) => inst.ty.clone(),
            InstValue::Store(_) => BuiltinType::Void.into(),
            InstValue::Alloca(inst) => inst.ty.clone(),
            InstValue::Branch(_) => BuiltinType::Void.into(),
            InstValue::Jump(_) => BuiltinType::Void.into(),
            InstValue::Gep(inst) => inst.ty.clone(),
            InstValue::Return(_) => BuiltinType::Void.into(),
            InstValue::Call(inst) => inst.ty.clone(),
            InstValue::Phi(inst) => inst.ty.clone(),
            InstValue::Cast(inst) => inst.new_ty.clone(),
        }
    }
}

/// An immediate value.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(ConstInt),
    Float(ConstFloat),
    Array(ConstArray),
}

impl Into<Value> for ConstValue {
    fn into(self) -> Value {
        Value::Const(self)
    }
}

impl ConstValue {
    pub fn ty(&self) -> Type {
        match self {
            ConstValue::Int(c) => c.ty.clone(),
            ConstValue::Float(c) => c.ty.clone(),
            ConstValue::Array(c) => c.ty.clone(),
        }
    }

    pub fn zero_of(ty: Type) -> Self {
        match ty {
            Type::Builtin(builtin_ty) => match builtin_ty {
                BuiltinType::Int => ConstValue::Int(ConstInt {
                    ty: BuiltinType::Int.into(),
                    value: 0,
                }),
                BuiltinType::Float => ConstValue::Float(ConstFloat {
                    ty: BuiltinType::Float.into(),
                    value: 0.0,
                }),
                BuiltinType::Void => todo!(),
                BuiltinType::Bool => todo!(),
                BuiltinType::UChar => todo!(),
                BuiltinType::Char => todo!(),
                BuiltinType::UShort => todo!(),
                BuiltinType::Short => todo!(),
                BuiltinType::UInt => todo!(),
                BuiltinType::UInt64 => todo!(),
                BuiltinType::Int64 => todo!(),
                BuiltinType::Double => todo!(),
            },
            Type::Pointer(pointee_ty) => ConstValue::Int(ConstInt {
                ty: Type::Pointer(pointee_ty),
                value: 0,
            }),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstInt {
    pub ty: Type,
    pub value: i64,
}

impl Into<Value> for ConstInt {
    fn into(self) -> Value {
        Value::Const(ConstValue::Int(self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstFloat {
    pub ty: Type,
    pub value: f64,
}

impl Into<Value> for ConstFloat {
    fn into(self) -> Value {
        Value::Const(ConstValue::Float(self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstArray {
    pub ty: Type,
    pub values: Vec<ConstValue>,
}

impl Into<Value> for ConstArray {
    fn into(self) -> Value {
        Value::Const(ConstValue::Array(self))
    }
}

/// alloca <ty>
#[derive(Debug, Clone)]
pub struct AllocaInst {
    pub ty: Type,
    pub name: String,
}

impl Into<Value> for AllocaInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Alloca(self))
    }
}

/// OP <ty> <left>, <right>
#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub ty: Type,
    pub operation: InfixOp,
    pub left_operand: ValueId,
    pub right_operand: ValueId,
}

impl Into<Value> for BinaryOperator {
    fn into(self) -> Value {
        Value::Instruction(InstValue::InfixOp(self))
    }
}

/// call <ty> <callee>(<ty> <arg1>, <ty> <arg2>, ...)
#[derive(Debug, Clone)]
pub struct CallInst {
    pub ty: Type,
    pub callee: ValueId,
    pub args: Vec<ValueId>,
}

impl Into<Value> for CallInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Call(self))
    }
}

#[derive(Debug, Clone)]
pub enum CastOp {
    Trunc,
    ZExt,
    SExt,
    FPTrunc,
    FPExt,
    FPToUI,
    FPToSI,
    UIToFP,
    SIToFP,
    PtrToInt,
    IntToPtr,
    BitCast,
}

/// CAST_OP <ty> <value> to <new_ty>
#[derive(Debug, Clone)]
pub struct CastInst {
    pub op: CastOp,
    pub value: ValueId,
    pub new_ty: Type,
}

impl Into<Value> for CastInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Cast(self))
    }
}

/// load <ty>, <ty>* <source>
#[derive(Debug, Clone)]
pub struct LoadInst {
    pub name: String,
    pub ty: Type,
    pub source: ValueId,
}

impl Into<Value> for LoadInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Load(self))
    }
}

/// store <ty> <value>, <ty>* <pointer>
#[derive(Debug, Clone)]
pub struct StoreInst {
    pub src: ValueId,
    pub dst: ValueId,
}

impl Into<Value> for StoreInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Store(self))
    }
}

/// gep [inbounds] [volatile]
///     <pointer_type> <pointer_value>, [integer_type] <index>
///     [, <integer_type> <index> ...]
#[derive(Debug, Clone)]
pub struct GetElementPtrInst {
    pub ty: Type,
    pub pointer: ValueId,
    pub indices: Vec<ValueId>,
}

impl Into<Value> for GetElementPtrInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Gep(self))
    }
}

/// br <label>
#[derive(Debug, Clone)]
pub struct JumpInst {
    pub bb: ValueId,
}

impl Into<Value> for JumpInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Jump(self))
    }
}

/// br i1 <cond>, label <iftrue>, label <iffalse>
#[derive(Debug, Clone)]
pub struct BranchInst {
    pub cond: ValueId,
    pub then_bb: ValueId,
    pub else_bb: ValueId,
}

impl Into<Value> for BranchInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Branch(self))
    }
}

/// ret <ty> <value>
#[derive(Debug, Clone)]
pub struct ReturnInst {
    // pub ty: Type,
    pub value: Option<ValueId>,
}
impl Into<Value> for ReturnInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Return(self))
    }
}

pub enum TermInst {
    Jump(JumpInst),
    Branch(BranchInst),
    Return(ReturnInst),
}

/// phi <ty> [ <val0>, <label0>], ...
#[derive(Debug, Clone)]
pub struct PhiInst {
    pub ty: Type,
    pub incoming: Vec<(ValueId, ValueId)>,
}

impl Into<Value> for PhiInst {
    fn into(self) -> Value {
        Value::Instruction(InstValue::Phi(self))
    }
}
