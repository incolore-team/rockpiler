use std::{collections::HashMap, fmt::Display};

use linked_hash_map::LinkedHashMap;

use crate::ast::{self, TransUnit};
#[macro_export]
macro_rules! value_cast {
    ($match:expr, $pat:pat => $result:expr) => {
        match $match.proto {
            $pat => $result,
            _ => unreachable!(),
        }
    };
}

const BUILTIN_PREFIX: &str = "_llp_builtin__";

#[derive(Debug, Default, Eq, Hash, PartialEq, Clone, Copy)]
pub struct ValueId(usize);

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
    pub fn insert(&mut self, obj: Value) -> ValueId {
        let id = ValueId(self.vals.len());
        self.vals.insert(id, obj);
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
    pub id_name: String,
    pub id: ValueId,
    pub uses: Vec<ValueId>,
    pub proto: ValueProto, // the real value
}

pub trait TypedValue {
    fn ty(&self, ctx: &ModuleContext) -> Ty;
}

impl TypedValue for Value {
    fn ty(&self, ctx: &ModuleContext) -> Ty {
        match &self.proto {
            ValueProto::None => todo!(),
            ValueProto::GlobalVal(v) => v.ty.clone(),
            ValueProto::ConstStr(_) => Ty::ptr(),
            ValueProto::Func(_v) => todo!(),
            ValueProto::BasicBlock(_) => todo!(),
            ValueProto::Inst(v) => v.ty(ctx),
            ValueProto::ConstVal(v) => match &v {
                ConstVal::None => todo!(),
                ConstVal::Int(v) => v.ty.clone(),
                ConstVal::Float(v) => v.ty.clone(),
            },
        }
    }
}

#[derive(Debug, Default)]
pub enum ValueProto {
    #[default]
    None,
    GlobalVal(GlobalVal),
    ConstStr(String),
    Func(Func),
    BasicBlock(BasicBlock),
    Inst(Inst),
    ConstVal(ConstVal),
}

#[derive(Debug, Default)]
pub enum ConstVal {
    #[default]
    None,
    Int(IntConstVal),
    Float(FloatConstVal),
}

impl ConstVal {
    pub fn ty(&self) -> Ty {
        match self {
            ConstVal::None => todo!(),
            ConstVal::Int(v) => v.ty.clone(),
            ConstVal::Float(v) => v.ty.clone(),
        }
    }

    pub fn int(val: i64, width: usize) -> ConstVal {
        ConstVal::Int(IntConstVal {
            val,
            ty: Ty::int(width),
        })
    }

    pub fn float(val: f64, width: usize) -> ConstVal {
        ConstVal::Float(FloatConstVal {
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
    fn ty(&self, ctx: &ModuleContext) -> Ty {
        match self {
            Inst::None => todo!(),
            Inst::Call(v) => {
                let func_id = v.func;
                let func = ctx.resolve_func(func_id);
                func.ret_ty.clone()
            }
            Inst::Ret(_) => unreachable!("ret inst should not be used as a input value"),
            Inst::Load(v) => v.ty.clone(),
        }
    }
}

pub type BasicBlockIndex = ValueId;
#[derive(Debug, Default)]
pub struct BasicBlock {
    pub name: String,
    pub insts: Vec<ValueId>,
    pub next_local_id: usize, // this is used for generating llvm ir value
}

impl BasicBlock {
    pub fn new(name: String, next_local_id: usize) -> BasicBlock {
        BasicBlock {
            name,
            insts: Vec::new(),
            next_local_id,
        }
    }
}

impl BasicBlock {
    pub fn append_inst(&mut self, inst: ValueId) {
        self.insts.push(inst);
    }
}

#[derive(Debug, Default, Clone)]
pub enum Ty {
    #[default]
    Void,
    Int(IntTy),
    Float(FloatTy),
    Ptr(PtrTy),
    Array(ArrayTy),
    Func(FuncTy),
}

impl From<&ast::Ty> for Ty {
    fn from(ty: &ast::Ty) -> Ty {
        match ty {
            ast::Ty::Basic(ast_basic_ty) => match ast_basic_ty {
                ast::BasicTy::Int(i) => match &i {
                    ast::IntTy::Implicit() => todo!(),
                    ast::IntTy::I8() => Ty::int(8),
                    ast::IntTy::I16() => Ty::int(16),
                    ast::IntTy::I32() => Ty::int(32),
                    ast::IntTy::I64() => Ty::int(64),
                    ast::IntTy::I128() => Ty::int(128),
                    ast::IntTy::ISize() => Ty::int(64),
                    ast::IntTy::U8() => Ty::int(8),
                    ast::IntTy::U16() => Ty::int(16),
                    ast::IntTy::U32() => Ty::int(32),
                    ast::IntTy::U64() => Ty::int(64),
                    ast::IntTy::U128() => Ty::int(128),
                    ast::IntTy::USize() => Ty::int(64),
                },
                ast::BasicTy::Float(f) => match &f {
                    ast::FloatTy::Implicit() => todo!(),
                    ast::FloatTy::F32() => Ty::float(32),
                    ast::FloatTy::F64() => Ty::float(64),
                },
                ast::BasicTy::Char() => Ty::uint(8),
                ast::BasicTy::Str() => Ty::ptr(),
                ast::BasicTy::Bool() => Ty::int(8),
            },
            ast::Ty::Pointer(_ast_pointer_ty) => todo!(),
            ast::Ty::Array(_ast_array_ty) => todo!(),
            ast::Ty::User(ast_user_ty) => match ast_user_ty.name.as_str() {
                "buf" => Ty::ptr(),
                _ => todo!(),
            },
        }
    }
}

impl Ty {
    pub fn int(bit: usize) -> Ty {
        Ty::Int(IntTy {
            signed: true,
            bit_width: bit,
        })
    }

    pub fn uint(bit: usize) -> Ty {
        Ty::Int(IntTy {
            signed: false,
            bit_width: bit,
        })
    }

    pub fn float(bit: usize) -> Ty {
        Ty::Float(FloatTy { bit_width: bit })
    }

    pub fn ptr() -> Ty {
        Ty::Ptr(PtrTy {})
    }

    pub fn array(base: Ty, size: ValueId) -> Ty {
        Ty::Array(ArrayTy {
            base: Box::new(base),
            size,
        })
    }

    pub fn func(ret_ty: Ty, param_tys: Vec<Ty>) -> Ty {
        Ty::Func(FuncTy {
            ret_ty: Box::new(ret_ty),
            param_tys,
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct IntTy {
    pub signed: bool,
    pub bit_width: usize,
}
#[derive(Debug, Default, Clone)]
pub struct FloatTy {
    pub bit_width: usize,
}
#[derive(Debug, Default, Clone)]
pub struct PtrTy {}
#[derive(Debug, Default, Clone)]
pub struct ArrayTy {
    pub base: Box<Ty>,
    pub size: ValueId,
}
#[derive(Debug, Default, Clone)]
pub struct FuncTy {
    pub ret_ty: Box<Ty>,
    pub param_tys: Vec<Ty>,
}

#[derive(Debug, Default)]
pub struct Func {
    pub name: String,
    pub is_external: bool,
    pub is_builtin: bool,
    pub bb_map: LinkedHashMap<String, ValueId>,
    pub entry_bb: Option<ValueId>, // only-symbol func has no entry_bb
    pub params: Vec<Param>,
    pub ret_ty: Ty,
}

#[derive(Debug, Default)]
pub struct Param {
    pub name: String,
    pub id_name: String,
    pub ty: Ty,
}

#[derive(Debug, Default)]
pub struct GlobalVal {
    pub name: String,
    pub ty: Ty,
    pub init_expr: Option<ast::Expr>,
    pub is_const: bool,
    pub is_external: bool,
    pub is_bultin: bool,
    pub init: Option<ValueId>,
}

#[derive(Debug, Default)]
pub struct ModuleContext {
    pub ur: ValueRegistry,

    pub funcs_map: LinkedHashMap<String, ValueId>,
    pub global_val_map: LinkedHashMap<String, ValueId>,
    pub const_str_map: LinkedHashMap<String, ValueId>,

    cur_bb: Option<ValueId>,
    cur_func: Option<ValueId>,
}

impl ModuleContext {
    pub fn register_func(&mut self, func: Func) -> ValueId {
        let name = func.name.clone();
        let id = self.ur.next_id();
        let func_value = Value {
            id_name: match func.is_builtin {
                true => format!("@{}{}", BUILTIN_PREFIX, name),
                false => format!("@{}", name),
            },
            id,
            uses: Vec::new(),
            proto: ValueProto::Func(func),
        };
        let id = self.ur.insert(func_value);
        self.funcs_map.insert(name, id);
        id
    }
    pub fn gen_local_id(&mut self) -> usize {
        let bb = self.get_cur_bb_mut();
        let ret = bb.next_local_id;
        bb.next_local_id += 1;
        ret
    }
    pub fn gen_local_id_name(&mut self) -> String {
        format!("%{}", self.gen_local_id())
    }

    pub fn register_global_val(&mut self, global_val: GlobalVal) -> ValueId {
        let name = global_val.name.clone();
        let id = self.ur.next_id();
        let global_val_value = Value {
            id_name: match global_val.is_bultin {
                true => format!("@{}{}", BUILTIN_PREFIX, name),
                false => format!("@{}", id),
            },
            id,
            uses: Vec::new(),
            proto: ValueProto::GlobalVal(global_val),
        };
        let id = self.ur.insert(global_val_value);
        self.global_val_map.insert(name, id);
        id
    }

    pub fn register_const_val(&mut self, const_val: ConstVal) -> ValueId {
        let id = self.ur.next_id();
        let const_val_value = Value {
            id_name: format!("@_const_val_{}", id),
            id,
            uses: Vec::new(),
            proto: ValueProto::ConstVal(const_val),
        };
        self.ur.insert(const_val_value)
    }

    pub fn register_bb_to_cur_func(&mut self, bb: BasicBlock) -> ValueId {
        let name = bb.name.clone();
        let id = self.ur.next_id();
        let bb_value = Value {
            id_name: format!("b{id}_{name}", id = id, name = name),
            id,
            uses: Vec::new(),
            proto: ValueProto::BasicBlock(bb),
        };
        let id = self.ur.insert(bb_value);
        value_cast!(self.get_cur_func_value_mut(), ValueProto::Func(ref mut func) => {
            func.bb_map.insert(name, id);
        });
        id
    }

    pub fn register_inst(&mut self, inst: Inst) -> ValueId {
        let id = self.ur.next_id();
        let inst_value = Value {
            id_name: self.gen_local_id_name(),
            id,
            uses: Vec::new(),
            proto: ValueProto::Inst(inst),
        };
        let id = self.ur.insert(inst_value);
        self.get_cur_bb_mut().insts.push(id);
        id
    }

    pub fn get_const_strs(&self) -> impl Iterator<Item = &String> {
        self.const_str_map.keys()
    }

    pub fn get_cur_func_value(&self) -> Option<&Value> {
        self.cur_func.and_then(|i| self.ur.get(i))
    }

    pub fn get_cur_func_value_mut(&mut self) -> &mut Value {
        self.cur_func.and_then(|i| self.ur.get_mut(i)).unwrap()
    }

    pub fn get_cur_func(&self) -> &Func {
        value_cast!(self.get_cur_func_value().unwrap(), ValueProto::Func(ref func) => {
            func
        })
    }

    pub fn get_cur_func_mut(&mut self) -> &mut Func {
        value_cast!(self.get_cur_func_value_mut(), ValueProto::Func(ref mut func) => {
            func
        })
    }

    pub fn get_cur_bb_value(&self) -> &Value {
        self.cur_bb.and_then(|i| self.ur.get(i)).unwrap()
    }

    pub fn get_cur_bb_value_mut(&mut self) -> &mut Value {
        self.cur_bb.and_then(|i| self.ur.get_mut(i)).unwrap()
    }

    pub fn get_cur_bb(&self) -> &BasicBlock {
        value_cast!(self.get_cur_bb_value(), ValueProto::BasicBlock(ref bb) => {
            bb
        })
    }

    pub fn get_cur_bb_mut(&mut self) -> &mut BasicBlock {
        value_cast!(self.get_cur_bb_value_mut(), ValueProto::BasicBlock(ref mut bb) => {
            bb
        })
    }

    pub fn find_or_create_const_str(&mut self, s: &str) -> ValueId {
        let m = self.const_str_map.get(s).copied();
        if let Some(i) = m {
            self.ur.get(i).unwrap().id
        } else {
            let id = self.ur.next_id();
            let str_id = self.const_str_map.len();
            let val = Value {
                id_name: format!(
                    "@.str{dot}{no}",
                    dot = if str_id == 0 {
                        "".to_string()
                    } else {
                        ".".to_string()
                    },
                    no = if str_id == 0 {
                        "".to_string()
                    } else {
                        str_id.to_string()
                    }
                ),
                id,
                uses: Vec::new(),
                proto: ValueProto::ConstStr(s.to_string()),
            };
            self.ur.insert(val);
            self.const_str_map.insert(s.to_string(), id);
            id
        }
    }

    pub fn get_func_value(&self, name: &str) -> ValueId {
        self.funcs_map.get(name).copied().unwrap_or_else(|| {
            panic!("Cannot find function {}", name);
        })
    }
    pub fn get_func(&self, name: &str) -> &Func {
        value_cast!(self.ur.get(self.get_func_value(name)).unwrap(), ValueProto::Func(ref func) => {
            func
        })
    }

    pub fn get_global_val_value(&self, name: &str) -> ValueId {
        self.global_val_map
            .get(name)
            .copied()
            .unwrap_or_else(|| {
                panic!("Cannot find global value {}", name);
            })
    }

    pub fn get_global_val(&self, name: &str) -> &GlobalVal {
        value_cast!(self.ur.get(self.get_global_val_value(name)).unwrap(), ValueProto::GlobalVal(ref global_val) => {
            global_val
        })
    }

    pub fn try_get_global_val(&self, name: &str) -> Option<&GlobalVal> {
        self.global_val_map.get(name).map(|i| {
            value_cast!(self.ur.get(*i).unwrap(), ValueProto::GlobalVal(ref global_val) => {
                global_val
            })
        })
    }

    pub fn resolve_global_val(&self, id: ValueId) -> &GlobalVal {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::GlobalVal(ref global_val) => {
            global_val
        })
    }

    pub fn resolve_func(&self, id: ValueId) -> &Func {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::Func(ref func) => {
            func
        })
    }

    pub fn resolve_func_mut(&mut self, id: ValueId) -> &mut Func {
        value_cast!(self.ur.get_mut(id).unwrap(), ValueProto::Func(ref mut func) => {
            func
        })
    }

    pub fn resolve_bb(&self, id: ValueId) -> &BasicBlock {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::BasicBlock(ref bb) => {
            bb
        })
    }

    pub fn resolve_inst(&self, id: ValueId) -> &Inst {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::Inst(ref inst) => {
            inst
        })
    }

    pub fn resolve_const_str(&self, id: ValueId) -> &String {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::ConstStr(ref s) => {
            s
        })
    }

    pub fn resolve_const_val(&self, id: ValueId) -> &ConstVal {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::ConstVal(ref val) => {
            val
        })
    }

    pub fn resolve_value(&self, id: ValueId) -> &Value {
        self.ur.get(id).unwrap()
    }

    pub fn resolve_value_mut(&mut self, id: ValueId) -> &mut Value {
        self.ur.get_mut(id).unwrap()
    }

    pub fn is_global_symbol(&self, id: ValueId) -> bool {
        self.global_val_map.values().any(|i| *i == id)
            || self.funcs_map.values().any(|i| *i == id)
            || self.const_str_map.values().any(|i| *i == id)
    }

    pub fn is_const_str(&self, id: ValueId) -> bool {
        self.const_str_map.values().any(|i| *i == id)
    }

    pub fn is_func(&self, id: ValueId) -> bool {
        self.funcs_map.values().any(|i| *i == id)
    }

    pub fn is_global_val(&self, id: ValueId) -> bool {
        self.global_val_map.values().any(|i| *i == id)
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

pub fn build_ir(ctx: &mut ModuleContext, ast: &TransUnit) {
    ast.funcs.iter().for_each(|f| {
        visit_func(ctx, f);
    });
}

fn visit_func(ctx: &mut ModuleContext, ast: &ast::Function) {
    let mut local_id: usize = 0;
    let func = Func {
        name: ast.name.clone(),
        is_external: false,
        is_builtin: false,
        bb_map: LinkedHashMap::new(),
        entry_bb: None,
        params: ast
            .params
            .iter()
            .map(|p| {
                let ret = Param {
                    name: p.name.clone(),
                    ty: Ty::from(&p.ty),
                    id_name: format!("%{}", local_id),
                };
                local_id += 1;
                ret
            })
            .collect(),
        ret_ty: Ty::from(&ast.ret_ty),
    };

    let func_id = ctx.register_func(func);
    ctx.cur_func = Some(func_id);
    let entry_bb_id = ctx.register_bb_to_cur_func(BasicBlock::new("entry".to_string(), local_id));
    let mut func = &mut ctx.resolve_func_mut(func_id);

    func.bb_map.insert("entry".to_string(), entry_bb_id);
    func.entry_bb = Some(entry_bb_id);
    ctx.cur_bb = Some(entry_bb_id);

    for stmt in &ast.body.statements {
        match stmt {
            ast::Statement::Expr(expr_stmt) => {
                visit_expr(ctx, &expr_stmt.expr);
            }
            ast::Statement::Return(ret_stmt) => match ret_stmt.expr {
                Some(ref expr) => {
                    let expr_id = visit_expr(ctx, expr);
                    let ret_inst = Inst::Ret(RetInst { val: Some(expr_id) });
                    ctx.register_inst(ret_inst);
                }
                None => {
                    let ret_inst = Inst::Ret(RetInst { val: None });
                    ctx.register_inst(ret_inst);
                }
            },
        }
    }
}

fn visit_expr(ctx: &mut ModuleContext, ast: &ast::Expr) -> ValueId {
    match ast {
        ast::Expr::Infix(infix) => visit_infix_expr(ctx, infix),
        ast::Expr::Prefix(prefix) => visit_prefix_expr(ctx, prefix),
        ast::Expr::Postfix(postfix) => visit_postfix_expr(ctx, postfix),
        ast::Expr::Primary(primary) => visit_primary_expr(ctx, primary),
    }
}

fn visit_infix_expr(ctx: &mut ModuleContext, ast: &ast::InfixExpr) -> ValueId {
    visit_expr(ctx, &ast.lhs);
    visit_expr(ctx, &ast.rhs);
    todo!();
}

fn visit_prefix_expr(ctx: &mut ModuleContext, ast: &ast::PrefixExpr) -> ValueId {
    visit_expr(ctx, &ast.rhs);
    todo!();
}

fn visit_postfix_expr(ctx: &mut ModuleContext, ast: &ast::PostfixExpr) -> ValueId {
    visit_expr(ctx, &ast.lhs);
    todo!();
}

fn visit_primary_expr(ctx: &mut ModuleContext, ast: &ast::PrimaryExpr) -> ValueId {
    match ast {
        ast::PrimaryExpr::Grouping(grouping) => visit_expr(ctx, grouping.as_ref()),
        ast::PrimaryExpr::Call(call) => visit_call(ctx, call),
        ast::PrimaryExpr::Ident(ident) => visit_ident(ctx, ident),
        ast::PrimaryExpr::Literal(literal) => visit_literal(ctx, literal),
    }
}

fn visit_call(ctx: &mut ModuleContext, ast: &ast::CallExpr) -> ValueId {
    let mut call_inst = CallInst {
        func: ctx.get_func_value(&ast.callee),
        args: Vec::new(),
    };
    for i in 0..ast.args.len() {
        let arg = &ast.args[i];
        call_inst.args.push(visit_expr(ctx, arg));
    }
    ctx.register_inst(Inst::Call(call_inst))
}

fn visit_ident(ctx: &mut ModuleContext, ast: &ast::IdentExpr) -> ValueId {
    let name = &ast.name;
    // case: global variable
    let gv_w = ctx.try_get_global_val(name);
    if let Some(gv) = gv_w {
        let ty = gv.ty.clone();
        let gv_id = ctx.get_global_val_value(name);
        let load_inst = Inst::Load(LoadInst { ptr: gv_id, ty });
        let load_id = ctx.register_inst(load_inst);
        return load_id;
    }
    todo!()
}

fn visit_literal(ctx: &mut ModuleContext, ast: &ast::LiteralExpr) -> ValueId {
    match &ast.value {
        ast::Literal::Int(literal_int) => {
            let id = ctx.ur.next_id();
            let value = Value {
                id_name: String::new(),
                id,
                uses: Vec::new(),
                proto: ValueProto::ConstVal(ConstVal::Int(IntConstVal {
                    ty: Ty::int(32),
                    val: *literal_int,
                })),
            };
            ctx.ur.insert(value)
        }
        ast::Literal::Char(_literal_char) => todo!(),
        ast::Literal::Float(_literal_float) => todo!(),
        ast::Literal::Bool(_literal_bool) => todo!(),
        ast::Literal::String(literal_string) => ctx.find_or_create_const_str(literal_string),
    }
}
