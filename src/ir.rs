use std::collections::HashMap;

use crate::{
    ast::*,
    scope::{SymbolId, SymbolTable},
};
use id_arena::Arena;
use linked_hash_map::LinkedHashMap;
use log::{debug, warn};

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
    // 一般存放符号对应的 alloca 语句 value id，或者全局变量 valud id
    pub sym2def: LinkedHashMap<SymbolId, ValueId>, // 今后考虑移动到 builder 里面
    pub functions: LinkedHashMap<String, ValueId>,
    pub builtins: LinkedHashMap<String, ValueId>,
    pub constants: LinkedHashMap<String, ValueId>,

    // 用于 ir 构建
    cur_func: Option<ValueId>,
    cur_bb: Option<ValueId>,

    // value -> users of the value
    pub value_user: HashMap<ValueId, Vec<ValueId>>,
    // value -> values used by the value
    pub value_using: HashMap<ValueId, Vec<ValueId>>,
    // value -> name of the value
    pub value_name: HashMap<ValueId, String>,
    // value -> parent of the value (e.g. inst -> bb)
    pub value_parent: HashMap<ValueId, ValueId>,
}

impl Module {
    pub fn new(syms: SymbolTable) -> Self {
        
        // module.add_builtin_types();
        Module {
            values: Arena::new(),
            types: Arena::new(),
            global_variables: LinkedHashMap::new(),
            syms,
            sym2def: LinkedHashMap::new(),
            functions: LinkedHashMap::new(),
            builtins: LinkedHashMap::new(),
            constants: LinkedHashMap::new(),
            cur_func: None,
            cur_bb: None,
            value_user: HashMap::new(),
            value_using: HashMap::new(),
            value_name: HashMap::new(),
            value_parent: HashMap::new(),
        }
    }

    pub fn get_bb_mut(&mut self, bb_id: ValueId) -> &mut BasicBlockValue {
        match &mut self.values[bb_id] {
            Value::BasicBlock(bb) => bb,
            _ => panic!("expect a basic block"),
        }
    }

    pub fn get_users_of(&self, val_id: ValueId) -> Vec<ValueId> {
        let users = self.value_user.get(&val_id);
        if users.is_none() {
            return vec![];
        }
        users.unwrap().clone()
    }

    pub fn get_bb(&self, bb_id: ValueId) -> &BasicBlockValue {
        match &self.values[bb_id] {
            Value::BasicBlock(bb) => bb,
            _ => panic!("expect a basic block"),
        }
    }

    pub fn get_bb_preds(&self, bb_id: ValueId) -> Vec<ValueId> {
        let bb_users = self.get_users_of(bb_id);
        let mut preds = vec![];
        if bb_users.is_empty() {
            return preds;
        }
        for user_id in bb_users {
            let user = &self.values[user_id];
            match user {
                Value::Instruction(inst) => match inst {
                    InstValue::Branch(_br) => {
                        // if br.then_bb == bb_id {
                        //     continue;
                        // }
                        let br_bb = self.value_parent[&user_id];
                        preds.push(br_bb);
                    }
                    InstValue::Jump(_jmp) => {
                        let jmp_bb = self.value_parent[&user_id];
                        preds.push(jmp_bb);
                    }
                    _ => panic!("expect a branch or jump instruction"),
                },
                _ => panic!("expect an instruction"),
            }
        }
        preds
    }

    pub fn get_phis(&self, bb_id: ValueId) -> Vec<ValueId> {
        let bb = self.get_bb(bb_id);
        let mut phis = vec![];
        for inst_id in &bb.insts {
            let inst = self.get_inst(*inst_id);
            match inst {
                InstValue::Phi(_phi) => {
                    phis.push(*inst_id);
                }
                _ => {}
            }
        }
        phis
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

    pub fn try_get_inst(&self, inst_id: ValueId) -> Option<&InstValue> {
        match &self.values[inst_id] {
            Value::Instruction(inst) => Some(inst),
            _ => {
                debug!("Value: {:?}, expect an instruction", &self.values[inst_id]);
                None
            }
        }
    }

    pub fn get_global_var(&self, var_id: ValueId) -> &GlobalVariableValue {
        match &self.values[var_id] {
            Value::GlobalVariable(gv) => gv,
            _ => panic!("expect a global variable"),
        }
    }

    pub fn get_value(&self, val_id: ValueId) -> &Value {
        &self.values[val_id]
    }

    pub fn alloc_value(&mut self, val: Value) -> ValueId {
        let val_id = self.values.alloc(val);
        let idx = val_id.index();
        if idx > 131072 {
            panic!("too many values");
        }
        debug!(
            "[{}] is allocated: {}",
            val_id.index(),
            self.inspect_value(val_id)
        );
        val_id
    }

    pub fn spawn_zero_value(&mut self, ty: Type) -> ValueId {
        let val = ConstValue::zero_of(ty);
        
        self.alloc_value(val.into())
    }

    pub fn cur_bb_mut(&mut self) -> &mut BasicBlockValue {
        self.get_bb_mut(self.cur_bb.unwrap())
    }

    pub fn cur_func_mut(&mut self) -> &mut FunctionValue {
        self.get_func_mut(self.cur_func.unwrap())
    }

    pub fn cur_bb(&self) -> &BasicBlockValue {
        self.get_bb(self.cur_bb.unwrap())
    }

    pub fn cur_func(&self) -> &FunctionValue {
        self.get_func(self.cur_func.unwrap())
    }

    pub fn set_insert_point(&mut self, bb: ValueId) {
        self.cur_bb = Some(bb);
    }

    pub fn set_cur_func(&mut self, func: ValueId) {
        self.cur_func = Some(func);
    }

    pub fn cur_func_value_id(&self) -> ValueId {
        self.cur_func.unwrap()
    }

    pub fn cur_bb_value_id(&self) -> ValueId {
        self.cur_bb.unwrap()
    }

    pub fn inspect_value(&self, value_id: ValueId) -> String {
        let value = &self.values[value_id];
        match value {
            Value::Instruction(inst) => {
                format!("[{}] inst: {:?}", value_id.index(), inst)
            }
            Value::BasicBlock(bb) => {
                format!("[{}] bb: {:?}", value_id.index(), bb)
            }
            Value::Function(func) => {
                format!("[{}] func: {:?}", value_id.index(), func)
            }
            Value::GlobalVariable(gv) => {
                format!("[{}] gv: {:?}", value_id.index(), gv)
            }
            Value::Const(c) => {
                format!("[{}] c: {:?}", value_id.index(), c)
            }
            Value::VariableValue(var) => {
                format!("[{}] var: {:?}", value_id.index(), var)
            }
        }
    }

    pub fn bb_has_phi(&self, bb_id: ValueId) -> bool {
        let bb = self.get_bb(bb_id);
        for inst_id in bb.insts.iter() {
            let inst = self.get_inst(*inst_id);
            match inst {
                InstValue::Phi(_) => return true,
                _ => {}
            }
        }
        false
    }

    pub fn inspect_value_users(&self, value_id: ValueId) -> String {
        let users = self.value_user.get(&value_id);
        if users.is_none() {
            return format!("value [{}] is not used", value_id.index());
        }
        let users = users.unwrap();
        let mut s = String::new();
        for user in users {
            s.push_str(&self.inspect_value(*user));
            s.push('\n');
        }
        s
    }

    /// 对一个 Value 做不再使用处理
    ///
    /// 将移除所有对它的使用记录
    /// 调用前确保没有任何指令依赖于该 Value
    pub fn mark_nolonger_used(&mut self, value_id: ValueId) {
        let tmp = self.value_user.get(&value_id);
        if tmp.is_none() {
            println!("{}", self.inspect_value(value_id));
            panic!("value {:?} is not used", value_id);
        }

        let all_users = tmp.unwrap().clone();
        self.value_user.remove(&value_id);

        for user in all_users {
            if let Some(users) = self.value_using.get_mut(&user) {
                users.retain(|&x| x != value_id);
            }
        }

        // todo: 验证确实没有任何指令依赖于该 Value
    }

    pub fn mark_nolonger_using(&mut self, user_id: ValueId, used_id: ValueId) {
        debug!("[{}] nolonger use [{}]", user_id.index(), used_id.index());
        debug!(
            "user: {:?}, used: {:?}",
            self.inspect_value(user_id),
            self.inspect_value(used_id)
        );
        if let Some(used_vals) = self.value_using.get_mut(&user_id) {
            used_vals.retain(|&x| x != used_id);
            self.value_user
                .get_mut(&used_id)
                .unwrap()
                .retain(|&x| x != user_id);
        }
    }

    pub fn mark_nolonger_using_any(&mut self, user_id: ValueId) {
        debug!("[{}] nolonger use any", user_id.index());
        debug!("user: {:?}", self.inspect_value(user_id));
        if let Some(used_vals) = self.value_using.get_mut(&user_id) {
            for used_id in used_vals.clone() {
                self.value_user
                    .get_mut(&used_id)
                    .unwrap()
                    .retain(|&x| x != user_id);
            }
            used_vals.clear();
        }
    }

    /// 替换一个 Value 为另一个 Value
    ///
    /// 把所有对 value 的使用，替换为对 new_value 的使用，并清空 value 自己的 uses 列表。
    ///
    /// 这意味着旧的 Value 不再被使用，而新的 Value 被使用
    /// 除了更新记录，此函数还负责将所有指令中对词 Value 的使用替换为新的 Value
    pub fn replace_value(&mut self, value_id: ValueId, new_value_id: ValueId) {
        debug!(
            "replace old=[{}] with new=[{}] \n\twhere old is {}, and new is {}",
            value_id.index(),
            new_value_id.index(),
            self.inspect_value(value_id),
            self.inspect_value(new_value_id)
        );
        let tmp = self.value_user.get(&value_id);
        if tmp.is_none() {
            warn!("value {} is not used", self.inspect_value(value_id));
            self.value_using.remove(&value_id);
            return;
        }

        let all_users = tmp.unwrap().clone();
        self.value_user.remove(&value_id);

        // 将所有使用 value_id 的用户，替换为使用 new_value_id
        // 相当于重新进行 mark_using 操作
        for cur_user in all_users {
            if let Some(used_by_users) = self.value_using.get_mut(&cur_user) {
                used_by_users.retain(|&x| x != value_id);
                used_by_users.push(new_value_id);
            }
        }

        // 将所有 value 对 value_id 的使用替换为 new_value_id
        self.values.iter_mut().for_each(|(_val_id, val)| match val {
            Value::Instruction(inst) => {
                inst.replace_operands(value_id, new_value_id);
            }
            _ => {}
        });

        // // 将原来的 value_id 占用的指令位置，替换为 new_value_id
        // let parent_bb_id = self.get_parent(value_id);
        // let parent_bb = self.values.get_mut(parent_bb_id).unwrap().as_bb_mut();
        // parent_bb.unwrap().insts.iter_mut().for_each(|inst| {
        //     if inst.clone() == value_id {
        //         *inst = new_value_id;
        //     }
        // });

        self.value_using.remove(&value_id);
    }

    pub fn get_parent_id(&self, value_id: ValueId) -> ValueId {
        let tmp = self.value_parent.get(&value_id);
        if tmp.is_none() {
            panic!("value has no parent: {}", self.inspect_value(value_id));
        }
        *tmp.unwrap()
    }

    pub fn get_parent(&self, value_id: ValueId) -> &BasicBlockValue {
        let parent_id = self.get_parent_id(value_id);
        self.values.get(parent_id).unwrap().as_bb().unwrap()
    }

    pub fn get_parent_mut(&mut self, value_id: ValueId) -> &mut BasicBlockValue {
        let parent_id = self.get_parent_id(value_id);
        self.values.get_mut(parent_id).unwrap().as_bb_mut().unwrap()
    }

    pub fn mark_using(&mut self, user: ValueId, used: ValueId) {
        debug!(
            "mark_using:\n   {}\n-> {}",
            self.inspect_value(user),
            self.inspect_value(used)
        );
        self.value_using
            .entry(user)
            .or_insert_with(Vec::new)
            .push(used);
        self.value_user
            .entry(used)
            .or_insert_with(Vec::new)
            .push(user);
    }

    pub fn mark_parent(&mut self, child: ValueId, parent: ValueId) {
        self.value_parent.insert(child, parent);
    }

    pub fn remove_phi_all_operands(&mut self, phi_id: ValueId) {
        let phi_value = self.values.get_mut(phi_id).unwrap();
        match phi_value {
            Value::Instruction(InstValue::Phi(phi_inst)) => {
                phi_inst.incomings.iter().for_each(|(value_id, bb_id)| {
                    self.value_user
                        .get_mut(value_id)
                        .unwrap()
                        .retain(|&x| x != phi_id);
                    self.value_user
                        .get_mut(bb_id)
                        .unwrap()
                        .retain(|&x| x != phi_id);
                    self.value_using
                        .get_mut(&phi_id)
                        .unwrap()
                        .retain(|&x| x != *value_id);
                    self.value_using
                        .get_mut(&phi_id)
                        .unwrap()
                        .retain(|&x| x != *bb_id);
                });
            }
            _ => panic!("expect a phi instruction"),
        }
    }

    pub fn spawn_load_inst(&mut self, src: ValueId) -> ValueId {
        let load_inst = LoadInst {
            ty: self.values.get(src).unwrap().ty(),
            ptr: src,
        };
        let val_id = self.alloc_value(load_inst.into());

        self.mark_using(val_id, src);

        self.cur_bb_mut().insts.push(val_id);
        self.mark_parent(val_id, self.cur_bb.unwrap());
        val_id
    }

    pub fn add_phi_incoming(&mut self, phi_id: ValueId, bb_id: ValueId, value_id: ValueId) {
        let phi_value = self.values.get_mut(phi_id).unwrap();
        match phi_value {
            Value::Instruction(InstValue::Phi(phi_inst)) => {
                phi_inst.incomings.push((value_id, bb_id));
                self.mark_using(phi_id, value_id);
                self.mark_using(phi_id, bb_id);
            }
            _ => panic!("expect a phi"),
        }
    }

    pub fn spawn_call_inst(&mut self, func: ValueId, args: Vec<ValueId>) -> ValueId {
        let func_value = self.values.get(func).unwrap();
        let call_inst = CallInst {
            ty: func_value.ty(),
            func,
            args,
            must_tail: false,
        };
        let val_id = self.alloc_value(call_inst.into());

        self.mark_using(val_id, func);

        self.cur_bb_mut().insts.push(val_id);
        self.mark_parent(val_id, self.cur_bb.unwrap());

        val_id
    }

    pub fn spawn_store_inst(&mut self, ptr: ValueId, value: ValueId) -> ValueId {
        {
            // make sure ptr is a pointer
            let inst_ = self.try_get_inst(ptr);
            match inst_ {
                Some(iv) => match iv {
                    InstValue::Alloca(_) => {}
                    InstValue::Gep(_) => {}
                    _ => todo!(),
                },
                None => debug!("ptr is not a pointer"),
            }
        }
        let store = StoreInst { ptr, value };
        let store_id = self.alloc_value(store.into());

        self.mark_using(store_id, ptr);
        self.mark_using(store_id, value);

        self.cur_bb_mut().insts.push(store_id);
        self.mark_parent(store_id, self.cur_bb.unwrap());
        store_id
    }

    pub fn spawn_gep_inst(
        &mut self,
        ty: Type,
        base: Type,
        pointer: ValueId,
        indices: Vec<ValueId>,
    ) -> ValueId {
        let gep = GetElementPtrInst {
            ty,
            base,
            ptr: pointer,
            indices: indices.clone(),
        };
        let gep_id = self.alloc_value(gep.into());

        self.mark_using(gep_id, pointer);
        for index in indices {
            self.mark_using(gep_id, index);
        }

        self.cur_bb_mut().insts.push(gep_id);
        self.mark_parent(gep_id, self.cur_bb.unwrap());
        gep_id
    }

    pub fn spawn_return_inst(&mut self, value: Option<ValueId>) -> ValueId {
        let ret = ReturnInst { value };
        let ret_id = self.alloc_value(ret.into());
        if let Some(value) = value {
            self.mark_using(ret_id, value);
        }
        self.cur_bb_mut().insts.push(ret_id);
        self.mark_parent(ret_id, self.cur_bb.unwrap());
        ret_id
    }

    pub fn alloc_phi_inst(&mut self, ty: Type) -> ValueId {
        let phi = PhiInst {
            ty,
            incomings: Vec::new(),
        };
        
        self.alloc_value(phi.into())
    }

    pub fn spawn_phi_inst(&mut self, ty: Type, incomings: Vec<(ValueId, ValueId)>) -> ValueId {
        let phi = PhiInst {
            ty,
            incomings: incomings.clone(),
        };
        let phi_id = self.alloc_value(phi.into());
        for (value, bb) in incomings {
            self.mark_using(phi_id, value);
            self.mark_using(phi_id, bb);
        }
        self.cur_bb_mut().insts.push(phi_id);
        self.mark_parent(phi_id, self.cur_bb.unwrap());
        phi_id
    }

    pub fn alloc_br_inst(&mut self, cond: ValueId, then_bb: ValueId, else_bb: ValueId) -> ValueId {
        let br = BranchInst {
            cond,
            then_bb,
            else_bb,
        };
        let br_id = self.alloc_value(br.into());
        self.mark_using(br_id, cond);
        self.mark_using(br_id, then_bb);
        self.mark_using(br_id, else_bb);
        br_id
    }

    pub fn alloc_jump_inst(&mut self, bb: ValueId) -> ValueId {
        let jump = JumpInst { bb };
        let jump_id = self.alloc_value(jump.into());
        self.mark_using(jump_id, bb);
        jump_id
    }

    pub fn spawn_jump_inst(&mut self, bb: ValueId) -> ValueId {
        let jump_id = self.alloc_jump_inst(bb);
        self.cur_bb_mut().insts.push(jump_id);
        self.mark_parent(jump_id, self.cur_bb.unwrap());
        jump_id
    }

    pub fn alloc_binop_inst(
        &mut self,
        ty: Type,
        op: InfixOp,
        lhs: ValueId,
        rhs: ValueId,
    ) -> ValueId {
        let binop = BinaryOperator { ty, op, lhs, rhs };
        let binop_id = self.alloc_value(binop.into());
        self.mark_using(binop_id, lhs);
        self.mark_using(binop_id, rhs);
        binop_id
    }

    pub fn spawn_binop_inst(
        &mut self,
        ty: Type,
        op: InfixOp,
        lhs: ValueId,
        rhs: ValueId,
    ) -> ValueId {
        let binop_id = self.alloc_binop_inst(ty, op, lhs, rhs);
        self.cur_bb_mut().insts.push(binop_id);
        self.mark_parent(binop_id, self.cur_bb.unwrap());
        binop_id
    }

    pub fn spawn_br_inst(&mut self, cond: ValueId, true_bb: ValueId, false_bb: ValueId) -> ValueId {
        let br_id = self.alloc_br_inst(cond, true_bb, false_bb);
        self.cur_bb_mut().insts.push(br_id);
        self.mark_parent(br_id, self.cur_bb.unwrap());
        br_id
    }

    pub fn alloc_basic_block(&mut self) -> ValueId {
        let bb = BasicBlockValue::default();
        
        self.alloc_value(Value::BasicBlock(bb))
    }
    pub fn spawn_basic_block(&mut self) -> ValueId {
        let bb_id = self.alloc_basic_block();
        self.cur_func_mut().bbs.append(bb_id);
        bb_id
    }

    pub fn spawn_alloca_inst(&mut self, name: String, ty: Type) -> ValueId {
        let alloca = AllocaInst { name, ty };
        let alloca_id = self.alloc_value(alloca.into());
        let entry_bb_id = self.cur_func().bbs.entry_bb();

        // 用于确定插入位置的临时变量
        let ins_pos;

        // 使用一个新的作用域来查找插入位置
        {
            let entry_bb = self.get_bb(*entry_bb_id);
            ins_pos = entry_bb
                .insts
                .iter()
                .position(|inst_id| !matches!(self.get_inst(*inst_id), InstValue::Alloca(_)))
                .unwrap_or(entry_bb.insts.len());
        }

        // 在找到的位置插入新的alloca指令
        let entry_bb_mut = self.get_bb_mut(*entry_bb_id);
        entry_bb_mut.insts.insert(ins_pos, alloca_id);

        alloca_id
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

    pub fn as_variable(&self) -> Option<&VariableValue> {
        match self {
            Value::VariableValue(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_bb(&self) -> Option<&BasicBlockValue> {
        match self {
            Value::BasicBlock(bb) => Some(bb),
            _ => None,
        }
    }

    pub fn as_bb_mut(&mut self) -> Option<&mut BasicBlockValue> {
        match self {
            Value::BasicBlock(bb) => Some(bb),
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
    pub is_const: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionValue {
    pub name: String,
    pub params: Vec<ValueId>, // Parameters
    pub ret_ty: Type,
    pub bbs: BasicBlockList, // BasicBlocks
    pub is_external: bool,
    pub is_variadic: bool,
}

impl FunctionValue {
    pub fn new(
        name: String,
        params: Vec<ValueId>,
        ret_ty: Type,
        is_external: bool,
        is_variadic: bool,
    ) -> Self {
        FunctionValue {
            name,
            params,
            ret_ty,
            bbs: BasicBlockList::default(),
            is_external,
            is_variadic,
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

    pub fn entry_bb(&self) -> &ValueId {
        self.bbs.get("entry").unwrap()
    }

    pub fn entry_bb_mut(&mut self) -> &mut ValueId {
        self.bbs.get_mut("entry").unwrap()
    }
}

impl FunctionValue {}

#[derive(Debug, Clone, Default)]
pub struct BasicBlockValue {
    pub name: String,
    pub insts: Vec<ValueId>, // 麻了，不用链表了
    pub terminator: Option<ValueId>,
}

impl BasicBlockValue {
    pub fn new(name: String) -> Self {
        BasicBlockValue {
            name,
            insts: Vec::new(),
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

impl From<BasicBlockValue> for Value {
    fn from(val: BasicBlockValue) -> Self {
        Value::BasicBlock(val)
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
            InstValue::Gep(inst) => inst.base.clone(),
            InstValue::Return(_) => BuiltinType::Void.into(),
            InstValue::Call(inst) => inst.ty.clone(),
            InstValue::Phi(inst) => inst.ty.clone(),
            InstValue::Cast(inst) => inst.new_ty.clone(),
        }
    }

    pub fn as_infix_op(&self) -> &BinaryOperator {
        match self {
            InstValue::InfixOp(op) => op,
            _ => panic!("expect an infix op"),
        }
    }

    pub fn as_load(&self) -> &LoadInst {
        match self {
            InstValue::Load(inst) => inst,
            _ => panic!("expect a load inst"),
        }
    }

    pub fn as_store(&self) -> &StoreInst {
        match self {
            InstValue::Store(inst) => inst,
            _ => panic!("expect a store inst"),
        }
    }

    pub fn as_alloca(&self) -> &AllocaInst {
        match self {
            InstValue::Alloca(inst) => inst,
            _ => panic!("expect an alloca inst"),
        }
    }

    pub fn as_branch(&self) -> &BranchInst {
        match self {
            InstValue::Branch(inst) => inst,
            _ => panic!("expect a branch inst"),
        }
    }

    pub fn as_jump(&self) -> &JumpInst {
        match self {
            InstValue::Jump(inst) => inst,
            _ => panic!("expect a jump inst"),
        }
    }

    pub fn as_gep(&self) -> &GetElementPtrInst {
        match self {
            InstValue::Gep(inst) => inst,
            _ => panic!("expect a gep inst"),
        }
    }

    pub fn as_return(&self) -> &ReturnInst {
        match self {
            InstValue::Return(inst) => inst,
            _ => panic!("expect a return inst"),
        }
    }

    pub fn as_call(&self) -> &CallInst {
        match self {
            InstValue::Call(inst) => inst,
            _ => panic!("expect a call inst"),
        }
    }

    pub fn as_phi(&self) -> &PhiInst {
        match self {
            InstValue::Phi(inst) => inst,
            _ => panic!("expect a phi inst"),
        }
    }

    pub fn as_cast(&self) -> &CastInst {
        match self {
            InstValue::Cast(inst) => inst,
            _ => panic!("expect a cast inst"),
        }
    }

    pub fn is_infix_op(&self) -> bool {
        match self {
            InstValue::InfixOp(_) => true,
            _ => false,
        }
    }

    pub fn is_load(&self) -> bool {
        match self {
            InstValue::Load(_) => true,
            _ => false,
        }
    }

    pub fn is_store(&self) -> bool {
        match self {
            InstValue::Store(_) => true,
            _ => false,
        }
    }

    pub fn is_alloca(&self) -> bool {
        match self {
            InstValue::Alloca(_) => true,
            _ => false,
        }
    }

    pub fn is_branch(&self) -> bool {
        match self {
            InstValue::Branch(_) => true,
            _ => false,
        }
    }

    pub fn is_jump(&self) -> bool {
        match self {
            InstValue::Jump(_) => true,
            _ => false,
        }
    }

    pub fn is_gep(&self) -> bool {
        match self {
            InstValue::Gep(_) => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            InstValue::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_call(&self) -> bool {
        match self {
            InstValue::Call(_) => true,
            _ => false,
        }
    }

    pub fn is_phi(&self) -> bool {
        match self {
            InstValue::Phi(_) => true,
            _ => false,
        }
    }

    pub fn is_cast(&self) -> bool {
        match self {
            InstValue::Cast(_) => true,
            _ => false,
        }
    }

    pub fn is_term(&self) -> bool {
        match self {
            InstValue::Branch(_) => true,
            InstValue::Jump(_) => true,
            InstValue::Return(_) => true,
            _ => false,
        }
    }

    pub fn has_output(&self) -> bool {
        match self {
            InstValue::InfixOp(_) => true,
            InstValue::Load(_) => true,
            InstValue::Store(_) => false,
            InstValue::Alloca(_) => true,
            InstValue::Branch(_) => false,
            InstValue::Jump(_) => false,
            InstValue::Gep(_) => true,
            InstValue::Return(_) => false,
            InstValue::Call(_) => true,
            InstValue::Phi(_) => true,
            InstValue::Cast(_) => true,
        }
    }

    pub fn replace_operands(&mut self, old_value_id: ValueId, new_value_id: ValueId) {
        match self {
            InstValue::InfixOp(op) => op.replace_operands(old_value_id, new_value_id),
            InstValue::Load(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Store(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Alloca(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Branch(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Jump(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Gep(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Return(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Call(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Phi(inst) => inst.replace_operands(old_value_id, new_value_id),
            InstValue::Cast(inst) => inst.replace_operands(old_value_id, new_value_id),
        }
    }
}

macro_rules! impl_replace_operands {
    ($ty:ty, $($field:ident),+) => {
        impl $ty {
            pub fn replace_operands(&mut self, old_value_id: ValueId, new_value_id: ValueId) {
                $(if self.$field == old_value_id {
                    self.$field = new_value_id;
                })*
            }
        }
    };
}

/// An immediate value.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(ConstInt),
    Float(ConstFloat),
    Array(ConstArray),
}

impl ConstValue {
    pub fn as_int(&self) -> Option<&ConstInt> {
        match self {
            ConstValue::Int(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<&ConstFloat> {
        match self {
            ConstValue::Float(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&ConstArray> {
        match self {
            ConstValue::Array(c) => Some(c),
            _ => None,
        }
    }
}

impl From<ConstValue> for Value {
    fn from(val: ConstValue) -> Self {
        Value::Const(val)
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

    pub fn is_array(&self) -> bool {
        match self {
            ConstValue::Array(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstInt {
    pub ty: Type,
    pub value: i64,
}

impl From<ConstInt> for Value {
    fn from(val: ConstInt) -> Self {
        Value::Const(ConstValue::Int(val))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstFloat {
    pub ty: Type,
    pub value: f64,
}

impl From<ConstFloat> for Value {
    fn from(val: ConstFloat) -> Self {
        Value::Const(ConstValue::Float(val))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstArray {
    pub ty: Type,
    pub values: Vec<ConstValue>,
}

impl From<ConstArray> for Value {
    fn from(val: ConstArray) -> Self {
        Value::Const(ConstValue::Array(val))
    }
}

/// alloca <ty>
#[derive(Debug, Clone)]
pub struct AllocaInst {
    pub ty: Type,
    pub name: String,
}

impl From<AllocaInst> for Value {
    fn from(val: AllocaInst) -> Self {
        Value::Instruction(InstValue::Alloca(val))
    }
}

impl AllocaInst {
    pub fn replace_operands(&mut self, _old_value_id: ValueId, _new_value_id: ValueId) {
        // do nothing
    }
}

/// OP <ty> <left>, <right>
#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub ty: Type,
    pub op: InfixOp,
    pub lhs: ValueId,
    pub rhs: ValueId,
}

impl From<BinaryOperator> for Value {
    fn from(val: BinaryOperator) -> Self {
        Value::Instruction(InstValue::InfixOp(val))
    }
}

impl_replace_operands!(BinaryOperator, lhs, rhs);

/// call <ty> <callee>(<ty> <arg1>, <ty> <arg2>, ...)
#[derive(Debug, Clone)]
pub struct CallInst {
    pub ty: Type,
    pub func: ValueId,
    pub args: Vec<ValueId>,
    pub must_tail: bool,
}

impl From<CallInst> for Value {
    fn from(val: CallInst) -> Self {
        Value::Instruction(InstValue::Call(val))
    }
}

impl CallInst {
    pub fn replace_operands(&mut self, old_value_id: ValueId, new_value_id: ValueId) {
        if self.func == old_value_id {
            self.func = new_value_id;
        }
        for arg in &mut self.args {
            if *arg == old_value_id {
                *arg = new_value_id;
            }
        }
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

impl From<CastInst> for Value {
    fn from(val: CastInst) -> Self {
        Value::Instruction(InstValue::Cast(val))
    }
}

impl_replace_operands!(CastInst, value);

/// load <ty>, <ty>* <source>
#[derive(Debug, Clone)]
pub struct LoadInst {
    pub ty: Type,
    pub ptr: ValueId,
}

impl From<LoadInst> for Value {
    fn from(val: LoadInst) -> Self {
        Value::Instruction(InstValue::Load(val))
    }
}

impl_replace_operands!(LoadInst, ptr);

/// store <ty> <value>, <ty>* <pointer>
#[derive(Debug, Clone)]
pub struct StoreInst {
    pub value: ValueId,
    pub ptr: ValueId,
}

impl From<StoreInst> for Value {
    fn from(val: StoreInst) -> Self {
        Value::Instruction(InstValue::Store(val))
    }
}

impl_replace_operands!(StoreInst, value, ptr);

/// gep [inbounds] [volatile]
///     <pointer_type> <pointer_value>, [integer_type] <index>
///     [, <integer_type> <index> ...]
#[derive(Debug, Clone)]
pub struct GetElementPtrInst {
    pub ty: Type,
    pub base: Type,
    pub ptr: ValueId,
    pub indices: Vec<ValueId>,
}

impl From<GetElementPtrInst> for Value {
    fn from(val: GetElementPtrInst) -> Self {
        Value::Instruction(InstValue::Gep(val))
    }
}

impl GetElementPtrInst {
    pub fn replace_operands(&mut self, old_value_id: ValueId, new_value_id: ValueId) {
        if self.ptr == old_value_id {
            self.ptr = new_value_id;
        }
        for index in &mut self.indices {
            if *index == old_value_id {
                *index = new_value_id;
            }
        }
    }
}

/// br <label>
#[derive(Debug, Clone)]
pub struct JumpInst {
    pub bb: ValueId,
}

impl From<JumpInst> for Value {
    fn from(val: JumpInst) -> Self {
        Value::Instruction(InstValue::Jump(val))
    }
}

impl_replace_operands!(JumpInst, bb);

/// br i1 <cond>, label <iftrue>, label <iffalse>
#[derive(Debug, Clone)]
pub struct BranchInst {
    pub cond: ValueId,
    pub then_bb: ValueId,
    pub else_bb: ValueId,
}

impl From<BranchInst> for Value {
    fn from(val: BranchInst) -> Self {
        Value::Instruction(InstValue::Branch(val))
    }
}

impl_replace_operands!(BranchInst, cond, then_bb, else_bb);

#[test]
fn test_macro_impl_replace_operands() {
    let mut arena = Arena::<Value>::new();
    let mut inst = BranchInst {
        cond: arena.alloc(
            (ConstInt {
                ty: BuiltinType::Int.into(),
                value: 1,
            })
            .into(),
        ),
        then_bb: arena.alloc(BasicBlockValue::default().into()),
        else_bb: arena.alloc(BasicBlockValue::default().into()),
    };
    let new_then_bb = arena.alloc(BasicBlockValue::default().into());
    let new_else_bb = arena.alloc(BasicBlockValue::default().into());

    inst.replace_operands(inst.then_bb, new_then_bb);
    assert_eq!(inst.then_bb, new_then_bb);

    inst.replace_operands(inst.else_bb, new_else_bb);
    assert_eq!(inst.then_bb, new_then_bb);
    assert_eq!(inst.else_bb, new_else_bb);
}

/// ret <ty> <value>
#[derive(Debug, Clone)]
pub struct ReturnInst {
    // pub ty: Type,
    pub value: Option<ValueId>,
}

impl From<ReturnInst> for Value {
    fn from(val: ReturnInst) -> Self {
        Value::Instruction(InstValue::Return(val))
    }
}

impl ReturnInst {
    fn replace_operands(&mut self, old_value_id: ValueId, new_value_id: ValueId) {
        if let Some(value) = &mut self.value {
            if *value == old_value_id {
                *value = new_value_id;
            }
        }
    }
}

pub enum TermInst {
    Jump(JumpInst),
    Branch(BranchInst),
    Return(ReturnInst),
}

impl From<TermInst> for Value {
    fn from(val: TermInst) -> Self {
        match val {
            TermInst::Jump(inst) => inst.into(),
            TermInst::Branch(inst) => inst.into(),
            TermInst::Return(inst) => inst.into(),
        }
    }
}

/// %x = phi <ty> [ <val0>, <label0>], ...
#[derive(Debug, Clone)]
pub struct PhiInst {
    pub ty: Type,
    pub incomings: Vec<(ValueId, ValueId)>, // preds. (value, bb)
}

impl PhiInst {
    pub fn replace_operands(&mut self, old_value_id: ValueId, new_value_id: ValueId) {
        for (value, _) in &mut self.incomings {
            if *value == old_value_id {
                *value = new_value_id;
            }
        }
    }
}

impl From<PhiInst> for Value {
    fn from(val: PhiInst) -> Self {
        Value::Instruction(InstValue::Phi(val))
    }
}
