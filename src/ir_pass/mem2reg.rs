use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    ast::Type,
    ir::{ConstValue, InstValue, Module, Value, ValueId},
};

pub fn run(module: &mut Module) {
    let mut pass = Mem2Reg::new(module);
    pass.run();
}

pub struct Mem2Reg<'a> {
    module: &'a mut Module,

    cur_func: Option<ValueId>,
    // AllocaInst -> Map<BasicBlock, Value>>
    var_defs: HashMap<ValueId, HashMap<ValueId, ValueId>>,
    // PhiInst -> Value
    dead_phis: HashMap<ValueId, ValueId>,
    // 暂未加入 bb 的 phi
    pending_phis: HashSet<ValueId>,
    incomplete_phis: VecDeque<IncompletePhi>,
    // 已经遍历过的基本块
    filled_bbs: HashSet<ValueId>,
}

struct IncompletePhi {
    phi: ValueId,
    bb_id: ValueId,
    ptr: ValueId,
}

impl Mem2Reg<'_> {
    pub fn new(module: &mut Module) -> Mem2Reg {
        Mem2Reg {
            module,
            cur_func: None,
            var_defs: HashMap::new(),
            dead_phis: HashMap::new(),
            pending_phis: HashSet::new(),
            incomplete_phis: VecDeque::new(),
            filled_bbs: HashSet::new(),
        }
    }

    pub fn run(&mut self) {
        for (_, func_val_id) in &self.module.functions.clone() {
            self.cur_func = Some(*func_val_id);
            self.run_on_func(*func_val_id);
            self.cur_func = None;
        }
    }

    pub fn run_on_func(&mut self, func_val_id: ValueId) {
        let func = self.module.get_func(func_val_id);
        if func.is_external {
            return;
        }

        let promotables = self.find_promotables();
        if promotables.is_empty() {
            return;
        }

        let bbs = func.bbs.bbs.clone();
        for (_, bb_id) in bbs.clone() {
            if self.filled_bbs.contains(&bb_id) {
                unreachable!("bb_id should not be filled yet");
            }

            let mut bb = self.module.get_bb(bb_id.to_owned()).clone();

            bb.insts.retain(|inst_id| {
                let inst_id = *inst_id;
                let inst = self.module.get_inst(inst_id);
                if let InstValue::Load(load_inst) = inst {
                    // if load src ptr is not promotable, keep it
                    if !promotables.contains(&load_inst.ptr) {
                        return true;
                    }
                    // var ptr = (AllocaInst)load.getPtr();
                    let var = self.read_var(bb_id, load_inst.ptr);
                    // // inst的use就不用移除了，因为另外一边是alloca，之后也会被移除。
                    self.module.replace_value(inst_id, var);
                    false
                } else if let InstValue::Store(store_inst) = inst {
                    // if store dst is not promotable, keep it
                    if !promotables.contains(&store_inst.ptr) {
                        return true;
                    }
                    let store_target = store_inst.ptr;
                    let store_val = store_inst.value;
                    self.write_var(bb_id, store_target, store_val);
                    self.module.mark_nolonger_using(inst_id, store_target);
                    // inst.removeAllOperandUseFromValue();
                    false
                } else {
                    true
                }
            });

            self.module.get_bb_mut(bb_id).insts = bb.insts.clone();

            self.filled_bbs.insert(bb_id);
        }

        // 3. 处理incompletePhi
        while !self.incomplete_phis.is_empty() {
            let incomplete_phi = self.incomplete_phis.pop_front().unwrap();
            self.add_phi_operands(incomplete_phi.phi, incomplete_phi.bb_id, incomplete_phi.ptr);
        }

        // 把所有phi指令加入基本块
        for phi in self.pending_phis.clone() {
            let bb = self.module.get_parent_mut(phi);
            bb.insts.insert(0, phi);
        }

        // 4. 最后移除alloca
        for (_, bb_id) in bbs {
            let bb = self.module.get_bb_mut(bb_id);
            bb.insts.retain(|inst_id| !promotables.contains(inst_id));
        }
    }

    /// var 表示的是任何一个变量，phi、alloca、const int
    fn read_var(&mut self, bb_id: ValueId, alloca_id: ValueId) -> ValueId {
        let ptr = alloca_id;
        let defs = self.var_defs.get(&ptr);
        if defs.is_none() {
            unreachable!("alloca_id should be in var_defs");
        }

        let defs = defs.unwrap();
        let def_in_cur_bb = defs.get(&bb_id);
        if let Some(def) = def_in_cur_bb {
            return self.find_in_dead_phis(*def);
        }

        self.read_var_recursive(bb_id, ptr)
    }

    fn read_var_recursive(&mut self, bb_id: ValueId, alloca_id: ValueId) -> ValueId {
        let preds = self.module.get_bb_preds(bb_id);
        let val_id;
        if !self.is_bb_sealed(bb_id) {
            let phi_val = self.create_incomplete_phi(alloca_id, bb_id);
            self.incomplete_phis.push_back(IncompletePhi {
                phi: phi_val,
                bb_id,
                ptr: alloca_id,
            });
            val_id = phi_val;
        } else if preds.len() == 1 {
            let pred = preds[0];
            val_id = self.read_var(pred, alloca_id);
        } else {
            // Break potential cycles with operandless phi
            let phi_id = self.create_incomplete_phi(alloca_id, bb_id);
            self.write_var(bb_id, alloca_id, phi_id);
            val_id = self.add_phi_operands(phi_id, bb_id, alloca_id)
        }
        self.write_var(bb_id, alloca_id, val_id);
        val_id
    }

    fn create_incomplete_phi(&mut self, alloca_id: ValueId, bb_id: ValueId) -> ValueId {
        let alloca = self.module.get_inst(alloca_id);
        let ty = alloca.ty();
        let phi = self.module.alloc_phi_inst(ty); // todo: set parent for it
        self.pending_phis.insert(phi);
        self.module.value_parent.insert(phi, bb_id);
        phi
    }

    /// 如果基本块的所有前驱都已经遍历过，则返回 true
    fn is_bb_sealed(&self, bb_id: ValueId) -> bool {
        let preds = self.module.get_bb_preds(bb_id);

        for pred in preds {
            if !self.filled_bbs.contains(&pred) {
                return false;
            }
        }
        true
    }

    fn find_in_dead_phis(&mut self, def: ValueId) -> ValueId {
        // return def, if def is not a PhiInst
        // `def` may be a non-inst (like a constant int)
        let def_val = self.module.values.get(def);
        match def_val {
            Some(Value::Instruction(inst)) => match inst {
                InstValue::Phi(_) => {}
                _ => return def,
            },
            _ => return def,
        }
        // if def is not a dead PhiInst, return def
        if !self.dead_phis.contains_key(&def) {
            return def;
        }

        let mut cur = def;
        let mut compress_todo: Vec<ValueId> = Vec::new();
        while let Some(next) = self.dead_phis.get(&cur) {
            compress_todo.push(cur);
            cur = *next;
        }

        // path compression
        for phi in compress_todo {
            self.dead_phis.insert(cur, phi);
        }

        def
    }

    pub fn add_phi_operands(
        &mut self,
        phi_id: ValueId,
        bb_id: ValueId,
        alloca_id: ValueId,
    ) -> ValueId {
        for pred in self.module.get_bb_preds(bb_id) {
            let val = self.read_var(pred, alloca_id);
            assert_eq!(
                self.module.get_value(val).ty(),
                self.module.get_inst(phi_id).ty()
            );
            self.module.add_phi_incoming(phi_id, bb_id, val)
        }
        self.try_remove_trivial_phi(phi_id, alloca_id)
    }

    /// 移除“平凡（trivial）” Phi 函数节点的算法。所谓“平凡”指的是这个 Phi 函数节点
    /// 的所有操作数（operands）都是同一个值（Value），或者都是该 Phi 函数节点自己（phi）。
    pub fn try_remove_trivial_phi(&mut self, phi_id: ValueId, alloca_id: ValueId) -> ValueId {
        let phi = self.module.get_inst(phi_id).as_phi();
        let mut same_val = None;
        let mut undef = false;
        for (opr_id, _bb_id) in phi.incomings.clone() {
            if (same_val.is_some() && opr_id == same_val.unwrap()) || opr_id == phi_id {
                continue;
            }
            if same_val.is_some() {
                return phi_id;
            } else {
                same_val = Some(opr_id);
            }
        }

        if same_val.is_none() {
            // 所有operand都是phi自己
            // assert self.module.value_user[phi_id].is_empty()
            undef = true;
            same_val = Some(self.get_undef_value(self.module.get_inst(phi_id).ty()));
        }

        let mut to_recursive = Vec::new();
        for user_id in self.module.get_users_of(phi_id) {
            // if user is another phi
            if let InstValue::Phi(_) = self.module.get_inst(user_id) {
                to_recursive.push(user_id);
            }
        }

        self.module.replace_value(phi_id, same_val.unwrap());
        self.module.remove_phi_all_operands(phi_id);
        self.pending_phis.remove(&phi_id);

        if !undef {
            self.dead_phis.insert(phi_id, same_val.unwrap());
        }

        for user_id in to_recursive {
            self.try_remove_trivial_phi(user_id, alloca_id);
        }

        same_val.unwrap()
    }

    fn get_undef_value(&mut self, ty: Type) -> ValueId {
        self.module.alloc_value(ConstValue::zero_of(ty).into())
    }

    // 记录 alloca_id 变量在 bb_id 基本块中曾经被设置值为 val_id
    fn write_var(&mut self, bb_id: ValueId, alloca_id: ValueId, val_id: ValueId) {
        if let Some(map) = self.var_defs.get_mut(&alloca_id) {
            // The map already exists, add the new key-value pair to it
            map.insert(bb_id, val_id);
        } else {
            // The map doesn't exist, create a new one and add the key-value pair to it
            let mut map = HashMap::new();
            map.insert(bb_id, val_id);
            self.var_defs.insert(alloca_id, map);
        }
    }

    // 这段代码的作用是在一个控制流图（Control Flow Graph, CFG）中寻找一个值的最终定义。
    //
    // 如果传入的值不是 PhiInst 类型的话，就直接返回这个值，因为它不是在 CFG 中定义的。如果这个值是一个已经被标记为“死亡”的 PhiInst，就返回这个 PhiInst 自己，因为它没有实际意义，可以被删除。否则，就递归地查找这个值的最终定义。
    //
    // 如果这个值是一个 PhiInst，且被标记为“死亡”，就需要使用路径压缩（path compression）算法，将这个 PhiInst 和它的所有“死亡”后继节点都指向它们的最终定义。这样做的目的是为了在 CFG 中去除这些没有实际意义的节点，从而简化 CFG 的结构。
    //
    // 最后，如果找到的最终定义是一个 PhiInst，但没有被标记为“死亡”，则返回这个 PhiInst，因为它是 CFG 中的一个有效节点。
    pub fn find_promotables(&self) -> Vec<ValueId> {
        let mut promotables = Vec::new();

        let func = self.module.get_func(self.cur_func.unwrap());
        let entry_id = *func.bbs.entry_bb();
        let insts = self.module.get_bb(entry_id).insts.clone();
        for inst_id in insts {
            let inst = self.module.get_inst(inst_id);
            let alloca_inst = match inst {
                crate::ir::InstValue::Alloca(alloca) => Some(alloca),
                _ => None,
            };

            if alloca_inst.is_none() {
                continue;
            }

            let users_of_alloca = self
                .module
                .value_user
                .get(&inst_id)
                .cloned()
                .unwrap_or_default();

            let mut inst_is_promotable = true;

            for user in users_of_alloca {
                let user_inst = self.module.get_inst(user);

                let load_inst = match user_inst {
                    crate::ir::InstValue::Load(load) => Some(load),
                    _ => None,
                };

                if load_inst.is_some() {
                    // ok, promotable if user is a load
                    continue;
                }

                let store_inst = match user_inst {
                    crate::ir::InstValue::Store(store) => Some(store),
                    _ => None,
                };

                if store_inst.is_some() {
                    let store_ptr = store_inst.unwrap().ptr;
                    if store_ptr == inst_id {
                        // ok, promotable if user is a store that stores to this alloca
                        continue;
                    }
                }

                inst_is_promotable = false;
                break;
            }

            if inst_is_promotable {
                promotables.push(inst_id);
            }
        }

        promotables
    }
}
