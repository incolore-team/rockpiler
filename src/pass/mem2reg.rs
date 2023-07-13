use std::collections::HashMap;

use crate::ir::{AllocaInst, BasicBlockValue, InstValue, Module, ValueId};

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
}

impl Mem2Reg<'_> {
    pub fn new(module: &mut Module) -> Mem2Reg {
        Mem2Reg {
            module: module,
            cur_func: None,
            var_defs: HashMap::new(),
            dead_phis: HashMap::new(),
        }
    }

    pub fn run(&mut self) {
        for (_, func_val_id) in &self.module.functions.clone() {
            self.run_on_func(func_val_id.to_owned());
        }
    }

    pub fn run_on_func(&mut self, func_val_id: ValueId) {
        self.cur_func = Some(func_val_id);
        {
            let mut filled_bbs: Vec<ValueId> = Vec::new();

            let func = self.module.get_func(func_val_id);
            if func.is_external {
                return;
            }

            let promotables = self.find_promotables();
            if promotables.len() == 0 {
                return;
            }

            for (_, bb_id) in func.bbs.bbs.clone() {
                if filled_bbs.contains(&bb_id) {
                    unreachable!("bb_id should not be filled yet");
                }

                let mut bb = self.module.get_bb(bb_id.to_owned()).clone();

                bb.insts.retain(|inst_id| {
                    let inst_id = inst_id.clone();
                    let inst = self.module.get_inst(inst_id);
                    if let InstValue::Load(load_inst) = inst {
                        // if load src ptr is not promotable, keep it
                        if !promotables.contains(&load_inst.src) {
                            return true;
                        }
                        // var ptr = (AllocaInst)load.getPtr();
                        let var = self.read_var(bb_id, load_inst.src);
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
                        self.module.mark_nolonger_use(inst_id);
                        // inst.removeAllOperandUseFromValue();
                        false
                    } else {
                        true
                    }
                });

                self.module.get_bb_mut(bb_id).insts = bb.insts.clone();

                filled_bbs.push(bb_id.clone());
            }
        }
        self.cur_func = None;
    }

    fn read_var(&self, bb_id: ValueId, alloca_id: ValueId) -> ValueId {
        let ptr = alloca_id;
        let defs = self.var_defs.get(&ptr);
        if defs.is_none() {
            unreachable!("alloca_id should be in var_defs");
        }

        let defs = defs.unwrap();
        let def_in_cur_bb = defs.get(&bb_id);
        if let Some(def) = def_in_cur_bb {
            return self.find_in_dead_phis(def.clone());
        }

        return self.read_var_recursive(bb_id, ptr);
    }

    fn read_var_recursive(&self, bb_id: ValueId, alloca_id: ValueId) -> ValueId {
        unimplemented!()
    }

    fn find_in_dead_phis(&self, def: ValueId) -> ValueId {
        // return def, if def is not a PhiInst
        let def_inst = self.module.get_inst(def.clone());
        if !matches!(def_inst, InstValue::Phi(_)) {
            return def;
        }

        return def.clone();
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
        let entry_id = func.bbs.entry_bb().clone();
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

            let users_of_alloca = self.module.value_user[&inst_id].clone();

            let mut inst_is_promotable = true;

            for user in users_of_alloca {
                let user_inst = self.module.get_inst(user);
                let store_inst = match user_inst {
                    crate::ir::InstValue::Store(store) => Some(store),
                    _ => None,
                };

                let user_is_store_inst = store_inst.is_some();
                if user_is_store_inst {
                    let store_ptr = store_inst.unwrap().ptr;
                    let store_ptr_is_alloca = store_ptr == inst_id;
                    if store_ptr_is_alloca {
                        // ok, promotable
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
