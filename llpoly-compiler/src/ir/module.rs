use linked_hash_map::LinkedHashMap;

use crate::value_cast;

use super::{scope::*, value::*, BUILTIN_PREFIX};


#[derive(Debug, Default)]
pub struct Module {
    pub ur: ValueRegistry,
    pub sr: ScopeRegistry,

    pub func_map: LinkedHashMap<String, ValueId>,
    pub const_str_map: LinkedHashMap<String, ValueId>,
    pub global_var_map: LinkedHashMap<String, ValueId>,

    pub(crate) cur_scope: ScopeId,
    pub(crate) cur_bb: Option<ValueId>,
    pub(crate) cur_func: Option<ValueId>,
}

impl Module {
    // pub fn register_func(&mut self, func: Func) -> ValueId {
    //     let name = func.name.clone();
    //     let id = self.ur.next_id();
    //     let func_value = Value {
    //         display_name: match func.is_builtin {
    //             true => format!("@{}{}", BUILTIN_PREFIX, name),
    //             false => format!("@{}", name),
    //         },
    //         id,
    //         uses: Vec::new(),
    //         proto: ValueProto::Func(func),
    //     };
    //     let id = self.ur.insert(func_value);
    //     self.funcs_map.insert(name, id);
    //     id
    // }
    pub fn generate_local_id(&mut self) -> usize {
        let func = self.cur_func_mut();
        let ret = func.next_local_id;
        func.next_local_id += 1;
        ret
    }
    pub fn generate_local_display_name(&mut self) -> String {
        format!("%{}", self.generate_local_id())
    }

    pub fn register_func(&mut self, func: Func) -> ValueId {
        let id = self.ur.next_id();
        let func_name = func.name.clone();
        let func_value = Value {
            display_name: match func.is_builtin {
                true => format!("@{}{}", BUILTIN_PREFIX, func_name),
                false => format!("@{}", func_name),
            },
            id,
            uses: Vec::new(),
            proto: ValueProto::Func(func),
        };
        let id = self.ur.insert(func_value);
        self.func_map.insert(func_name, id);
        id
    }

    pub fn get_func(&self, id: ValueId) -> &Func {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::Func(ref func) => {
            func
        })
    }

    pub fn get_func_mut(&mut self, id: ValueId) -> &mut Func {
        value_cast!(self.ur.get_mut(id).unwrap(), ValueProto::Func(ref mut func) => {
            func
        })
    }

    /// Register a variable to UR
    pub fn register_var(&mut self, var: Var) -> ValueId {
        let value;
        let mut is_global = false;
        let var_name = var.name.clone();
        if var.is_global {
            value = Value {
                display_name: match var.is_bultin {
                    true => format!("@{}{}", BUILTIN_PREFIX, var_name),
                    false => format!("@{}", var_name),
                },
                proto: ValueProto::Var(var),
                ..Default::default()
            };
            is_global = true;
        } else if var.is_param {
            value = Value {
                display_name: self.generate_local_display_name(),
                proto: ValueProto::Var(var),
                ..Default::default()
            };
        } else if var.is_const {
            value = Value {
                display_name: format!("@{}", var_name),
                proto: ValueProto::Var(var),
                ..Default::default()
            };
        } else {
            todo!()
        }
        let id = self.ur.insert(value);
        if is_global {
            self.global_var_map.insert(var_name, id);
        }
        id
    }
    /// Register a variable to UR but do not generate a new display name
    pub fn register_psuedo_var(&mut self, var: Var, display_name: String) -> ValueId {
        let id = self.ur.next_id();
        let var_name = var.name.clone();
        let is_global = var.is_global.clone();
        let value = Value {
            display_name,
            id,
            uses: Vec::new(),
            proto: ValueProto::Var(var),
        };
        let id = self.ur.insert(value);
        if is_global {
            self.global_var_map.insert(var_name, id);
        }
        id
    }
    pub fn register_imm_val(&mut self, const_val: ImmVal) -> ValueId {
        let id = self.ur.next_id();
        let const_val_value = Value {
            display_name: format!("@_const_val_{}", id),
            id,
            uses: Vec::new(),
            proto: ValueProto::ImmVal(const_val),
        };
        self.ur.insert(const_val_value)
    }

    pub fn register_bb(&mut self, bb: BasicBlock) -> ValueId {
        let name = bb.name.clone();
        let id = self.ur.next_id();
        let bb_value = Value {
            display_name: format!("b{id}_{name}", id = id, name = name),
            id,
            uses: Vec::new(),
            proto: ValueProto::BasicBlock(bb),
        };
        let id = self.ur.insert(bb_value);
        let cur_func = value_cast!(self.cur_func.and_then(|i| self.ur.get_mut(i)).unwrap(), ValueProto::Func(ref mut func) => {
            func
        });
        cur_func.bb_map.insert(name, id);
        id
    }

    pub fn register_inst(&mut self, inst: Inst) -> ValueId {
        let id = self.ur.next_id();
        let inst_value = Value {
            display_name: self.generate_local_display_name(),
            id,
            uses: Vec::new(),
            proto: ValueProto::Inst(inst),
        };
        let id = self.ur.insert(inst_value);
        let cur_bb = value_cast!(self.cur_bb.and_then(|i| self.ur.get_mut(i)).unwrap(), ValueProto::BasicBlock(ref mut bb) => {
            bb
        });
        cur_bb.insts.push(id);
        id
    }
    pub fn cur_func(&self) -> &Func {
        value_cast!(self.cur_func.and_then(|i| self.ur.get(i)).unwrap(), ValueProto::Func(ref func) => {
            func
        })
    }

    pub fn cur_func_mut(&mut self) -> &mut Func {
        value_cast!(self.cur_func.and_then(|i| self.ur.get_mut(i)).unwrap(), ValueProto::Func(ref mut func) => {
            func
        })
    }

    pub fn cur_bb(&self) -> &BasicBlock {
        value_cast!(self.cur_bb.and_then(|i| self.ur.get(i)).unwrap(), ValueProto::BasicBlock(ref bb) => {
            bb
        })
    }

    pub fn cur_bb_mut(&mut self) -> &mut BasicBlock  {
        value_cast!(self.cur_bb.and_then(|i| self.ur.get_mut(i)).unwrap(), ValueProto::BasicBlock(ref mut bb) => {
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
                display_name: format!(
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

    pub fn get_bb(&self, id: ValueId) -> &BasicBlock {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::BasicBlock(ref bb) => {
            bb
        })
    }

    pub fn get_inst(&self, id: ValueId) -> &Inst {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::Inst(ref inst) => {
            inst
        })
    }

    pub fn get_var(&self, id: ValueId) -> &Var {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::Var(ref var) => {
            var
        })
    }

    pub fn get_const_str(&self, id: ValueId) -> &String {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::ConstStr(ref s) => {
            s
        })
    }

    pub fn get_imm_val(&self, id: ValueId) -> &ImmVal {
        value_cast!(self.ur.get(id).unwrap(), ValueProto::ImmVal(ref val) => {
            val
        })
    }

    pub fn get_value(&self, id: ValueId) -> &Value {
        self.ur.get(id).unwrap()
    }

    pub fn get_value_mut(&mut self, id: ValueId) -> &mut Value {
        self.ur.get_mut(id).unwrap()
    }
}
