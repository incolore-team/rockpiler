use crate::ir::{AllocaInst, BasicBlockValue, Module, ValueId};

pub fn run(module: &mut Module) {
    let mut pass = Mem2Reg::new(module);
    pass.run();
}

pub struct Mem2Reg<'a> {
    module: &'a mut Module,

    cur_func: Option<ValueId>,
}

impl Mem2Reg<'_> {
    pub fn new(module: &mut Module) -> Mem2Reg {
        Mem2Reg {
            module: module,
            cur_func: None,
        }
    }

    pub fn run(&mut self) {
        for (_, func_val_id) in &self.module.functions.clone() {
            self.run_on_func(func_val_id.to_owned());
        }
    }

    pub fn run_on_func(&mut self, func_val_id: ValueId) {
        self.cur_func = Some(func_val_id);
        {}
        self.cur_func = None;
    }

    pub fn find_promotables(&mut self) -> Vec<ValueId> {
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

            let mut promotable = true;

            for user in users_of_alloca {
                let user_inst = self.module.get_inst(user);
                let store_inst = match user_inst {
                    crate::ir::InstValue::Store(store) => Some(store),
                    _ => None,
                };

                if store_inst.is_some() {
                    let store_ptr = store_inst.unwrap().ptr;
                    if (store_ptr == inst_id) {
                        promotable = false;
                        continue;
                    }
                }
            }
        }

        promotables
    }
}
