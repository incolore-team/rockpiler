use crate::ir::{Module, ValueId};

pub fn run(module: &mut Module) {
    let mut namer = InstNamer::new(module);
    namer.run();
}

pub struct InstNamer<'a> {
    module: &'a mut Module,
    next_id: usize,
}

impl InstNamer<'_> {
    pub fn new(module: &mut Module) -> InstNamer {
        InstNamer {
            module: module,
            next_id: 0,
        }
    }

    pub fn run(&mut self) {
        for (_, var_val_id) in &self.module.global_variables.clone() {
            self.visit_global_variable(var_val_id.to_owned());
        }

        for (_, func_val_id) in &self.module.functions.clone() {
            self.visit_function(func_val_id.to_owned());
        }
    }

    pub fn visit_global_variable(&mut self, var_val_id: ValueId) {
        let var = self.module.get_global_var(var_val_id);
        let glob_name = format!("@{}", var.name);
        self.assign(var_val_id, glob_name);
    }

    pub fn visit_function(&mut self, func_val_id: ValueId) {
        self.next_id = 0;

        self.visit_params(func_val_id);

        self.next_id += 1;

        let func = self.module.get_func(func_val_id);

        for (_, block_val_id) in &func.bbs.bbs.clone() {
            self.visit_bb(*block_val_id);
        }
    }

    pub fn visit_params(&mut self, func_val_id: ValueId) {
        let func = self.module.get_func(func_val_id);
        for param_val_id in &func.params.clone() {
            let name = self.generate_local_name();
            self.assign(*param_val_id, name);
        }
    }

    pub fn visit_bb(&mut self, block_val_id: ValueId) {
        let block = self.module.get_bb(block_val_id.clone()).clone();
        if block.name != "entry" {
            let name = self.generate_bb_name();
            self.assign(block_val_id, name);
        }
        for inst_val_id in &block.insts.clone() {
            self.visit_inst(*inst_val_id);
        }
    }

    pub fn visit_inst(&mut self, val_id: ValueId) {
        let inst = self.module.get_inst(val_id);
        if !inst.has_output() {
            return;
        }
        let name = self.generate_local_name();
        self.assign(val_id, name)
    }

    pub fn assign(&mut self, val_id: ValueId, name: String) {
        log::debug!("set name {} to value {}", name.clone(), val_id.index());
        self.module.value_name.insert(val_id, name);
    }

    pub fn generate_local_name(&mut self) -> String {
        let name = format!("%{}", self.next_id);
        self.next_id += 1;
        name
    }

    pub fn generate_bb_name(&mut self) -> String {
        let name = format!("{}", self.next_id);
        self.next_id += 1;
        name
    }
}
