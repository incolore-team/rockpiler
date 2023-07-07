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
        InstNamer { module: module, next_id: 1 }
    }

    pub fn run(&mut self) {
        for (_, func_val_id) in &self.module.functions {
            self.visit_function(func_val_id.to_owned());
        }
    }

    pub fn visit_function(&mut self, func_val_id: ValueId) {
        let func = self.module.get_func(func_val_id);
        for (_, block_val_id) in &func.bbs.bbs {
            self.visit_bb(*block_val_id);
        }
    }

    pub fn visit_bb(&mut self, block_val_id: ValueId) {
        let block = self.module.get_bb(block_val_id);
        for inst_val_id in &block.insts {
            self.visit_insts(*inst_val_id);
        }
    }

    pub fn visit_insts(&mut self, inst_val_id: ValueId) {
        let inst = self.module.get_inst(inst_val_id);
        let name = self.generate_local_name();
        inst.set_name(name);
    }

    pub fn generate_local_name(&mut self) -> String {
        let name = self.module.next_id.to_string();
        self.module.next_id += 1;
        name
    }
}