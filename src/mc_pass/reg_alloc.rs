use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

pub struct SimpleGlobalAllocator {
    pub core_reg_count: usize,
    func: AsmFunc,
    register_mapping: HashMap<VirtReg, AsmOperand>,
    stack_mapping: HashMap<VirtReg, StackOperand>,
    address_mapping: HashMap<VirtReg, usize>, // for alloca
    used_reg: HashSet<usize>,
    used_vfp_reg: HashSet<usize>,
    temps: Vec<Reg>,
    fp_temps: Vec<VfpReg>,
    caller_saved: Vec<VirtReg>,
    callee_saved: Vec<VirtReg>,
    caller_saved_fp: Vec<VirtReg>,
    callee_saved_fp: Vec<VirtReg>,
    init_allow: Vec<usize>,
    init_allow_fp: Vec<usize>,
    mov_bonus: HashMap<VirtReg, HashMap<VirtReg, f64>>,
    abi_bonus: HashMap<VirtReg, HashMap<usize, f64>>,
    weight: HashMap<VirtReg, f64>,
    vreg_ind: isize,
    ig: HashMap<VirtReg, HashSet<VirtReg>>,
    live_info: HashMap<AsmBlock, LiveInfo>,
    all_values: HashSet<AsmOperand>,
}

impl SimpleGlobalAllocator {
    const CORE_REG_COUNT: usize = 11;

    pub fn new(func: AsmFunc) -> Self {
        let mut init_allow_fp: Vec<usize> = (0..32).collect();
        init_allow_fp.remove(14);
        init_allow_fp.remove(15);
        let mut sga = SimpleGlobalAllocator {
            core_reg_count: Self::CORE_REG_COUNT,
            func,
            register_mapping: HashMap::new(),
            stack_mapping: HashMap::new(),
            address_mapping: HashMap::new(),
            used_reg: HashSet::new(),
            used_vfp_reg: HashSet::new(),
            temps: vec![Reg::new(Reg::Type::Ip), Reg::new(Reg::Type::Lr)],
            fp_temps: vec![VfpReg::new(14), VfpReg::new(15)],
            caller_saved: Vec::new(),
            callee_saved: Vec::new(),
            caller_saved_fp: Vec::new(),
            callee_saved_fp: Vec::new(),
            init_allow: (0..11).collect(),
            init_allow_fp: init_allow_fp,
            mov_bonus: HashMap::new(),
            abi_bonus: HashMap::new(),
            weight: HashMap::new(),
            vreg_ind: -1,
            ig: HashMap::new(),
            live_info: HashMap::new(),
            all_values: HashSet::new(),
        };
        for i in 0..4 {
            let reg = Reg::new(Reg::Type::Values[i]);
            let vreg = sga.get_new_vreg(false, "precolored");
            sga.caller_saved.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        for i in 4..11 {
            let reg = Reg::new(Reg::Type::Values[i]);
            let vreg = sga.get_new_vreg(false, "precolored");
            sga.callee_saved.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        for i in 0..14 {
            let reg = VfpReg::new(i);
            let vreg = sga.get_new_vreg(true, "precolored");
            sga.caller_saved_fp.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        for i in 16..32 {
            let reg = VfpReg::new(i);
            let vreg = sga.get_new_vreg(true, "precolored");
            sga.callee_saved_fp.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        sga
    }

    pub fn process(module: &mut AsmModule) -> &AsmModule {
        for func in &module.funcs {
            let mut sga = SimpleGlobalAllocator::new(func.clone());
            sga.do_analysis();
        }
        module
    }

    // ...

    pub fn get_new_vreg(&mut self, is_float: bool, comment: &str) -> VirtReg {
        let ret = VirtReg::new(self.vreg_ind, is_float);
        ret.comment = comment.to_string();
        self.vreg_ind -= 1;
        ret
    }

    pub fn do_analysis(&mut self) {
        self.init_all_values_set();
        let mut liveness_analyzer = LivenessAnalyzer::new(self.func.clone());
        liveness_analyzer.execute();
        self.live_info = liveness_analyzer.live_info;
        self.build_interference_graph();
        self.do_color();
        self.do_fix_up();
    }

    pub fn do_color(&mut self) {
        let mut sorted: Vec<_> = self.weight.iter().collect();
        sorted.sort_by(|a, b| a.1.partial_cmp(b.1).unwrap());

        for (vreg, _) in sorted {
            if self.register_mapping.contains_key(vreg) {
                continue;
            }
            let mut allowed_regs = self.get_allowed_regs_list(vreg.is_float);
            let ig_vreg = self.ig.get(vreg).unwrap_or(&HashSet::new()).clone();
            for ig_vreg in ig_vreg {
                if vreg.is_float == ig_vreg.is_float && self.register_mapping.contains_key(&ig_vreg)
                {
                    allowed_regs.remove(&LocalRegAllocator::reg2ind(
                        self.register_mapping.get(&ig_vreg).unwrap(),
                        vreg.is_float,
                    ));
                }
            }
            if allowed_regs.is_empty() {
                continue; // add spill cost
            }
            let mut max_bonus = -1.0;
            let mut max_id = *allowed_regs.iter().next().unwrap();
            for id in allowed_regs {
                let mut bonus = 0.0;
                let mov_bonus = self.mov_bonus.get(vreg).unwrap_or(&HashMap::new()).clone();
                for (mov_vreg, value) in mov_bonus {
                    if self.register_mapping.contains_key(&mov_vreg) {
                        let real_reg = self.register_mapping.get(&mov_vreg).unwrap();
                        if vreg.is_float == real_reg.is_float
                            && LocalRegAllocator::reg2ind(real_reg, vreg.is_float) == id
                        {
                            bonus += value;
                        }
                    }
                }
                let abi_bonus = self
                    .abi_bonus
                    .get(vreg)
                    .unwrap_or(&HashMap::new())
                    .get(&id)
                    .unwrap_or(&0.0);
                bonus += abi_bonus;
                if bonus > max_bonus {
                    max_bonus = bonus;
                    max_id = id;
                }
            }
            let real_reg = LocalRegAllocator::ind2reg(max_id, vreg.is_float);
            if real_reg.is_float {
                self.used_vfp_reg.insert(max_id);
            } else {
                self.used_reg.insert(max_id);
            }
            self.register_mapping.insert(vreg.clone(), real_reg);
        }
    }

    pub fn get_allowed_regs_list(&self, is_float: bool) -> HashSet<usize> {
        if is_float {
            HashSet::from_iter(self.init_allow_fp.clone())
        } else {
            HashSet::from_iter(self.init_allow.clone())
        }
    }

    pub fn build_interference_graph(&mut self) {
        for bb in &self.func.bbs {
            let live = self.live_info_of(&bb).live_out.clone();
            let size = bb.insts.len();
            for i in (0..size).rev() {
                let inst = &bb.insts[i];
                let uses = LocalRegAllocator::filter_virt_reg(&inst.uses);
                let defs = LocalRegAllocator::filter_virt_reg(&inst.defs);
                for vreg in &defs {
                    let mut live_clone = live.clone();
                    live_clone.extend_from_slice(&defs);
                    for vreg2 in live_clone {
                        self.add_edge(vreg.clone(), vreg2.clone());
                    }
                }
                live.retain(|x| !defs.contains(x));
                if inst.is_instance_of("CallInst") {
                    for vreg in &live {
                        if vreg.is_float {
                            for cr in &self.caller_saved_fp {
                                self.add_edge(vreg.clone(), cr.clone());
                            }
                        } else {
                            for cr in &self.caller_saved {
                                self.add_edge(vreg.clone(), cr.clone());
                            }
                        }
                    }
                }
                live.extend_from_slice(&uses);
            }
        }

        let move_cost = 1.0;
        let nest_level = 0;
        let load_cost = 16.0;
        let scale_factor = 10f64.powi(nest_level);
        for bb in &self.func.bbs {
            for inst in &bb.insts {
                if inst.is_instance_of("MovInst") {
                    let op1 = &inst.uses[0];
                    let op2 = &inst.defs[0];
                    if op1.is_instance_of("VirtReg") && op2.is_instance_of("VirtReg") {
                        let bonus = move_cost * scale_factor;
                        let map1 = self.mov_bonus.entry(op1.clone()).or_insert(HashMap::new());
                        *map1.entry(op2.clone()).or_insert(0.0) += bonus;
                        let map2 = self.mov_bonus.entry(op2.clone()).or_insert(HashMap::new());
                        *map2.entry(op1.clone()).or_insert(0.0) += bonus;
                        *self.weight.entry(op1.clone()).or_insert(0.0) += bonus;
                        *self.weight.entry(op2.clone()).or_insert(0.0) += bonus;
                    }
                }
                if inst.is_instance_of("ConstrainRegInst") {
                    let in_cons = get_in_constraints(inst);
                    let out_cons = get_out_constraints(inst);
                    let bonus = move_cost * scale_factor;
                    for vreg in &inst.uses {
                        if vreg.is_instance_of("VirtReg") {
                            if let Some(cons) = get_reg_from_constraint(&in_cons, vreg) {
                                let ind = LocalRegAllocator::reg2ind(&cons, vreg.is_float);
                                let map1 =
                                    self.abi_bonus.entry(vreg.clone()).or_insert(HashMap::new());
                                *map1.entry(ind).or_insert(0.0) += bonus;
                                *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                            }
                        }
                    }
                    for vreg in &inst.defs {
                        if vreg.is_instance_of("VirtReg") {
                            if let Some(cons) = get_reg_from_constraint(&out_cons, vreg) {
                                let ind = LocalRegAllocator::reg2ind(&cons, vreg.is_float);
                                let map1 =
                                    self.abi_bonus.entry(vreg.clone()).or_insert(HashMap::new());
                                *map1.entry(ind).or_insert(0.0) += bonus;
                                *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                            }
                        }
                    }
                }
                let bonus = load_cost * scale_factor;
                for vreg in &inst.uses {
                    if vreg.is_instance_of("VirtReg") {
                        *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                    }
                }
                for vreg in &inst.defs {
                    if vreg.is_instance_of("VirtReg") {
                        *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                    }
                }
            }
        }
    }
    fn add_edge(&mut self, vreg: VirtReg, vreg2: VirtReg) {
        if vreg != vreg2 {
            self.ig
                .entry(vreg.clone())
                .or_insert(HashSet::new())
                .insert(vreg2.clone());
            self.ig
                .entry(vreg2.clone())
                .or_insert(HashSet::new())
                .insert(vreg.clone());
        }
    }

    pub fn do_fix_up(&mut self) {
        for blk in &mut self.func {
            let mut added_inst_count = 0;
            let blk_insts_len = blk.insts.len();
            for i in 0..blk_insts_len {
                let inst = &blk.insts[i];
                let mut to_insert_before = Vec::new();
                let mut to_insert_after = Vec::new();
                let mut in_constraints = None;
                let mut out_constraints = None;
                if inst.is_instance_of("ConstrainRegInst") {
                    in_constraints = get_in_constraints(inst);
                    out_constraints = get_out_constraints(inst);
                }

                let mut free_temp: HashSet<usize> = HashSet::from_iter(vec![0, 1]);
                let mut dead: HashSet<usize> = HashSet::new();
                let mut dead_fp: HashSet<usize> = HashSet::new();
                for j in 0..inst.uses.len() {
                    let vreg = &inst.uses[j];
                    if !vreg.is_instance_of("VirtReg") {
                        continue;
                    }
                    let real_reg = self.register_mapping.get(vreg);
                    let constraint_reg = get_reg_from_constraint(&in_constraints, vreg);
                    // ... handle the logic here ...
                }

                // Process defs
                // ... handle the logic here ...

                // Insert the added instructions
                blk.insts.splice(i..i, to_insert_before);
                let offset = i + to_insert_before.len();
                blk.insts.splice(offset + 1..offset + 1, to_insert_after);
            }
        }

        // Handle usedReg and set func.used_callee_saved_reg,
        // and insert the related save and restore instructions.
        // Finally, update the used callee saved register to the function.
        let mut used = Vec::new();
        for ind in &self.used_reg {
            let t = Reg::Type::Values[*ind];
            if t.is_callee_saved() {
                used.push((
                    Reg::new(t).into(),
                    StackOperand::new(StackOperand::Type::Spill, self.func.sm.alloc_spill(4)),
                ));
            }
        }
        for ind in &self.used_vfp_reg {
            if VfpReg::is_callee_saved(*ind) {
                used.push((
                    VfpReg::new(*ind).into(),
                    StackOperand::new(StackOperand::Type::Spill, self.func.sm.alloc_spill(4)),
                ));
            }
        }
        self.func.used_callee_saved_reg = used;
        LocalRegAllocator::insert_save_reg(&mut self.func);
    }

    pub fn spill_to_stack(
        &mut self,
        mut val: Option<AsmOperand>,
        vreg: &VirtReg,
        free_temp: &mut HashSet<usize>,
        blk: &mut AsmBlock,
        to_insert_after: &mut Vec<AsmInst>,
        exclude: bool,
    ) -> AsmOperand {
        if val.is_none() {
            assert!(!free_temp.is_empty());
            let alloc = *free_temp.iter().next().unwrap();
            if vreg.is_float {
                val = Some(self.fp_temps[alloc].clone().into());
            } else {
                val = Some(self.temps[alloc].clone().into());
            }

            if exclude {
                free_temp.remove(&alloc);
            }
        }
        let spilled_loc = self.allocate_or_get_spill(vreg);
        let store;
        if vreg.is_float {
            store = VSTRInst::new(
                blk,
                val.clone().unwrap(),
                spilled_loc.clone(),
                "Spill ".to_string() + &vreg.comment,
            );
        } else {
            store = StoreInst::new(
                blk,
                val.clone().unwrap(),
                spilled_loc.clone(),
                "Spill ".to_string() + &vreg.comment,
            );
        }
        to_insert_after.extend(Generator::expand_stack_operand_load_store_tmp(
            store,
            self.temps[*free_temp.iter().next().unwrap()].clone(),
        ));
        val.unwrap()
    }

    pub fn load_to_reg(
        &mut self,
        mut to: Option<AsmOperand>,
        vreg: &VirtReg,
        free_temp: &mut HashSet<usize>,
        blk: &mut AsmBlock,
        to_insert_before: &mut Vec<AsmInst>,
        exclude: bool,
    ) -> AsmOperand {
        if to.is_none() {
            assert!(!free_temp.is_empty());
            let alloc = *free_temp.iter().next().unwrap();
            if vreg.is_float {
                to = Some(self.fp_temps[alloc].clone().into());
            } else {
                to = Some(self.temps[alloc].clone().into());
            }
            if exclude {
                free_temp.remove(&alloc);
            }
        }
        let spilled_loc = self.allocate_or_get_spill(vreg);
        let load;
        if vreg.is_float {
            load = VLDRInst::new(
                blk,
                to.clone().unwrap(),
                spilled_loc.clone(),
                "load spilled ".to_string() + &vreg.comment,
            );
        } else {
            load = LoadInst::new(
                blk,
                to.clone().unwrap(),
                spilled_loc.clone(),
                "load spilled ".to_string() + &vreg.comment,
            );
        }
        if !vreg.is_float {
            to_insert_before.extend(Generator::expand_stack_operand_load_store_tmp(
                load,
                to.clone().unwrap(),
            ));
        } else {
            let tmp;
            if let Some(to) = &to {
                tmp = to.clone();
            } else {
                tmp = self.temps[*free_temp.iter().next().unwrap()].clone().into();
            }
            to_insert_before.extend(Generator::expand_stack_operand_load_store_tmp(load, tmp));
        }
        to.unwrap()
    }

    pub fn get_reg_from_constraint(
        constraints: &Option<HashMap<AsmOperand, VirtReg>>,
        vreg: &VirtReg,
    ) -> Option<AsmOperand> {
        if let Some(constraints) = constraints {
            for (phy_reg, vreg_) in constraints {
                if vreg == vreg_ {
                    return Some(phy_reg.clone());
                }
            }
        }
        None
    }
    pub fn operands_of(&self, i: &AsmInst) -> Vec<AsmOperand> {
        let mut ret = Vec::new();
        ret.extend(i.defs.clone());
        ret.extend(i.uses.clone());
        ret
    }
    pub fn make_mov(
        &self,
        blk: &mut AsmBlock,
        is_float: bool,
        reg_to: &AsmOperand,
        reg_from: &AsmOperand,
        comment: String,
    ) -> Box<dyn AsmInst> {
        let mov: Box<dyn AsmInst>;
        if is_float {
            mov = Box::new(VMovInst::new(
                blk,
                VMovInst::Ty::CPY,
                reg_to.clone(),
                reg_from.clone(),
                comment,
            ));
        } else {
            mov = Box::new(MovInst::new(
                blk,
                MovInst::Ty::REG,
                reg_to.clone(),
                reg_from.clone(),
                comment,
            ));
        }
        mov
    }

    pub fn allocate_or_get_spill(&mut self, vreg: &VirtReg) -> StackOperand {
        if let Some(spilled_loc) = self.stack_mapping.get(vreg) {
            return spilled_loc.clone();
        }
        let spilled_loc = StackOperand::new(StackOperand::Type::Spill, self.func.sm.alloc_spill(4));
        self.stack_mapping.insert(vreg.clone(), spilled_loc.clone());
        spilled_loc
    }

    pub fn get_allowed_regs_list(&self, is_float: bool) -> Vec<usize> {
        if is_float {
            self.init_allow_fp.clone()
        } else {
            self.init_allow.clone()
        }
    }

    pub fn init_all_values_set(&mut self) {
        for bb in &self.func.bbs {
            for inst in &bb.insts {
                self.all_values.extend(inst.uses.clone());
                self.all_values.extend(inst.defs.clone());
            }
        }
    }

    pub fn live_info_of(&self, b: &AsmBlock) -> LiveInfo {
        let ret = self.live_info.get(b).unwrap();
        ret.clone()
    }
}
