use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::rc::Rc;

use crate::mc::*;
use crate::mc_inst::*;

use super::liveness::{LiveInfo, LivenessAnalyzer};

// impl extend_from_slice for HashSet
fn extend_from_slice(mut set: &HashSet<VirtReg>, slice: &[VirtReg]) {
    for item in slice {
        set.insert(item.clone());
    }
}

pub fn reg2ind(opr: &AsmOperand, is_float: bool) -> usize {
    if is_float {
        match opr {
            AsmOperand::VfpReg(r) => r.index.try_into().unwrap(),
            _ => panic!("Expected a VfpReg"),
        }
    } else {
        match opr {
            AsmOperand::IntReg(reg) => i64::from(reg.ty) as usize,
            _ => panic!("Expected a Reg"),
        }
    }
}

pub fn ind2reg(index: usize, is_float: bool) -> Option<AsmOperand> {
    if index < 0 {
        None
    } else if is_float {
        Some(AsmOperand::VfpReg(VfpReg::from(index as i64)))
    } else {
        Some(AsmOperand::IntReg(Reg::from(index as i64)))
    }
}

pub fn filter_virt_reg(uses: &Vec<AsmOperand>) -> Vec<VirtReg> {
    uses.iter()
        .filter_map(|op| match op {
            AsmOperand::VirtReg(vr) => Some(vr.clone()),
            _ => None,
        })
        .collect()
}

pub fn get_reg_from_constraint(
    in_constraints: &mut HashMap<AsmOperand, VirtReg>,
    vreg: &VirtReg,
) -> Option<AsmOperand> {
    let mut phy_reg: Option<AsmOperand> = None;

    let mut key_to_remove = None;
    for (key, value) in in_constraints.iter() {
        if value == vreg {
            phy_reg = Some(key.clone());
            key_to_remove = Some(key.clone());
            break;
        }
    }
    if let Some(key) = key_to_remove {
        in_constraints.remove(&key);
    }
    phy_reg
}

pub struct SimpleGlobalAllocator<'a> {
    module: &'a mut AsmModule,
    pub core_reg_count: u32,
    func_id: AsmValueId,
    register_mapping: HashMap<VirtReg, AsmOperand>,
    stack_mapping: HashMap<VirtReg, StackOperand>,
    address_mapping: HashMap<VirtReg, u32>, // for alloca
    used_reg: HashSet<u32>,
    used_vfp_reg: HashSet<u32>,
    temps: Vec<Reg>,
    fp_temps: Vec<VfpReg>,
    caller_saved: Vec<VirtReg>,
    callee_saved: Vec<VirtReg>,
    caller_saved_fp: Vec<VirtReg>,
    callee_saved_fp: Vec<VirtReg>,
    init_allow: Vec<u32>,
    init_allow_fp: Vec<u32>,
    mov_bonus: HashMap<VirtReg, HashMap<VirtReg, f64>>,
    abi_bonus: HashMap<VirtReg, HashMap<u32, f64>>,
    weight: HashMap<VirtReg, f64>,
    vreg_ind: isize,
    ig: HashMap<VirtReg, HashSet<VirtReg>>,
    live_info: HashMap<AsmValueId, Rc<RefCell<LiveInfo>>>,

    all_values: HashSet<AsmOperand>,
}

impl SimpleGlobalAllocator<'_> {
    const CORE_REG_COUNT: u32 = 11;

    pub fn new(func_id: AsmValueId, module: &mut AsmModule) -> Self {
        let mut init_allow_fp: Vec<u32> = (0..32).collect();
        init_allow_fp.remove(14);
        init_allow_fp.remove(15);
        let mut sga = SimpleGlobalAllocator {
            core_reg_count: Self::CORE_REG_COUNT,
            func_id,
            module,
            register_mapping: HashMap::new(),
            stack_mapping: HashMap::new(),
            address_mapping: HashMap::new(),
            used_reg: HashSet::new(),
            used_vfp_reg: HashSet::new(),
            temps: vec![Reg::new(RegType::Ip), Reg::new(RegType::Lr)],
            fp_temps: vec![VfpReg::from(14), VfpReg::from(15)],
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
            let reg = Reg::new(RegType::from(i));
            let vreg = sga.get_new_vreg(false);
            sga.caller_saved.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        for i in 4..11 {
            let reg = Reg::new(RegType::from(i));
            let vreg = sga.get_new_vreg(false);
            sga.callee_saved.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        for i in 0..14 {
            let reg = VfpReg::from(i);
            let vreg = sga.get_new_vreg(true);
            sga.caller_saved_fp.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        for i in 16..32 {
            let reg = VfpReg::from(i);
            let vreg = sga.get_new_vreg(true);
            sga.callee_saved_fp.push(vreg.clone());
            sga.register_mapping.insert(vreg, reg.into());
        }
        sga
    }

    pub fn process(module: &mut AsmModule) {
        for func_id in &module.funcs {
            let mut sga = SimpleGlobalAllocator::new(*func_id, module);
            sga.do_analysis();
        }
    }

    // ...

    pub fn get_new_vreg(&mut self, is_float: bool) -> VirtReg {
        let ret = VirtReg::new(self.vreg_ind as i32, is_float);
        self.vreg_ind -= 1;
        ret
    }

    pub fn do_analysis(&mut self) {
        self.init_all_values_set();
        let mut liveness_analyzer = LivenessAnalyzer::new(self.func_id.clone(), self.module);
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
                    allowed_regs.remove(reg2ind(
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
                        if vreg.is_float == real_reg.is_float()
                            && reg2ind(real_reg, vreg.is_float) == id as usize
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
            let real_reg = ind2reg(max_id as usize, vreg.is_float).unwrap();
            if real_reg.is_float() {
                self.used_vfp_reg.insert(max_id);
            } else {
                self.used_reg.insert(max_id);
            }
            self.register_mapping.insert(vreg.clone(), real_reg);
        }
    }

    pub fn build_interference_graph(&mut self) {
        let func = self.module.get_func(self.func_id);
        let bbs = func.bbs.clone();
        for bb_id in bbs {
            let mut live = (*self.live_info_of(&bb_id)).borrow_mut().live_out;
            let bb = self.module.get_bb(bb_id);
            let size = bb.insts.len();
            for i in (0..size).rev() {
                let inst_id = &bb.insts[i];
                let inst = self.module.get_inst(inst_id.clone());
                let uses = filter_virt_reg(&inst.get_uses());
                let defs = filter_virt_reg(&inst.get_defs());
                for vreg in &defs {
                    let mut live_clone = live.clone();
                    extend_from_slice(&live_clone, &defs);
                    for vreg2 in live_clone {
                        self.add_edge(vreg.clone(), vreg2.clone());
                    }
                }
                live.retain(|x| !defs.contains(x));
                let inst = self.module.get_inst(inst_id.clone());
                if inst.is_call() {
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
                extend_from_slice(&live, &uses);
            }
        }

        let move_cost = 1.0;
        let nest_level = 0;
        let load_cost = 16.0;
        let scale_factor = 10f64.powi(nest_level);
        let func = self.module.get_func(self.func_id);
        let bbs = func.bbs.clone();
        for bb_id in bbs {
            let bb = self.module.get_bb(bb_id);
            for inst_id in &bb.insts {
                let inst = self.module.get_inst(inst_id.clone());
                if inst.is_mov() {
                    let op1 = &inst.get_uses()[0];
                    let op2 = &inst.get_defs()[0];
                    if op1.as_virt_reg().is_some() && op2.as_virt_reg().is_some() {
                        let bonus = move_cost * scale_factor;
                        let map1 = self
                            .mov_bonus
                            .entry(*op1.clone().as_virt_reg().unwrap())
                            .or_insert(HashMap::new());
                        *map1
                            .entry(*op2.clone().as_virt_reg().unwrap())
                            .or_insert(0.0) += bonus;
                        let map2 = self
                            .mov_bonus
                            .entry(*op2.clone().as_virt_reg().unwrap())
                            .or_insert(HashMap::new());
                        *map2
                            .entry(*op1.clone().as_virt_reg().unwrap())
                            .or_insert(0.0) += bonus;
                        *self
                            .weight
                            .entry(*op1.clone().as_virt_reg().unwrap())
                            .or_insert(0.0) += bonus;
                        *self
                            .weight
                            .entry(*op2.clone().as_virt_reg().unwrap())
                            .or_insert(0.0) += bonus;
                    }
                }
                if inst.has_reg_constraint() {
                    let in_cons = inst.get_in_constraints_mut();
                    let out_cons = inst.get_out_constraints_mut();
                    let bonus = move_cost * scale_factor;
                    for vreg in &inst.get_uses() {
                        if let Some(vreg) = vreg.as_virt_reg() {
                            if let Some(cons) = get_reg_from_constraint(in_cons, vreg) {
                                let ind = reg2ind(&cons, vreg.is_float);
                                let map1 =
                                    self.abi_bonus.entry(vreg.clone()).or_insert(HashMap::new());
                                *map1.entry(ind as u32).or_insert(0.0) += bonus;
                                *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                            }
                        }
                    }
                    for vreg in &inst.get_defs() {
                        if let Some(vreg) = vreg.as_virt_reg() {
                            if let Some(cons) = get_reg_from_constraint(out_cons, vreg) {
                                let ind = reg2ind(&cons, vreg.is_float);
                                let map1 =
                                    self.abi_bonus.entry(vreg.clone()).or_insert(HashMap::new());
                                *map1.entry(ind as u32).or_insert(0.0) += bonus;
                                *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                            }
                        }
                    }
                }
                let bonus = load_cost * scale_factor;
                for vreg in &inst.get_uses() {
                    if let Some(vreg) = vreg.as_virt_reg() {
                        *self.weight.entry(vreg.clone()).or_insert(0.0) += bonus;
                    }
                }
                for vreg in &inst.get_defs() {
                    if let Some(vreg) = vreg.as_virt_reg() {
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
        let func = self.module.get_func(self.func_id);
        for blk_id in func.bbs {
            let mut added_inst_count = 0;
            let blk = self.module.get_bb(blk_id);
            let blk_insts_len = blk.insts.len();
            for i in 0..blk_insts_len {
                let inst_id = &blk.insts[i];
                let mut to_insert_before = Vec::new();
                let mut to_insert_after = Vec::new();
                let mut in_constraints = None;
                let mut out_constraints = None;
                let mut inst = self.module.get_inst_mut(inst_id.clone());
                if inst.has_reg_constraint() {
                    in_constraints = Some(inst.get_in_constraints_mut());
                    out_constraints = Some(inst.get_out_constraints_mut());
                }

                let mut free_temp: HashSet<u32> = HashSet::from_iter(vec![0, 1]);
                let mut dead: HashSet<u32> = HashSet::new();
                let mut dead_fp: HashSet<u32> = HashSet::new();
                for j in 0..inst.get_uses().len() {
                    todo!()
                }
                todo!();

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
            let t = RegType::from(*ind as i64);
            if Reg::is_callee_saved(*ind as i64) {
                used.push((
                    AsmOperand::IntReg(Reg::new(t)),
                    StackOperand::new(StackOperandType::Spill, func.stack_state.alloc_spill(4)),
                ));
            }
        }
        for ind in &self.used_vfp_reg {
            if VfpReg::is_callee_saved(*ind as i64) {
                used.push((
                    AsmOperand::VfpReg(VfpReg::from(*ind as i64)),
                    StackOperand::new(StackOperandType::Spill, func.stack_state.alloc_spill(4)),
                ));
            }
        }
        self.func_id.used_callee_saved_reg = used;
        LocalRegAllocator::insert_save_reg(&mut self.func_id);
    }

    pub fn spill_to_stack(
        &mut self,
        mut val: Option<AsmOperand>,
        vreg: &VirtReg,
        free_temp: &mut HashSet<u32>,
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
        to_insert_after.extend(self.module.expand_stack_operand_load_store_tmp(
            store,
            self.temps[*free_temp.iter().next().unwrap()].clone(),
        ));
        val.unwrap()
    }

    pub fn load_to_reg(
        &mut self,
        mut to: Option<AsmOperand>,
        vreg: &VirtReg,
        free_temp: &mut HashSet<u32>,
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
            to_insert_before.extend(
                self.module
                    .expand_stack_operand_load_store_tmp(load, to.clone().unwrap()),
            );
        } else {
            let tmp;
            if let Some(to) = &to {
                tmp = to.clone();
            } else {
                tmp = self.temps[*free_temp.iter().next().unwrap()].clone().into();
            }
            to_insert_before.extend(self.module.expand_stack_operand_load_store_tmp(load, tmp));
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
        ret.extend(i.get_defs().clone());
        ret.extend(i.get_uses().clone());
        ret
    }
    pub fn make_mov(
        &self,
        blk: &mut AsmBlock,
        is_float: bool,
        reg_to: &AsmOperand,
        reg_from: &AsmOperand,
        comment: String,
    ) -> AsmValueId {
        let mov_id: AsmValueId;
        if is_float {
            let mov = VMovInst::new(VMovInst::Ty::CPY, reg_to.clone(), reg_from.clone());
            mov_id = self.module.alloc_value(AsmValue::Inst(AsmInst::Mov(mov)))
        } else {
            let mov = MovInst::new(MovInst::Ty::REG, reg_to.clone(), reg_from.clone());
            mov_id = self.module.alloc_value(AsmValue::Inst(AsmInst::VMov(mov)))
        }
        mov_id
    }

    pub fn allocate_or_get_spill(&mut self, vreg: &VirtReg) -> StackOperand {
        if let Some(spilled_loc) = self.stack_mapping.get(vreg) {
            return spilled_loc.clone();
        }
        let spilled_loc =
            StackOperand::new(StackOperandType::Spill, func.stack_state.alloc_spill(4));
        self.stack_mapping.insert(vreg.clone(), spilled_loc.clone());
        spilled_loc
    }

    pub fn get_allowed_regs_list(&self, is_float: bool) -> Vec<u32> {
        if is_float {
            self.init_allow_fp.clone()
        } else {
            self.init_allow.clone()
        }
    }

    pub fn init_all_values_set(&mut self) {
        let func = self.module.get_func(self.func_id);
        let bbs = func.bbs.clone();
        for bb_id in bbs {
            let bb = self.module.get_bb(bb_id);
            for inst_id in &bb.insts {
                let inst = self.module.get_inst(*inst_id);
                self.all_values.extend(inst.get_uses().clone());
                self.all_values.extend(inst.get_defs().clone());
            }
        }
    }

    pub fn live_info_of(&self, bb_id: &AsmValueId) -> Rc<RefCell<LiveInfo>> {
        let ret = self.live_info.get(&bb_id).unwrap();
        *ret
    }
}
