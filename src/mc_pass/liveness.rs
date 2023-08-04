use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use log::{debug, trace, warn};

use crate::{mc::*, mc_inst::AsmInstTrait};

type AsmBBId = AsmValueId;
pub struct LivenessAnalyzer<'a> {
    pub live_info: HashMap<AsmBBId, Rc<RefCell<LiveInfo>>>,
    pub func_id: AsmValueId,
    module: &'a mut AsmModule,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LiveInfo {
    pub live_use: HashSet<VirtReg>, // Values used in this block. If a value is redefined in this block, it will appear in live_def. If this value is used in this block before it is defined, it will still be included in the live_use set.
    pub live_def: HashSet<VirtReg>, // Values defined in this block.
    pub live_in: HashSet<VirtReg>, // LiveIn, entry active, that is, if there is a use on any path starting from the entry point (used before the value is defined), then it is entry active.
    pub live_out: HashSet<VirtReg>, // LiveOut, exit active. If there is a use on any path starting from the exit of BB (used before the value is defined), then it is exit active.
}

impl Default for LiveInfo {
    fn default() -> Self {
        LiveInfo {
            live_use: HashSet::new(),
            live_def: HashSet::new(),
            live_in: HashSet::new(),
            live_out: HashSet::new(),
        }
    }
}

impl<'a> LivenessAnalyzer<'a> {
    pub fn new(func_id: AsmValueId, module: &mut AsmModule) -> Self {
        LivenessAnalyzer {
            live_info: HashMap::new(),
            func_id,
            module,
        }
    }

    pub fn log_debug_info(&self) {
        let func = self.module.get_func(self.func_id);
        let mut debug_info = format!("liveInfo of {}:", func.name);
        for bb_id in func.bbs.clone() {
            let bb = self.module.get_bb(bb_id);
            let s = format!("\n{}:\n{:?}", bb.name, self.live_info.get(&bb_id));
            debug_info.push_str(&s);
        }
        debug!("{}", debug_info)
    }

    pub fn execute(&mut self) {
        let func = self.module.get_func(self.func_id);
        for block in func.bbs.clone() {
            self.analyze_use_def(block);
        }

        let mut stable = false;
        let mut counter = 0;
        let mut reversed_block_ids: Vec<&AsmValueId> = func.bbs.iter().rev().collect();

        while !stable {
            stable = true;
            for block_id in reversed_block_ids {
                let block_stable = self.analyze_in_out(*block_id);
                if !block_stable {
                    stable = false;
                }
            }
            counter += 1;
            if counter > 100000 {
                warn!("LiveIntervalAnalyzer: too many iterations");
            }
        }
        self.log_debug_info();
    }

    pub fn live_info_of(&mut self, bb_id: AsmValueId) -> Rc<RefCell<LiveInfo>> {
        let block: &AsmBlock = self.module.get_bb(bb_id);
        if !self.live_info.contains_key(&bb_id) {
            self.live_info
                .insert(bb_id, Rc::new(RefCell::new(LiveInfo::default())));
        }
        *self.live_info.get(&bb_id).unwrap()
    }

    fn analyze_use_def(&mut self, bb_id: AsmValueId) {
        let block: &AsmBlock = self.module.get_bb(bb_id);
        let mut live_info = self.live_info_of(bb_id).borrow_mut();
        for inst_id in &block.insts {
            let inst = self.module.get_inst(*inst_id);
            for operand in &inst.get_uses() {
                if let AsmOperand::VirtReg(vr) = operand {
                    if !live_info.live_def.contains(vr) {
                        live_info.live_use.insert(vr.clone());
                    }
                } else {
                    warn!("operand is not VirtReg, but {:?}", operand);
                }
            }
            for operand in &inst.get_defs() {
                if let AsmOperand::VirtReg(vr) = operand {
                    live_info.live_def.insert(vr.clone());
                } else {
                    warn!("operand is not VirtReg, but {:?}", operand);
                }
            }
        }
    }

    fn analyze_in_out(&mut self, bb_id: AsmValueId) -> bool {
        let block: &AsmBlock = self.module.get_bb(bb_id);
        let mut live_info = self.live_info_of(bb_id).borrow_mut();
        let mut live_out_before = HashSet::new();
        live_out_before.extend(&live_info.live_out);
        for succ_id in &block.succs {
            live_info
                .live_out
                .extend(&self.live_info_of(*succ_id).borrow_mut().live_in);
        }
        live_info.live_in.clear();
        live_info.live_in.extend(&live_info.live_out);
        for live_def in &live_info.live_def {
            live_info.live_in.remove(live_def);
        }
        live_info.live_in.extend(&live_info.live_use);
        let stable = live_out_before == live_info.live_out;
        trace!(
            "block {} liveIn: {:?} liveOut: {:?} stable: {}",
            block.name,
            live_info.live_in,
            live_info.live_out,
            stable
        );
        stable
    }
}
