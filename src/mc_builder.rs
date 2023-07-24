use crate::{ir::*, mc::*};

pub fn build(module: &mut Module) -> AsmModule {
    let mut builder = McBuilder::new(module);
    builder.build_module();
    builder.module
}

struct McBuilder<'a> {
    ir_module: &'a Module,
    module: AsmModule,
}

impl Into<AsmGlobalVariable> for GlobalVariableValue {
    fn into(self) -> AsmGlobalVariable {
        todo!()
    }
}

impl Into<AsmFunction> for FunctionValue {
    fn into(self) -> AsmFunction {
        todo!()
    }
}

impl McBuilder<'_> {
    fn new<'a>(ir_module: &'a Module) -> McBuilder<'a> {
        McBuilder {
            ir_module,
            module: AsmModule::new(),
        }
    }

    fn build_module(&mut self) {
        self.build_global_variables();
        self.build_functions();
    }

    fn build_global_variables(&mut self) {
        for (name, id) in &self.ir_module.global_variables.clone() {
            let global = self.ir_module.get_global_var(*id);
            let val = AsmGlobalVariable::from(global.clone().into());
            let val_id = self.module.alloc_value(AsmValue::GlobalVariable(val));
            if global.initializer.is_some() {
                self.module.bss_globals.push(val_id);
            } else {
                self.module.globals.push(val_id);
            }
        }
    }

    fn build_functions(&mut self) {
        for (_, func_id) in &self.ir_module.functions {
            self.build_function(func_id);
        }
    }

    fn build_function(&mut self, func_id: &ValueId) {
        let func = self.ir_module.get_func(*func_id);
        let asm_func = AsmFunction::from(func.clone().into());
        let asm_func_id = self.module.alloc_value(AsmValue::Function(asm_func));
        self.module.functions.push(asm_func_id);

        // TODO: 对应生成AsmBlock并放到Map里。
        // TODO: prologue
        // TODO: handle callling convention
    }

    // fn build_basic_blocks(&mut self, blocks: &Vec<BasicBlock>) -> Vec<asmBasicBlock> {
    //     let mut asm_blocks = Vec::new();
    //     for block in blocks {
    //         asm_blocks.push(self.build_basic_block(block));
    //     }
    //     asm_blocks
    // }

    // fn build_basic_block(&mut self, block: &BasicBlock) -> asmBasicBlock {
    //     let mut asm_block = asmBasicBlock::new();
    //     asm_block.name = block.name.clone();
    //     asm_block.instructions = self.build_instructions(&block.instructions);
    //     asm_block
    // }

    // fn build_instructions(&mut self, instructions: &Vec<Instruction>) -> Vec<asmInstruction> {
    //     let mut asm_instructions = Vec::new();
    //     for instruction in instructions {
    //         asm_instructions.push(self.build_instruction(instruction));
    //     }
    //     asm_instructions
    // }

    // fn build_instruction(&mut self, instruction: &Instruction) -> asmInstruction {
    //     let mut asm_instruction = asmInstruction::new();
    //     asm_instruction.opcode = instruction.opcode.clone();
    //     asm_instruction.operands = self.build_operands(&instruction.operands);
    //     asm_instruction
    // }

    // fn build_operands(&mut self, operands: &Vec<Operand>) -> Vec<asmOperand> {
    //     let mut asm_operands = Vec::new();
    //     for operand in operands {
    //         asm_operands.push(self.build_operand(operand));
    //     }
    //     asm_operands
    // }

    // fn build_operand(&mut self, operand: &Operand) -> asmOperand {
    //     todo!()
    // }
}
