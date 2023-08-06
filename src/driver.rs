use log::{debug, trace};

use crate::{
    arm_printer,
    cli::Args,
    ir_builder,
    ir_pass::{inst_namer, mem2reg},
    ir_printer, mc_builder,
    scope::SymbolTable,
    sema::ToSemaTrait,
};

pub fn drive(args: Args) {
    assert!(!args.inputs.is_empty());
    let prelude = include_str!("prelude.c").to_string();
    for f_input in args.inputs {
        trace!("compiling {:?}", f_input);
        let mut src = std::fs::read_to_string(f_input).expect("unable to read file");
        src = format!("{}\n{}", prelude, src);
        trace!("================== SRC => AST ==================");
        let ast = crate::parser::parse(&src);
        trace!("ast: {:#?}", ast);
        if ast.as_ref().err().is_some() {
            panic!("unable to parse file");
        }
        trace!("================== AST => SEMA+AST ==================");
        let mut syms = SymbolTable::new();
        let mut ast = ast.unwrap();
        ast.to_sema(&mut syms);
        trace!("syms: \n{}", syms.print_table());
        trace!("ast: {:#?}", ast);
        trace!("================== SEMA+AST => Pre-SSA IR ==================");
        debug!(";{}", args.output.display());
        let mut module = ir_builder::build(&mut ast, syms);
        inst_namer::run(&mut module);
        trace!("================== Pre-SSA Module as LLVM IR ==================");
        debug!(";{}", args.output.display());
        ir_printer::print(&mut module);
        mem2reg::run(&mut module);
        inst_namer::run(&mut module);

        trace!("================== SSA Module as LLVM IR ==================");
        debug!(";{}", args.output.display());
        ir_printer::print(&mut module);
        trace!("================== Arm Assembly Module ==================");
        let mut arm_module = mc_builder::build(&mut module);
        arm_printer::print(&mut arm_module);
    }
}
