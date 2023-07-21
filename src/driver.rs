use log::trace;

use crate::{
    cli::Args,
    ir_printer,
    ir_pass::{inst_namer, mem2reg},
    scope::SymbolTable,
    sema::ToSemaTrait,
};

pub fn drive(args: Args) {
    assert!(args.inputs.len() > 0);
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
        let mut module = crate::ir_builder::build(&mut ast, syms);
        inst_namer::run(&mut module);
        trace!("================== Pre-SSA Module as LLVM IR ==================");
        ir_printer::print(&mut module);
        mem2reg::run(&mut module);
        inst_namer::run(&mut module);

        trace!("================== SSA Module as LLVM IR ==================");
        ir_printer::print(&mut module);
    }
}
