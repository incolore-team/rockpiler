use log::{trace};

use crate::{cli::Args, scope::SymbolTable, sema::ToSemaTrait};

pub fn drive(args: Args) {
    assert!(args.inputs.len() > 0);
    for f_input in args.inputs {
        trace!("compiling {:?}", f_input);
        let src = std::fs::read_to_string(f_input).expect("unable to read file");
        trace!("================== SRC => AST ==================");
        let ast = crate::parser::parse(&src);
        trace!("ast: {:?}", ast);
        if ast.as_ref().err().is_some() {
            panic!("unable to parse file");
        }
        trace!("================== AST => SEMA+AST ==================");
        let mut syms = SymbolTable::new();
        let mut ast = ast.unwrap();
        ast.to_sema(&mut syms);
        trace!("syms: {}", syms.print_table());
        trace!("ast: {:?}", ast);
        trace!("================== SEMA+AST => Pre-SSA IR ==================");
        let module = crate::ir_builder::build(&mut ast, &mut syms);

    }
}
