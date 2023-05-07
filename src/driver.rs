use log::{trace};

use crate::cli::Args;

pub fn drive(args: Args) {
    assert!(args.inputs.len() > 0);
    for f_input in args.inputs {
        trace!("compiling {:?}", f_input);
        let src = std::fs::read_to_string(f_input).expect("unable to read file");
        let ast = crate::parser::parse(&src);
        trace!("ast: {:?}", ast);
        if ast.err().is_some() {
            panic!("unable to parse file");
        }
    }
}