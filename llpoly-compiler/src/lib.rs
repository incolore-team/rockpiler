#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub mod ast;
pub mod cli_args;
pub mod driver;
pub mod emit_llvm;
pub mod ir;
pub mod parser;
