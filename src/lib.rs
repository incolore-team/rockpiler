#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub mod cli;
pub mod driver;
pub mod parser;
pub mod ast;
pub mod scope;
pub mod symbol;
pub mod ir;
pub mod sema;
pub mod ir_builder;
