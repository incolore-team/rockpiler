pub mod scope;
pub mod sema;
pub mod ty;
pub mod value;
pub mod module;
pub mod ssa;


use self::{
    ty::Ty,
};
#[macro_export]
macro_rules! value_cast {
    ($match:expr, $pat:pat => $result:expr) => {
        match $match.proto {
            $pat => $result,
            _ => unreachable!(),
        }
    };
}

const BUILTIN_PREFIX: &str = "_llp_builtin__";

