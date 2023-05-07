use crate::ast::{FuncDecl, VarDecl, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeSymbol {
    type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncSymbol {
    func: FuncDecl,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarSymbol {
    var: VarDecl,
}

impl TypeSymbol {
    pub fn new(type_: Type) -> Self {
        Self {
            type_
        }
    }
}

impl FuncSymbol {
    pub fn new(func: FuncDecl) -> Self {
        Self { func }
    }
}

impl VarSymbol {
    pub fn new(var: VarDecl) -> Self {
        Self { var }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    Dummy,
    Type(TypeSymbol),
    Func(FuncSymbol),
    Var(VarSymbol),
}
