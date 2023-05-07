
#[derive(Clone,PartialEq)]
pub struct TypeSymbol {
    name: String,
}

#[derive(Clone,PartialEq)]
pub struct FuncSymbol {
    name: String,
}

#[derive(Clone,PartialEq)]
pub struct VarSymbol {
    name: String,
}

impl TypeSymbol {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl FuncSymbol {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl VarSymbol {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}
#[derive(Clone)]
pub enum Symbol {
    Dummy,
    Type(TypeSymbol),
    Func(FuncSymbol),
    Var(VarSymbol),
}

