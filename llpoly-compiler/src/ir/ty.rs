use crate::ast;

use super::value::ValueId;


#[derive(Debug, Default, Clone)]
pub enum Ty {
    #[default]
    Void,
    Int(IntTy),
    Float(FloatTy),
    Ptr(PtrTy),
    Array(ArrayTy),
    Func(FuncTy),
}

impl From<&ast::Ty> for Ty {
    fn from(ty: &ast::Ty) -> Ty {
        match ty {
            ast::Ty::Basic(ast_basic_ty) => match ast_basic_ty {
                ast::BasicTy::Int(i) => match &i {
                    ast::IntTy::Implicit() => todo!(),
                    ast::IntTy::I8() => Ty::int(8),
                    ast::IntTy::I16() => Ty::int(16),
                    ast::IntTy::I32() => Ty::int(32),
                    ast::IntTy::I64() => Ty::int(64),
                    ast::IntTy::I128() => Ty::int(128),
                    ast::IntTy::ISize() => Ty::int(64),
                    ast::IntTy::U8() => Ty::int(8),
                    ast::IntTy::U16() => Ty::int(16),
                    ast::IntTy::U32() => Ty::int(32),
                    ast::IntTy::U64() => Ty::int(64),
                    ast::IntTy::U128() => Ty::int(128),
                    ast::IntTy::USize() => Ty::int(64),
                },
                ast::BasicTy::Float(f) => match &f {
                    ast::FloatTy::Implicit() => todo!(),
                    ast::FloatTy::F32() => Ty::float(32),
                    ast::FloatTy::F64() => Ty::float(64),
                },
                ast::BasicTy::Char() => Ty::uint(8),
                ast::BasicTy::Str() => Ty::ptr(),
                ast::BasicTy::Bool() => Ty::int(8),
            },
            ast::Ty::Pointer(_ast_pointer_ty) => todo!(),
            ast::Ty::Array(_ast_array_ty) => todo!(),
            ast::Ty::User(ast_user_ty) => match ast_user_ty.name.as_str() {
                "buf" => Ty::ptr(),
                _ => todo!(),
            },
        }
    }
}

impl Ty {
    pub fn int(bit: usize) -> Ty {
        Ty::Int(IntTy {
            signed: true,
            bit_width: bit,
        })
    }

    pub fn uint(bit: usize) -> Ty {
        Ty::Int(IntTy {
            signed: false,
            bit_width: bit,
        })
    }

    pub fn float(bit: usize) -> Ty {
        Ty::Float(FloatTy { bit_width: bit })
    }

    pub fn ptr() -> Ty {
        Ty::Ptr(PtrTy {})
    }

    pub fn array(base: Ty, size: ValueId) -> Ty {
        Ty::Array(ArrayTy {
            base: Box::new(base),
            size,
        })
    }

    pub fn func(ret_ty: Ty, param_tys: Vec<Ty>) -> Ty {
        Ty::Func(FuncTy {
            ret_ty: Box::new(ret_ty),
            param_tys,
        })
    }
}

#[derive(Debug, Default, Clone)]
pub struct IntTy {
    pub signed: bool,
    pub bit_width: usize,
}
#[derive(Debug, Default, Clone)]
pub struct FloatTy {
    pub bit_width: usize,
}
#[derive(Debug, Default, Clone)]
pub struct PtrTy {}
#[derive(Debug, Default, Clone)]
pub struct ArrayTy {
    pub base: Box<Ty>,
    pub size: ValueId,
}
#[derive(Debug, Default, Clone)]
pub struct FuncTy {
    pub ret_ty: Box<Ty>,
    pub param_tys: Vec<Ty>,
}