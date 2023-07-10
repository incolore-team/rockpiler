use crate::sema::SemaRef;

#[derive(Debug, PartialEq, Clone)]
pub struct TransUnit {
    pub func_decls: Vec<FuncDecl>,

    pub var_decls: Vec<VarDecl>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOp {
    Incr,
    Decr,
    Not,
    BitNot,
    Pos,
    Neg,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOp {
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
    LogicAnd,
    LogicOr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Assign,
}
#[derive(Debug, PartialEq, Clone)]
pub enum PostfixOp {
    Incr,
    Decr,
    CallAccess(CallAccess),
    DotAccess(DotAccess),
    IndexAccess(IndexAccess),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallAccess {
    pub id: String,
    pub args: Vec<Box<Expr>>,

    pub sema_ref: Option<SemaRef>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexAccess {
    pub index: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DotAccess {
    pub field: String,

    pub sema_ref: Option<SemaRef>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub ret_ty: Type,
    pub body: Block,

    pub sema_ref: Option<SemaRef>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDecl {
    pub name: String,
    pub type_: Type,
    pub is_const: bool,
    pub init: Option<InitVal>,

    pub sema_ref: Option<SemaRef>,
}

impl From<Param> for VarDecl {
    fn from(param: Param) -> Self {
        Self {
            name: param.name,
            type_: param.type_,
            is_const: false,
            init: None,
            sema_ref: param.sema_ref,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Param {
    pub name: String,
    pub type_: Type,

    pub sema_ref: Option<SemaRef>,
}

impl Param {
    pub fn new(name: String, type_: Type) -> Self {
        Self {
            name,
            type_,
            sema_ref: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub stmts: Vec<Box<Stmt>>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    VarDecls(VarDecls),
    Expr(ExprStmt),
    Block(Block),
    IfElse(IfElseStmt),
    While(WhileStmt),
    For(ForStmt),
    Break,
    DoWhile(DoWhileStmt),
    Continue,
    Return(ReturnStmt),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStmt {
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDecls {
    pub decls: Vec<VarDecl>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct ExprStmt {
    pub expr: Option<Box<Expr>>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct IfElseStmt {
    pub cond: Box<Expr>,
    pub if_stmt: Box<Stmt>,
    pub else_if_conds: Vec<Box<Expr>>,
    pub else_if_stmts: Vec<Box<Stmt>>,
    pub else_stmt: Option<Box<Stmt>>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
    pub cond: Box<Expr>,
    pub body: Box<Stmt>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct ForStmt {
    pub init: Option<Box<Expr>>,
    pub cond: Option<Box<Expr>>,
    pub update: Option<Box<Expr>>,
    pub body: Box<Stmt>,
}
#[derive(Debug, PartialEq, Clone)]

pub struct DoWhileStmt {
    pub stmt: Box<Stmt>,
    pub cond: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InitVal {
    Expr(Box<Expr>),
    Array(ArrayInitVal),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayInitVal(pub Vec<InitVal>);

// ===================== Expr =====================

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Infix(InfixExpr),
    Prefix(PrefixExpr),
    Postfix(PostfixExpr),
    Primary(PrimaryExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimaryExpr {
    Group(Box<Expr>),
    Call(CallExpr),
    Ident(IdentExpr),
    Literal(Literal),
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpr {
    pub lhs: Box<Expr>,
    pub op: InfixOp,
    pub rhs: Box<Expr>,

    pub infer_ty: Option<Type>,
    pub infer_val: Option<Literal>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpr {
    pub op: PrefixOp,
    pub rhs: Box<Expr>,

    pub infer_ty: Option<Type>,
    pub infer_val: Option<Literal>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PostfixExpr {
    pub lhs: Box<Expr>,
    pub op: PostfixOp,

    pub infer_ty: Option<Type>,
    pub infer_val: Option<Literal>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub id: String,
    pub args: Vec<Box<Expr>>,

    pub sema_ref: Option<SemaRef>,

    pub infer_ty: Option<Type>,
    pub infer_val: Option<Literal>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentExpr {
    pub id: String,

    pub sema_ref: Option<SemaRef>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Char(char),
    Float(f64),
    Bool(bool),
    String(String),
}

impl From<Literal> for usize {
    fn from(literal: Literal) -> Self {
        if let Literal::Int(value) = literal {
            value as usize
        } else {
            panic!("invalid array size");
        }
    }
}

// ===================== End Expr =====================

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Builtin(BuiltinType),
    Pointer(PointerType),
    Array(ArrayType),
    Record(RecordType),
    Function(FunctionType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinType {
    Void,
    Bool,
    UChar,
    Char,
    UShort,
    Short,
    UInt,
    Int,
    UInt64,
    Int64,
    Float,
    Double,
}

impl Into<Type> for BuiltinType {
    fn into(self) -> Type {
        Type::Builtin(self)
    }
}

impl Into<Type> for PointerType {
    fn into(self) -> Type {
        Type::Pointer(self)
    }
}

impl Into<Type> for ArrayType {
    fn into(self) -> Type {
        Type::Array(self)
    }
}

impl Into<Type> for RecordType {
    fn into(self) -> Type {
        Type::Record(self)
    }
}

impl Into<Type> for FunctionType {
    fn into(self) -> Type {
        Type::Function(self)
    }
}

impl Type {
    pub fn is_arithmetic(&self) -> bool {
        match self {
            Type::Builtin(builtin) => builtin.is_arithmetic(),
            Type::Pointer(_) | Type::Array(_) | Type::Record(_) | Type::Function(_) => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Builtin(builtin) => builtin.is_integer(),
            Type::Pointer(_) | Type::Array(_) | Type::Record(_) | Type::Function(_) => false,
        }
    }
}

impl BuiltinType {
    pub fn is_arithmetic(&self) -> bool {
        !matches!(
            self,
            BuiltinType::Void | BuiltinType::Bool | BuiltinType::Float | BuiltinType::Double
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            BuiltinType::Bool
                | BuiltinType::UChar
                | BuiltinType::Char
                | BuiltinType::UShort
                | BuiltinType::Short
                | BuiltinType::UInt
                | BuiltinType::Int
                | BuiltinType::UInt64
                | BuiltinType::Int64
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PointerType {
    pub type_: Box<Type>,
}

impl PointerType {
    pub fn new(type_: Type) -> Self {
        Self {
            type_: Box::new(type_),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArrayType {
    Constant(ConstantArrayType),
    Incomplete(IncompleteArrayType),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstantArrayType {
    pub element_type: Box<Type>,
    pub size: usize,
    pub size_info: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IncompleteArrayType {
    pub element_type: Box<Type>,
    pub size_info: Option<i64>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RecordType {
    pub tag_type: TagType,
    pub record_decl: RecordDecl,
}
#[derive(Debug, PartialEq, Clone)]
pub struct TagType {
    pub tag_decl: TagDecl,
    pub keyword: Keyword,
}

// TagDecl结构体，表示记录类型的声明
#[derive(Debug, PartialEq, Clone)]
pub struct TagDecl {
    pub name: String,
    pub keyword: Keyword,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Struct,
    Union,
    Class,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RecordDecl {
    pub tag_type: TagType,
    pub name: String,
    pub fields: Vec<FieldDecl>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldDecl {
    pub name: String,
    pub field_type: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub return_type: Box<Type>,
    pub param_types: Vec<Box<Type>>,
    pub param_count: usize,
    pub is_variadic: bool,
}
impl Type {
    // ...

    pub fn can_assign_from(&self, other_type: &Type) -> bool {
        match (self, other_type) {
            // Builtin types can be assigned from compatible Builtin types
            (Type::Builtin(builtin_self), Type::Builtin(builtin_other)) => {
                builtin_self == builtin_other
            }
            // Pointer types can be assigned from compatible Pointer types
            (Type::Pointer(pointer_self), Type::Pointer(pointer_other)) => {
                pointer_self.type_.can_assign_from(&*pointer_other.type_)
            }
            // Array types can be assigned from compatible Array types
            (Type::Array(array_self), Type::Array(array_other)) => {
                match (array_self, array_other) {
                    (ArrayType::Constant(const_self), ArrayType::Constant(const_other)) => {
                        const_self
                            .element_type
                            .can_assign_from(&*const_other.element_type)
                            && const_self.size == const_other.size
                    }
                    (ArrayType::Incomplete(inc_self), ArrayType::Incomplete(inc_other)) => inc_self
                        .element_type
                        .can_assign_from(&*inc_other.element_type),
                    _ => false,
                }
            }
            // Record types can be assigned from compatible Record types
            (Type::Record(record_self), Type::Record(record_other)) => {
                record_self.tag_type == record_other.tag_type
            }
            // Function types can be assigned from compatible Function types
            (Type::Function(func_self), Type::Function(func_other)) => {
                func_self
                    .return_type
                    .can_assign_from(&*func_other.return_type)
                    && func_self.param_count == func_other.param_count
                    && func_self
                        .param_types
                        .iter()
                        .zip(func_other.param_types.iter())
                        .all(|(param_self, param_other)| param_self.can_assign_from(&**param_other))
                    && func_self.is_variadic == func_other.is_variadic
            }
            // All other combinations are not assignable
            _ => false,
        }
    }
}
