use std::{cell::Cell, fmt::Display, sync::Mutex};

#[derive(Debug, Clone)]
pub struct TransUnit {
    pub funcs: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct NodeMeta {
    id: usize,
    ty: Ty,
    map: Option<std::collections::HashMap<String, String>>,
}
static mut NEXT_NODE_ID: Mutex<Cell<usize>> = Mutex::new(Cell::new(0));

impl Default for NodeMeta {
    fn default() -> Self {
        let new_id = unsafe {
            let next_id = NEXT_NODE_ID.lock().unwrap();
            let id = next_id.get();
            next_id.set(id + 1);
            id
        };
        Self {
            id: new_id,
            ty: Ty::int(64),
            map: None,
        }
    }
}

impl NodeMeta {
    pub fn get_id(&self) -> usize {
        self.id
    }

    pub fn get_ty(&self) -> Ty {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Ty) {
        self.ty = ty;
    }

    pub fn get_attr(&self, key: &str) -> Option<&String> {
        if self.map.is_none() {
            // lazy init
            None
        } else {
            match self.map.as_ref().unwrap().get(key) {
                Some(v) => Some(v),
                None => None,
            }
        }
    }

    pub fn set_attr(&mut self, key: &str, value: &str) {
        if self.map.is_none() {
            // lazy init
            self.map = Some(std::collections::HashMap::new());
        }
        self.map
            .as_mut()
            .unwrap()
            .insert(key.to_string(), value.to_string());
    }
}

#[derive(Debug, Clone)]
pub enum InfixOp {
    // infix_bitwise
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
    // infix_logic
    LogicAnd,
    LogicOr,
    // infix_arith
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // infix_cmp
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::BitAnd => write!(f, "&"),
            InfixOp::BitOr => write!(f, "|"),
            InfixOp::BitXor => write!(f, "^"),
            InfixOp::BitShl => write!(f, "<<"),
            InfixOp::BitShr => write!(f, ">>"),
            InfixOp::LogicAnd => write!(f, "&&"),
            InfixOp::LogicOr => write!(f, "||"),
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Mod => write!(f, "%"),
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Lte => write!(f, "<="),
            InfixOp::Gte => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrefixOp {
    Pos,
    Neg,
    Not,
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Pos => write!(f, "+"),
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PostfixOp {
    CallAccess(Box<CallAccess>),
    ArrayAccess(Box<ArrayAccess>),
    DotAccess(Box<DotAccess>),
}

impl Display for PostfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PostfixOp::CallAccess(expr) => write!(f, "{}", expr),
            PostfixOp::ArrayAccess(expr) => write!(f, "{}", expr),
            PostfixOp::DotAccess(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallAccess {
    pub meta: Box<NodeMeta>,

    pub callee: String,
    pub args: Vec<Expr>,
}

impl Display for CallAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        write!(f, "{}", self.callee).expect(FMT_ERR);
        for arg in &self.args {
            write!(f, ", ").expect(FMT_ERR);
            write!(f, "{}", arg).expect(FMT_ERR);
        }
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub meta: Box<NodeMeta>,

    pub index: Box<Expr>,
}

impl Display for ArrayAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[").expect(FMT_ERR);
        write!(f, "{}", self.index).expect(FMT_ERR);
        write!(f, "]").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DotAccess {
    pub meta: Box<NodeMeta>,

    pub field: String,
}

impl Display for DotAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".").expect(FMT_ERR);
        write!(f, "{}", self.field).expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub meta: Box<NodeMeta>,

    pub name: String,
    pub params: Vec<Param>,
    pub ret_ty: Ty,
    pub body: Block,
}
#[derive(Debug, Clone)]
pub struct Param {
    pub meta: Box<NodeMeta>,

    pub name: String,
    pub ty: Ty,
}

impl Param {
    pub fn new(name: String, ty: Ty) -> Self {
        Self {
            meta: Box::<NodeMeta>::default(),
            name,
            ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprStmt),
    Return(ReturnStmt),
}
#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub meta: Box<NodeMeta>,

    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub meta: Box<NodeMeta>,

    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Infix(InfixExpr),
    Prefix(PrefixExpr),
    Postfix(PostfixExpr),
    Primary(PrimaryExpr),
}

static FMT_ERR: &str = "Failed to format expression";

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);

        match self {
            Expr::Infix(expr) => write!(f, "{} {} {}", expr.lhs, expr.op, expr.rhs),
            Expr::Prefix(expr) => write!(f, "{}{}", expr.op, expr.rhs),
            Expr::Postfix(expr) => write!(f, "{}{}", expr.lhs, expr.op),
            Expr::Primary(expr) => write!(f, "{}", expr),
        }
        .expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryExpr {
    Grouping(Box<Expr>),
    Call(CallExpr),
    Ident(IdentExpr),
    Literal(LiteralExpr),
}

impl Display for PrimaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimaryExpr::Grouping(expr) => write!(f, "{}", expr),
            PrimaryExpr::Call(expr) => write!(f, "{}", expr),
            PrimaryExpr::Ident(expr) => write!(f, "{}", expr),
            PrimaryExpr::Literal(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub meta: Box<NodeMeta>,
    pub lhs: Box<Expr>,
    pub op: InfixOp,
    pub rhs: Box<Expr>,
}

impl Display for InfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        write!(f, "{}", self.lhs).expect(FMT_ERR);
        write!(f, "{}", self.op).expect(FMT_ERR);
        write!(f, "{}", self.rhs).expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub meta: Box<NodeMeta>,

    pub op: PrefixOp,
    pub rhs: Box<Expr>,
}

impl Display for PrefixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        write!(f, "{}", self.op).expect(FMT_ERR);
        write!(f, "{}", self.rhs).expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct PostfixExpr {
    pub meta: Box<NodeMeta>,

    pub lhs: Box<Expr>,
    pub op: PostfixOp,
}

impl Display for PostfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        write!(f, "{}", self.lhs).expect(FMT_ERR);
        write!(f, "{}", self.op).expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub meta: Box<NodeMeta>,

    pub callee: String,
    pub args: Vec<Expr>,
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        write!(f, "{}", self.callee).expect(FMT_ERR);
        write!(f, "(").expect(FMT_ERR);
        for arg in &self.args {
            write!(f, "{}", arg).expect(FMT_ERR);
        }
        write!(f, ")").expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IdentExpr {
    pub meta: Box<NodeMeta>,

    pub name: String,
}

impl Display for IdentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        write!(f, "{}", self.name).expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub meta: Box<NodeMeta>,

    pub value: Literal,
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(").expect(FMT_ERR);
        match &self.value {
            Literal::Int(value) => write!(f, "{}", value),
            Literal::Char(value) => write!(f, "{}", value),
            Literal::Float(value) => write!(f, "{}", value),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "{}", value),
        }
        .expect(FMT_ERR);
        write!(f, ")").expect(FMT_ERR);
        Ok(())
    }
}

impl LiteralExpr {
    pub fn of_int(value: i64) -> Self {
        Self {
            value: Literal::Int(value),
            meta: Box::<NodeMeta>::default(),
        }
    }
    pub fn of_float(value: f64) -> Self {
        Self {
            meta: Box::<NodeMeta>::default(),
            value: Literal::Float(value),
        }
    }
    pub fn of_bool(value: bool) -> Self {
        Self {
            meta: Box::<NodeMeta>::default(),
            value: Literal::Bool(value),
        }
    }
    pub fn of_string(value: String) -> Self {
        Self {
            meta: Box::<NodeMeta>::default(),
            value: Literal::String(value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Char(char),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(value) => write!(f, "{}", value),
            Literal::Char(value) => write!(f, "{}", value),
            Literal::Float(value) => write!(f, "{}", value),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Basic(BasicTy),
    Pointer(PointerTy),
    Array(ArrayTy),
    User(UserTy),
}

impl Default for Ty {
    fn default() -> Self {
        Self::Basic(BasicTy::default())
    }
}

impl Default for BasicTy {
    fn default() -> Self {
        Self::Int(IntTy::I32())
    }
}

impl Ty {
    pub fn int(size: u8) -> Self {
        // size must be one of 0, 8, 16, 32, 64, 128
        match size {
            0 => Self::Basic(BasicTy::Int(IntTy::Implicit())),
            8 => Self::Basic(BasicTy::Int(IntTy::I8())),
            16 => Self::Basic(BasicTy::Int(IntTy::I16())),
            32 => Self::Basic(BasicTy::Int(IntTy::I32())),
            64 => Self::Basic(BasicTy::Int(IntTy::I64())),
            128 => Self::Basic(BasicTy::Int(IntTy::I128())),
            _ => panic!("invalid size"),
        }
    }
    pub fn float(size: u8) -> Self {
        // size must be one of 0, 32, 64
        match size {
            0 => Self::Basic(BasicTy::Float(FloatTy::Implicit())),
            32 => Self::Basic(BasicTy::Float(FloatTy::F32())),
            64 => Self::Basic(BasicTy::Float(FloatTy::F64())),
            _ => panic!("invalid size"),
        }
    }
    pub fn bool() -> Self {
        Self::Basic(BasicTy::Bool())
    }
    pub fn char() -> Self {
        Self::Basic(BasicTy::Char())
    }
    pub fn str() -> Self {
        Self::Basic(BasicTy::Str())
    }
    pub fn pointer(ty: Ty) -> Self {
        Self::Pointer(PointerTy { ty: Box::new(ty) })
    }
}

#[derive(Debug, Clone)]
pub enum BasicTy {
    Int(IntTy),
    Float(FloatTy),
    Char(),
    Str(),
    Bool(),
}
#[derive(Debug, Clone)]
pub enum IntTy {
    Implicit(),
    I8(),
    I16(),
    I32(),
    I64(),
    I128(),
    ISize(),

    U8(),
    U16(),
    U32(),
    U64(),
    U128(),
    USize(),
}
#[derive(Debug, Clone)]
pub enum FloatTy {
    Implicit(),
    F32(),
    F64(),
}
#[derive(Debug, Clone)]
pub struct PointerTy {
    pub ty: Box<Ty>,
}
#[derive(Debug, Clone)]
pub struct ArrayTy {
    pub ty: Box<Ty>,
    pub size: usize,
    pub _size_expr: Option<Box<Expr>>,
}
#[derive(Debug, Clone)]
pub struct UserTy {
    pub name: String,
}
#[derive(Debug, Clone)]
pub struct GenericTy {
    pub name: String,
    pub args: Vec<Ty>,
}
#[derive(Debug, Clone)]
pub struct VoidTy();
