mod visitor;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Literal(Literal),
    Column(ColumnName),
    ScalarFunc(String, Vec<Expr>),
    AggFunc(String, Vec<Expr>),
    Case,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Tilde,
    Not,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinaryOp {
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    And,
    Or,
    Xor,

    Add,
    Sub,
    Times,
    Div,
    IDiv,
    Mod,

    Like,
    Regexp,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Ne,
    Nse,

    In,
    Nin,
    Is,
    IsNot,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Date(),
    String(String),
    Number(Number),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub struct Limit {
    pub count: Expr,
    pub offset: Option<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Select(Select),
}

#[derive(Debug, PartialEq)]
pub struct Select {
    pub projects: Projects,
    pub table: Option<TableExpr>,
    pub where_clause: Option<Expr>,
    pub limit: Option<Limit>,
    pub orderby: Option<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum TableExpr {
    List(Vec<Table>),
    Table(Table),
}

#[derive(Debug, PartialEq)]
pub enum Table {
    Name(TableName),
    Derived(DerivedTable),
    Join(Join),
}

#[derive(Debug, PartialEq)]
pub struct Join {
    pub kind: JoinKind,
    pub left: Box<Table>,
    pub right: Box<Table>,
    pub predicate: Option<JoinPredicate>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum JoinKind {
    Left,
    Right,
    Inner,
    Outer,
}

#[derive(Debug, PartialEq)]
pub enum JoinPredicate {
    On(Expr),
    Using(Vec<ColumnName>),
}

#[derive(Debug, PartialEq)]
pub struct ColumnName {
    pub name: String,
    pub db: Option<String>,
    pub qualifier: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct TableName {
    pub name: String,
    pub qualifier: Option<String>,
    pub alias: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct DerivedTable {
    pub subquery: Box<Select>,
    pub alias: String,
}

#[derive(Debug, PartialEq)]
pub enum Projects {
    Star,
    NonStar(Vec<AliasedColumn>),
}

#[derive(Debug, PartialEq)]
pub struct AliasedColumn {
    pub expr: Expr,
    pub alias: Option<String>,
}
