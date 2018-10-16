
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Literal(Literal),
    Column(ColumnName),
    ScalarFunc,
    AggFunc,
    Case,
}

pub enum UnaryOp {
    Plus,
    Minus,
    Tilde,
    Not,
}

pub enum BinaryOp {
    And,
    Or,
    Xor,

    Like,
    Regex,

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

pub enum Literal {
    Boolean(bool),
    Date(),
    String(String),
    Number(Number),
    Null,
}

pub enum Number {
    Integer(i64),
    Float(f64),
}

pub struct Limit {
    pub limit: Expr,
    pub offset: Option<Expr>,
}

pub enum Statement {
    Select(Select),
}

pub struct Select {
    pub projects: Projects,
    pub limit: Option<Limit>,
    pub table: Option<TableExpr>,
}

pub enum TableExpr {
    List(Vec<Table>),
    Table(Table),
}

pub enum Table {
    Name(TableName),
    Derived(DerivedTable),
    Join(Join),
}

pub struct Join {
    pub left: Box<Table>,
    pub right: Box<Table>,
    pub predicate: JoinPredicate,
}

pub enum JoinPredicate {
    On(Expr),
    Using(Vec<ColumnName>),
}

pub struct ColumnName {
    pub name: String,
    pub db: Option<String>,
    pub qualifier: Option<String>,
}

pub struct TableName {
    pub name: String,
    pub qualifier: String,
    pub alias: Option<String>,
}

pub struct DerivedTable {
    pub subquery: Box<Select>,
    pub alias: String,
}

pub enum Projects {
    Star,
    NonStar(Vec<AliasedColumn>),
}

pub struct AliasedColumn {
    pub expr: Expr,
    pub alias: Option<String>,
}
