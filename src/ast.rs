pub enum Statement {
    SelectStatement,
    Set(Set),
    Use(Use),
    DropTable(DropTable),
}

pub struct Use {
    pub db_name: String,
}

pub struct CTE {
    pub table_name: TableName,
    pub column_exprs: Vec<ColName>,
    pub query: SelectStatement,
}

pub struct With {
    pub ctes: Vec<CTE>,
    pub recursive: bool,
}

pub enum SelectStatement {
    SimpleSelect(SimpleSelect),
    Select(Select),
    Union(Union),
}

pub struct Select {
    pub with: With,
    pub comments: Vec<String>,
    pub query_globals: QueryGlobals,
    pub select_exprs: Vec<SelectExpr>,
    pub wher: Where,
    pub group_by: GroupBy,
    pub having: Where,
    pub order_by: OrderBy,
    pub limit: Limit,
    pub lock: String, // TODO what is this?
}

pub struct QueryGlobals {
    pub distinct: bool,
    pub straight_join: bool,
}

pub struct Union {
    pub with: With,
    pub typ: String,
    pub left: Box<SelectStatement>,
    pub right: Box<SelectStatement>,
}

pub enum UnionType {
    Union,
    UnionAll,
    Minus,
    Except,
    Intersect,
}

pub struct Set {
    pub scope: String,
    pub comments: Vec<String>,
    pub exprs: Vec<UpdateExpr>,
}

pub struct DropTable {
    pub name: TableName,
    pub exists: bool,
    pub temporary: bool,
    pub opt: Option<DropTableModifier>,
}

pub enum DropTableModifier {
    Restrict,
    Cascade,
}

pub enum SelectExpr {
    StarExpr(StarExpr),
    NonStarExpr(NonStarExpr),
}

pub struct StarExpr {
    pub database_name: String,
    pub table_name: String,
}

pub struct NonStarExpr {
    pub expr: Expr,
    pub alias: String,
}

pub enum TableExpr {
    Aliased(AliasedTableExpr),
    Paren(ParenTableExpr),
    Join(JoinTableExpr),
}

pub struct AliasedTableExpr {
    pub expr: SimpleTableExpr,
    pub alias: String,
    pub hints: IndexHints,
}

pub enum SimpleTableExpr {
    TableName(TableName),
    Subquery(Subquery),
}

pub struct TableName {
    pub name: String,
    pub qualifier: String,
}

pub struct ParenTableExpr {
    pub expr: Box<TableExpr>,
}

pub struct JoinTableExpr {
    pub left: Box<TableExpr>,
    pub right: Box<TableExpr>,
    pub join: JoinKind,
    pub on: Expr,
    pub using: Vec<ColName>,
}

pub enum JoinKind {
    Join,
    Straight,
    Left,
    Right,
    Cross,
    Natural,
    NaturalLeft,
    NaturalRight,
}

pub struct IndexHints {
    pub typ: String,
    pub indexes: Vec<String>,
}

// TODO what is this?
// const (
// AST_USE    = "use"
// AST_IGNORE = "ignore"
// AST_FORCE  = "force"
// )

pub struct Where {
    pub typ: WhereType,
    pub expr: Expr,
}

pub enum WhereType {
    Where,
    Having,
}

pub enum Expr {
    And,
    Or,
    Xor,
    Not,
    ComparisonExpr,
    Like,
    Regex,
    RLike,
    RangeCond,
    Exists,
    DateVal,
    StrVal,
    NumVal,
    ValArg,
    KeywordVal,
    NullVal,
    ColName,
    TrueVal,
    FalseVal,
    UnknownVal,
    ValTuple,
    Subquery,
    BinaryExpr,
    UnaryExpr,
    FuncExpr,
    CaseExpr,
}

pub struct AndExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

pub struct OrExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

pub struct XorExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

pub struct NotExpr {
    pub expr: Box<Expr>,
}

pub struct ComparisonExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub comparison_op: ComparisonOp,
    pub subquery_op: SubqueryOp,
}

pub enum ComparisonOp {
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
    Not,
}

pub enum SubqueryOp {
    All,
    Any,
    Sum,
}

pub enum LikeOp {
    Like,
    LikeBinary,
    NotLike,
    NotLikeBinary,
}

pub struct LikeExpr {
    pub op: LikeOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub escape: Box<Expr>,
}

pub struct RangeCond {
    pub left: Box<Expr>,
    pub from: Box<Expr>,
    pub to: Box<Expr>,
    pub op: RangeCondOp,
}

pub enum RangeCondOp {
    Between,
    NotBetween,
}

pub struct Regex {
    pub operand: Box<Expr>,
    pub pattern: Box<Expr>,
}

type RLike = Regex;

pub struct Exists {
    pub subquery: Subquery,
}

pub struct DateVal {
    pub name: DateType,
    pub val: String,
}

pub enum DateType {
    Date,
    Time,
    Timestamp,
    Datetime,
}

type StrVal = String;
type NumVal = String;
type ValArg = String;
type KeywordVal = String;
struct NullVal;
struct UnknownVal;
struct TrueVal;
struct FalseVal;

pub struct ColName {
    pub database: String,
    pub name: String,
    pub qualifier: String,
}

pub enum Tuple {
    ValTuple(ValTuple),
    Subquery(Subquery),
}

type ValTuple = Vec<Expr>;

pub struct Subquery {
    pub select: SelectStatement,
    pub is_derived: bool,
}

pub struct BinaryExpr {
    pub operator: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

pub enum BinaryOp {
    BitAnd,
    BitOr,
    BitXor,
    Plus,
    Minus,
    Mult,
    Div,
    Idiv,
    Mod,
}

pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub operator: UnaryOp,
}

pub enum UnaryOp {
    Plus,
    Minus,
    Tilde,
}

pub struct Func {
    pub name: String,
    pub distinct: bool,
    pub exprs: Vec<SelectExpr>,
    pub order_by: OrderBy,
    pub separator: String,
}

pub struct Case {
    pub expr: Box<Expr>,
    pub whens: Vec<When>,
    pub else_: Box<Expr>,
}

pub struct When {
    pub cond: Expr,
    pub val: Expr,
}

type GroupBy = Vec<Expr>;
type OrderBy = Vec<Order>;

pub struct Order {
    pub expr: Expr,
    pub direction: OrderDirection,
}

pub enum OrderDirection {
    Asc,
    Desc,
}

pub struct Limit {
    pub offset: Expr,
    pub row_count: Expr,
}

pub struct UpdateExpr {
    pub name: ColName,
    pub expr: Expr,
}

pub struct SimpleSelect {
    pub comments: Vec<String>,
    pub query_globals: QueryGlobals,
    pub select_exprs: Vec<SelectExpr>,
    pub limit: Limit,
}

pub enum ShowModifier {
    None,
    Full,
    Session,
    Global,
}

pub struct Show {
    pub section: String, // TODO probably an enum
    pub key: String,
    pub from: Expr,
    pub like_or_where: Expr,
    pub modifier: ShowModifier,
}

pub enum ExplainType {
    Extended,
    Json,
    Traditional,
    Partitions,
}

pub struct Explain {
    pub section: String, // TODO this is probably an enum
    pub table: TableName,
    pub column: ColName,
    pub explain_type: ExplainType,
    pub connection: String,
    pub statement: Box<Statement>,
}

pub enum KillScope {
    Connection,
    Query,
}

pub struct Kill {
    pub scope: KillScope,
    pub id: Expr,
}

pub enum FlushKind {
    Logs,
    Sample,
}

pub struct Flush {
    pub kind: FlushKind,
}

pub struct AlterTable {
    table: TableName,
    specs: Vec<AlterSpec>,
}

pub enum AlterationType {
    RenameColumn,
    DropColumn,
    ModifyColumn,
    RenameTable,
}

pub struct AlterSpec {
    pub type_: AlterationType,
    pub column: ColName,
    pub new_column: ColName,
    pub new_table: TableName,
    pub new_column_type: String,
}

pub struct RenameTable {
    pub renames: Vec<RenameSpec>,
}

pub struct RenameSpec {
    pub table: TableName,
    pub new_table: TableName,
}
