use cst;

#[derive(Serialize)]
pub enum Statement {
    Select(Select),
    SimpleSelect(SimpleSelect),
    Union(Union),
    Set(Set),
    Use(Use),
    DropTable(DropTable),
}

impl Statement {
    fn from_cst(stmt: &cst::Statement) -> Statement {
        match stmt {
            cst::Statement::Select(sel) => {
                Statement::Select(SelectStatement::Select(Select::from_cst(sel)))
            }
            //_ => unimplemented!(),
        }
    }
}

#[derive(Serialize)]
pub struct Use {
    pub db_name: String,
}

#[derive(Serialize)]
pub struct CTE {
    pub table_name: TableName,
    pub column_exprs: Vec<ColName>,
    pub query: SelectStatement,
}

#[derive(Serialize)]
pub struct With {
    pub ctes: Vec<CTE>,
    pub recursive: bool,
}

#[derive(Serialize)]
pub enum SelectStatement {
    SimpleSelect(SimpleSelect),
    Select(Select),
    Union(Union),
}

#[derive(Serialize)]
pub struct Select {
    pub with: Option<With>,
    pub comments: Vec<String>,
    pub query_globals: QueryGlobals,
    pub select_exprs: Vec<SelectExpr>,
    pub where_: Option<Where>,
    pub group_by: Option<GroupBy>,
    pub having: Option<Where>,
    pub order_by: Option<OrderBy>,
    pub limit: Option<Limit>,
    pub lock: String, // TODO what is this?
}

impl Select {
    fn from_cst(sel: &cst::Select) -> Select {
        Select {
            with: None,           // TODO support in cst
            comments: Vec::new(), // TODO cst
            query_globals: QueryGlobals {
                distinct: false,      // TODO cst
                straight_join: false, // TODO cst
            },
            select_exprs: SelectExpr::from_cst(&sel.projects),
            where_: sel.where_clause.as_ref().map(|expr| Where {
                expr: Expr::from_cst(expr),
                typ: WhereType::Where,
            }),
            group_by: None, // TODO cst
            having: None,   // TODO cst
            order_by: None, // TODO
            limit: sel.limit.as_ref().map(Limit::from_cst),
            lock: "".to_string(), // TODO cst
        }
    }
}

#[derive(Serialize)]
pub struct QueryGlobals {
    pub distinct: bool,
    pub straight_join: bool,
}

#[derive(Serialize)]
pub struct Union {
    pub with: With,
    pub typ: String,
    pub left: Box<SelectStatement>,
    pub right: Box<SelectStatement>,
}

#[derive(Serialize)]
pub enum UnionType {
    Union,
    UnionAll,
    Minus,
    Except,
    Intersect,
}

#[derive(Serialize)]
pub struct Set {
    pub scope: String,
    pub comments: Vec<String>,
    pub exprs: Vec<UpdateExpr>,
}

#[derive(Serialize)]
pub struct DropTable {
    pub name: TableName,
    pub exists: bool,
    pub temporary: bool,
    pub opt: Option<DropTableModifier>,
}

#[derive(Serialize)]
pub enum DropTableModifier {
    Restrict,
    Cascade,
}

#[derive(Serialize)]
pub enum SelectExpr {
    StarExpr(StarExpr),
    NonStarExpr(NonStarExpr),
}

impl SelectExpr {
    // TODO this is all very bad. need to fix star in cst
    fn from_cst(s: &cst::Projects) -> Vec<SelectExpr> {
        use cst::Projects::{NonStar, Star};
        match s {
            Star => vec![SelectExpr::StarExpr(StarExpr::from_cst())],
            NonStar(cols) => cols
                .iter()
                .map(NonStarExpr::from_cst)
                .map(SelectExpr::NonStarExpr)
                .collect(),
        }
    }
}

#[derive(Serialize)]
pub struct StarExpr {
    pub database_name: String,
    pub table_name: String,
}

impl StarExpr {
    // TODO this should take real params
    fn from_cst() -> StarExpr {
        StarExpr {
            database_name: "".to_string(),
            table_name: "".to_string(),
        }
    }
}

#[derive(Serialize)]
pub struct NonStarExpr {
    pub expr: Expr,
    pub alias: Option<String>,
}

impl NonStarExpr {
    fn from_cst(col: &cst::AliasedColumn) -> NonStarExpr {
        NonStarExpr {
            expr: Expr::from_cst(&col.expr),
            alias: col.alias.clone(),
        }
    }
}

#[derive(Serialize)]
pub enum TableExpr {
    Aliased(AliasedTableExpr),
    Paren(ParenTableExpr),
    Join(JoinTableExpr),
}

#[derive(Serialize)]
pub struct AliasedTableExpr {
    pub expr: SimpleTableExpr,
    pub alias: String,
    pub hints: IndexHints,
}

#[derive(Serialize)]
pub enum SimpleTableExpr {
    TableName(TableName),
    Subquery(Subquery),
}

#[derive(Serialize)]
pub struct TableName {
    pub name: String,
    pub qualifier: String,
}

#[derive(Serialize)]
pub struct ParenTableExpr {
    pub expr: Box<TableExpr>,
}

#[derive(Serialize)]
pub struct JoinTableExpr {
    pub left: Box<TableExpr>,
    pub right: Box<TableExpr>,
    pub join: JoinKind,
    pub on: Expr,
    pub using: Vec<ColName>,
}

#[derive(Serialize)]
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

#[derive(Serialize)]
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

#[derive(Serialize)]
pub struct Where {
    pub typ: WhereType,
    pub expr: Expr,
}

#[derive(Serialize)]
pub enum WhereType {
    Where,
    Having,
}

#[derive(Serialize)]
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

impl Expr {
    fn from_cst(expr: &cst::Expr) -> Expr {
        use cst::{Expr as CExpr, UnaryOp as CUnaryOp};
        match expr {
            CExpr::Binary(..) => Expr::And,
            CExpr::Unary(CUnaryOp::Not, expr) => NotExpr {
                expr: Box::new(Expr::from_cst(expr)),
            },
            CExpr::Unary(op, expr) => UnaryExpr {
                expr: Box::new(Expr::from_cst(expr)),
                operator: match op {
                    CUnaryOp::Plus => UnaryOp::Plus,
                    CUnaryOp::Minus => UnaryOp::Minus,
                    CUnaryOp::Tilde => UnaryOp::Tilde,
                },
            },
            CExpr::Literal(_) => Expr::And,
            CExpr::Column(_) => Expr::And,
            CExpr::ScalarFunc(..) => Expr::And,
            CExpr::AggFunc(..) => Expr::And,
            CExpr::Case => Expr::And,
        }
    }
}

#[derive(Serialize)]
pub struct AndExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Serialize)]
pub struct OrExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Serialize)]
pub struct XorExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Serialize)]
pub struct NotExpr {
    pub expr: Box<Expr>,
}

#[derive(Serialize)]
pub struct ComparisonExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub comparison_op: ComparisonOp,
    pub subquery_op: SubqueryOp,
}

#[derive(Serialize)]
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

#[derive(Serialize)]
pub enum SubqueryOp {
    All,
    Any,
    Sum,
}

#[derive(Serialize)]
pub enum LikeOp {
    Like,
    LikeBinary,
    NotLike,
    NotLikeBinary,
}

#[derive(Serialize)]
pub struct LikeExpr {
    pub op: LikeOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub escape: Box<Expr>,
}

#[derive(Serialize)]
pub struct RangeCond {
    pub left: Box<Expr>,
    pub from: Box<Expr>,
    pub to: Box<Expr>,
    pub op: RangeCondOp,
}

#[derive(Serialize)]
pub enum RangeCondOp {
    Between,
    NotBetween,
}

#[derive(Serialize)]
pub struct Regex {
    pub operand: Box<Expr>,
    pub pattern: Box<Expr>,
}

type RLike = Regex;

#[derive(Serialize)]
pub struct Exists {
    pub subquery: Subquery,
}

#[derive(Serialize)]
pub struct DateVal {
    pub name: DateType,
    pub val: String,
}

#[derive(Serialize)]
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

#[derive(Serialize)]
pub struct ColName {
    pub database: String,
    pub name: String,
    pub qualifier: String,
}

#[derive(Serialize)]
pub enum Tuple {
    ValTuple(ValTuple),
    Subquery(Subquery),
}

type ValTuple = Vec<Expr>;

#[derive(Serialize)]
pub struct Subquery {
    pub select: SelectStatement,
    pub is_derived: bool,
}

#[derive(Serialize)]
pub struct BinaryExpr {
    pub operator: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Serialize)]
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

#[derive(Serialize)]
pub struct UnaryExpr {
    pub expr: Box<Expr>,
    pub operator: UnaryOp,
}

#[derive(Serialize)]
pub enum UnaryOp {
    Plus,
    Minus,
    Tilde,
}

#[derive(Serialize)]
pub struct Func {
    pub name: String,
    pub distinct: bool,
    pub exprs: Vec<SelectExpr>,
    pub order_by: OrderBy,
    pub separator: String,
}

#[derive(Serialize)]
pub struct Case {
    pub expr: Box<Expr>,
    pub whens: Vec<When>,
    pub else_: Box<Expr>,
}

#[derive(Serialize)]
pub struct When {
    pub cond: Expr,
    pub val: Expr,
}

type GroupBy = Vec<Expr>;
type OrderBy = Vec<Order>;

#[derive(Serialize)]
pub struct Order {
    pub expr: Expr,
    pub direction: OrderDirection,
}

#[derive(Serialize)]
pub enum OrderDirection {
    Asc,
    Desc,
}

#[derive(Serialize)]
pub struct Limit {
    pub row_count: Expr,
    pub offset: Option<Expr>,
}

impl Limit {
    fn from_cst(lim: &cst::Limit) -> Limit {
        Limit {
            row_count: Expr::from_cst(&lim.count),
            offset: lim.offset.as_ref().map(Expr::from_cst),
        }
    }
}

#[derive(Serialize)]
pub struct UpdateExpr {
    pub name: ColName,
    pub expr: Expr,
}

#[derive(Serialize)]
pub struct SimpleSelect {
    pub comments: Vec<String>,
    pub query_globals: QueryGlobals,
    pub select_exprs: Vec<SelectExpr>,
    pub limit: Limit,
}

#[derive(Serialize)]
pub enum ShowModifier {
    None,
    Full,
    Session,
    Global,
}

#[derive(Serialize)]
pub struct Show {
    pub section: String, // TODO probably an enum
    pub key: String,
    pub from: Expr,
    pub like_or_where: Expr,
    pub modifier: ShowModifier,
}

#[derive(Serialize)]
pub enum ExplainType {
    Extended,
    Json,
    Traditional,
    Partitions,
}

#[derive(Serialize)]
pub struct Explain {
    pub section: String, // TODO this is probably an enum
    pub table: TableName,
    pub column: ColName,
    pub explain_type: ExplainType,
    pub connection: String,
    pub statement: Box<Statement>,
}

#[derive(Serialize)]
pub enum KillScope {
    Connection,
    Query,
}

#[derive(Serialize)]
pub struct Kill {
    pub scope: KillScope,
    pub id: Expr,
}

#[derive(Serialize)]
pub enum FlushKind {
    Logs,
    Sample,
}

#[derive(Serialize)]
pub struct Flush {
    pub kind: FlushKind,
}

#[derive(Serialize)]
pub struct AlterTable {
    table: TableName,
    specs: Vec<AlterSpec>,
}

#[derive(Serialize)]
pub enum AlterationType {
    RenameColumn,
    DropColumn,
    ModifyColumn,
    RenameTable,
}

#[derive(Serialize)]
pub struct AlterSpec {
    pub type_: AlterationType,
    pub column: ColName,
    pub new_column: ColName,
    pub new_table: TableName,
    pub new_column_type: String,
}

#[derive(Serialize)]
pub struct RenameTable {
    pub renames: Vec<RenameSpec>,
}

#[derive(Serialize)]
pub struct RenameSpec {
    pub table: TableName,
    pub new_table: TableName,
}
