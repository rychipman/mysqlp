use std::str::FromStr;

use combine::{
    parser::{
        char::{char as chr, digit, letter, string as strn},
        *,
    },
    stream::Stream,
    *,
};

use cst;

pub fn statement<I>() -> impl Parser<Input = I, Output = cst::Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    select_stmt().skip(chr(';')).map(cst::Statement::Select)
}

pub fn select_stmt<I>() -> impl Parser<Input = I, Output = cst::Select>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let simple_spec = (projects(), optional(limit()));
    let simple = simple_spec.map(|(pj, lim)| cst::Select {
        projects: pj,
        limit: lim,
        table: None,
    });

    let standard_spec = (projects(), from(), optional(limit()));
    let standard = standard_spec.map(|(pj, tbl, lim)| cst::Select {
        projects: pj,
        limit: lim,
        table: Some(tbl),
    });

    keyword("select").with(standard.or(simple))
}

fn from<I>() -> impl Parser<Input = I, Output = cst::TableExpr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    keyword("from").with(table_expr())
}

fn join_or_table<I>() -> impl Parser<Input = I, Output = cst::Table>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(attempt(join()), table())
}

fn table<I>() -> impl Parser<Input = I, Output = cst::Table>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(table_name(), derived_table())
}

fn table_expr<I>() -> impl Parser<Input = I, Output = cst::TableExpr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // TODO: add table list
    choice!(join_or_table()).map(cst::TableExpr::Table)
}

fn table_name<I>() -> impl Parser<Input = I, Output = cst::Table>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let leading_dot_spec = chr('.').with(ident());
    let leading_dot = leading_dot_spec.map(|id| (None, id));

    let standard_spec = (ident(), optional(chr('.').with(ident())));
    let standard = standard_spec.map(|(l, r)| match r {
        Some(id) => (Some(l), id),
        None => (None, l),
    });

    leading_dot
        .or(standard)
        .map(|(db, tbl)| cst::TableName {
            name: tbl,
            qualifier: db,
            alias: None, // TODO figure out where this should be used
        })
        .map(cst::Table::Name)
}

parser!{
    fn expr[I]()(I) ->  cst::Expr
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice!(attempt(binary_expr()),
                column_name().map(cst::Expr::Column),
                literal_expr())
    }
}

// need this to get around mutual recursion issue with select_expr
parser!{
    fn derived_table[I]()(I) -> cst::Table
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        chr('(')
            .with(select_stmt())
            .skip(chr(')'))
            .map(|sel| cst::DerivedTable {
                subquery: Box::new(sel),
                alias: "TODO".to_string(),
            })
            .map(cst::Table::Derived)
    }
}

parser!{
    fn join[I]()(I) -> cst::Table
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        // Note: parser combinators cannot support left recursion,
        // so joins are treated as right associative here.
        (
            table(),
            join_kind(),
            keyword("join"),
            join_or_table(),
            optional(join_predicate()),
        )
            .map(|(t1, kind, _, t2, pred)| {
                cst::Table::Join(cst::Join {
                    kind: kind,
                    left: Box::new(t1),
                    right: Box::new(t2),
                    predicate: pred,
                })
            })
    }
}

pub fn limit<I>() -> impl Parser<Input = I, Output = cst::Limit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let spec = choice!(
        expr().map(|expr| cst::Limit {
            count: expr,
            offset: None
        }),
        (expr(), chr(','), expr()).map(|(lim, _, off)| cst::Limit {
            count: lim,
            offset: Some(off)
        }),
        (expr(), keyword("offset"), expr()).map(|(off, _, lim)| cst::Limit {
            count: lim,
            offset: Some(off)
        })
    );
    keyword("limit").with(spec)
}

pub fn join_kind<I>() -> impl Parser<Input = I, Output = cst::JoinKind>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        keyword("left").map(|_| cst::JoinKind::Left),
        keyword("right").map(|_| cst::JoinKind::Left),
        keyword("inner").map(|_| cst::JoinKind::Left),
        keyword("outter").map(|_| cst::JoinKind::Left)
    )
}

pub fn join_predicate<I>() -> impl Parser<Input = I, Output = cst::JoinPredicate>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        keyword("on").with(expr().map(cst::JoinPredicate::On)),
        keyword("using").with(column_list().map(cst::JoinPredicate::Using))
    )
}

pub fn projects<I>() -> impl Parser<Input = I, Output = cst::Projects>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let non_star = select_expr_list().map(cst::Projects::NonStar);
    star().or(non_star)
}

pub fn star<I>() -> impl Parser<Input = I, Output = cst::Projects>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    chr('*').skip(many_blank()).map(|_| cst::Projects::Star)
}

pub fn select_expr_list<I>() -> impl Parser<Input = I, Output = Vec<cst::AliasedColumn>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by1(select_expr(), chr(','))
}

pub fn select_expr<I>() -> impl Parser<Input = I, Output = cst::AliasedColumn>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (expr(), optional(alias())).map(|(exp, als)| cst::AliasedColumn {
        expr: exp,
        alias: als,
    })
}

pub fn alias<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let alias_name = ident().or(string());
    optional(keyword("as")).with(alias_name)
}

fn binary_expr<I>() -> impl Parser<Input = I, Output = cst::Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // Note that everything has to be right associative.
    // TODO: figure out how to use chainl1 to get around this.
    (
        choice!(column_name().map(cst::Expr::Column), literal_expr()),
        binary_op(),
        expr(),
    )
        .map(|(simple_arg, op, recusive_arg)| {
            cst::Expr::Binary(Box::new(simple_arg), op, Box::new(recusive_arg))
        })
}

fn column_list<I>() -> impl Parser<Input = I, Output = Vec<cst::ColumnName>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(column_name())
}

fn column_name<I>() -> impl Parser<Input = I, Output = cst::ColumnName>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    ident().map(|id| cst::ColumnName {
        name: id,
        db: None,
        qualifier: None,
    })
}

fn binary_op<I>() -> impl Parser<Input = I, Output = cst::BinaryOp>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        choice!(keyword("and"), keyword("&&")).map(|_| cst::BinaryOp::And),
        choice!(keyword("or"), keyword("||")).map(|_| cst::BinaryOp::Or),
        keyword("xor").map(|_| cst::BinaryOp::Xor),
        keyword("like").map(|_| cst::BinaryOp::Like),
        keyword("regexp").map(|_| cst::BinaryOp::Regex),
        keyword("<").map(|_| cst::BinaryOp::Lt),
        keyword(">").map(|_| cst::BinaryOp::Gt),
        keyword("<=").map(|_| cst::BinaryOp::Le),
        keyword(">=").map(|_| cst::BinaryOp::Ge),
        choice!(keyword("!="), keyword("<>")).map(|_| cst::BinaryOp::Ne),
        keyword("<=>").map(|_| cst::BinaryOp::Nse),
        keyword("in").map(|_| cst::BinaryOp::In),
        attempt(keyword("not in").map(|_| cst::BinaryOp::Nin)),
        attempt(keyword("is not").map(|_| cst::BinaryOp::Is)),
        keyword("is").map(|_| cst::BinaryOp::IsNot)
    )
}

fn many_blank<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_many(one_of(" \n\t\r".chars()))
}

fn keyword<I>(kw: &'static str) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    strn(kw).skip(many_blank()).map(|s| s.to_string())
}

fn string_contents<I>(delim: char) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let valid = many1(none_of(vec![delim]));
    let escaped_delim = (chr('\\'), chr(delim)).map(|(bs, c)| vec![bs, c]);
    // TODO some more escaping stuff to do here
    many(
        valid
            .or(escaped_delim)
            .map(|cs| cs.iter().collect::<String>()),
    )
}

fn unquoted_ident<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let letter_or_special = || letter().or(chr('_')).or(chr('$'));
    let first = letter_or_special().map(|c: char| c.to_string());
    let rest =
        many(letter_or_special().or(digit())).map(|cs: Vec<char>| cs.iter().collect::<String>());
    let ident = (first, rest).map(|(f, r)| f + &r);
    ident.skip(many_blank())
}

fn quoted_ident<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    chr('`').with(string_contents('`')).skip(chr('`'))
}

fn ident<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    quoted_ident().or(unquoted_ident())
}

fn delimited_string<I>(delim: char) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    chr(delim).with(string_contents(delim)).skip(chr(delim))
}

fn string<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    delimited_string('"').or(delimited_string('\''))
}

#[allow(dead_code)]
fn mantissa<I>(base: u32) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(satisfy(move |c: char| c.to_digit(base).is_some()))
}

#[allow(dead_code)]
fn uint10<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    mantissa(10).map(|s| i64::from_str(&s).unwrap())
}

#[allow(dead_code)]
fn int10<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (sign_opt(), uint10()).map(|(sgn, int)| sgn * int)
}

#[allow(dead_code)]
fn exponent<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    one_of("eE".chars()).with(int10())
}

#[allow(dead_code)]
fn fraction<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    chr('.')
        .with(mantissa(10))
        .map(|s| f64::from_str(&format!("0.{}", s)).unwrap())
}

#[allow(dead_code)]
fn sign<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    one_of("+-".chars()).map(|c| match c {
        '+' => 1,
        '-' => -1,
        _ => panic!("impossible sign char"),
    })
}

#[allow(dead_code)]
fn sign_opt<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    optional(sign()).map(|s| match s {
        Some(s) => s,
        None => 1,
    })
}

fn literal_expr<I>() -> impl Parser<Input = I, Output = cst::Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    literal().map(cst::Expr::Literal)
}

fn literal<I>() -> impl Parser<Input = I, Output = cst::Literal>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // TODO: Dates
    choice!(
        string().map(cst::Literal::String),
        keyword("null").map(|_| cst::Literal::Null),
        boolean(),
        number().map(cst::Literal::Number)
    )
}

fn boolean<I>() -> impl Parser<Input = I, Output = cst::Literal>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        keyword("true").map(|_| cst::Literal::Boolean(true)),
        keyword("false").map(|_| cst::Literal::Boolean(false))
    )
}

fn number<I>() -> impl Parser<Input = I, Output = cst::Number>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        attempt(float().map(cst::Number::Float)),
        integer().map(cst::Number::Integer)
    )
}

fn float<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (integer(), fraction()).map(|(int, fr)| int as f64 + fr)
}

fn integer<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // TODO other bases, signs?
    uint10()
}
