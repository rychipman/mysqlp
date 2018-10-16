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

fn table_expr<I>() -> impl Parser<Input = I, Output = cst::TableExpr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(table_name(), derived_table()).map(cst::TableExpr::Table)
}

fn table_name<I>() -> impl Parser<Input = I, Output = cst::Table>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let dotted_parts = (optional(ident()), chr('.'), ident());
    let dotted = dotted_parts.map(|(qual, _, id)| cst::TableName {
        name: id,
        qualifier: qual,
        alias: None,
    });
    let undotted = ident().map(|id| cst::TableName {
        name: id,
        qualifier: None,
        alias: None,
    });
    dotted.or(undotted).map(cst::Table::Name)
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

pub fn limit<I>() -> impl Parser<Input = I, Output = cst::Limit>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let spec = choice!(
        expr().map(|expr| cst::Limit {
            limit: expr,
            offset: None
        }),
        (expr(), chr(','), expr()).map(|(lim, _, off)| cst::Limit {
            limit: lim,
            offset: Some(off)
        }),
        (expr(), keyword("offset"), expr()).map(|(off, _, lim)| cst::Limit {
            limit: lim,
            offset: Some(off)
        })
    );
    keyword("limit").with(spec)
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

fn expr<I>() -> impl Parser<Input = I, Output = cst::Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    column_name().map(cst::Expr::Column)
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
    let first = letter().map(|c: char| c.to_string());
    let rest = many(letter().or(digit())).map(|cs: Vec<char>| cs.iter().collect::<String>());
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

// fn number<I>() -> impl Parser<Input = I, Output = String>
// where
// I: Stream<Item = char>,
// I::Error: ParseError<I::Item, I::Range, I::Position>,
// {
// panic!("TODO");
// }

fn mantissa<I>(base: u32) -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(satisfy(move |c: char| c.to_digit(base).is_some()))
}

fn uint10<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    mantissa(10).map(|s| i64::from_str(&s).unwrap())
}

fn int10<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (sign_opt(), uint10()).map(|(sgn, int)| sgn * int)
}

fn exponent<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    one_of("eE".chars()).with(int10())
}

fn fraction<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    chr('.')
        .with(mantissa(10))
        .map(|s| f64::from_str(&format!("0.{}", s)).unwrap())
}

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
