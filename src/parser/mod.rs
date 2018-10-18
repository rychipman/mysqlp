mod sql;

use combine::stream::state::State;
use combine::Parser;

use super::cst;

pub fn parse_sql(input: &str) -> Result<cst::Statement, String> {
    let state = State::new(input);
    let res = sql::statement().easy_parse(state);
    match res {
        Ok((stmt, _rest)) => Ok(stmt),
        Err(err) => Err(format!("{}", err)),
    }
}

#[cfg(test)]
pub fn parse_expr(input: &str) -> Result<cst::Expr, String> {
    let state = State::new(input);
    let res = sql::expr().easy_parse(state);
    match res {
        Ok((stmt, _rest)) => Ok(stmt),
        Err(err) => Err(format!("{}", err)),
    }
}

#[cfg(test)]
mod tests {
    use super::parse_expr;
    use super::parse_sql;

    macro_rules! test_parse_success {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let input = $value;
                    match parse_sql(input) {
                        Ok(_) => println!("PASS"),
                        Err(msg) => panic!(format!("parsing failed: {}", msg)),
                    }
                }
            )*
        }
    }

    test_parse_success!{
        dual_int: "select 1;",
        dual_string: "select 'abc';",
        dual_bool: "select true;",
        dual_explicitly: "select true from dual;",
        a_from_foo: "select a from foo;",
        star: "select * from foo;",
        star_plus: "select *, a from foo;",
        join_join: "select * from foo join bar;",
        join_comma: "select * from foo, bar;",
        join_inner: "select * from foo inner join bar;",
        join_left: "select * from foo left join bar;",
        join_right: "select * from foo right join bar;",
        join_left_outer: "select * from foo left outer join bar;",
        join_right_outer: "select * from foo right outer join bar;",
        join_pred_col: "select * from foo join bar on a;",
        join_pred_literal: "select * from foo join bar on 1;",
        join_pred_expr: "select * from foo join bar on a+1;",
        join_pred_using: "select * from foo join bar using a,b;",
        alias_col_as: "select a as b from foo;",
        alias_col_no_as: "select a b from foo;",
        alias_table: "select a from foo tbl_f;",
        select_where_simple: "select * from foo where a;",
        select_orderby: "select * from bar order by a;",
        expressions: "select 1,1+2,3,hello(world,dragon);",
    }

    macro_rules! test_parse_expr {
        ($($name:ident: $value:expr => $output:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let input = $value;
                    let expected = $output;
                    let parsed = parse_expr(input);
                    assert!(parsed == expected, "parsed = {:?}, expected = {:?}", parsed, expected);
                }
            )*
        }
    }

    macro_rules! b {
        ($e:expr) => {
            Box::new($e)
        };
    }

    use cst::BinaryOp::*;
    use cst::Expr::*;
    use cst::Literal::*;
    use cst::Number::*;
    use cst::UnaryOp::*;
    use cst::*;

    test_parse_expr!{
         bool_mul_plus: "true * false + true" =>
    Ok(Binary(
             b!(Binary(b!(Literal(Boolean(true))), Times, b!(Literal(Boolean(false))))),
             Add,
             b!(Literal(Boolean(true))))),
         int_mul_plus: "3 * 4 + 5" =>
    Ok(Binary(
            b!(Binary(b!(Literal(Number(Integer(3)))), Times, b!(Literal(Number(Integer(4)))))),
            Add,
            b!(Literal(Number(Integer(5)))))),
         unary: "+3 - - 4" =>
    Ok(Binary(
            b!(Unary(Plus, b!(Literal(Number(Integer(3)))))),
            Sub,
            b!(Unary(Minus, b!(Literal(Number(Integer(4)))))))),
         single_arg_func: "hello ( goodbye )" =>
    Ok(ScalarFunc("hello".to_string(),
                  vec![Column(ColumnName { name: "goodbye".to_string(), db: None, qualifier: None })])),
         multi_arg_func: "hello(goodbye, world, dragon)" =>
    Ok(ScalarFunc("hello".to_string(),
                  vec![Column(ColumnName { name: "goodbye".to_string(), db: None, qualifier: None }),
                       Column(ColumnName { name: "world".to_string(), db: None, qualifier: None }),
                       Column(ColumnName { name: "dragon".to_string(), db: None, qualifier: None })])),
         agg_func: "avg(34)" =>
    Ok(AggFunc("avg".to_string(), vec![Literal(Number(Integer(34)))])),
         not_in: "a   not    in    b" =>
    Ok(Binary(
            b!(Column(ColumnName { name: "a".to_string(), db: None, qualifier: None })),
            Nin,
            b!(Column(ColumnName { name: "b".to_string(), db: None, qualifier: None })))),
     }

    macro_rules! test_parse_failure {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let input = $value;
                    match parse_sql(input) {
                        Ok(_) => panic!("expected parsing to fail, but it succeeded"),
                        Err(_) => println!("PASS"),
                    }
                }
            )*
        }
    }

    test_parse_failure! {
        star_star: "select *, * from foo;",
        star_after_other_column: "select a, * from foo;",
        no_semicolon: "select * from foo",
        semicolon_only: ";",
        empty: "",
    }
}
