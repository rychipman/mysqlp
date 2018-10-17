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
        expressions: "select 1,1+2,3,hello(world,dragon);",
    }

    macro_rules! test_parse_expr_success {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let input = $value;
                    match parse_expr(input) {
                        Ok(_) => println!("PASS"),
                        Err(msg) => panic!(format!("parsing failed: {}", msg)),
                    }
                }
            )*
        }
    }

    test_parse_expr_success!{
        add: "3+hello+world",
        sub: "3-4+5",
        add_sub: "3+4-5",
        modulo: "3%4%5",
        func: "hello(world)",
        func_multi_arg: "hello(goodbye, world, dragon)",
        unary: "+3 - - 4",
    }

    #[test]
    fn stuff() {
        println!("{:?}", parse_expr("3 * 4 + 5"));
        println!("{:?}", parse_expr("+3--4"));
        println!("{:?}", parse_expr("hello(goodbye)"));
        println!("{:?}", parse_expr("hello(goodbye,world,dragon)"));
        panic!();
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
