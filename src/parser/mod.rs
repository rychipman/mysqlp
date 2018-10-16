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
mod tests {
    use super::parse_sql;

    #[test]
    fn test_parse_success() {
        let cases = vec![
            "select * from foo left join bar using a;",
            "select * from foo inner join bar on a;",
            "select * from foo right join bar on 1;",
            "select * from foo inner join bar on 1+2;",
            "select * from foo left join bar using a,b;",
            "select * from foo;",
            "select *, 3+4 from foo;",
            "select a from foo;",
            "select a as col_a from foo;",
            "select a col_a from foo;",
            "select a from foo tbl_b;",
        ];
        for case in cases {
            match parse_sql(case) {
                Ok(_) => println!("PASS"),
                Err(msg) => panic!(format!("failed to parse query {}: {}", case, msg)),
            }
        }
    }
    #[test]
    fn test_parse_failure() {
        let cases = vec![
            "select 3+4, * from foo;",
            "hello",
            "slect * form foo;",
            ";select * from foo",
        ];
        for case in cases {
            match parse_sql(case) {
                Ok(_) => panic!(format!("succeed to parse bad query {}", case)),
                Err(_msg) => println!("PASS"),
            }
        }
    }
}
