mod parser;
pub mod cst;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}

#[cfg(test)]
mod test {
    use super::parse;

    #[test]
    fn test_parse() {
        let stmt = parse("select * from foo;").unwrap();
        assert_eq!(stmt.text, "select * from foo");
    }
}
