mod parser;
pub mod cst;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}
