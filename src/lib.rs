extern crate combine;

pub mod cst;
mod parser;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}
