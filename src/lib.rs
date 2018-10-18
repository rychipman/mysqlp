#![recursion_limit = "128"]
extern crate bytes;
extern crate combine;
#[macro_use]
extern crate lazy_static;

mod ast;
pub mod cst;
mod parser;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}
