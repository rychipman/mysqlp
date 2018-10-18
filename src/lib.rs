#![recursion_limit = "128"]
extern crate bytes;
extern crate combine;
extern crate prost;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate prost_derive;

mod ast;
pub mod cst;
mod parser;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}
