extern crate bytes;
extern crate combine;
extern crate prost;
#[macro_use]
extern crate prost_derive;

pub mod cst;
mod ast;
mod parser;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}
