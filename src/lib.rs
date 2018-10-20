#![recursion_limit = "128"]
extern crate bytes;
extern crate combine;
#[macro_use]
extern crate lazy_static;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

mod ast;
pub mod cst;
mod parser;

pub fn parse(sql: &str) -> Result<cst::Statement, String> {
    parser::parse_sql(sql)
}

pub fn test_stmt_json() -> String {
    use ast::*;
    let stmt = Statement::Use(Use {
        db_name: "testdb".to_string(),
    });
    serde_json::to_string(&stmt).unwrap()
}
