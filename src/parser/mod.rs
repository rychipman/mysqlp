extern crate combine;

use self::combine::stream::state::State;
use self::combine::Parser;
use super::cst;

mod sql;

pub fn parse_sql(input: &str) -> Result<cst::Statement, String> {
    let state = State::new(input);
    let res = sql::statement().easy_parse(state);
    match res {
        Ok((stmt, _rest)) => Ok(stmt),
        Err(err) => Err(format!("{}", err)),
    }
}
