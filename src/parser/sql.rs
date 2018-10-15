extern crate combine;
use self::combine::stream::Stream;
use self::combine::*;

use cst;

pub fn statement<I>() -> impl Parser<Input = I, Output = cst::Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(satisfy(|c| c != ';')).map(|chars: Vec<char>| {
        let s: String = chars.iter().collect();
        cst::Statement{
            text: s,
        }
    })
}
