type IoResult<'a, T> = nom::IResult<&'a [u8], T>;

use tuple_utils::Prepend;

pub struct Meta {}
pub struct Context {}
