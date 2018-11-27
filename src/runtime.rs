type IoResult<'a, T> = nom::IResult<&'a [u8], T>;

use tuple_utils::Prepend;

// Runtime Endian
pub enum Endian {
    Big,
    Little,
}

// Runtime Meta
pub struct Meta {
    endian: Endian,
}

pub struct Context {}
