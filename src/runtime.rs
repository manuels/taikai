type IoResult<'a, T> = nom::IResult<&'a [u8], T>;

use nom::*;
use tuple_utils::Prepend;
use byteorder::WriteBytesExt;

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

/*
// Repeat::Until not implemented, yet
macro_rules! repeat_while(
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => (
    {
        use nom::lib::std::result::Result::*;
        use nom::Err;

        let mut v = vec![];
        let mut i_ = $i.clone();
        while $cond {
            i_ = match $submac!(i_, $($args)*) {
                Ok((i,o))               => {
                    v.push(o);
                    i
                },
                Err(Err::Error(_))      => {
                    return Ok(($i, v))
                },
                Err(e) => return Err(e),
            }
        }

        Ok((i_, v))
    }
    );
    ($i:expr, $cond:expr, $f:expr) => (
        repeat_while!($i, $cond, call!($f));
    );
);
*/
