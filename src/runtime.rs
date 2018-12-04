type IoResult<'a, T> = nom::IResult<&'a [u8], T>;

use nom::*;
use tuple_utils::Prepend;
use byteorder::WriteBytesExt;
use encoding_rs;

// Runtime Endian
pub enum Endian {
    Big,
    Little,
}

// Runtime Meta
pub struct Meta {
    endian: Endian,
}

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

#[inline]
fn decode_string(s: &[u8], label: &[u8]) -> std::io::Result<String> {
    let enc = encoding_rs::Encoding::for_label_no_replacement(label);
    let res = enc.unwrap_or_else(|| panic!("Unknown encoding: {:?}", label))
        .decode_without_bom_handling_and_without_replacement(&s);

    if let Some(s) = res {
        Ok(s.to_string())
    } else {
        panic!("cannot decode string")
    }
}

#[inline]
fn encode_string<W: std::io::Write>(_io: &mut W, s: &str, label: &[u8]) -> std::io::Result<()> {
    let enc = encoding_rs::Encoding::for_label_no_replacement(label);
    let encoder = enc.unwrap_or_else(|| panic!("Unknown encoding: {:?}", label));

    let (bytes, _enc, unmappable) = encoder.encode(s);
    assert!(!unmappable);

    _io.write_all(&bytes)
}
