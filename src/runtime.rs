type IoResult<T> = nom::IResult<&[u8], T>

pub trait PrependUnit<T> {
    type Output;
    fn prepend(self, x: T) -> Self::Output;
}

impl<T> PrependUnit<T> for () {
    type Output = (T, );

    #[inline]
    #[allow(non_snake_case)]
    fn prepend(self, x: T) -> (T, ) {
        match self {
            _ => (x, )
        }
    }
}

pub struct Meta {}
pub struct Context {}
