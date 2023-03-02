pub use crate::utils::*;
pub use chumsky::{error::Cheap, prelude::*, stream::Stream, BoxStream, Flat};

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);
