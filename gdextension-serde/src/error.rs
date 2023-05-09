
use serde::ser;

use std::error;
use std::result;
use std::fmt::{self, Display};

#[derive(Clone, Debug)]
pub struct Error {
  err: Box<ErrorImpl>,
}

#[derive(Clone, Debug)]
struct ErrorImpl {
  code: ErrorCode,
}

#[derive(Clone, Debug)]
enum ErrorCode {
  Custom(String),
}

pub type Result<T> = result::Result<T, Error>;

impl Display for Error {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> result::Result<(), fmt::Error> {
    match &self.err.code {
      ErrorCode::Custom(s) => {
        write!(f, "{}", s)
      }
    }
  }

}

impl error::Error for Error {}

impl ser::Error for Error {

  fn custom<T: Display>(msg: T) -> Self {
    let error_impl = ErrorImpl {
      code: ErrorCode::Custom(msg.to_string()),
    };
    Error {
      err: Box::new(error_impl),
    }
  }

}
