// Copyright 2024 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Error and result types for [`ser`].

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
