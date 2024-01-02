// Copyright 2023, 2024 Silvio Mayolo
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

//! Parsing of [`SExpr`](super::sexpr::SExpr) values as dotted
//! S-expression lists.
//!
//! This module provides the inverse operation to
//! [`SExpr::dotted_list`](super::sexpr::SExpr::dotted_list). That
//! function takes a vector and a terminator and produces an `SExpr`
//! value, while [`DottedExpr::new`] takes an `SExpr` value and
//! interprets it as a vector together with a terminator.

use crate::sexpr::{SExpr, SExprF};
use gdlisp_util::source::SourceOffset;

/// A dotted list expression consists of a sequence of elements and a
/// final terminator.
///
/// Note that a `DottedExpr` does not *own* any data itself. Instead,
/// it acts as a view on an existing [`SExpr`] owned by someone else.
///
/// All fields on `DottedExpr` are public, but users will generally
/// want to construct this struct from an `SExpr` using
/// [`DottedExpr::new`].
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DottedExpr<'a> {
  /// The sequence of elements leading up to the terminator.
  pub elements: Vec<&'a SExpr>,
  /// The terminator element.
  pub terminal: &'a SExpr,
}

/// The type of errors produced by [`DottedExpr::try_into_vec`].
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
#[non_exhaustive]
pub struct TryFromDottedExprError {
  pub pos: SourceOffset,
}

impl<'a> DottedExpr<'a> {

  /// Given an `SExpr`, construct a `DottedExpr` view representing it.
  ///
  /// A dotted list is, here, defined to be a collection of zero or
  /// more cons cells, linked together by their cdr, and terminated by
  /// an arbitrary element. Note that, under this definition, *every*
  /// `SExpr` can be meaningfully interpreted as a dotted list
  /// (possibly with an empty element list), which is why this
  /// function cannot fail.
  ///
  /// If `sexpr` is not a cons cell, then it is viewed as a dotted
  /// list of zero elements where the terminator is `sexpr`. If
  /// `sexpr` is a cons cell, then the first element of the dotted
  /// list is its car, and the rest of the dotted list, as well as its
  /// terminator, is calculated recursively on the cdr of `sexpr`.
  pub fn new(sexpr: &'a SExpr) -> DottedExpr<'a> {
    let mut elements = Vec::new();
    let terminal = accumulate_sexpr(&mut elements, sexpr);
    DottedExpr { elements, terminal }
  }

  pub fn try_into_vec(self) -> Result<Vec<&'a SExpr>, TryFromDottedExprError> {
    if self.terminal.value == SExprF::NIL {
      Ok(self.elements)
    } else {
      Err(TryFromDottedExprError { pos: self.terminal.pos })
    }
  }

}

/// Adds any car values for the S-expression into the vector and
/// returns the final dotted list terminator.
fn accumulate_sexpr<'a>(vec: &mut Vec<&'a SExpr>, sexpr: &'a SExpr) -> &'a SExpr {
  match &sexpr.value {
    SExprF::Cons(car, cdr) => {
      vec.push(car);
      accumulate_sexpr(vec, cdr)
    }
    _ => sexpr
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn int(n: i32) -> SExpr {
    SExpr::int(n, SourceOffset::default())
  }

  fn nil() -> SExpr {
    SExpr::nil(SourceOffset::default())
  }

  fn list(data: Vec<SExpr>) -> SExpr {
    SExpr::dotted_list(data, nil())
  }

  #[test]
  fn simple_dot() {
    let ast = int(33);
    let dot = DottedExpr::new(&ast);

    assert!(dot.elements.is_empty());
    assert_eq!(*dot.terminal, ast);
    assert!(dot.try_into_vec().is_err());

  }

  #[test]
  fn proper_list() {
    let vec = vec!(int(1), int(2), int(3));
    let ast = list(vec.clone());
    let dot = DottedExpr::new(&ast);

    assert_eq!(dot.elements, vec.iter().collect::<Vec<_>>());
    assert_eq!(*dot.terminal, nil());

    let vec1 = dot.try_into_vec();
    assert_eq!(vec1, Ok(vec.iter().collect()));

  }

  #[test]
  fn improper_list() {
    let vec = vec!(int(1), int(2), int(3));
    let end = int(4);
    let ast = SExpr::dotted_list(vec.clone(), end.clone());
    let dot = DottedExpr::new(&ast);

    assert_eq!(dot.elements, vec.iter().collect::<Vec<_>>());
    assert_eq!(*dot.terminal, end);

    assert!(dot.try_into_vec().is_err());

  }

}
