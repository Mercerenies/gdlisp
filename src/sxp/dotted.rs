
//! Parsing of [`AST`](super::ast::AST) values as dotted S-expressions.
//!
//! This module provides the inverse operation to
//! [`AST::dotted_list`](super::ast::AST::dotted_list). That function
//! takes a vector and a terminator and produces an `AST` value, while
//! [`DottedExpr::new`] takes an `AST` value and interprets it as a
//! vector and a terminator.

use super::ast::{AST, ASTF};
use crate::pipeline::source::SourceOffset;

use std::convert::TryFrom;

/// A dotted list expression consists of a sequence of elements and a
/// final terminator.
///
/// Note that a `DottedExpr` does not *own* any data itself. Instead,
/// it acts as a view on an existing [`AST`](super::ast::AST) owned by
/// someone else.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DottedExpr<'a> {
  /// The sequence of elements leading up to the terminator.
  pub elements: Vec<&'a AST>,
  /// The terminator element.
  pub terminal: &'a AST,
}

/// The type of errors produced by [`TryFrom::<DottedExpr>::try_from`].
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct TryFromDottedExprError {
  pub pos: SourceOffset,
}

fn accumulate_ast<'a>(vec: &mut Vec<&'a AST>, ast: &'a AST) -> &'a AST {
  match &ast.value {
    ASTF::Cons(car, cdr) => {
      vec.push(&*car);
      accumulate_ast(vec, &*cdr)
    }
    _ => ast
  }
}

impl<'a> DottedExpr<'a> {

  /// Given an `AST`, construct a `DottedExpr` view representing it.
  ///
  /// A dotted list is, here, defined to be a collection of zero or
  /// more cons cells, linked together by their cdr, and terminated by
  /// an arbitrary element. Note that, under this definition, *every*
  /// `AST` can be meaningfully interpreted as a dotted list, which is
  /// why this function cannot fail.
  ///
  /// If `ast` is not a cons cell, then it is a dotted list of zero
  /// elements where the terminator is `ast`. If `ast` is a cons cell,
  /// then the first element of the dotted list is its car, and the
  /// rest of the dotted list, as well as its terminator, is
  /// calculated recursively on the cdr of `ast`.
  pub fn new(ast: &'a AST) -> DottedExpr<'a> {
    let mut elements = Vec::new();
    let terminal = accumulate_ast(&mut elements, ast);
    DottedExpr { elements, terminal }
  }

}

/// A dotted list whose terminator is
/// [`ASTF::Nil`](super::ast::ASTF::Nil) is called a proper list. It is
/// reasonable to interpret a proper list as a simple vector, since
/// the nil terminator is merely a placeholder for the end of the
/// list. [`TryFrom::<DottedExpr>::try_from`] performs this
/// conversion, returning [`DottedExpr::elements`] if
/// [`DottedExpr::terminal`] is nil and producing an error otherwise.
impl<'a> TryFrom<DottedExpr<'a>> for Vec<&'a AST> {
  type Error = TryFromDottedExprError;

  fn try_from(expr: DottedExpr<'a>) -> Result<Vec<&'a AST>, TryFromDottedExprError> {
    if expr.terminal.value == ASTF::Nil {
      Ok(expr.elements)
    } else {
      Err(TryFromDottedExprError { pos: expr.terminal.pos })
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;

  fn int(n: i32) -> AST {
    AST::new(ASTF::Int(n), SourceOffset::default())
  }

  fn nil() -> AST {
    AST::nil(SourceOffset::default())
  }

  fn list(data: Vec<AST>) -> AST {
    AST::dotted_list(data, nil())
  }

  #[test]
  fn simple_dot() {
    let ast = int(33);
    let dot = DottedExpr::new(&ast);

    assert!(dot.elements.is_empty());
    assert_eq!(*dot.terminal, ast);
    assert!(Vec::try_from(dot).is_err());

  }

  #[test]
  fn proper_list() {
    let vec = vec!(int(1), int(2), int(3));
    let ast = list(vec.clone());
    let dot = DottedExpr::new(&ast);

    assert_eq!(dot.elements, vec.iter().collect::<Vec<_>>());
    assert_eq!(*dot.terminal, nil());

    let vec1 = Vec::try_from(dot);
    assert_eq!(vec1, Ok(vec.iter().collect()));

  }

  #[test]
  fn improper_list() {
    let vec = vec!(int(1), int(2), int(3));
    let end = int(4);
    let ast = AST::dotted_list(vec.clone(), end.clone());
    let dot = DottedExpr::new(&ast);

    assert_eq!(dot.elements, vec.iter().collect::<Vec<_>>());
    assert_eq!(*dot.terminal, end);

    assert!(Vec::try_from(dot).is_err());

  }

}
