
//! Provides the [`Reify`] trait.

use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::library;
use super::ast::{AST, ASTF};

/// This trait describes any type for which there is a reasonable way
/// to convert a `&self` into [`crate::gdscript::expr::Expr`]. Note
/// that this is subtly different from [`Into`], which requires
/// ownership of `self` to do the conversion. The resulting `Expr`
/// should not share any data with `&self`.
pub trait Reify {
  fn reify(&self) -> Expr;
}

impl Reify for AST {

  fn reify(&self) -> Expr {
    match &self.value {
      ASTF::Nil => {
        Expr::null(self.pos)
      }
      ASTF::Cons(a, b) => {
        Expr::call(Some(library::gdlisp_root(self.pos)), "cons", vec!(a.reify(), b.reify()), self.pos)
      }
      ASTF::Array(v) => {
        Expr::new(ExprF::ArrayLit(v.iter().map(Reify::reify).collect()), self.pos)
      }
      ASTF::Dictionary(vec) => {
        Expr::new(ExprF::DictionaryLit(vec.iter().map(|(k, v)| (k.reify(), v.reify())).collect()), self.pos)
      }
      ASTF::Int(n) => {
        Expr::from_value(*n, self.pos)
      }
      ASTF::Bool(b) => {
        Expr::from_value(*b, self.pos)
      }
      ASTF::Float(f) => {
        Expr::from_value(*f, self.pos)
      }
      ASTF::String(s) => {
        Expr::from_value(s.to_owned(), self.pos)
      }
      ASTF::Symbol(s) => {
        let s = Expr::from_value(s.to_owned(), self.pos);
        Expr::call(Some(library::gdlisp_root(self.pos)), "intern", vec!(s), self.pos)
      }
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

  #[allow(dead_code)]
  fn list(data: Vec<AST>) -> AST {
    AST::dotted_list(data, nil())
  }

  fn cons(a: AST, b: AST) -> AST {
    AST::new(ASTF::cons(a, b), SourceOffset::default())
  }

  #[test]
  fn reify_test() {
    assert_eq!(nil().reify().to_gd(), "null");
    assert_eq!(cons(int(1), int(2)).reify().to_gd(), "GDLisp.cons(1, 2)");
    assert_eq!(AST::new(ASTF::Array(vec!(int(1), nil())), SourceOffset::default()).reify().to_gd(), "[1, null]");
    assert_eq!(AST::new(ASTF::Bool(false), SourceOffset::default()).reify().to_gd(), "false");
    assert_eq!(AST::new(ASTF::Bool(true), SourceOffset::default()).reify().to_gd(), "true");
    assert_eq!(AST::symbol("foo", SourceOffset::default()).reify().to_gd(), "GDLisp.intern(\"foo\")");
  }

}
