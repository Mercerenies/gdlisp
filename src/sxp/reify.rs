
//! Provides the [`Reify`] trait.

use crate::gdscript::expr::Expr;
use crate::gdscript::library;
use super::ast::{AST, ASTF};

use ordered_float::OrderedFloat;

/// This trait describes any type for which there is a reasonable way
/// to convert a `&self` into [`crate::gdscript::expr::Expr`]. Note
/// that this is subtly different from [`Into`], which requires
/// ownership of `self` to do the conversion. The resulting `Expr`
/// should not share any data with `&self`.
pub trait Reify {
  fn reify(&self) -> Expr;
}

impl Reify for i32 {
  fn reify(&self) -> Expr {
    Expr::from(*self)
  }
}

impl Reify for OrderedFloat<f32> {
  fn reify(&self) -> Expr {
    Expr::from(*self)
  }
}

impl Reify for bool {
  fn reify(&self) -> Expr {
    Expr::from(*self)
  }
}

impl Reify for String {
  fn reify(&self) -> Expr {
    Expr::from(self.to_owned())
  }
}

impl Reify for AST {

  fn reify(&self) -> Expr {
    match &self.value {
      ASTF::Nil => {
        Expr::null()
      }
      ASTF::Cons(a, b) => {
        Expr::Call(Some(Box::new(library::gdlisp_root())), String::from("cons"), vec!(a.reify(), b.reify()))
      }
      ASTF::Array(v) => {
        Expr::ArrayLit(v.iter().map(Reify::reify).collect())
      }
      ASTF::Dictionary(vec) => {
        Expr::DictionaryLit(vec.iter().map(|(k, v)| (k.reify(), v.reify())).collect())
      }
      ASTF::Int(n) => {
        n.reify()
      }
      ASTF::Bool(b) => {
        b.reify()
      }
      ASTF::Float(f) => {
        f.reify()
      }
      ASTF::String(s) => {
        s.reify()
      }
      ASTF::Symbol(s) => {
        Expr::Call(Some(Box::new(library::gdlisp_root())), String::from("intern"), vec!(s.reify()))
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
