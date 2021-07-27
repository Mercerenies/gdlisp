
//! Provides the [`Reify`] trait.

use crate::gdscript::expr::Expr;
use crate::gdscript::library;
use super::ast::AST;

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
    match self {
      AST::Nil => {
        Expr::null()
      }
      AST::Cons(a, b) => {
        Expr::Call(Some(Box::new(library::gdlisp_root())), String::from("cons"), vec!(a.reify(), b.reify()))
      }
      AST::Array(v) => {
        Expr::ArrayLit(v.iter().map(Reify::reify).collect())
      }
      AST::Dictionary(vec) => {
        Expr::DictionaryLit(vec.iter().map(|(k, v)| (k.reify(), v.reify())).collect())
      }
      AST::Int(n) => {
        n.reify()
      }
      AST::Bool(b) => {
        b.reify()
      }
      AST::Float(f) => {
        f.reify()
      }
      AST::String(s) => {
        s.reify()
      }
      AST::Symbol(s) => {
        Expr::Call(Some(Box::new(library::gdlisp_root())), String::from("intern"), vec!(s.reify()))
      }
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn reify_test() {
    assert_eq!(AST::Nil.reify().to_gd(), "null");
    assert_eq!(AST::cons(AST::Int(1), AST::Int(2)).reify().to_gd(), "GDLisp.cons(1, 2)");
    assert_eq!(AST::Array(vec!(AST::Int(1), AST::Nil)).reify().to_gd(), "[1, null]");
    assert_eq!(AST::Bool(false).reify().to_gd(), "false");
    assert_eq!(AST::Bool(true).reify().to_gd(), "true");
    assert_eq!(AST::symbol("foo").reify().to_gd(), "GDLisp.intern(\"foo\")");
  }

}
