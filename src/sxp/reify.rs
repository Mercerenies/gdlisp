
use crate::gdscript::expr::Expr;
use crate::gdscript::library;
use super::ast::AST;

use ordered_float::OrderedFloat;

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
        library::nil()
      }
      AST::Cons(a, b) => {
        Expr::Call(Some(Box::new(library::cons_class())), String::from("new"), vec!(a.reify(), b.reify()))
      }
      AST::Array(v) => {
        Expr::ArrayLit(v.iter().map(Reify::reify).collect())
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
        Expr::Call(Some(Box::new(library::symbol_class())), String::from("new"), vec!(s.reify()))
      }
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::ast;

  #[test]
  fn reify_test() {
    assert_eq!(AST::Nil.reify().to_gd(), "GDLisp.Nil");
    assert_eq!(ast::cons(AST::Int(1), AST::Int(2)).reify().to_gd(), "GDLisp.Cons.new(1, 2)");
    assert_eq!(AST::Array(vec!(AST::Int(1), AST::Nil)).reify().to_gd(), "[1, GDLisp.Nil]");
    assert_eq!(AST::Bool(false).reify().to_gd(), "false");
    assert_eq!(AST::Bool(true).reify().to_gd(), "true");
    assert_eq!(AST::Symbol(String::from("foo")).reify().to_gd(), "GDLisp.Symbol.new(\"foo\")");
  }

}