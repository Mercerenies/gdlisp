
//! Provides the [`Reify`] trait.

pub mod pretty;

use crate::gdscript::expr::Expr;
use crate::compile::names::fresh::FreshNameGenerator;
use super::ast::AST;

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
    let mut gen = FreshNameGenerator::new(vec!());
    let (stmts, result) = pretty::reify_pretty_expr(self, u32::MAX, &mut gen);
    assert!(stmts.is_empty()); // At u32::MAX, we should never have need to generate any helper statements.
    result
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;
  use crate::sxp::ast::ASTF;

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
