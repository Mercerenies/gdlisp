
//! Miscellaneous helpers for wrapping commonly-used operations as
//! GDScript expressions.

use super::expr::{Expr, ExprF};
use super::literal::Literal;

/// Call the GDScript function `int` on the expression, unless the
/// expression is provably already an integer.
pub fn int(expr: Expr) -> Expr {
  if let ExprF::Literal(Literal::Int(_)) = &expr.value {
    expr
  } else {
    let pos = expr.pos;
    Expr::simple_call("int", vec!(expr), pos)
  }
}

/// Call the GDScript function `float` on the expression, unless the
/// expression is provably already a floating-point value.
pub fn float(expr: Expr) -> Expr {
  if let ExprF::Literal(Literal::Float(_)) = &expr.value {
    expr
  } else {
    let pos = expr.pos;
    Expr::simple_call("float", vec!(expr), pos)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;

  #[test]
  fn int_test() {
    assert_eq!(int(Expr::var("a", SourceOffset::default())),
               Expr::call(None, "int", vec!(Expr::var("a", SourceOffset::default())), SourceOffset::default()));
    assert_eq!(int(Expr::from_value(10, SourceOffset::default())),
               Expr::from_value(10, SourceOffset::default()));
  }

  #[test]
  fn float_test() {
    assert_eq!(float(Expr::var("a", SourceOffset::default())),
               Expr::call(None, "float", vec!(Expr::var("a", SourceOffset::default())), SourceOffset::default()));
  }

}
