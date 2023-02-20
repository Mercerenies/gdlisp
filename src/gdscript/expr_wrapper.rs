// Copyright 2023 Silvio Mayolo
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
