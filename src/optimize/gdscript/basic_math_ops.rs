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

use super::ExpressionLevelPass;
use crate::gdscript::expr::{self, Expr, ExprF};
use crate::gdscript::op;
use crate::compile::error::GDError;
use super::constant;

pub struct BasicMathOps;

impl ExpressionLevelPass for BasicMathOps {
  fn run_on_expr(&self, expr: &Expr) -> Result<Expr, GDError> {
    let mut expr = expr.clone();
    // (TODO Expand this)

    // Negation on constant
    if let ExprF::Unary(op::UnaryOp::Not, inner) = &expr.value {
      if let Some(b) = constant::deduce_bool(inner) {
        expr = Expr::from_value(!b, expr.pos);
      }
    }

    // Ternary if on constant
    if let ExprF::TernaryIf(if_expr) = &expr.value {
      let expr::TernaryIf { true_case, cond, false_case } = if_expr.clone();
      if let Some(b) = constant::deduce_bool(&cond) {
        expr = if b { *true_case } else { *false_case };
      }
    }

    // Double negation elimination (TODO Maybe only in if statements;
    // it does technically Booleanize at least)
    if let ExprF::Unary(op::UnaryOp::Not, outer) = &expr.value {
      if let ExprF::Unary(op::UnaryOp::Not, inner) = &outer.value {
        expr = (**inner).clone();
      }
    }

    Ok(expr)
  }
}
