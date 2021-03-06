
use super::ExpressionLevelPass;
use crate::gdscript::expr::{self, Expr};
use crate::gdscript::op;
use crate::compile::error::Error;
use super::constant;

pub struct BasicMathOps;

impl ExpressionLevelPass for BasicMathOps {
  fn run_on_expr(&self, expr: &Expr) -> Result<Expr, Error> {
    let mut expr = expr.clone();
    // (TODO Expand this)

    // Negation on constant
    if let Expr::Unary(op::UnaryOp::Not, inner) = &expr {
      if let Some(b) = constant::deduce_bool(inner) {
        expr = Expr::from(!b);
      }
    }

    // Ternary if on constant
    if let Expr::TernaryIf(if_expr) = &expr {
      let expr::TernaryIf { true_case, cond, false_case } = if_expr.clone();
      if let Some(b) = constant::deduce_bool(&cond) {
        expr = if b { *true_case } else { *false_case };
      }
    }

    // Double negation elimination (TODO Maybe only in if statements;
    // it does technically Booleanize at least)
    if let Expr::Unary(op::UnaryOp::Not, outer) = &expr {
      if let Expr::Unary(op::UnaryOp::Not, inner) = &**outer {
        expr = (**inner).clone();
      }
    }

    Ok(expr)
  }
}
