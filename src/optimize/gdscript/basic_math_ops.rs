
use super::ExpressionLevelPass;
use crate::gdscript::expr::Expr;
use crate::gdscript::op;
use crate::compile::error::Error;

pub struct BasicMathOps;

impl ExpressionLevelPass for BasicMathOps {
  fn run_on_expr(&self, expr: &Expr) -> Result<Expr, Error> {
    // Currently, this only does one thing: eliminate double
    // negations. (TODO Expand this)

    // Double negation elimination
    if let Expr::Unary(op::UnaryOp::Not, outer) = expr {
      if let Expr::Unary(op::UnaryOp::Not, inner) = &**outer {
        return self.run_on_expr(inner);
      }
    }

    Ok(expr.clone())
  }
}
