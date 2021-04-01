
// Helper functions for checking whether code is a noop

use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use super::constant;

pub fn is_code_seq_noop(stmts: &[Stmt]) -> bool {
  stmts.iter().all(|stmt| is_code_noop(stmt))
}

pub fn is_code_noop(stmt: &Stmt) -> bool {
  !constant::stmt_has_side_effects(stmt)
}

pub fn is_expr_noop(expr: &Expr) -> bool {
  !constant::expr_has_side_effects(expr)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::op;

  #[test]
  fn expr_noop() {
    // True
    assert!(is_expr_noop(&Expr::var("example_variable")));
    assert!(is_expr_noop(&Expr::from(1)));
    assert!(is_expr_noop(&Expr::Unary(op::UnaryOp::Negate, Box::new(Expr::from(1)))));
    assert!(is_expr_noop(&Expr::ArrayLit(vec!())));
    assert!(is_expr_noop(&Expr::ArrayLit(vec!(Expr::from(1), Expr::from(2), Expr::from(3)))));
    assert!(is_expr_noop(&Expr::Subscript(Box::new(Expr::from(1)), Box::new(Expr::from(2)))));
    assert!(is_expr_noop(&Expr::Attribute(Box::new(Expr::from(1)), String::from("attribute_name"))));

    // False
    let call = Expr::Call(None, String::from("function_name"), vec!());
    assert!(!is_expr_noop(&call));
    assert!(!is_expr_noop(&Expr::Unary(op::UnaryOp::Negate, Box::new(call.clone()))));
    assert!(!is_expr_noop(&Expr::Binary(Box::new(Expr::from(2)), op::BinaryOp::Add, Box::new(call.clone()))));
  }

  #[test]
  fn stmt_noop() {
    // True
    assert!(is_code_noop(&Stmt::Expr(Expr::from(1))));
    assert!(is_code_noop(&Stmt::PassStmt));
    // False
    let call = Expr::Call(None, String::from("function_name"), vec!());
    assert!(!is_code_noop(&Stmt::Expr(call)));
    assert!(!is_code_noop(&Stmt::BreakStmt));
    assert!(!is_code_noop(&Stmt::ContinueStmt));
  }

}
