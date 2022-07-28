
// Helper functions for checking whether code is a noop

use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use super::constant;

pub fn is_code_seq_noop(stmts: &[Stmt]) -> bool {
  stmts.iter().all(is_code_noop)
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
  use crate::gdscript::expr::ExprF;
  use crate::gdscript::stmt::StmtF;
  use crate::pipeline::source::SourceOffset;

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  #[test]
  fn expr_noop() {
    // True
    assert!(is_expr_noop(&e(ExprF::Var(String::from("example_variable")))));
    assert!(is_expr_noop(&e(ExprF::from(1))));
    assert!(is_expr_noop(&e(ExprF::Unary(op::UnaryOp::Negate, Box::new(e(ExprF::from(1)))))));
    assert!(is_expr_noop(&e(ExprF::ArrayLit(vec!()))));
    assert!(is_expr_noop(&e(ExprF::ArrayLit(vec!(e(ExprF::from(1)), e(ExprF::from(2)), e(ExprF::from(3)))))));
    assert!(is_expr_noop(&e(ExprF::Subscript(Box::new(e(ExprF::from(1))), Box::new(e(ExprF::from(2)))))));
    assert!(is_expr_noop(&e(ExprF::Attribute(Box::new(e(ExprF::from(1))), String::from("attribute_name")))));

    // False
    let call = e(ExprF::Call(None, String::from("function_name"), vec!()));
    assert!(!is_expr_noop(&call));
    assert!(!is_expr_noop(&e(ExprF::Unary(op::UnaryOp::Negate, Box::new(call.clone())))));
    assert!(!is_expr_noop(&e(ExprF::Binary(Box::new(e(ExprF::from(2))), op::BinaryOp::Add, Box::new(call.clone())))));
  }

  #[test]
  fn stmt_noop() {
    // True
    assert!(is_code_noop(&Stmt::expr(e(ExprF::from(1)))));
    assert!(is_code_noop(&s(StmtF::PassStmt)));
    // False
    let call = e(ExprF::Call(None, String::from("function_name"), vec!()));
    assert!(!is_code_noop(&Stmt::expr(call)));
    assert!(!is_code_noop(&s(StmtF::BreakStmt)));
    assert!(!is_code_noop(&s(StmtF::ContinueStmt)));
  }

}
