
// Helper functions for checking whether code is a noop

use crate::gdscript::expr::{self, Expr};
use crate::gdscript::stmt::Stmt;

pub fn is_code_seq_noop(stmts: &[Stmt]) -> bool {
  stmts.iter().all(|stmt| is_code_noop(stmt))
}

pub fn is_code_noop(stmt: &Stmt) -> bool {
  match stmt {
    Stmt::Expr(e) => is_expr_noop(e),
    Stmt::IfStmt(_) => false, // TODO We can be more precise
    Stmt::ForLoop(_) => false, // TODO What if the collection is always empty?
    Stmt::WhileLoop(_) => false, // TODO What if the condition is constant false?
    Stmt::PassStmt => true,
    Stmt::BreakStmt | Stmt::ContinueStmt | Stmt::VarDecl(_, _)
      | Stmt::ReturnStmt(_) | Stmt::Assign(_, _, _) => false,
    Stmt::MatchStmt(_, _) => false, // TODO We can be more precise
  }
}

pub fn is_expr_noop(expr: &Expr) -> bool {
  match expr {
    Expr::Var(_) => true, // TODO Consider setget shenanigans
    Expr::Literal(_) => true,
    Expr::Subscript(_, _) => false,
    Expr::Attribute(_, _) => false,
    Expr::Call(_, _, _) => false,
    Expr::SuperCall(_, _) => false,
    Expr::Unary(_, e) => is_expr_noop(&*e),
    Expr::Binary(a, _, b) => is_expr_noop(&*a) && is_expr_noop(&*b),
    Expr::TernaryIf(expr::TernaryIf { true_case: a, cond: b, false_case: c }) =>
      is_expr_noop(&*a) && is_expr_noop(&*b) && is_expr_noop(&*c),
    Expr::ArrayLit(v) => v.iter().all(is_expr_noop),
  }
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

    // False
    let call = Expr::Call(None, String::from("function_name"), vec!());
    assert!(!is_expr_noop(&Expr::Subscript(Box::new(Expr::from(1)), Box::new(Expr::from(2)))));
    assert!(!is_expr_noop(&Expr::Attribute(Box::new(Expr::from(1)), String::from("attribute_name"))));
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
