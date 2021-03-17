
// Helper functions for checking whether code is a noop

use crate::gdscript::expr::Expr;
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
    Expr::ArrayLit(v) => v.iter().all(is_expr_noop),
  }
}
