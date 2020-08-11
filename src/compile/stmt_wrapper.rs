
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::compile::body::builder::StmtBuilder;

pub trait StmtWrapper {

  // The wrapping behavior for the statement wrapper is performed
  // here.
  fn wrap_expr(&mut self, expr: Expr) -> Stmt;

  // Normally, a wrapper, as the name implies, wraps an expression in
  // a statement. Sometimes, however, the expression has no side
  // effects, and the work itself has already been done elsewhere. In
  // this case, is_vacuous specifies whether we actually *need* the
  // result or not. If is_vacuous is true and the expression has no
  // side effects, it will be elided completely.
  fn is_vacuous(&self) -> bool {
    false
  }

  // Wraps the expression and places it in the statement builder. If
  // the wrapper is vacuous, this method does nothing.
  fn wrap_to_builder(&mut self, builder: &mut StmtBuilder, expr: Expr) {
    if !self.is_vacuous() {
      builder.append(self.wrap_expr(expr));
    }
  }

}

pub struct Return;
pub struct Vacuous;

impl StmtWrapper for Return {
  fn wrap_expr(&mut self, expr: Expr) -> Stmt {
    Stmt::ReturnStmt(expr)
  }
}

impl StmtWrapper for Vacuous {

  fn wrap_expr(&mut self, expr: Expr) -> Stmt {
    Stmt::Expr(expr)
  }

  fn is_vacuous(&self) -> bool {
    true
  }

}
