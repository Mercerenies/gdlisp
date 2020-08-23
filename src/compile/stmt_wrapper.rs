
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::gdscript::op;
use crate::compile::body::builder::StmtBuilder;
use super::StExpr;

pub trait StmtWrapper {

  // The wrapping behavior for the statement wrapper is performed
  // here.
  fn wrap_expr(&self, expr: Expr) -> Stmt;

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
  // the wrapper is vacuous and the expression is stateless, this
  // method does nothing.
  fn wrap_to_builder(&self, builder: &mut StmtBuilder, expr: StExpr) {
    let StExpr(expr, stateful) = expr;
    if stateful || !self.is_vacuous() {
      builder.append(self.wrap_expr(expr));
    }
  }

  fn wrap_to_stmts(&self, expr: StExpr) -> Vec<Stmt> {
    let StExpr(expr, stateful) = expr;
    if stateful || !self.is_vacuous() {
      vec!(self.wrap_expr(expr))
    } else {
      vec!()
    }
  }

}

pub struct Return;
pub struct Vacuous;
pub struct AssignToVar(pub String);

impl StmtWrapper for Return {
  fn wrap_expr(&self, expr: Expr) -> Stmt {
    Stmt::ReturnStmt(expr)
  }
}

impl StmtWrapper for Vacuous {

  fn wrap_expr(&self, expr: Expr) -> Stmt {
    Stmt::Expr(expr)
  }

  fn is_vacuous(&self) -> bool {
    true
  }

}

impl StmtWrapper for AssignToVar {
  fn wrap_expr(&self, expr: Expr) -> Stmt {
    let lhs = Box::new(Expr::Var(self.0.clone()));
    let rhs = Box::new(expr);
    Stmt::Assign(lhs, op::AssignOp::Eq, rhs)
  }
}
