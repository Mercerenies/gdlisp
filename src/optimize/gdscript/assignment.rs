
use crate::gdscript::op;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;

#[derive(Clone, Debug)]
pub struct AssignmentStmt<'a> {
  pub assign_type: AssignType,
  pub var_name: &'a str,
  pub expr: &'a Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignType {
  VarDecl,
  Assignment(op::AssignOp),
}

impl<'a> AssignmentStmt<'a> {

  pub fn match_stmt(stmt: &'a Stmt) -> Option<AssignmentStmt<'a>> {
    match stmt {
      Stmt::VarDecl(var_name, expr) => {
        Some(AssignmentStmt { assign_type: AssignType::VarDecl, var_name, expr })
      }
      Stmt::Assign(var_name, op, expr) => {
        if let Expr::Var(var_name) = &**var_name {
          Some(AssignmentStmt { assign_type: AssignType::Assignment(*op), var_name, expr })
        } else {
           None
        }
      }
      _ => {
        None
      }
    }
  }

}

impl AssignType {

  pub fn ensure_eq(self) -> Self {
    match self {
      AssignType::VarDecl => AssignType::VarDecl,
      AssignType::Assignment(_) => AssignType::Assignment(op::AssignOp::Eq),
    }
  }

}

impl<'a> From<AssignmentStmt<'a>> for Stmt {

  fn from(stmt: AssignmentStmt) -> Stmt {
    let AssignmentStmt { assign_type, var_name, expr } = stmt;
    let var_name = var_name.to_owned();
    let expr = expr.clone();
    match assign_type {
      AssignType::VarDecl => Stmt::VarDecl(var_name, expr),
      AssignType::Assignment(op) => Stmt::Assign(Box::new(Expr::Var(var_name)), op, Box::new(expr)),
    }
  }

}
