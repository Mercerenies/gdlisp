
use super::literal;
//use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  LocalVar(String),
  Literal(literal::Literal),
  Subscript(Box<Expr>, Box<Expr>),
  Progn(Vec<Expr>),
  IfStmt(Box<Expr>, Box<Expr>, Box<Expr>),
  CondStmt(Vec<(Expr, Option<Expr>)>),
  Call(String, Vec<Expr>),
  BuiltInCall(String, Vec<Expr>),
  Let(Vec<(String, Expr)>, Box<Expr>),
}

pub fn if_stmt(cond: Expr, t: Expr, f: Expr) -> Expr {
  Expr::IfStmt(Box::new(cond), Box::new(t), Box::new(f))
}
