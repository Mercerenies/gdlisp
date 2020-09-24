
use super::literal;
//use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  Var(String),
  Literal(literal::Literal),
  Subscript(Box<Expr>, Box<Expr>),
//  IfStmt(IfStmt),
//  Call(
}
