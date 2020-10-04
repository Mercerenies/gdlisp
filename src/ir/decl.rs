
use super::arglist::ArgList;
use super::expr::Expr;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Decl {
  FnDecl(FnDecl),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnDecl {
  pub name: String,
  pub args: ArgList,
  pub body: Expr,
}
