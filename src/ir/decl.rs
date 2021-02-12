
use super::arglist::ArgList;
use super::expr::Expr;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Decl {
  FnDecl(FnDecl),
  MacroDecl(MacroDecl),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnDecl {
  pub name: String,
  pub args: ArgList,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MacroDecl {
  pub name: String,
  pub args: ArgList,
  pub body: Expr,
}

impl Decl {

  pub fn name(&self) -> &str {
    match self {
      Decl::FnDecl(decl) => &decl.name,
      Decl::MacroDecl(decl) => &decl.name,
    }
  }

}
