
use super::arglist::ArgList;
use super::expr::Expr;
use super::import::ImportDecl;
use super::symbol_table::Namespace;

use std::collections::HashSet;

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct TopLevel {
  pub imports: Vec<ImportDecl>,
  pub decls: Vec<Decl>,
}

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

impl TopLevel {

  pub fn new() -> TopLevel {
    TopLevel::default()
  }

}

impl Decl {

  pub fn name(&self) -> &str {
    match self {
      Decl::FnDecl(decl) => &decl.name,
      Decl::MacroDecl(decl) => &decl.name,
    }
  }

  // Gets the direct dependencies required by the declaration.
  pub fn dependencies(&self) -> HashSet<String> {
    match self {
      Decl::FnDecl(f) => f.body.get_functions().into_names().collect(),
      Decl::MacroDecl(m) => m.body.get_functions().into_names().collect(),
    }
  }

  pub fn is_macro(&self) -> bool {
    matches!(self, Decl::MacroDecl(_))
  }

  pub fn namespace(&self) -> Namespace {
    match self {
      Decl::FnDecl(_) => Namespace::Function,
      Decl::MacroDecl(_) => Namespace::Function,
    }
  }

}
