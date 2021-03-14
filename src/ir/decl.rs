
use super::arglist::{ArgList, SimpleArgList};
use super::expr::Expr;
use super::literal::Literal;
use super::import::ImportDecl;
use super::identifier::{Namespace, Id, IdLike};

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
  ConstDecl(ConstDecl),
  ClassDecl(ClassDecl),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConstDecl {
  pub name: String,
  pub value: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassDecl {
  pub name: String,
  pub extends: String,
  pub main_class: bool,
  pub constructor: ConstructorDecl,
  pub decls: Vec<ClassInnerDecl>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConstructorDecl { // TODO Super
  pub args: SimpleArgList,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ClassInnerDecl {
  ClassVarDecl(ClassVarDecl),
  ClassFnDecl(ClassFnDecl),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassVarDecl {
  pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassFnDecl {
  pub name: String,
  pub args: SimpleArgList,
  pub body: Expr,
}

impl TopLevel {

  pub fn new() -> TopLevel {
    TopLevel::default()
  }

}

impl Decl {

  pub fn to_id(&self) -> Id {
    Id::new(self.namespace(), self.name().to_owned())
  }

  pub fn id_like<'a>(&'a self) -> Box<dyn IdLike + 'a> {
    Id::build(self.namespace(), self.name())
  }

  pub fn name(&self) -> &str {
    match self {
      Decl::FnDecl(decl) => &decl.name,
      Decl::MacroDecl(decl) => &decl.name,
      Decl::ConstDecl(decl) => &decl.name,
      Decl::ClassDecl(decl) => &decl.name,
    }
  }

  // Gets the direct dependencies required by the declaration.
  pub fn dependencies(&self) -> HashSet<Id> {
    match self {
      Decl::FnDecl(f) => {
        let mut ids: HashSet<Id> = f.body.get_ids().collect();
        for name in f.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids
      }
      Decl::MacroDecl(m) => {
        let mut ids: HashSet<Id> = m.body.get_ids().collect();
        for name in m.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids
      }
      Decl::ConstDecl(c) => {
        c.value.get_ids().collect()
      }
      Decl::ClassDecl(c) => {
        let mut ids = HashSet::new();
        ids.extend(c.constructor.dependencies());
        for d in &c.decls {
          ids.extend(d.dependencies());
        }
        ids.remove(&Id::new(Namespace::Value, String::from("self")));
        ids
      }
    }
  }

  pub fn is_macro(&self) -> bool {
    matches!(self, Decl::MacroDecl(_))
  }

  pub fn namespace(&self) -> Namespace {
    match self {
      Decl::FnDecl(_) => Namespace::Function,
      Decl::MacroDecl(_) => Namespace::Function,
      Decl::ConstDecl(_) => Namespace::Value,
      Decl::ClassDecl(_) => Namespace::Value,
    }
  }

}

impl ClassDecl {

  pub fn new(name: String, extends: String) -> ClassDecl {
    ClassDecl {
      name: name,
      extends: extends,
      main_class: false,
      constructor: ConstructorDecl::default(),
      decls: vec!(),
    }
  }

}

impl ConstructorDecl {

  pub fn dependencies(&self) -> HashSet<Id> {
    let mut ids: HashSet<Id> = self.body.get_ids().collect();
    for name in self.args.iter_vars() {
      ids.remove(&*Id::build(Namespace::Value, name));
    }
    ids
  }

}

impl ClassInnerDecl {

  pub fn dependencies(&self) -> HashSet<Id> {
    match self {
      ClassInnerDecl::ClassVarDecl(_) => HashSet::new(),
      ClassInnerDecl::ClassFnDecl(func) => {
        let mut ids: HashSet<Id> = func.body.get_ids().collect();
        for name in func.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids
      }
    }
  }

}

impl Default for ConstructorDecl {

  fn default() -> ConstructorDecl {
    ConstructorDecl {
      args: SimpleArgList { args: vec!() },
      body: Expr::Literal(Literal::Nil),
    }
  }

}
