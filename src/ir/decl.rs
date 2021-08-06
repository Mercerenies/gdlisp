
use super::arglist::{ArgList, SimpleArgList};
use super::expr::{self, Expr, Locals, Functions};
use super::literal::Literal;
use super::import::ImportDecl;
use super::identifier::{Namespace, Id, IdLike};
use super::export::Visibility;
use crate::gdscript::decl::Static;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::collections::HashSet;

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct TopLevel {
  pub imports: Vec<ImportDecl>,
  pub decls: Vec<Decl>,
  pub minimalist_flag: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DeclF {
  FnDecl(FnDecl),
  MacroDecl(MacroDecl),
  ConstDecl(ConstDecl),
  ClassDecl(ClassDecl),
  EnumDecl(EnumDecl),
  DeclareDecl(DeclareDecl),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decl {
  pub value: DeclF,
  pub pos: SourceOffset,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnDecl {
  pub visibility: Visibility,
  pub call_magic: Option<String>,
  pub name: String,
  pub args: ArgList,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MacroDecl {
  pub visibility: Visibility,
  pub name: String,
  pub args: ArgList,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConstDecl {
  pub visibility: Visibility,
  pub name: String,
  pub value: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumDecl {
  pub visibility: Visibility,
  pub name: String,
  pub clauses: Vec<(String, Option<Expr>)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassDecl {
  pub visibility: Visibility,
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
  ClassSignalDecl(ClassSignalDecl),
  ClassConstDecl(ConstDecl),
  ClassVarDecl(ClassVarDecl),
  ClassFnDecl(ClassFnDecl),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassSignalDecl {
  pub name: String,
  pub args: SimpleArgList,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassVarDecl {
  pub export: Option<Export>,
  pub name: String,
  pub value: Option<Expr>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassFnDecl {
  pub is_static: Static,
  pub name: String,
  pub args: SimpleArgList,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeclareDecl {
  pub visibility: Visibility,
  pub declare_type: DeclareType,
  pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DeclareType {
  Value,
  Function(ArgList),
  Superglobal,
  SuperglobalFn(ArgList),
}

// TODO This is a bit confusing, since "export" is used in GDLisp to
// mean "exported from a module", not "exported to the interface". We
// should probably rename this so that GDScript "exports" are called
// something else.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Export {
  pub args: Vec<Expr>,
}

impl TopLevel {

  pub fn new() -> TopLevel {
    TopLevel::default()
  }

}

impl Decl {

  pub fn new(value: DeclF, pos: SourceOffset) -> Decl {
    Decl { value, pos }
  }

  pub fn to_id(&self) -> Id {
    Id::new(self.namespace(), self.name().to_owned())
  }

  pub fn id_like<'a>(&'a self) -> Box<dyn IdLike + 'a> {
    Id::build(self.namespace(), self.name())
  }

  pub fn name(&self) -> &str {
    match &self.value {
      DeclF::FnDecl(decl) => &decl.name,
      DeclF::MacroDecl(decl) => &decl.name,
      DeclF::ConstDecl(decl) => &decl.name,
      DeclF::ClassDecl(decl) => &decl.name,
      DeclF::EnumDecl(decl) => &decl.name,
      DeclF::DeclareDecl(decl) => &decl.name,
    }
  }

  // Gets the direct dependencies required by the declaration.
  pub fn dependencies(&self) -> HashSet<Id> {
    match &self.value {
      DeclF::FnDecl(f) => {
        let mut ids: HashSet<Id> = f.body.get_ids().collect();
        for name in f.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids
      }
      DeclF::MacroDecl(m) => {
        let mut ids: HashSet<Id> = m.body.get_ids().collect();
        for name in m.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids
      }
      DeclF::ConstDecl(c) => {
        c.value.get_ids().collect()
      }
      DeclF::ClassDecl(c) => {
        let mut ids = HashSet::new();
        ids.insert(Id::new(Namespace::Value, c.extends.to_owned()));
        ids.extend(c.constructor.dependencies());
        for d in &c.decls {
          ids.extend(d.dependencies());
        }
        ids.remove(&Id::new(Namespace::Value, String::from("self")));
        ids
      }
      DeclF::EnumDecl(enum_decl) => {
        let mut ids = HashSet::new();
        for (_, expr) in &enum_decl.clauses {
          if let Some(expr) = expr {
            ids.extend(expr.get_ids());
          }
        }
        ids
      }
      DeclF::DeclareDecl(_) => {
        HashSet::new()
      }
    }
  }

  pub fn is_macro(&self) -> bool {
    matches!(&self.value, DeclF::MacroDecl(_))
  }

  #[deprecated(note="Use visibility() or export::Visibility constants instead")]
  pub fn is_exported_by_default(&self) -> bool {
    // (sys/declare ...) statements are never exported and are always
    // file-local by default.
    !(matches!(&self.value, DeclF::DeclareDecl(_)))
  }

  pub fn namespace(&self) -> Namespace {
    match &self.value {
      DeclF::FnDecl(_) => Namespace::Function,
      DeclF::MacroDecl(_) => Namespace::Function,
      DeclF::ConstDecl(_) => Namespace::Value,
      DeclF::ClassDecl(_) => Namespace::Value,
      DeclF::EnumDecl(_) => Namespace::Value,
      DeclF::DeclareDecl(d) => d.declare_type.namespace(),
    }
  }

  pub fn visibility(&self) -> Visibility {
    match &self.value {
      DeclF::FnDecl(d) => d.visibility,
      DeclF::MacroDecl(d) => d.visibility,
      DeclF::ConstDecl(d) => d.visibility,
      DeclF::ClassDecl(d) => d.visibility,
      DeclF::EnumDecl(d) => d.visibility,
      DeclF::DeclareDecl(d) => d.visibility,
    }
  }

  pub fn visibility_mut(&mut self) -> &mut Visibility {
    match &mut self.value {
      DeclF::FnDecl(d) => &mut d.visibility,
      DeclF::MacroDecl(d) => &mut d.visibility,
      DeclF::ConstDecl(d) => &mut d.visibility,
      DeclF::ClassDecl(d) => &mut d.visibility,
      DeclF::EnumDecl(d) => &mut d.visibility,
      DeclF::DeclareDecl(d) => &mut d.visibility,
    }
  }

}

impl EnumDecl {

  pub fn value_names(&self) -> impl Iterator<Item=&str> {
    self.clauses.iter().map(|(x, _)| &**x)
  }

}

impl ClassDecl {

  pub fn new(name: String, extends: String, pos: SourceOffset) -> ClassDecl {
    ClassDecl {
      visibility: Visibility::CLASS,
      name: name,
      extends: extends,
      main_class: false,
      constructor: ConstructorDecl::empty(pos),
      decls: vec!(),
    }
  }

}

impl ConstructorDecl {

  /// An empty constructor, marked as starting at `pos`. `pos` should
  /// be the position of the start of the class declaration.
  pub fn empty(pos: SourceOffset) -> ConstructorDecl {
    ConstructorDecl {
      args: SimpleArgList { args: vec!() },
      body: Expr::literal(Literal::Nil, pos),
    }
  }

  pub fn dependencies(&self) -> HashSet<Id> {
    let mut ids: HashSet<Id> = self.body.get_ids().collect();
    for name in self.args.iter_vars() {
      ids.remove(&*Id::build(Namespace::Value, name));
    }
    ids.remove(&*Id::build(Namespace::Value, "self"));
    ids
  }

  pub fn get_names(&self) -> (Locals, Functions) {
    let (mut loc, func) = self.body.get_names();
    for name in self.args.iter_vars() {
      loc.remove(name);
    }
    loc.remove("self");
    (loc, func)
  }

}

impl ClassInnerDecl {

  pub fn dependencies(&self) -> HashSet<Id> {
    match self {
      ClassInnerDecl::ClassSignalDecl(_) => HashSet::new(),
      ClassInnerDecl::ClassConstDecl(_) => HashSet::new(),
      ClassInnerDecl::ClassVarDecl(_) => HashSet::new(),
      ClassInnerDecl::ClassFnDecl(func) => {
        let mut ids: HashSet<Id> = func.body.get_ids().collect();
        for name in func.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids.remove(&*Id::build(Namespace::Value, "self"));
        ids
      }
    }
  }

  pub fn get_names(&self) -> (Locals, Functions) {
    match self {
      ClassInnerDecl::ClassSignalDecl(_) | ClassInnerDecl::ClassVarDecl(_) |
        ClassInnerDecl::ClassConstDecl(_) => (Locals::new(), Functions::new()),
      ClassInnerDecl::ClassFnDecl(fndecl) => {
        let (mut loc, func) = fndecl.body.get_names();
        for name in fndecl.args.iter_vars() {
          loc.remove(name);
        }
        if !bool::from(fndecl.is_static) {
          loc.remove("self");
        }
        (loc, func)
      }
    }
  }

  pub fn is_static(&self) -> bool {
    match self {
      ClassInnerDecl::ClassSignalDecl(_) | ClassInnerDecl::ClassVarDecl(_) => false,
      ClassInnerDecl::ClassConstDecl(_) => true,
      ClassInnerDecl::ClassFnDecl(decl) => decl.is_static.into(),
    }
  }

}

impl DeclareType {

  pub fn namespace(&self) -> Namespace {
    match self {
      DeclareType::Value | DeclareType::Superglobal => Namespace::Value,
      DeclareType::Function(_) | DeclareType::SuperglobalFn(_) => Namespace::Function,
    }
  }

}

impl Sourced for Decl {
  type Item = DeclF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &DeclF {
    &self.value
  }

}

impl From<DeclareType> for Namespace {
  fn from(d: DeclareType) -> Namespace {
    d.namespace()
  }
}

impl From<(ClassDecl, Vec<Expr>)> for expr::LambdaClass {
  fn from(arg: (ClassDecl, Vec<Expr>)) -> expr::LambdaClass {
    let (decl, args) = arg;
    expr::LambdaClass {
      extends: decl.extends,
      args: args,
      constructor: decl.constructor,
      decls: decl.decls,
    }
  }
}
