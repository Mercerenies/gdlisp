
use super::arglist::{ArgList, SimpleArgList};
use super::expr::{self, Expr, Locals, Functions};
use super::literal::Literal;
use super::import::ImportDecl;
use super::identifier::{Namespace, Id, IdLike};
use super::export::Visibility;
use crate::gdscript::decl::Static;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::collections::HashSet;
use std::borrow::Cow;

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
  SymbolMacroDecl(SymbolMacroDecl),
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
pub struct SymbolMacroDecl {
  pub visibility: Visibility,
  pub name: String,
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
  pub constructor: Option<ConstructorDecl>,
  pub decls: Vec<ClassInnerDecl>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConstructorDecl {
  pub args: SimpleArgList,
  pub super_call: Vec<Expr>,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ClassInnerDeclF {
  ClassSignalDecl(ClassSignalDecl),
  ClassConstDecl(ConstDecl),
  ClassVarDecl(ClassVarDecl),
  ClassFnDecl(ClassFnDecl),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassInnerDecl {
  pub value: ClassInnerDeclF,
  pub pos: SourceOffset,
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

#[derive(Clone, Debug, Copy, Eq, PartialEq)]
pub struct DuplicateMainClassError(pub SourceOffset);

impl TopLevel {

  pub fn new() -> TopLevel {
    TopLevel::default()
  }

  /// Find the class in `self` for which [`ClassDecl::main_class`] is
  /// true. If there is no such class, then `None` is returned. If
  /// there are multiple such classes, then
  /// [`DuplicateMainClassError`] is returned.
  pub fn find_main_class(&self) -> Result<Option<&ClassDecl>, DuplicateMainClassError> {
    let all_main_classes: Vec<_> = self.decls.iter().filter(|decl| {
      if let DeclF::ClassDecl(cdecl) = &decl.value {
        cdecl.main_class
      } else {
        false
      }
    }).collect();
    match all_main_classes.len() {
      0 => {
        // No main class; return None
        Ok(None)
      }
      1 => {
        // Single main class; return it
        if let DeclF::ClassDecl(cdecl) = &all_main_classes[0].value {
          Ok(Some(&cdecl))
        } else {
          panic!("Internal error in TopLevel::find_main_class (this is a bug in GDLisp)")
        }
      }
      _ => {
        // Duplicate main classes; produce an error at the position of
        // the second
        let second_main_class = all_main_classes[1];
        Err(DuplicateMainClassError(second_main_class.pos))
      }
    }
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
      DeclF::SymbolMacroDecl(decl) => &decl.name,
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
      DeclF::SymbolMacroDecl(m) => {
        m.body.get_ids().collect()
      }
      DeclF::ConstDecl(c) => {
        c.value.get_ids().collect()
      }
      DeclF::ClassDecl(c) => {
        let mut ids = HashSet::new();
        ids.insert(Id::new(Namespace::Value, c.extends.to_owned()));
        ids.extend(c.constructor_or_default(SourceOffset::from(0)).dependencies());
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
    matches!(&self.value, DeclF::MacroDecl(_) | DeclF::SymbolMacroDecl(_))
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
      DeclF::SymbolMacroDecl(_) => Namespace::Value,
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
      DeclF::SymbolMacroDecl(d) => d.visibility,
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
      DeclF::SymbolMacroDecl(d) => &mut d.visibility,
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

  pub fn new(name: String, extends: String) -> ClassDecl {
    ClassDecl {
      visibility: Visibility::CLASS,
      name: name,
      extends: extends,
      main_class: false,
      constructor: None,
      decls: vec!(),
    }
  }

  /// The class' constructor, or an empty constructor if there is no
  /// constructor. In the latter case, the empty constructor will be
  /// reported as being at source position default_pos, which should
  /// be the position of the start of the class declaration.
  pub fn constructor_or_default(&self, default_pos: SourceOffset) -> Cow<ConstructorDecl> {
    self.constructor.as_ref().map_or_else(|| Cow::Owned(ConstructorDecl::empty(default_pos)), Cow::Borrowed)
  }

}

impl ConstructorDecl {

  /// An empty constructor, marked as starting at `pos`. `pos` should
  /// be the position of the start of the class declaration.
  pub fn empty(pos: SourceOffset) -> ConstructorDecl {
    ConstructorDecl {
      args: SimpleArgList { args: vec!() },
      super_call: vec!(),
      body: Expr::literal(Literal::Nil, pos),
    }
  }

  pub fn dependencies(&self) -> HashSet<Id> {
    let mut ids: HashSet<Id> = self.body.get_ids().collect();
    for expr in &self.super_call {
      for id in expr.get_ids() {
        ids.insert(id);
      }
    }
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

  pub fn new(value: ClassInnerDeclF, pos: SourceOffset) -> ClassInnerDecl {
    ClassInnerDecl { value, pos }
  }

  pub fn dependencies(&self) -> HashSet<Id> {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(_) => HashSet::new(),
      ClassInnerDeclF::ClassConstDecl(_) => HashSet::new(),
      ClassInnerDeclF::ClassVarDecl(_) => HashSet::new(),
      ClassInnerDeclF::ClassFnDecl(func) => {
        let mut ids: HashSet<Id> = func.body.get_ids().collect();
        for name in func.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids.remove(&*Id::build(Namespace::Value, "self"));
        ids
      }
    }
  }

  pub fn name(&self) -> &str {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(signal) => &signal.name,
      ClassInnerDeclF::ClassConstDecl(constant) => &constant.name,
      ClassInnerDeclF::ClassVarDecl(var) => &var.name,
      ClassInnerDeclF::ClassFnDecl(func) => &func.name,
    }
  }

  pub fn get_names(&self) -> (Locals, Functions) {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(_) | ClassInnerDeclF::ClassVarDecl(_) |
        ClassInnerDeclF::ClassConstDecl(_) => (Locals::new(), Functions::new()),
      ClassInnerDeclF::ClassFnDecl(fndecl) => {
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
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(_) | ClassInnerDeclF::ClassVarDecl(_) => false,
      ClassInnerDeclF::ClassConstDecl(_) => true,
      ClassInnerDeclF::ClassFnDecl(decl) => decl.is_static.into(),
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

impl Sourced for ClassInnerDecl {
  type Item = ClassInnerDeclF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &ClassInnerDeclF {
    &self.value
  }

}

impl From<DeclareType> for Namespace {
  fn from(d: DeclareType) -> Namespace {
    d.namespace()
  }
}

impl From<SymbolMacroDecl> for MacroDecl {
  fn from(decl: SymbolMacroDecl) -> MacroDecl {
    MacroDecl {
      visibility: decl.visibility,
      name: decl.name,
      args: ArgList::empty(),
      body: decl.body,
    }
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

#[cfg(test)]
mod tests {
  use super::*;

  fn sample_class(class_name: &str, main_class: bool) -> ClassDecl {
    ClassDecl {
      visibility: Visibility::Public,
      name: String::from(class_name),
      extends: String::from("Reference"),
      main_class,
      constructor: None,
      decls: vec!(),
    }
  }

  #[test]
  fn find_main_class_test_1() {
    let example = TopLevel {
      imports: vec!(),
      decls: vec!(
        Decl::new(DeclF::ClassDecl(sample_class("Foo", false)), SourceOffset::default()),
        Decl::new(DeclF::ClassDecl(sample_class("Bar", false)), SourceOffset::default()),
      ),
      minimalist_flag: false,
    };
    assert_eq!(example.find_main_class(), Ok(None));
  }

  #[test]
  fn find_main_class_test_2() {
    let example = TopLevel {
      imports: vec!(),
      decls: vec!(
        Decl::new(DeclF::ClassDecl(sample_class("Foo", true)), SourceOffset::default()),
        Decl::new(DeclF::ClassDecl(sample_class("Bar", false)), SourceOffset::default()),
      ),
      minimalist_flag: false,
    };
    assert_eq!(example.find_main_class(), Ok(Some(&sample_class("Foo", true))));
  }

  #[test]
  fn find_main_class_test_3() {
    let example = TopLevel {
      imports: vec!(),
      decls: vec!(
        Decl::new(DeclF::ClassDecl(sample_class("Foo", false)), SourceOffset::default()),
        Decl::new(DeclF::ClassDecl(sample_class("Bar", true)), SourceOffset::default()),
      ),
      minimalist_flag: false,
    };
    assert_eq!(example.find_main_class(), Ok(Some(&sample_class("Bar", true))));
  }

  #[test]
  fn find_main_class_test_4() {
    let example = TopLevel {
      imports: vec!(),
      decls: vec!(
        Decl::new(DeclF::ClassDecl(sample_class("Foo", true)), SourceOffset::default()),
        Decl::new(DeclF::ClassDecl(sample_class("Bar", true)), SourceOffset::default()),
      ),
      minimalist_flag: false,
    };
    assert_eq!(example.find_main_class(), Err(DuplicateMainClassError(SourceOffset(0))));
  }

  #[test]
  fn find_main_class_test_5() {
    // Nontrivial source offset
    let example = TopLevel {
      imports: vec!(),
      decls: vec!(
        Decl::new(DeclF::ClassDecl(sample_class("Foo", true)), SourceOffset(10)),
        Decl::new(DeclF::ClassDecl(sample_class("Bar", true)), SourceOffset(20)),
      ),
      minimalist_flag: false,
    };
    // Should be reported at the source position of the *second* main class
    assert_eq!(example.find_main_class(), Err(DuplicateMainClassError(SourceOffset(20))));
  }

}
