
use super::arglist::ordinary::ArgList;
use super::arglist::constructor::ConstructorArgList;
use super::arglist::simple::SimpleArgList;
use super::expr::{self, Expr, Locals, Functions};
use super::literal::Literal;
use super::import::ImportDecl;
use super::identifier::{Namespace, ClassNamespace, Id, IdLike};
use super::export::Visibility;
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use crate::gdscript::decl::Static;
use crate::gdscript::library;
use crate::pipeline::source::{SourceOffset, Sourced};
use crate::compile::body::class_initializer::InitTime;
use crate::compile::body::synthetic_field::{Getter, Setter};

use std::collections::HashMap;
use std::borrow::Cow;
use std::convert::TryInto;
use std::iter;

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
  pub args: ConstructorArgList,
  pub super_call: SuperCall,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SuperCall {
  pub call: Vec<Expr>,
  pub pos: SourceOffset,
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
  pub init_time: InitTime,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassFnDecl {
  pub is_static: Static,
  pub name: InstanceFunctionName,
  pub args: SimpleArgList,
  pub body: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeclareDecl {
  pub visibility: Visibility,
  pub declare_type: DeclareType,
  pub name: String,
  // If this doesn't exist, it will be treated as
  // `lisp_to_gd(self.name)` after compilation. The declaration is
  // allowed to override this, however.
  pub target_name: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DeclareType {
  /// A value, with no hint as to its value or `const` status.
  Value,
  /// A value, equivalent to [`DeclareType::Value`], but with a value
  /// hint indicating that its value is immutable at runtime.
  Constant,
  /// A function available at the top-level of the current module.
  Function(ArgList),
  /// A superglobal value, available from everywhere in the Godot
  /// ecosystem without import.
  Superglobal,
  /// A superglobal function, available from everywhere in the Godot
  /// ecosystem without import.
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

/// The name of an instance function in GDLisp, which can either be an
/// ordinary string or a special accessor.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InstanceFunctionName {
  /// An ordinary string name, which will compile to an ordinary
  /// method in GDScript.
  Ordinary(String),
  /// A setter for the field with the given name.
  Setter(String),
  /// A getter for the field with the given name.
  Getter(String),
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
          Ok(Some(cdecl))
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

  pub fn inner_exprs(&self) -> impl Iterator<Item=&Expr> + '_ {
    self.decls.iter().flat_map(|x| x.inner_exprs())
  }

}

impl Decl {

  pub fn new(value: DeclF, pos: SourceOffset) -> Decl {
    Decl { value, pos }
  }

  pub fn to_id(&self) -> Id {
    Id::new(self.namespace(), self.name().to_owned())
  }

  pub fn id_like<'a>(&'a self) -> Box<dyn IdLike<NS=Namespace> + 'a> {
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
  pub fn dependencies(&self) -> HashMap<Id, SourceOffset> {
    match &self.value {
      DeclF::FnDecl(f) => {
        let mut ids: HashMap<Id, SourceOffset> = f.body.get_ids().collect();
        for name in f.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids
      }
      DeclF::MacroDecl(m) => {
        let mut ids: HashMap<Id, SourceOffset> = m.body.get_ids().collect();
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
        let mut ids = HashMap::new();
        ids.insert(Id::new(Namespace::Value, c.extends.to_owned()), self.pos);
        ids.extend(c.constructor_or_default(SourceOffset::from(0)).dependencies());
        for d in &c.decls {
          ids.extend(d.dependencies());
        }
        ids.remove(&Id::new(Namespace::Value, String::from("self")));
        ids
      }
      DeclF::EnumDecl(enum_decl) => {
        let mut ids = HashMap::new();
        for (_, expr) in &enum_decl.clauses {
          if let Some(expr) = expr {
            ids.extend(expr.get_ids());
          }
        }
        ids
      }
      DeclF::DeclareDecl(_) => {
        // Declare declarations have no dependencies; they are
        // assertions to the compiler.
        HashMap::new()
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

  pub fn inner_exprs(&self) -> Box<dyn Iterator<Item=&Expr> + '_> {
    match &self.value {
      DeclF::FnDecl(cdecl) => Box::new(iter::once(&cdecl.body)),
      DeclF::MacroDecl(cdecl) => Box::new(iter::once(&cdecl.body)),
      DeclF::SymbolMacroDecl(cdecl) => Box::new(iter::once(&cdecl.body)),
      DeclF::ConstDecl(cdecl) => Box::new(iter::once(&cdecl.value)),
      DeclF::DeclareDecl(_) => Box::new(iter::empty()),
      DeclF::EnumDecl(cdecl) => {
        Box::new(cdecl.clauses.iter().flat_map(|(_name, value)| value))
      }
      DeclF::ClassDecl(cdecl) => {
        let constructor_exprs = cdecl.constructor.iter().flat_map(|constructor| {
          constructor.super_call.call.iter()
            .chain(iter::once(&constructor.body))
        });
        let inner_exprs = cdecl.decls.iter().flat_map(ClassInnerDecl::inner_exprs);
        Box::new(constructor_exprs.chain(inner_exprs))
      }
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
      args: ConstructorArgList { args: vec!() },
      super_call: SuperCall::empty(pos),
      body: Expr::literal(Literal::Nil, pos),
    }
  }

  pub fn dependencies(&self) -> HashMap<Id, SourceOffset> {
    let mut ids: HashMap<Id, SourceOffset> = self.body.get_ids().collect();
    for expr in &self.super_call.call {
      for (id, pos) in expr.get_ids() {
        ids.insert(id, pos);
      }
    }
    for (name, is_instance_field) in self.args.iter_vars() {
      // Instance field arguments are accessed directly on `self` and
      // are not available as GDLisp-side local variables.
      if !is_instance_field {
        ids.remove(&*Id::build(Namespace::Value, name));
      }
    }
    ids.remove(&*Id::build(Namespace::Value, "self"));
    ids
  }

  pub fn get_names(&self) -> (Locals, Functions) {
    let (mut loc, func) = self.body.get_names();
    for (name, is_instance_field) in self.args.iter_vars() {
      // Instance field arguments are accessed directly on `self` and
      // are not available as GDLisp-side local variables.
      if !is_instance_field {
        loc.remove(name);
      }
    }
    loc.remove("self");
    (loc, func)
  }

}

impl SuperCall {

  /// An empty super call, which invokes the super constructor with no
  /// arguments.
  pub fn empty(pos: SourceOffset) -> SuperCall {
    SuperCall { call: vec!(), pos: pos }
  }

}

impl ClassInnerDecl {

  pub fn new(value: ClassInnerDeclF, pos: SourceOffset) -> ClassInnerDecl {
    ClassInnerDecl { value, pos }
  }

  pub fn dependencies(&self) -> HashMap<Id, SourceOffset> {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(_) => HashMap::new(),
      ClassInnerDeclF::ClassConstDecl(_) => HashMap::new(),
      ClassInnerDeclF::ClassVarDecl(_) => HashMap::new(),
      ClassInnerDeclF::ClassFnDecl(func) => {
        let mut ids: HashMap<Id, SourceOffset> = func.body.get_ids().collect();
        for name in func.args.iter_vars() {
          ids.remove(&*Id::build(Namespace::Value, name));
        }
        ids.remove(&*Id::build(Namespace::Value, "self"));
        ids
      }
    }
  }

  pub fn name(&self) -> Cow<str> {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(signal) => Cow::Borrowed(&signal.name),
      ClassInnerDeclF::ClassConstDecl(constant) => Cow::Borrowed(&constant.name),
      ClassInnerDeclF::ClassVarDecl(var) => Cow::Borrowed(&var.name),
      ClassInnerDeclF::ClassFnDecl(func) => func.name.method_name(),
    }
  }

  pub fn namespace(&self) -> ClassNamespace {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(_) => ClassNamespace::Signal,
      ClassInnerDeclF::ClassConstDecl(_) => ClassNamespace::Value,
      ClassInnerDeclF::ClassVarDecl(_) => ClassNamespace::Value,
      ClassInnerDeclF::ClassFnDecl(_) => ClassNamespace::Function,
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

  pub fn inner_exprs(&self) -> Box<dyn Iterator<Item=&Expr> + '_> {
    match &self.value {
      ClassInnerDeclF::ClassSignalDecl(_) => Box::new(iter::empty()),
      ClassInnerDeclF::ClassConstDecl(cdecl) => Box::new(iter::once(&cdecl.value)),
      ClassInnerDeclF::ClassVarDecl(cdecl) => Box::new(cdecl.value.iter()),
      ClassInnerDeclF::ClassFnDecl(cdecl) => Box::new(iter::once(&cdecl.body)),
    }
  }

}

impl DeclareType {

  pub fn namespace(&self) -> Namespace {
    match self {
      DeclareType::Value | DeclareType::Constant | DeclareType::Superglobal => Namespace::Value,
      DeclareType::Function(_) | DeclareType::SuperglobalFn(_) => Namespace::Function,
    }
  }

}

impl InstanceFunctionName {

  /// Given an [`AST`] representing the name of an instance function,
  /// returns the name as an [`InstanceFunctionName`]. Specifically,
  /// the following are attempted in order.
  ///
  /// 1. If the [`AST`] argument holds an [`ASTF::Symbol`], then this
  /// is an [`InstanceFunctionName::Ordinary`] name.
  ///
  /// 2. Otherwise, if the argument is a (proper) list of two
  /// elements, both of which are symbols, and if the first symbol is
  /// `set`, then this is an [`InstanceFunctionName::Setter`].
  ///
  /// 3. If the argument is a (proper) list of two elements, both of
  /// which are symbols, and if the first symbol is `get`, then this
  /// is an [`InstanceFunctionName::Getter`].
  ///
  /// 4. Otherwise, parsing fails and `None` is returned.
  pub fn parse(ast: &AST) -> Option<InstanceFunctionName> {
    if let ASTF::Symbol(name) = &ast.value {
      return Some(InstanceFunctionName::Ordinary(name.to_owned()));
    }
    let list: Vec<_> = DottedExpr::new(ast).try_into().ok()?;
    if let [accessor_type, field_name] = &*list {
      if let ASTF::Symbol(field_name) = &field_name.value {
        if let ASTF::Symbol(accessor_type) = &accessor_type.value {
          match accessor_type.as_ref() {
            "set" => { return Some(InstanceFunctionName::Setter(field_name.to_owned())); }
            "get" => { return Some(InstanceFunctionName::Getter(field_name.to_owned())); }
            _ => {}
          }
        }
      }
    }
    None
  }

  /// The name of the method being defined, using the GDLisp naming
  /// conventions (e.g., characters such as `-` will *not* be
  /// converted in the returned value). For
  /// [`InstanceFunctionName::Ordinary`], this is simply the declared
  /// name of the method. For other types of method names, specialized
  /// prefixes will be added to compute the runtime name.
  pub fn method_name(&self) -> Cow<str> {
    match self {
      InstanceFunctionName::Ordinary(name) => Cow::Borrowed(name),
      InstanceFunctionName::Setter(field_name) => Cow::Owned(Setter::method_name(field_name)),
      InstanceFunctionName::Getter(field_name) => Cow::Owned(Getter::method_name(field_name)),
    }
  }

  /// Returns true if this is the name of a constructor function, i.e.
  /// if this is an ordinary function whose name is equal to
  /// [`CONSTRUCTOR_NAME`](library::CONSTRUCTOR_NAME).
  pub fn is_constructor_function(&self) -> bool {
    if let InstanceFunctionName::Ordinary(fname) = self {
      fname == library::CONSTRUCTOR_NAME
    } else {
      false
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
  use crate::AST_PARSER;

  fn parse_ast(input: &str) -> AST {
    AST_PARSER.parse(input).unwrap()
  }

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

  #[test]
  fn parse_instance_function_name_test() {

    assert_eq!(InstanceFunctionName::parse(&parse_ast("abc")),
               Some(InstanceFunctionName::Ordinary(String::from("abc"))));

    assert_eq!(InstanceFunctionName::parse(&parse_ast("foo-bar")),
               Some(InstanceFunctionName::Ordinary(String::from("foo-bar"))));

    assert_eq!(InstanceFunctionName::parse(&parse_ast("(set pizza)")),
               Some(InstanceFunctionName::Setter(String::from("pizza"))));

    assert_eq!(InstanceFunctionName::parse(&parse_ast("(get pizza)")),
               Some(InstanceFunctionName::Getter(String::from("pizza"))));

  }

  #[test]
  fn parse_instance_function_name_failures_test() {
    assert_eq!(InstanceFunctionName::parse(&parse_ast("0")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("(get a b c)")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("(put abc)")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("(get 100)")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("\"alpha\"")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("()")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("(set)")), None);
    assert_eq!(InstanceFunctionName::parse(&parse_ast("(get)")), None);
  }

  #[test]
  fn is_constructor_name_test() {
    assert!(InstanceFunctionName::Ordinary(String::from("_init")).is_constructor_function());
    assert!(!InstanceFunctionName::Ordinary(String::from("init")).is_constructor_function());
    assert!(!InstanceFunctionName::Ordinary(String::from("foobar")).is_constructor_function());
    assert!(!InstanceFunctionName::Setter(String::from("_init")).is_constructor_function());
    assert!(!InstanceFunctionName::Getter(String::from("_init")).is_constructor_function());
    assert!(!InstanceFunctionName::Setter(String::from("abcdef")).is_constructor_function());
    assert!(!InstanceFunctionName::Getter(String::from("baz")).is_constructor_function());
  }

}
