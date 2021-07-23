
use crate::ir::access_type::AccessType;
use crate::gdscript::expr::Expr;
use crate::gdscript::decl;
use crate::gdscript::literal::Literal;
use crate::gdscript::library::CELL_CONTENTS;

use std::borrow::ToOwned;
use std::convert::TryFrom;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LocalVar {
  pub name: VarName,
  pub access_type: AccessType,
  pub scope: VarScope,
  pub assignable: bool,
  pub value_hint: Option<ValueHint>,
}

// VarName will eventually translate into an Expr (via the below From
// instance) and consists of all expressions which denote valid
// variable "name" translations.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VarName {
  // A variable which is local to the current scope.
  Local(String),
  // A file-level constant defined in the current file.
  FileConstant(String),
  // A superglobal name, such as built-in GDScript constants.
  Superglobal(String),
  // A file-level constant defined in another file and imported.
  ImportedConstant(Box<VarName>, String),
  // The current file, as a constant value. Special-cased since it needs to compile to a load(...) call.
  CurrentFile(String),
}

#[derive(Clone, Debug)]
pub enum VarNameIntoExtendsError {
  CannotExtendLocal(String),
  CannotExtendCurrentFile(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarScope { GlobalVar, LocalVar }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueHint {
  ClassName,
  Literal(Literal),
  Enum(Vec<String>),
  Superglobal, // Currently, superglobal is only used if explicitly requested with sys/declare
}

pub trait ValueHintsTable {
  fn get_value_hint(&self, name: &str) -> Option<&ValueHint>;
}

pub struct VacuousValueHintsTable;

impl ValueHintsTable for VacuousValueHintsTable {
  fn get_value_hint(&self, _name: &str) -> Option<&ValueHint> {
    None
  }
}

impl LocalVar {

  pub fn read(name: String) -> LocalVar {
    LocalVar::local(name, AccessType::Read)
  }

  pub fn rw(name: String) -> LocalVar {
    LocalVar::local(name, AccessType::RW)
  }

  pub fn closed_rw(name: String) -> LocalVar {
    LocalVar::local(name, AccessType::ClosedRW)
  }

  pub fn local(name: String, access_type: AccessType) -> LocalVar {
    LocalVar {
      name: VarName::Local(name),
      access_type: access_type,
      scope: VarScope::LocalVar,
      assignable: true,
      value_hint: None,
    }
  }

  pub fn superglobal(name: String) -> LocalVar {
    LocalVar {
      name: VarName::Superglobal(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  pub fn file_constant(name: String) -> LocalVar {
    LocalVar {
      name: VarName::FileConstant(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  pub fn current_file(name: String) -> LocalVar {
    LocalVar {
      name: VarName::CurrentFile(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  pub fn self_var() -> LocalVar { // TODO Should this be a special case?
    // Note: Cannot assign to self
    LocalVar::local(String::from("self"), AccessType::ClosedRead).no_assign()
  }

  // Disable assignment on self (intended to be used in a builder-style)
  pub fn no_assign(mut self) -> Self {
    self.assignable = false;
    self
  }

  pub fn with_hint(mut self, value_hint: ValueHint) -> Self {
    self.value_hint = Some(value_hint);
    self
  }

  pub fn simple_name(&self) -> Option<&str> {
    self.name.simple_name()
  }

  pub fn expr(&self) -> Expr {
    let inner: Expr = self.name.clone().into();
    if self.access_type.requires_cell() {
      Expr::Attribute(Box::new(inner), CELL_CONTENTS.to_owned())
    } else {
      inner
    }
  }

  // TODO Put all of the declaration-site stuff here as well, like
  // .expr() for access, so we have it all in one place (i.e. the
  // difference between "var x = ..." and "var x = Cell.new(...)")

}

impl VarName {

  pub fn load_expr(filename: String) -> Expr {
    Expr::Call(None, String::from("load"), vec!(Expr::from(filename)))
  }

  pub fn local(name: &str) -> VarName {
    VarName::Local(String::from(name))
  }

  pub fn file_constant(name: &str) -> VarName {
    VarName::FileConstant(String::from(name))
  }

  pub fn superglobal(name: &str) -> VarName {
    VarName::Superglobal(String::from(name))
  }

  pub fn imported_constant(orig_name: VarName, name: &str) -> VarName {
    VarName::ImportedConstant(Box::new(orig_name), String::from(name))
  }

  pub fn current_file(filename: &str) -> VarName {
    VarName::CurrentFile(String::from(filename))
  }

  pub fn to_gd(&self) -> String {
    Expr::from(self.clone()).to_gd()
  }

  pub fn simple_name(&self) -> Option<&str> {
    match self {
      VarName::Local(s) => Some(&s),
      VarName::FileConstant(s) => Some(&s),
      VarName::Superglobal(s) => Some(&s),
      VarName::ImportedConstant(_, _) => None,
      VarName::CurrentFile(_) => None,
    }
  }

  // If a name is available at top-level scope A.gd and some file B.gd
  // imports A.gd and calls the top-level constant AConst, then
  // calling name.as_imported("AConst") will convert the name to how
  // it should be referenced from B.gd.
  pub fn into_imported(self, import_name: String) -> VarName {
    match self {
      VarName::Local(s) => {
        // To be honest, this case probably should never occur. So
        // we'll just pretend it's FileConstant.
        VarName::ImportedConstant(Box::new(VarName::FileConstant(import_name)), s)
      }
      VarName::FileConstant(s) => {
        // Import file constants by qualifying the name.
        VarName::ImportedConstant(Box::new(VarName::FileConstant(import_name)), s)
      }
      VarName::Superglobal(s) => {
        // Superglobals are always in scope and don't change on import.
        VarName::Superglobal(s)
      }
      VarName::ImportedConstant(lhs, s) => {
        // Import the constant transitively.
        let lhs = Box::new(lhs.into_imported(import_name));
        VarName::ImportedConstant(lhs, s)
      }
      VarName::CurrentFile(_) => {
        // The current file imports as the name of the import itself.
        VarName::FileConstant(import_name)
      }
    }
  }

}

// VarName can always be converted into an Expr. VarName is, by
// definition, the subset of GDScript expressions suitable for
// variable name expansion.
impl From<VarName> for Expr {

  fn from(var_name: VarName) -> Expr {
    match var_name {
      VarName::Local(s) => Expr::Var(s),
      VarName::FileConstant(s) => Expr::Var(s),
      VarName::Superglobal(s) => Expr::Var(s),
      VarName::ImportedConstant(lhs, s) => Expr::Attribute(Box::new(Expr::from(*lhs)), s),
      VarName::CurrentFile(filename) => VarName::load_expr(filename),
    }
  }

}

// Classes extend from variables which have VarName. We can (attempt
// to) convert from VarName to decl::ClassExtends. We cannot, however,
// extend from local variables using this technique.
impl TryFrom<VarName> for decl::ClassExtends {
  type Error = VarNameIntoExtendsError;

  fn try_from(var_name: VarName) -> Result<decl::ClassExtends, VarNameIntoExtendsError> {
    match var_name {
      VarName::Local(s) => {
        Err(VarNameIntoExtendsError::CannotExtendLocal(s))
      }
      VarName::FileConstant(s) => {
        Ok(decl::ClassExtends::Qualified(vec!(s)))
      }
      VarName::Superglobal(s) => {
        Ok(decl::ClassExtends::Qualified(vec!(s)))
      }
      VarName::ImportedConstant(lhs, s) => {
        let decl::ClassExtends::Qualified(mut vec) = decl::ClassExtends::try_from(*lhs)?;
        vec.push(s);
        Ok(decl::ClassExtends::Qualified(vec))
      }
      VarName::CurrentFile(s) => {
        Err(VarNameIntoExtendsError::CannotExtendCurrentFile(s))
      }
    }
  }

}

impl ValueHint {

  pub fn enumeration<'a>(values: impl Iterator<Item=&'a str>) -> ValueHint {
    ValueHint::Enum(values.map(str::to_owned).collect())
  }

}
