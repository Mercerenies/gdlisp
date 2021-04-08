
use crate::ir::locals::AccessType;
use crate::gdscript::expr::Expr;
use crate::gdscript::literal::Literal;
use crate::gdscript::library::CELL_CONTENTS;

use std::borrow::ToOwned;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LocalVar {
  pub name: Expr,
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

  pub fn new(name: String, access_type: AccessType, scope: VarScope, assignable: bool) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type, scope, assignable, value_hint: None }
  }

  pub fn read(name: String) -> LocalVar {
    LocalVar::new(name, AccessType::Read, VarScope::LocalVar, true)
  }

  pub fn rw(name: String) -> LocalVar {
    LocalVar::new(name, AccessType::RW, VarScope::LocalVar, true)
  }

  pub fn closed_rw(name: String) -> LocalVar {
    LocalVar::new(name, AccessType::ClosedRW, VarScope::LocalVar, true)
  }

  pub fn global(name: String) -> LocalVar {
    LocalVar::new(name, AccessType::Read, VarScope::GlobalVar, false)
  }

  pub fn local(name: String, access_type: AccessType) -> LocalVar {
    LocalVar::new(name, access_type, VarScope::LocalVar, true)
  }

  pub fn self_var() -> LocalVar {
    // Note: Cannot assign to self
    LocalVar::new(String::from("self"), AccessType::ClosedRead, VarScope::LocalVar, false)
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
    if let Expr::Var(name) = &self.name {
      Some(name)
    } else {
      None
    }
  }

  pub fn expr(&self) -> Expr {
    let inner = self.name.clone();
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

}

impl From<VarName> for Expr {

  fn from(var_name: VarName) -> Expr {
    match var_name {
      VarName::Local(s) => Expr::Var(s),
      VarName::FileConstant(s) => Expr::Var(s),
      VarName::Superglobal(s) => Expr::Var(s),
      VarName::ImportedConstant(lhs, s) => Expr::Attribute(Box::new(Expr::from(*lhs)), s),
    }
  }

}

impl ValueHint {

  pub fn enumeration<'a>(values: impl Iterator<Item=&'a str>) -> ValueHint {
    ValueHint::Enum(values.map(str::to_owned).collect())
  }

}
