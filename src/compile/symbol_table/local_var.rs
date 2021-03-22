
use crate::ir::locals::AccessType;
use crate::gdscript::expr::Expr;
use crate::gdscript::library::CELL_CONTENTS;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LocalVar {
  pub name: Expr,
  pub access_type: AccessType,
  pub scope: VarScope,
  pub assignable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarScope { GlobalVar, LocalVar }

impl LocalVar {

  pub fn new(name: String, access_type: AccessType, scope: VarScope, assignable: bool) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type, scope, assignable }
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
    LocalVar::new(name, AccessType::Read, VarScope::GlobalVar, true)
  }

  pub fn local(name: String, access_type: AccessType) -> LocalVar {
    LocalVar::new(name, access_type, VarScope::LocalVar, true)
  }

  pub fn self_var() -> LocalVar {
    // Note: Cannot assign to self
    LocalVar::new(String::from("self"), AccessType::ClosedRead, VarScope::LocalVar, false)
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
