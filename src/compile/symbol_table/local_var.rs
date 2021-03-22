
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

  pub fn read(name: String) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type: AccessType::Read, scope: VarScope::LocalVar, assignable: true }
  }

  pub fn rw(name: String) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type: AccessType::RW, scope: VarScope::LocalVar, assignable: true }
  }

  pub fn closed_rw(name: String) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type: AccessType::ClosedRW, scope: VarScope::LocalVar, assignable: true }
  }

  pub fn global(name: String) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type: AccessType::Read, scope: VarScope::GlobalVar, assignable: true }
  }

  pub fn new(name: String, access_type: AccessType, scope: VarScope, assignable: bool) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type, scope, assignable }
  }

  pub fn local(name: String, access_type: AccessType) -> LocalVar {
    LocalVar { name: Expr::Var(name), access_type, scope: VarScope::LocalVar, assignable: true }
  }

  pub fn self_var() -> LocalVar {
    LocalVar {
      name: Expr::var("self"),
      access_type: AccessType::ClosedRead,
      scope: VarScope::LocalVar,
      assignable: false, // Cannot assign to self
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
