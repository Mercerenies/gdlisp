
use super::Compiler;
use super::stmt_wrapper::{self, StmtWrapper};
use super::body::builder::StmtBuilder;
use crate::gdscript::expr::Expr;
use crate::gdscript::library;
use crate::ir::locals::AccessType;

#[derive(Debug, Clone)]
pub struct StExpr(pub Expr, pub SideEffects); // An expression and a declaration of whether or not it's stateful.

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum SideEffects { None, ReadsState, ModifiesState }

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NeedsResult { No, Yes }

impl From<NeedsResult> for bool {
  fn from(s: NeedsResult) -> bool {
    s == NeedsResult::Yes
  }
}

impl From<bool> for NeedsResult {
  fn from(b: bool) -> NeedsResult {
    if b { NeedsResult::Yes } else { NeedsResult::No }
  }
}

impl NeedsResult {
  pub fn into_destination<'a>(self,
                              compiler: &mut Compiler<'a>,
                              builder: &mut StmtBuilder,
                              prefix: &str)
                              -> (Box<dyn StmtWrapper>, Expr) {
    if self.into() {
      let var_name = compiler.declare_var(builder, prefix, None);
      let destination = Box::new(stmt_wrapper::assign_to_var(var_name.clone())) as Box<dyn StmtWrapper>;
      (destination, Expr::Var(var_name))
    } else {
      let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
      (destination, library::nil())
    }
  }
}

impl SideEffects {
  pub fn reads_state(&self) -> bool {
    *self >= SideEffects::ReadsState
  }
  pub fn modifies_state(&self) -> bool {
    *self >= SideEffects::ModifiesState
  }
}

// If we're writing to a variable, then the side effect is obviously
// ModifiesState and this From instance should not be used. If we're
// reading from a variable, then we can convert the AccessType of that
// variable into a SideEffects by determining whether or not the
// variable is ever modified (x.is_written_to) to figure out if we're
// querying mutable state or simply accessing a constant value.
impl From<AccessType> for SideEffects {
  fn from(x: AccessType) -> SideEffects {
    if x.is_written_to() {
      SideEffects::ReadsState
    } else {
      SideEffects::None
    }
  }
}
