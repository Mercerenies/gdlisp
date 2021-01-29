
use super::Compiler;
use super::stmt_wrapper::{self, StmtWrapper};
use super::body::builder::StmtBuilder;
use crate::gdscript::expr::Expr;
use crate::gdscript::library;

#[derive(Debug, Clone)]
pub struct StExpr(pub Expr, pub bool); // An expression and a declaration of whether or not it's stateful.

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
      let destination = Box::new(stmt_wrapper::AssignToVar(var_name.clone())) as Box<dyn StmtWrapper>;
      (destination, Expr::Var(var_name))
    } else {
      let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
      (destination, library::nil())
    }
  }
}
