
// Functionality for checking whether an expression is actually an
// allowable constant expression.
//
// For the moment, what's allowable as a constant expression is
// *extremely* conservative. Even things like if-statements where all
// arguments are constant are disallowed, as we have no way to compile
// that on the GDScript side. Long term, it would be nice to support
// things like that which are constant "in spirit" and work around
// GDScript limitations. But for now, we're being super strict.

use super::error::Error;
use super::symbol_table::local_var::{ValueHint, ValueHintsTable};
use crate::gdscript::expr::Expr;
use crate::gdscript::op;

pub trait MaybeConstant {

  fn is_allowable_const(&self, table: &impl ValueHintsTable) -> bool;

  fn validate_const_expr(&self, name: &str, table: &impl ValueHintsTable) -> Result<(), Error> {
    if self.is_allowable_const(table) {
      Ok(())
    } else {
      Err(Error::NotConstantEnough(name.to_owned()))
    }
  }

}

impl MaybeConstant for Expr {
  fn is_allowable_const(&self, table: &impl ValueHintsTable) -> bool {
    match self {
      Expr::Var(v) => {
        if let Some(value_hint) = table.get_value_hint(v) {
          // Again, playing it super-safe right now. Only superglobals
          // are allowed, and I'll only use sys/declare to define
          // superglobals when I know it's safe. sys/declare is used
          // to bypass the compiler's safety mechanisms, so here is
          // one such backdoor we're putting in.
          *value_hint == ValueHint::Superglobal
        } else {
          false // TODO Better?
        }
      }
      Expr::Literal(_) => {
        true
      }
      Expr::Subscript(a, b) => {
        a.is_allowable_const(table) && b.is_allowable_const(table)
      }
      Expr::Attribute(_, _) => {
        false // I know, but Godot seems to disallow this one on principle
      }
      Expr::Call(obj, name, args) => {
        // Again, very conservative. I know Vector2 and Vector3 are safe.
        #[allow(clippy::collapsible_if)] // (Meh, it's more readable this way IMO)
        if obj.is_none() {
          if name == "Vector2" || name == "Vector3" {
            return args.iter().all(|x| x.is_allowable_const(table));
          }
        }
        false
      }
      Expr::SuperCall(_, _) => {
        false
      }
      Expr::Unary(_, a) => {
        a.is_allowable_const(table)
      }
      Expr::Binary(a, op, b) => {
        // So casts don't seem to be considered const in GDScript...
        a.is_allowable_const(table) && b.is_allowable_const(table) && *op != op::BinaryOp::Cast
      }
      Expr::TernaryIf(t) => {
        t.true_case.is_allowable_const(table) && t.cond.is_allowable_const(table) && t.false_case.is_allowable_const(table)
      }
      Expr::ArrayLit(arr) => {
        arr.iter().all(|x| x.is_allowable_const(table))
      }
      Expr::DictionaryLit(arr) => {
        arr.iter().all(|(k, v)| k.is_allowable_const(table) && v.is_allowable_const(table))
      }
    }
  }
}
