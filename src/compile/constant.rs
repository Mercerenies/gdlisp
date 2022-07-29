
//! Functionality for checking whether an expression is actually an
//! allowable constant expression.
//!
//! For the moment, what's allowable as a constant expression is
//! *extremely* conservative. Even things like if-statements where all
//! arguments are constant are disallowed, as we have no way to
//! compile that on the GDScript side. Long term, it would be nice to
//! support things like that which are constant "in spirit" and work
//! around GDScript limitations. But for now, we're being super
//! strict.

use super::error::{GDError, GDErrorF};
use super::symbol_table::local_var::{ValueHint, ValueHintsTable};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::op;
use crate::pipeline::source::Sourced;

/// Trait representing data which can be checked for a const-ness
/// property.
pub trait MaybeConstant: Sourced {

  /// Returns whether or not the expression is an allowable constant
  /// value. `table` should contain information which can provide
  /// useful hints ([`ValueHint`]) about the sort of value a given
  /// name refers to. If there are no hints to be provided, then
  /// [`VacuousValueHintsTable`](crate::compile::symbol_table::local_var::VacuousValueHintsTable)
  /// can be provided as a null implementation of the trait.
  fn is_allowable_const(&self, table: &impl ValueHintsTable) -> bool;

  /// Check whether [`MaybeConstant::is_allowable_const`] is true. If
  /// not, return [`GDErrorF::NotConstantEnough`]. The `name` is used to
  /// produce a more convenient error message and is not used to
  /// determine whether or not the value is constant.
  fn validate_const_expr(&self, name: &str, table: &impl ValueHintsTable) -> Result<(), GDError> {
    if self.is_allowable_const(table) {
      Ok(())
    } else {
      Err(GDError::new(GDErrorF::NotConstantEnough(name.to_owned()), self.get_source()))
    }
  }

}

impl MaybeConstant for Expr {
  fn is_allowable_const(&self, table: &impl ValueHintsTable) -> bool {
    match &self.value {
      ExprF::Var(v) => {
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
      ExprF::Literal(_) => {
        true
      }
      ExprF::Subscript(a, b) => {
        a.is_allowable_const(table) && b.is_allowable_const(table)
      }
      ExprF::Attribute(_, _) => {
        false // I know, but Godot seems to disallow this one on principle
      }
      ExprF::Call(obj, name, args) => {
        // Again, very conservative. I know Vector2 and Vector3 are safe.
        #[allow(clippy::collapsible_if)] // (Meh, it's more readable this way IMO)
        if obj.is_none() {
          if name == "Vector2" || name == "Vector3" {
            return args.iter().all(|x| x.is_allowable_const(table));
          }
        }
        false
      }
      ExprF::SuperCall(_, _) => {
        false
      }
      ExprF::Unary(_, a) => {
        a.is_allowable_const(table)
      }
      ExprF::Binary(a, op, b) => {
        // So casts don't seem to be considered const in GDScript...
        a.is_allowable_const(table) && b.is_allowable_const(table) && *op != op::BinaryOp::Cast
      }
      ExprF::TernaryIf(t) => {
        t.true_case.is_allowable_const(table) && t.cond.is_allowable_const(table) && t.false_case.is_allowable_const(table)
      }
      ExprF::ArrayLit(arr) => {
        arr.iter().all(|x| x.is_allowable_const(table))
      }
      ExprF::DictionaryLit(arr) => {
        arr.iter().all(|(k, v)| k.is_allowable_const(table) && v.is_allowable_const(table))
      }
    }
  }
}
