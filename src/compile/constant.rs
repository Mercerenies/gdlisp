
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
use crate::gdscript::expr::Expr;
use crate::gdscript::op;

pub trait MaybeConstant {

  fn is_allowable_const(&self) -> bool;

  fn validate_const_expr(&self, name: &str) -> Result<(), Error> {
    if self.is_allowable_const() {
      Ok(())
    } else {
      Err(Error::NotConstantEnough(name.to_owned()))
    }
  }

}

impl MaybeConstant for Expr {
  fn is_allowable_const(&self) -> bool {
    match self {
      Expr::Var(_) => {
        false // TODO Better?
      }
      Expr::Literal(_) => {
        true
      }
      Expr::Subscript(a, b) => {
        a.is_allowable_const() && b.is_allowable_const()
      }
      Expr::Attribute(_, _) => {
        false // I know, but Godot seems to disallow this one on principle
      }
      Expr::Call(_, _, _) | Expr::SuperCall(_, _) => {
        false
      }
      Expr::Unary(_, a) => {
        a.is_allowable_const()
      }
      Expr::Binary(a, op, b) => {
        // So casts don't seem to be considered const in GDScript...
        a.is_allowable_const() && b.is_allowable_const() && *op != op::BinaryOp::Cast
      }
      Expr::TernaryIf(t) => {
        t.true_case.is_allowable_const() && t.cond.is_allowable_const() && t.false_case.is_allowable_const()
      }
      Expr::ArrayLit(arr) => {
        arr.iter().all(Expr::is_allowable_const)
      }
      Expr::DictionaryLit(arr) => {
        arr.iter().all(|(k, v)| k.is_allowable_const() && v.is_allowable_const())
      }
    }
  }
}
