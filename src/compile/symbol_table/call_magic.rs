
// Function calls, by default, compile to function calls in GDScript,
// naturally. However, there are many situations where the function on
// the GDScript side is simply a trivial wrapper around some operation
// that should be inlined. This is such a trivial inline step that's
// so ubiquitously useful that we do it here as a rule. If a built-in
// function is called directly (i.e. not through a funcref) then it
// can trigger a special CallMagic which effectively inlines it.

use dyn_clone::{self, DynClone};

use crate::gdscript::expr::Expr;
use crate::gdscript::library;
use crate::compile::error::Error;
use super::function_call::FnCall;

pub trait CallMagic : DynClone {
  fn compile(&self, call: FnCall, args: Vec<Expr>) -> Result<Expr, Error>;
}

dyn_clone::clone_trait_object!(CallMagic);

#[derive(Clone)]
pub struct DefaultCall;

impl CallMagic for DefaultCall {
  // TODO Currently, this uses the GD name in error messages, which is
  // super wonky, especially for stdlib calls. Store the Lisp name and
  // use it for this.
  fn compile(&self, call: FnCall, mut args: Vec<Expr>) -> Result<Expr, Error> {
    let FnCall { scope: _, object, function, specs } = call;
    // First, check arity
    if args.len() < specs.min_arity() as usize {
      return Err(Error::TooFewArgs(function, args.len()));
    }
    if args.len() > specs.max_arity() as usize {
      return Err(Error::TooManyArgs(function, args.len()));
    }
    let rest = if args.len() < (specs.required + specs.optional) as usize {
      vec!()
    } else {
      args.split_off((specs.required + specs.optional) as usize)
    };
    let rest = library::construct_list(rest);
    // Extend with nulls
    while args.len() < (specs.required + specs.optional) as usize {
      args.push(Expr::null());
    }
    if specs.rest {
      args.push(rest);
    }
    Ok(Expr::Call(object, function, args))
  }
}
