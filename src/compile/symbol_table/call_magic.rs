
// Function calls, by default, compile to function calls in GDScript,
// naturally. However, there are many situations where the function on
// the GDScript side is simply a trivial wrapper around some operation
// that should be inlined. This is such a trivial inline step that's
// so ubiquitously useful that we do it here as a rule. If a built-in
// function is called directly (i.e. not through a funcref) then it
// can trigger a special CallMagic which effectively inlines it.

// For a good example, look at the + GDLisp function. In general, it
// compiles to GDLisp.plus, which iterates over its arguments, adds
// them, and returns the result. But, of course, (+ a b) shouldn't
// require a for loop, so any time we call + with an arity known at
// compile time (i.e. without invoking funcrefs or anything like
// that), we can compile directly to the + operator in GDScript, which
// is much more efficient.

// Note that this is *not* general-purpose inlining, which I'll
// implement later as a general pass over the IR. This is for the very
// specific case of certain GDScript functions written in GDLisp.gd
// which I know how to inline effectively by hand.

use dyn_clone::{self, DynClone};

use crate::gdscript::expr::Expr;
use crate::gdscript::op::BinaryOp;
use crate::gdscript::library;
use crate::compile::error::Error;
use crate::util;
use super::function_call::FnCall;

pub trait CallMagic : DynClone {
  fn compile(&self, call: FnCall, args: Vec<Expr>) -> Result<Expr, Error>;
}

dyn_clone::clone_trait_object!(CallMagic);

#[derive(Clone)]
pub struct DefaultCall;

#[derive(Clone)]
pub struct CompileToBinOp {
  pub zero: Expr,
  pub bin: BinaryOp,
  pub assoc: Assoc,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Assoc { Left, Right }

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

impl CallMagic for CompileToBinOp {
  fn compile(&self, _call: FnCall, args: Vec<Expr>) -> Result<Expr, Error> {
    if args.is_empty() {
      Ok(self.zero.clone())
    } else {
      Ok(match self.assoc {
        Assoc::Left => {
          util::fold1(args.into_iter(), |x, y| Expr::Binary(Box::new(x), self.bin, Box::new(y)))
        }
        Assoc::Right => {
          util::fold1(args.into_iter().rev(), |x, y| Expr::Binary(Box::new(y), self.bin, Box::new(x)))
        }
      }.unwrap())
    }
  }
}
