
use crate::gdscript::expr::Expr;
use crate::compile::error::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
  pub scope: FnScope,
  pub object: Option<Box<Expr>>,
  pub function: String,
  pub specs: FnSpecs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnScope { Local, Global }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnSpecs {
  pub required: u32,
  pub optional: u32,
  pub rest: bool,
}

impl FnCall {

  pub fn unqualified(specs: FnSpecs, scope: FnScope, function: String) -> FnCall {
    FnCall { specs, scope, object: None, function }
  }

  pub fn qualified(specs: FnSpecs, scope: FnScope, object: Expr, function: String) -> FnCall {
    FnCall { specs, scope, object: Some(Box::new(object)), function }
  }

  // TODO Currently, this uses the GD name in error messages, which is
  // super wonky, especially for stdlib calls. Store the Lisp name and
  // use it for this.
  pub fn into_expr(self, args: Vec<Expr>) -> Result<Expr, Error> {
    // First, check arity
    if args.len() < self.specs.min_arity() as usize {
      return Err(Error::TooFewArgs(self.function, args.len()));
    }
    if args.len() > self.specs.max_arity() as usize {
      return Err(Error::TooManyArgs(self.function, args.len()));
    }
    // TODO Handle rest args
    Ok(Expr::Call(self.object, self.function, args))
  }

}

impl FnSpecs {

  pub fn new(required: u32, optional: u32, rest: bool) -> FnSpecs {
    FnSpecs { required, optional, rest }
  }

  pub fn min_arity(&self) -> u32 {
    self.required
  }

  pub fn max_arity(&self) -> u32 {
    // TODO Is u32.MAX correct here? If we put an upper limit on
    // function arity, use that instead.
    if self.rest { u32::MAX } else { self.required + self.optional }
  }

}
