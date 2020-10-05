
use crate::gdscript::expr::Expr;

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

  pub fn into_expr(self, args: Vec<Expr>) -> Expr {
    // TODO Handle rest args
    Expr::Call(self.object, self.function, args)
  }

}

impl FnSpecs {

  pub fn new(required: u32, optional: u32, rest: bool) -> FnSpecs {
    FnSpecs { required, optional, rest }
  }

}
