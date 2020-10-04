
use crate::gdscript::expr::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
  pub scope: FnScope,
  pub object: Option<Box<Expr>>,
  pub function: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnScope { Local, Global }

impl FnCall {

  pub fn unqualified(scope: FnScope, function: String) -> FnCall {
    FnCall { scope, object: None, function }
  }

  pub fn qualified(scope: FnScope, object: Expr, function: String) -> FnCall {
    FnCall { scope, object: Some(Box::new(object)), function }
  }

  pub fn into_expr(self, args: Vec<Expr>) -> Expr {
    Expr::Call(self.object, self.function, args)
  }

}
