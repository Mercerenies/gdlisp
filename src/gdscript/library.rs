
// Convenient access to the builtins in GDLisp.gd

use super::expr::Expr;

pub const GDLISP_NAME: &'static str = "GDLisp";

pub fn gdlisp_root() -> Expr {
  Expr::Var(String::from(GDLISP_NAME))
}

pub fn on_gdlisp_root(name: String) -> Expr {
  Expr::Attribute(Box::new(gdlisp_root()), name)
}
