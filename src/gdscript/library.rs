
// Convenient access to the builtins in GDLisp.gd

use super::expr::Expr;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};

pub const GDLISP_NAME: &'static str = "GDLisp";

pub fn gdlisp_root() -> Expr {
  Expr::Var(String::from(GDLISP_NAME))
}

pub fn on_gdlisp_root(name: String) -> Expr {
  Expr::Attribute(Box::new(gdlisp_root()), name)
}

pub fn nil() -> Expr {
  on_gdlisp_root(String::from("Nil"))
}

pub fn cons_class() -> Expr {
  on_gdlisp_root(String::from("Cons"))
}

pub fn bind_builtins(table: &mut SymbolTable) {
  table.set_fn("cons".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, false), FnScope::Global, cons_class(), "new".to_owned()));
}
