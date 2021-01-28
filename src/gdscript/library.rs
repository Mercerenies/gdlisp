
// Convenient access to the builtins in GDLisp.gd

use super::expr::Expr;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};

pub const GDLISP_NAME: &'static str = "GDLisp";

pub const CELL_CONTENTS: &'static str = "contents";

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

pub fn cell_class() -> Expr {
  on_gdlisp_root(String::from("Cell"))
}

pub fn construct_list(vec: Vec<Expr>) -> Expr {
  vec.into_iter().rev().fold(nil(), |rest, first| {
    Expr::Call(Some(Box::new(cons_class())), String::from("new"), vec!(first, rest))
  })
}

pub fn construct_cell(expr: Expr) -> Expr {
  Expr::Call(Some(Box::new(cell_class())), String::from("new"), vec!(expr))
}

pub fn bind_builtins(table: &mut SymbolTable) {
  table.set_fn("cons".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, false), FnScope::Global, cons_class(), "new".to_owned()));
  table.set_fn("funcall".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "funcall".to_owned()));
  table.set_fn("+".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, true), FnScope::Global, gdlisp_root(), "plus".to_owned()));
}
