
// Convenient access to the builtins in GDLisp.gd

use super::expr::Expr;
use super::op;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic;

pub const GDLISP_NAME: &str = "GDLisp";

pub const CELL_CONTENTS: &str = "contents";

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

  // Cons
  table.set_fn("cons".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, false), FnScope::Global, cons_class(), "new".to_owned()),
               Box::new(call_magic::DefaultCall));

  // Length
  table.set_fn("length".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, false), FnScope::Global, gdlisp_root(), "length".to_owned()),
               Box::new(call_magic::DefaultCall));

  // Funcall
  table.set_fn("funcall".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "funcall".to_owned()),
               Box::new(call_magic::DefaultCall));

  // + (Addition)
  table.set_fn("+".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, true), FnScope::Global, gdlisp_root(), "plus".to_owned()),
               Box::new(
                 call_magic::CompileToBinOp {
                   zero: Expr::from(0),
                   bin: op::BinaryOp::Add,
                   assoc: call_magic::Assoc::Left,
                 }));

  // * (Multiplication)
  table.set_fn("*".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, true), FnScope::Global, gdlisp_root(), "times".to_owned()),
               Box::new(
                 call_magic::CompileToBinOp {
                   zero: Expr::from(1),
                   bin: op::BinaryOp::Times,
                   assoc: call_magic::Assoc::Left,
                 }));

  // - (Subtraction)
  table.set_fn("-".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "minus".to_owned()),
               Box::new(call_magic::MinusOperation));

  // / (Division)
  table.set_fn("/".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "div".to_owned()),
               Box::new(call_magic::DivOperation));

  // div (Integer Division)
  table.set_fn("div".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "intdiv".to_owned()),
               Box::new(call_magic::IntDivOperation));

  // = (Equality)
  table.set_fn("=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "eq".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::Eq }));

  // < (Less Than)
  table.set_fn("<".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "lt".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LT }));

  // > (Greater Than)
  table.set_fn(">".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "gt".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GT }));

  // <= (Less Than or Equal)
  table.set_fn("<=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "le".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LE }));

  // >= (Greater Than or Equal)
  table.set_fn(">=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "ge".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GE }));

  // /= (Not Equal)
  table.set_fn("/=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, true), FnScope::Global, gdlisp_root(), "ne".to_owned()),
               Box::new(call_magic::NEqOperation { fallback: Box::new(call_magic::DefaultCall) }));

  // not (Not)
  table.set_fn("not".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, false), FnScope::Global, gdlisp_root(), "not_".to_owned()),
               Box::new(call_magic::BooleanNotOperation));

}
