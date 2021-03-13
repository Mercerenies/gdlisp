
// Convenient access to the builtins in GDLisp.gd

pub mod classes;

use super::expr::Expr;
use super::op;
use crate::compile::symbol_table::{SymbolTable, LocalVar};
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic;
use crate::ir::arglist::VarArg;
use crate::ir::identifier::{Id, Namespace};
use classes::GDSCRIPT_CLASS_NAMES;

use std::collections::HashSet;

pub const GDLISP_NAME: &str = "GDLisp";
pub const CELL_CONTENTS: &str = "contents";
pub const CONSTRUCTOR_NAME: &str = "_init";

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

pub fn symbol_class() -> Expr {
  on_gdlisp_root(String::from("Symbol"))
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

  // All built-in global class names
  for name in &GDSCRIPT_CLASS_NAMES {
    table.set_var((*name).to_owned(), LocalVar::global((*name).to_owned()));
  }

  // Cons
  table.set_fn("cons".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, None), FnScope::Global, cons_class(), "new".to_owned()),
               Box::new(call_magic::DefaultCall));

  // intern
  table.set_fn("intern".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, symbol_class(), "new".to_owned()),
               Box::new(call_magic::DefaultCall));

  // Length
  table.set_fn("length".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "length".to_owned()),
               Box::new(call_magic::DefaultCall));

  // Funcall
  table.set_fn("funcall".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "funcall".to_owned()),
               Box::new(call_magic::DefaultCall));

  // + (Addition)
  table.set_fn("+".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "plus".to_owned()),
               Box::new(
                 call_magic::CompileToBinOp {
                   zero: Expr::from(0),
                   bin: op::BinaryOp::Add,
                   assoc: call_magic::Assoc::Left,
                 }));

  // * (Multiplication)
  table.set_fn("*".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "times".to_owned()),
               Box::new(
                 call_magic::CompileToBinOp {
                   zero: Expr::from(1),
                   bin: op::BinaryOp::Times,
                   assoc: call_magic::Assoc::Left,
                 }));

  // - (Subtraction)
  table.set_fn("-".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "minus".to_owned()),
               Box::new(call_magic::MinusOperation));

  // / (Division)
  table.set_fn("/".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "div".to_owned()),
               Box::new(call_magic::DivOperation));

  // div (Integer Division)
  table.set_fn("div".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "intdiv".to_owned()),
               Box::new(call_magic::IntDivOperation));

  // = (Equality)
  table.set_fn("=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "eq".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::Eq }));

  // < (Less Than)
  table.set_fn("<".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "lt".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LT }));

  // > (Greater Than)
  table.set_fn(">".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "gt".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GT }));

  // <= (Less Than or Equal)
  table.set_fn("<=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "le".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LE }));

  // >= (Greater Than or Equal)
  table.set_fn(">=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "ge".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GE }));

  // /= (Not Equal)
  table.set_fn("/=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "ne".to_owned()),
               Box::new(call_magic::NEqOperation { fallback: Box::new(call_magic::DefaultCall) }));

  // not
  table.set_fn("not".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "not_".to_owned()),
               Box::new(call_magic::BooleanNotOperation));

  // list
  table.set_fn("list".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "list".to_owned()),
               Box::new(call_magic::ListOperation));

  // yield
  table.set_fn("yield".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 2, None), FnScope::Global, gdlisp_root(), "yield_".to_owned()),
               Box::new(call_magic::YieldOperation));

  // vector
  table.set_fn("vector".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 1, None), FnScope::Global, gdlisp_root(), "vector".to_owned()),
               Box::new(call_magic::VectorOperation));

  // list->array
  table.set_fn("list->array".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "list_to_array".to_owned()),
               Box::new(call_magic::DefaultCall));

  // array->list
  table.set_fn("array->list".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "array_to_list".to_owned()),
               Box::new(call_magic::DefaultCall));

  // elt (Array element)
  table.set_fn("elt".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, None), FnScope::Global, gdlisp_root(), "elt".to_owned()),
               Box::new(call_magic::ArraySubscript));

}

pub fn all_builtin_names() -> HashSet<Id> {
  // This is a *really* roundabout way of doing this, but whatever.
  // The canonical list is given in bind_builtins, so for the sake of
  // DRY we'll delegate to that function.
  let mut table = SymbolTable::new();
  bind_builtins(&mut table);
  let mut names = HashSet::new();
  for (func, _, _) in table.fns() {
    names.insert(Id::new(Namespace::Function, func.to_owned()));
  }
  for (var, _) in table.vars() {
    names.insert(Id::new(Namespace::Value, var.to_owned()));
  }
  names
}
