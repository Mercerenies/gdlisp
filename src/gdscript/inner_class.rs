
use super::decl::{self, Decl};
use crate::pipeline::can_load::CanLoad;
use crate::compile::symbol_table::SymbolTable;

// Helpers to make sure GDLisp inner classes (and any feature of
// GDLisp that compiles to inner classes, like lambdas and lambda
// classes) can access statics from the enclosing scope.

pub const OUTER_REFERENCE_NAME: &str = "__gdlisp_outer_class";

pub fn add_outer_class_ref(inner_class: &mut decl::ClassDecl, current_file: &impl CanLoad) {
  let load_expr = current_file.load_expr().expect("Error identifying current file"); // TODO Expect
  let var_decl = Decl::VarDecl(None, String::from(OUTER_REFERENCE_NAME), Some(load_expr));
  inner_class.body.push(var_decl);
}
