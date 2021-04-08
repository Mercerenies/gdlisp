
use super::decl::{self, Decl};
use super::expr::Expr;
use crate::pipeline::can_load::CanLoad;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::function_call::FnScope;
use crate::compile::symbol_table::local_var::VarName;

// Helpers to make sure GDLisp inner classes (and any feature of
// GDLisp that compiles to inner classes, like lambdas and lambda
// classes) can access statics from the enclosing scope.

pub const OUTER_REFERENCE_NAME: &str = "__gdlisp_outer_class"; // TODO Need to make this unique (so inheritance doesn't clobber it)

pub fn add_outer_class_ref(inner_class: &mut decl::ClassDecl, current_file: &impl CanLoad) {
  let current_filename = current_file.current_filename().expect("Error identifying current file"); // TODO Expect
  let load_expr = VarName::load_expr(current_filename.to_string());
  let var_decl = Decl::VarDecl(None, String::from(OUTER_REFERENCE_NAME), Some(load_expr));
  inner_class.body.push(var_decl);
}

fn modify_global_simple_fns(table: &mut SymbolTable, prefix: Expr) {
  for (_name, call, _magic) in table.fns_mut() {
    if call.scope != FnScope::Superglobal && call.object.is_none() {
      call.object = Some(Box::new(prefix.clone()));
    }
  }
}

pub fn qualify_with_class_ref(table: &mut SymbolTable) {
  modify_global_simple_fns(table, Expr::var(OUTER_REFERENCE_NAME));
}
