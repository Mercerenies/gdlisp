
use super::decl::{self, Decl};
use crate::pipeline::can_load::CanLoad;
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::preload_resolver::PreloadResolver;

// Helpers to make sure GDLisp inner classes (and any feature of
// GDLisp that compiles to inner classes, like lambdas and lambda
// classes) can access statics from the enclosing scope.

pub const OUTER_REFERENCE_NAME: &str = "__gdlisp_outer_class";

pub fn add_outer_class_ref_named(inner_class: &mut decl::ClassDecl, resolver: &dyn PreloadResolver, current_file: &impl CanLoad, var_name: String) {
  let current_filename = current_file.current_filename()
    .and_then(|fname| resolver.resolve_preload(&fname))
    .expect("Error identifying current file"); // TODO Expect
  let load_expr = VarName::load_expr(current_filename);
  let var_decl = Decl::VarDecl(None, var_name, Some(load_expr));
  inner_class.body.push(var_decl);
}
