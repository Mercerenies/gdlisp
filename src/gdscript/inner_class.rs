
use super::decl::{self, Decl};
use crate::pipeline::can_load::CanLoad;

pub const OUTER_REFERENCE_NAME: &str = "__gdlisp_outer_class";

pub fn add_outer_class_ref(inner_class: &mut decl::ClassDecl, current_file: &impl CanLoad) {
  let load_expr = current_file.load_expr().expect("Error identifying current file"); // TODO Expect
  let var_decl = Decl::VarDecl(None, String::from(OUTER_REFERENCE_NAME), Some(load_expr));
  inner_class.body.push(var_decl);
}
