
use crate::ir::expr::Expr;
use crate::ir::arglist::ArgList;

// flet and labels use a lot of common code and only really differ in
// the way in which bindings work. We factor out those differences
// here so that all of the common code can be written only once over
// in ir::special_form.

pub struct FLetLocalBinding;
pub struct LabelsLocalBinding;

pub trait LocalBinding {
  fn wrap_in_expr(&self, clauses: Vec<(String, ArgList, Expr)>, body: Box<Expr>) -> Expr;
  fn has_recursive_bindings(&self) -> bool;
}

impl LocalBinding for FLetLocalBinding {
  fn wrap_in_expr(&self, clauses: Vec<(String, ArgList, Expr)>, body: Box<Expr>) -> Expr {
    Expr::FLet(clauses, body)
  }
  fn has_recursive_bindings(&self) -> bool {
    false
  }
}

impl LocalBinding for LabelsLocalBinding {
  fn wrap_in_expr(&self, clauses: Vec<(String, ArgList, Expr)>, body: Box<Expr>) -> Expr {
    Expr::Labels(clauses, body)
  }
  fn has_recursive_bindings(&self) -> bool {
    true
  }
}
