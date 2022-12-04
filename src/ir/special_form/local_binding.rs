
use crate::ir::expr::{Expr, ExprF, LocalFnClause, FunctionBindingType};
use crate::pipeline::source::SourceOffset;

// flet and labels use a lot of common code and only really differ in
// the way in which bindings work. We factor out those differences
// here so that all of the common code can be written only once over
// in ir::special_form.
//
// TODO I think FunctionBindingType makes this irrelevant. These
// differences can just go over there.

pub struct FLetLocalBinding;
pub struct LabelsLocalBinding;

pub trait LocalBinding {
  fn function_binding_type(&self) -> FunctionBindingType;
  fn has_recursive_bindings(&self) -> bool;

  fn wrap_in_expr(&self, clauses: Vec<LocalFnClause>, body: Box<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::FunctionLet(self.function_binding_type(), clauses, body), pos)
  }

}

impl LocalBinding for FLetLocalBinding {
  fn function_binding_type(&self) -> FunctionBindingType {
    FunctionBindingType::OuterScoped
  }
  fn has_recursive_bindings(&self) -> bool {
    false
  }
}

impl LocalBinding for LabelsLocalBinding {
  fn function_binding_type(&self) -> FunctionBindingType {
    FunctionBindingType::Recursive
  }
  fn has_recursive_bindings(&self) -> bool {
    true
  }
}
