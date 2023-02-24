// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

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
