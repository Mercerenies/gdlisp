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

use crate::ir::expr::{Expr, ExprF, AssignTarget};
use crate::pipeline::source::SourceOffset;

// This enum represents the different ways a (set ...) assignment form
// can expand.
#[derive(Clone, Debug)]
pub enum AssignmentForm {
  Simple(AssignTarget),
  SetterCall(String, Vec<Expr>),
}

impl AssignmentForm {

  pub fn str_to_setter_prefix(name: &str) -> String {
    format!("set-{}", name)
  }

  pub fn into_expr(self, rhs: Expr, pos: SourceOffset) -> Expr {
    match self {
      AssignmentForm::Simple(target) => {
        Expr::new(ExprF::Assign(target, Box::new(rhs)), pos)
      }
      AssignmentForm::SetterCall(f, mut args) => {
        args.insert(0, rhs);
        Expr::call(f, args, pos)
      }
    }
  }

}
