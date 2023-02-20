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

use crate::gdscript::op;
use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::expr::{Expr, ExprF};

#[derive(Clone, Debug)]
pub struct AssignmentStmt<'a> {
  pub assign_type: AssignType,
  pub var_name: &'a str,
  pub expr: &'a Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignType {
  VarDecl,
  Assignment(op::AssignOp),
}

impl<'a> AssignmentStmt<'a> {

  pub fn match_stmt(stmt: &'a Stmt) -> Option<AssignmentStmt<'a>> {
    match &stmt.value {
      StmtF::VarDecl(var_name, expr) => {
        Some(AssignmentStmt { assign_type: AssignType::VarDecl, var_name, expr })
      }
      StmtF::Assign(var_name, op, expr) => {
        if let ExprF::Var(var_name) = &var_name.value {
          Some(AssignmentStmt { assign_type: AssignType::Assignment(*op), var_name, expr })
        } else {
           None
        }
      }
      _ => {
        None
      }
    }
  }

}

impl AssignType {

  pub fn ensure_eq(self) -> Self {
    match self {
      AssignType::VarDecl => AssignType::VarDecl,
      AssignType::Assignment(_) => AssignType::Assignment(op::AssignOp::Eq),
    }
  }

}

impl<'a> From<AssignmentStmt<'a>> for Stmt {

  fn from(stmt: AssignmentStmt) -> Stmt {
    let AssignmentStmt { assign_type, var_name, expr } = stmt;
    let var_name = var_name.to_owned();
    let expr = expr.clone();
    let pos = expr.pos;
    match assign_type {
      AssignType::VarDecl =>
        Stmt::var_decl(var_name, expr, pos),
      AssignType::Assignment(op) =>
        Stmt::new(StmtF::Assign(Box::new(Expr::var(&var_name, expr.pos)), op, Box::new(expr)), pos),
    }
  }

}
