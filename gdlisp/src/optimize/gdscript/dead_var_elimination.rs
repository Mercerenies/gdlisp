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

use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::decl;
use crate::compile::error::GDError;
use super::FunctionOptimization;
use super::stmt_walker;
use super::variables::get_variable_info;

pub struct DeadVarElimination;

impl DeadVarElimination {
  fn run_on_body(&self, stmts: &mut Vec<Stmt>) {
    let vars = get_variable_info(stmts);
    *stmts = stmt_walker::walk_stmts_ok(stmts, stmt_walker::on_each_stmt_ok(|stmt| {
      if let StmtF::VarDecl(var_name, expr) = &stmt.value {
        if let Some(info) = vars.get(var_name) {
          if !info.is_ever_used() {
            return vec!(Stmt::expr(expr.clone()));
          }
        }
      }
      vec!(stmt.clone())
    }));
  }
}

/*
 * If a variable is never used, then we can omit it entirely.
 */
impl FunctionOptimization for DeadVarElimination {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), GDError> {
    self.run_on_body(&mut function.body);
    Ok(())
  }
  fn run_on_init_function(&self, function: &mut decl::InitFnDecl) -> Result<(), GDError> {
    self.run_on_body(&mut function.body);
    Ok(())
  }
}
