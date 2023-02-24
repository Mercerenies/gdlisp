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

use crate::gdscript::expr::ExprF;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl;
use crate::compile::error::GDError;
use super::FunctionOptimization;
use super::constant;
use super::expr_walker;
use super::variables::get_variable_info;

pub struct DirectVarSubstitute;

// NOTE: This optimization assumes that a local variable is only
// declared once in a given function. I don't know Godot's exact
// scoping rules, but I know the GDLisp compiler ensures uniqueness of
// names in local variables. If we ever stop doing so, we need to
// alter this a bit.

impl DirectVarSubstitute {
  fn run_on_body(&self, stmts: &mut Vec<Stmt>) {
    let vars = get_variable_info(stmts);
    *stmts = expr_walker::walk_exprs_ok(stmts, |var_expr| {
      if let ExprF::Var(var_name) = &var_expr.value {
        if let Some(info) = vars.get(var_name) {
          if info.is_read_only() && constant::expr_is_constant(&info.value) {
            return info.value.clone();
          }
        }
      }
      var_expr.clone()
    });
  }
}

/*
 * If a variable is declared and assigned a constant value, and if the
 * variable is never written to again, then we can simply use the
 * constant value anywhere we would use the variable.
 */
impl FunctionOptimization for DirectVarSubstitute {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), GDError> {
    self.run_on_body(&mut function.body);
    Ok(())
  }
  fn run_on_init_function(&self, function: &mut decl::InitFnDecl) -> Result<(), GDError> {
    self.run_on_body(&mut function.body);
    Ok(())
  }
}
