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
use crate::compile::error::GDError;
use super::StatementLevelPass;
use super::noop;

pub struct DeadCodeElimination;

// TODO Apply to code after a return
impl StatementLevelPass for DeadCodeElimination {

  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, GDError> {
    let mut stmt = stmt.clone();

    // If the statement itself has no effect, then omit it entirely
    if noop::is_code_noop(&stmt) {
      return Ok(vec!());
    }

    // Check for empty else clause
    if let StmtF::IfStmt(if_stmt) = &mut stmt.value {
      if let Some(else_clause) = &if_stmt.else_clause {
        if noop::is_code_seq_noop(else_clause) {
          if_stmt.else_clause = None;
        }
      }
    }

    Ok(vec!(stmt))
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::optimize::gdscript::FunctionOptimization;
  use crate::gdscript::stmt;
  use crate::gdscript::expr::{Expr, ExprF};
  use crate::gdscript::decl;
  use crate::gdscript::arglist::ArgList;
  use crate::pipeline::source::SourceOffset;

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  #[test]
  fn else_do_not_run_on_stmt() {
    /* (Change nothing)
     * if 0:
     *   return 1
     * else:
     *   return 2
     */
    let stmt = stmt::if_else(e(ExprF::from(0)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(1))))), vec!(s(StmtF::ReturnStmt(e(ExprF::from(2))))), SourceOffset::default());
    let decl = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt) };
    let mut transformed_decl = decl.clone();
    DeadCodeElimination.run_on_function(&mut transformed_decl).unwrap();
    assert_eq!(decl, transformed_decl);
  }

  #[test]
  fn else_run_on_stmt() {
    /* (Eliminate spurious else)
     * if 0:
     *   return 1
     * else:
     *   pass
     */

    let stmt0 = stmt::if_else(e(ExprF::from(0)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(1))))), vec!(s(StmtF::PassStmt)), SourceOffset::default());
    let decl0 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt0) };

    let stmt1 = stmt::if_then(e(ExprF::from(0)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(1))))), SourceOffset::default());
    let decl1 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt1) };

    let mut transformed_decl = decl0.clone();
    DeadCodeElimination.run_on_function(&mut transformed_decl).unwrap();
    assert_eq!(decl1, transformed_decl);
  }

}
