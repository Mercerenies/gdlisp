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

use crate::gdscript::stmt::{self, Stmt, StmtF};
use crate::gdscript::expr::{self, Expr, ExprF};
use crate::gdscript::op;
use crate::compile::error::GDError;
use super::StatementLevelPass;

pub struct TernaryIfFold;

/*
 * Factor out common assignment in an if-statement. Specifically, if
 * we have a statement of the form
 *
 * if a:
 *     example = b
 * elif c:
 *     example = c
 * ...
 * else:
 *     example = z
 *
 * Where every branch is a simple assignment to a common variable and
 * none of the conditions depend on that variable in any way.
 */
impl TernaryIfFold {

  fn fold_into_ternary(acc: Expr, next: (Expr, Expr)) -> Expr {
    let pos = acc.pos;
    Expr::from_value(
      expr::TernaryIf {
        true_case: Box::new(next.1),
        cond: Box::new(next.0),
        false_case: Box::new(acc),
      },
      pos,
    )
  }

  fn match_simple_assign<'a>(&self, stmts: &'a [Stmt]) -> Option<(&'a str, &'a Expr)> {
    if let [Stmt { value: StmtF::Assign(lhs, op::AssignOp::Eq, rhs), pos: _ }] = stmts {
      if let ExprF::Var(var_name) = &lhs.value {
        return Some((var_name, rhs));
      }
    }
    None
  }

  fn try_to_run(&self, if_stmt: &stmt::IfStmt) -> Option<Stmt> {
    let stmt::IfStmt { if_clause, elif_clauses, else_clause } = if_stmt;

    // Check if
    if let Some((var_name, if_expr)) = self.match_simple_assign(&if_clause.1) {
      let mut clauses = vec!((if_clause.0.clone(), if_expr.clone()));

      // Check elif
      for (elif_cond, elif_stmts) in elif_clauses {
        if let Some((var_name_elif, elif_expr)) = self.match_simple_assign(elif_stmts) {
          if var_name_elif == var_name {
            clauses.push((elif_cond.clone(), elif_expr.clone()));
            continue;
          }
        }
        return None;
      }

      // Check else (Note: Don't fire if we're missing the else clause)
      let (else_var_name, else_body) = self.match_simple_assign(else_clause.as_ref()?)?;
      if else_var_name != var_name {
        return None;
      }
      let else_body = else_body.clone();

      let final_expr = clauses.into_iter().rev().fold(else_body, TernaryIfFold::fold_into_ternary);
      let final_expr_pos = final_expr.pos;
      return Some(Stmt::simple_assign(Expr::var(var_name, final_expr.pos), final_expr, final_expr_pos));
    }

    None
  }

}

impl StatementLevelPass for TernaryIfFold {
  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, GDError> {
    if let StmtF::IfStmt(if_stmt) = &stmt.value {
      if let Some(stmt) = self.try_to_run(if_stmt) {
        return Ok(vec!(stmt));
      }
    }
    Ok(vec!(stmt.clone()))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  #[test]
  fn ternary_if_fold_test_1() {
    /*
     * if a:
     *   x = 1
     * else:
     *   x = 2
     */
    let stmt1 = s(StmtF::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(1))))))),
      elif_clauses: vec!(),
      else_clause: Some(vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(2))))))),
    }));
    let stmt2 = s(StmtF::Assign(
      Box::new(Expr::var("x", SourceOffset::default())),
      op::AssignOp::Eq,
      Box::new(e(ExprF::TernaryIf(expr::TernaryIf {
        true_case: Box::new(e(ExprF::from(1))),
        cond: Box::new(Expr::var("a", SourceOffset::default())),
        false_case: Box::new(e(ExprF::from(2))),
      }))),
    ));
    assert_eq!(TernaryIfFold.run_on_stmt(&stmt1).unwrap(), vec!(stmt2));
  }

  #[test]
  fn ternary_if_fold_test_2() {
    /*
     * if a:
     *   x = 1
     * elif b:
     *   x = 2
     * else:
     *   x = 3
     */
    let stmt1 = s(StmtF::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(1))))))),
      elif_clauses: vec!((Expr::var("b", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(2)))))))),
      else_clause: Some(vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(3))))))),
    }));
    let stmt2 = s(StmtF::Assign(
      Box::new(Expr::var("x", SourceOffset::default())),
      op::AssignOp::Eq,
      Box::new(e(ExprF::TernaryIf(expr::TernaryIf {
        true_case: Box::new(e(ExprF::from(1))),
        cond: Box::new(Expr::var("a", SourceOffset::default())),
        false_case: Box::new(e(ExprF::TernaryIf(expr::TernaryIf {
          true_case: Box::new(e(ExprF::from(2))),
          cond: Box::new(Expr::var("b", SourceOffset::default())),
          false_case: Box::new(e(ExprF::from(3))),
        }))),
      }))),
    ));
    assert_eq!(TernaryIfFold.run_on_stmt(&stmt1).unwrap(), vec!(stmt2));
  }

  #[test]
  fn ternary_if_fold_test_no_trigger_1() {
    /* (Different variables)
     * if a:
     *   x = 1
     * elif b:
     *   y = 2
     * else:
     *   x = 3
     */
    let stmt1 = s(StmtF::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(1))))))),
      elif_clauses: vec!((Expr::var("b", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("y", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(2)))))))),
      else_clause: Some(vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(3))))))),
    }));
    assert_eq!(TernaryIfFold.run_on_stmt(&stmt1).unwrap(), vec!(stmt1));
  }

  #[test]
  fn ternary_if_fold_test_no_trigger_2() {
    /* (No else)
     * if a:
     *   x = 1
     * elif b:
     *   x = 2
     */
    let stmt1 = s(StmtF::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(1))))))),
      elif_clauses: vec!((Expr::var("b", SourceOffset::default()), vec!(s(StmtF::Assign(Box::new(Expr::var("x", SourceOffset::default())), op::AssignOp::Eq, Box::new(e(ExprF::from(2)))))))),
      else_clause: None,
    }));
    assert_eq!(TernaryIfFold.run_on_stmt(&stmt1).unwrap(), vec!(stmt1));
  }

}
