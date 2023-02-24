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

//! Provides mechanisms for walking GDScript
//! [`Stmt`](crate::gdscript::stmt::Stmt) statements or sequences
//! thereof.
//!
//! The primary endpoints of this module are [`walk_stmt`] and
//! [`walk_stmts`] for traversing, respectively, a single statement or
//! a slice of statements.

use crate::gdscript::stmt::{self, Stmt, StmtF};
use crate::util::extract_err;

struct StmtWalker<'a, E> {
  imp: Box<WalkFn<'a, E>>,
}

type WalkFn<'a, E> = dyn FnMut(&[Stmt]) -> Result<Vec<Stmt>, E> + 'a;

impl<'a, E> StmtWalker<'a, E> {

  fn new(function: impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, E> + 'a) -> StmtWalker<'a, E> {
    StmtWalker { imp: Box::new(function) }
  }

  fn walk_stmts(&mut self, stmts: &[Stmt]) -> Result<Vec<Stmt>, E> {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(self.walk_stmt(stmt)?);
    }
    (self.imp)(&result)
  }

  fn walk_stmt(&mut self, stmt: &Stmt) -> Result<Vec<Stmt>, E> {
    let new_stmt = match &stmt.value {
      StmtF::Expr(_) | StmtF::PassStmt | StmtF::BreakStmt | StmtF::ContinueStmt
        | StmtF::VarDecl(_, _) | StmtF::ReturnStmt(_) | StmtF::Assign(_, _, _) => stmt.value.clone(),
      StmtF::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause }) => {
        let if_clause = (if_clause.0.clone(), self.walk_stmts(&if_clause.1)?);
        let elif_clauses = elif_clauses.iter().map(|(e, s)| self.walk_stmts(s).map(|s| (e.clone(), s))).collect::<Result<_, _>>()?;
        let else_clause = else_clause.as_ref().map(|s| self.walk_stmts(s)).transpose()?;
        StmtF::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause })
      }
      StmtF::ForLoop(stmt::ForLoop { iter_var, collection, body }) => {
        let body = self.walk_stmts(body)?;
        StmtF::ForLoop(stmt::ForLoop {
          iter_var: iter_var.clone(),
          collection: collection.clone(),
          body: body,
        })
      }
      StmtF::WhileLoop(stmt::WhileLoop { condition, body }) => {
        let body = self.walk_stmts(body)?;
        StmtF::WhileLoop(stmt::WhileLoop {
          condition: condition.clone(),
          body: body,
        })
      }
      StmtF::MatchStmt(expr, clauses) => {
        let clauses = clauses.iter().map(|(p, s)| self.walk_stmts(s).map(|s| (p.clone(), s))).collect::<Result<_, _>>()?;
        StmtF::MatchStmt(expr.clone(), clauses)
      }
    };
    Ok(vec!(Stmt::new(new_stmt, stmt.pos)))
  }

}

/// Takes a callable designed to operate on one `&Stmt` and returns a
/// callable which operates on each of a slice `&[Stmt]` of statements
/// individually, effectively mapping the callable over the slice.
///
/// The resulting vectors from each call to `walker` are concatenated
/// together into the final vector result.
pub fn on_each_stmt<'a, E>(mut walker: impl FnMut(&Stmt) -> Result<Vec<Stmt>, E> + 'a)
                           -> impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, E> + 'a {
  move |stmts| {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(walker(stmt)?);
    }
    Ok(result)
  }
}

/// As [`on_each_stmt`] but with no error type.
pub fn on_each_stmt_ok<'a>(mut walker: impl FnMut(&Stmt) -> Vec<Stmt> + 'a)
                           -> impl FnMut(&[Stmt]) -> Vec<Stmt> + 'a {
  let mut new_walker = on_each_stmt(move |x| Ok(walker(x)));
  move |stmts| {
    let result = new_walker(stmts);
    extract_err(result)
  }
}

/// Walks the statement `stmt`, calling `walker` on each sequence of
/// statements in `stmt` (including the sequence consisting of `stmt`
/// itself). The calls are done in postorder, so changes made on the
/// inner structure of the statement during the walk will be reflected
/// in the later calls.
pub fn walk_stmt<'a, E>(stmt: &Stmt, walker: impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, E> + 'a)
                        -> Result<Vec<Stmt>, E> {
  let stmts = vec!(stmt.clone());
  walk_stmts(&stmts[..], walker)
}

/// Walks the sequence of statements `stmts`, calling `walker` on each
/// sequence in `stmts` (including `stmts` itself). The calls are done
/// in postorder, so changes made on the inner structure during the
/// walk will be reflected in later calls.
pub fn walk_stmts<'a, E>(stmts: &[Stmt], walker: impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, E> + 'a)
                         -> Result<Vec<Stmt>, E> {
  let mut walker = StmtWalker::new(walker);
  walker.walk_stmts(stmts)
}

/// As [`walk_stmt`] but with no error type.
pub fn walk_stmt_ok<'a>(stmt: &Stmt, mut walker: impl FnMut(&[Stmt]) -> Vec<Stmt> + 'a)
                        -> Vec<Stmt> {
  let result = walk_stmt(stmt, move |x| Ok(walker(x)));
  extract_err(result)
}

/// As [`walk_stmts`] but with no error type.
pub fn walk_stmts_ok<'a>(stmts: &[Stmt], mut walker: impl FnMut(&[Stmt]) -> Vec<Stmt> + 'a)
                         -> Vec<Stmt> {
  let result = walk_stmts(stmts, move |x| Ok(walker(x)));
  extract_err(result)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::{Expr, ExprF};
  use crate::pipeline::source::SourceOffset;

  // TODO More test coverage

  #[derive(Copy, Clone, Debug, Eq, PartialEq)]
  struct SampleError;

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  #[test]
  fn test_walk_simple_stmt() {
    let stmt = s(StmtF::PassStmt);
    let mut call_result = Vec::new();
    walk_stmt_ok(&stmt, |x| { call_result.push(x.to_vec()); x.to_vec() });
    assert_eq!(call_result, vec!(vec!(s(StmtF::PassStmt))));
  }

  #[test]
  fn test_walk_simple_stmt_seq() {
    let stmts = vec!(s(StmtF::PassStmt), s(StmtF::BreakStmt));
    let mut call_result = Vec::new();
    walk_stmts_ok(&stmts, |x| { call_result.push(x.to_vec()); x.to_vec() });
    assert_eq!(call_result, vec!(vec!(s(StmtF::PassStmt), s(StmtF::BreakStmt))));
  }

  #[test]
  fn test_walk_complex_stmt() {
    let stmt = stmt::if_else(e(ExprF::from(1)), vec!(s(StmtF::PassStmt)), vec!(s(StmtF::BreakStmt)), SourceOffset::default());
    let mut call_result = Vec::new();
    walk_stmt_ok(&stmt, |x| { call_result.push(x.to_vec()); x.to_vec() });
    assert_eq!(call_result, vec!(
      vec!(s(StmtF::PassStmt)),
      vec!(s(StmtF::BreakStmt)),
      vec!(stmt),
    ));
  }

  #[test]
  fn test_walk_complex_stmt_seq() {
    let stmt = stmt::if_else(e(ExprF::from(1)), vec!(s(StmtF::PassStmt)), vec!(s(StmtF::BreakStmt)), SourceOffset::default());
    let stmts = vec!(stmt.clone(), s(StmtF::PassStmt));
    let mut call_result = Vec::new();
    walk_stmts_ok(&stmts, |x| { call_result.push(x.to_vec()); x.to_vec() });
    assert_eq!(call_result, vec!(
      vec!(s(StmtF::PassStmt)),
      vec!(s(StmtF::BreakStmt)),
      vec!(stmt, s(StmtF::PassStmt)),
    ));
  }

  #[test]
  fn test_walk_simple_error_1() {
    let stmts = vec!(s(StmtF::PassStmt));
    let result = walk_stmts(&stmts, on_each_stmt(|_| Err(SampleError)));
    assert_eq!(result, Err(SampleError));
  }

  #[test]
  fn test_walk_simple_error_2() {
    let stmts = vec!();
    let result = walk_stmts(&stmts, on_each_stmt(|_| Err(SampleError)));
    assert_eq!(result, Ok(vec!()));
  }

}
