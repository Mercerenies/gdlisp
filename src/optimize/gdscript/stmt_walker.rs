
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
  pub imp: Box<WalkFn<'a, E>>,
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
        let body = self.walk_stmts(&body)?;
        StmtF::ForLoop(stmt::ForLoop {
          iter_var: iter_var.clone(),
          collection: collection.clone(),
          body: body,
        })
      }
      StmtF::WhileLoop(stmt::WhileLoop { condition, body }) => {
        let body = self.walk_stmts(&body)?;
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
  walker.walk_stmts(&stmts)
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

// TODO Test me :)
