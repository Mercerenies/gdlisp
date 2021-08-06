
use crate::gdscript::stmt::{self, Stmt, StmtF};
use crate::compile::error::Error;
use crate::pipeline::source::SourceOffset;
use super::StatementLevelPass;
use super::constant;

pub struct ConstantConditionalBranch;

// Eliminate the "if" part of the conditional
fn kill_if_branch(if_stmt: &stmt::IfStmt, pos: SourceOffset) -> Vec<Stmt> {
  if if_stmt.elif_clauses.is_empty() {
    // No elif, so the else clause becomes the whole conditional
    if_stmt.else_clause.clone().unwrap_or_default()
  } else {
    // Use the first elif
    let new_if_stmt = stmt::IfStmt {
      if_clause: if_stmt.elif_clauses[0].clone(),
      elif_clauses: if_stmt.elif_clauses[1..].to_vec(),
      else_clause: if_stmt.else_clause.clone(),
    };
    vec!(Stmt::new(StmtF::IfStmt(new_if_stmt), pos))
  }
}

impl ConstantConditionalBranch {

  pub fn run_on_stmt_acc(&self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(self.run_on_stmt(stmt)?);
    }
    Ok(result)
  }

}

impl StatementLevelPass for ConstantConditionalBranch {

  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    // Check for obviously true or false cases
    if let StmtF::IfStmt(if_stmt) = &stmt.value {

      // If branch
      if let Some(b) = constant::deduce_bool(&if_stmt.if_clause.0) {
        if b {
          // Definitely true case
          return self.run_on_stmt_acc(&if_stmt.if_clause.1);
        } else {
          // Definitely false case
          let rest_of_stmt = kill_if_branch(&if_stmt, stmt.pos);
          return self.run_on_stmt_acc(&rest_of_stmt);
        }
      }

      // Elif branches

      // If any are definitely false, we can run_on_stmt them
      let mut v = if_stmt.elif_clauses.clone();
      v.retain(|(e, _)| constant::deduce_bool(e) != Some(false));
      if v != if_stmt.elif_clauses.clone() {
        let mut new_if_stmt = if_stmt.clone();
        new_if_stmt.elif_clauses = v;
        return self.run_on_stmt(&Stmt::new(StmtF::IfStmt(new_if_stmt), stmt.pos));
      }

      // If any are definitely true, then they become the else branch
      let match_index = if_stmt.elif_clauses.iter().position(|(e, _)| constant::deduce_bool(e) == Some(true));
      if let Some(match_index) = match_index {
        let mut new_if_stmt = if_stmt.clone();
        new_if_stmt.elif_clauses = if_stmt.elif_clauses[0..match_index].to_vec();
        new_if_stmt.else_clause = Some(if_stmt.elif_clauses[match_index].1.clone());
        return self.run_on_stmt(&Stmt::new(StmtF::IfStmt(new_if_stmt), stmt.pos));
      }

    }
    Ok(vec!(stmt.clone()))
  }

}
