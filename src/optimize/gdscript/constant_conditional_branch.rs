
pub struct ConstantConditionalBranch;

use crate::gdscript::stmt::{self, Stmt};
use crate::compile::error::Error;
use super::StatementLevelPass;
use super::constant;

// Eliminate the "if" part of the conditional
fn kill_if_branch(if_stmt: &stmt::IfStmt) -> Vec<Stmt> {
  if if_stmt.elif_clauses.is_empty() {
    // No elif, so the else clause becomes the whole conditional
    if_stmt.else_clause.clone().unwrap_or(vec!())
  } else {
    // Use the first elif
    let new_if_stmt = stmt::IfStmt {
      if_clause: if_stmt.elif_clauses[0].clone(),
      elif_clauses: if_stmt.elif_clauses[1..].iter().cloned().collect(),
      else_clause: if_stmt.else_clause.clone(),
    };
    vec!(Stmt::IfStmt(new_if_stmt))
  }
}

impl ConstantConditionalBranch {

  pub fn eliminate_acc(&self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(self.eliminate(stmt)?);
    }
    Ok(result)
  }

}

impl StatementLevelPass for ConstantConditionalBranch {

  fn eliminate(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    // Check for obviously true or false cases
    if let Stmt::IfStmt(if_stmt) = &stmt {

      // If branch
      if let Some(b) = constant::deduce_bool(&if_stmt.if_clause.0) {
        if b {
          // Definitely true case
          return self.eliminate_acc(&if_stmt.if_clause.1);
        } else {
          // Definitely false case
          let rest_of_stmt = kill_if_branch(&if_stmt);
          return self.eliminate_acc(&rest_of_stmt);
        }
      }

      // Elif branches

      // If any are definitely false, we can eliminate them
      let mut v = if_stmt.elif_clauses.clone();
      v.retain(|(e, _)| constant::deduce_bool(e) != Some(false));
      if v != if_stmt.elif_clauses.clone() {
        let mut new_if_stmt = if_stmt.clone();
        new_if_stmt.elif_clauses = v;
        return self.eliminate(&Stmt::IfStmt(new_if_stmt));
      }

      // If any are definitely true, then they become the else branch
      let match_index = if_stmt.elif_clauses.iter().position(|(e, _)| constant::deduce_bool(e) == Some(true));
      if let Some(match_index) = match_index {
        let mut new_if_stmt = if_stmt.clone();
        new_if_stmt.elif_clauses = if_stmt.elif_clauses[0..match_index].to_vec();
        new_if_stmt.else_clause = Some(if_stmt.elif_clauses[match_index].1.clone());
        return self.eliminate(&Stmt::IfStmt(new_if_stmt));
      }

    }
    Ok(vec!(stmt.clone()))
  }

}
