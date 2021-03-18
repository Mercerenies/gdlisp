
pub mod noop;
pub mod walker;
pub mod constant;

use crate::gdscript::decl::{self, Decl};
use crate::gdscript::stmt::{self, Stmt};
use crate::compile::error::Error;

pub struct DeadCodeElimination;
pub struct ConstantConditionalBranch;

// Note: If optimization results in an error, the code is guaranteed
// to be in a valid, correct state. It may or may not be rolled back
// to the way it started, but it should perform equivalently at
// runtime.

pub trait FunctionOptimization {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error>;
}

pub trait FileOptimization {
  fn run_on_file(&self, file: &mut decl::TopLevelClass) -> Result<(), Error>;
}

fn on_decl(opt: &impl FunctionOptimization, decl: &mut Decl) -> Result<(), Error> {
  match decl {
    Decl::FnDecl(_, fndecl) => {
      opt.run_on_function(fndecl)
    }
    Decl::ClassDecl(cdecl) => {
      for d in &mut cdecl.body {
        on_decl(opt, d)?;
      }
      Ok(())
    }
    Decl::VarDecl(_, _, _) | Decl::ConstDecl(_, _) | Decl::SignalDecl(_, _) => {
      Ok(())
    }
  }
}

// Every FunctionOptimization is a FileOptimization by applying it to
// each function in the file.
impl<T> FileOptimization for T where T : FunctionOptimization {
  fn run_on_file(&self, file: &mut decl::TopLevelClass) -> Result<(), Error> {
    for d in &mut file.body {
      on_decl(self, d)?;
    }
    Ok(())
  }
}

impl DeadCodeElimination {

  pub fn eliminate(stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    let mut stmt = stmt.clone();

    // Check for empty else clause
    if let Stmt::IfStmt(if_stmt) = &mut stmt {
      if let Some(else_clause) = &if_stmt.else_clause {
        if noop::is_code_seq_noop(&else_clause) {
          if_stmt.else_clause = None;
        }
      }
    }

    Ok(vec!(stmt))
  }

}

impl ConstantConditionalBranch {

  pub fn eliminate(stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    // Check for obviously true or false cases
    if let Stmt::IfStmt(if_stmt) = &stmt {

      // If branch
      if let Some(b) = constant::deduce_bool(&if_stmt.if_clause.0) {
        if b {
          // Definitely true case
          return ConstantConditionalBranch::eliminate_acc(&if_stmt.if_clause.1);
        } else {
          // Definitely false case
          let rest_of_stmt = ConstantConditionalBranch::kill_if_branch(&if_stmt);
          return ConstantConditionalBranch::eliminate_acc(&rest_of_stmt);
        }
      }

      // Elif branches

      // If any are definitely false, we can eliminate them
      let mut v = if_stmt.elif_clauses.clone();
      v.retain(|(e, _)| constant::deduce_bool(e) != Some(false));
      if v != if_stmt.elif_clauses.clone() {
        let mut new_if_stmt = if_stmt.clone();
        new_if_stmt.elif_clauses = v;
        return ConstantConditionalBranch::eliminate(&Stmt::IfStmt(new_if_stmt));
      }

      // If any are definitely true, then they become the else branch
      let match_index = if_stmt.elif_clauses.iter().position(|(e, _)| constant::deduce_bool(e) == Some(true));
      if let Some(match_index) = match_index {
        let mut new_if_stmt = if_stmt.clone();
        new_if_stmt.elif_clauses = if_stmt.elif_clauses[0..match_index].to_vec();
        new_if_stmt.else_clause = Some(if_stmt.elif_clauses[match_index].1.clone());
        return ConstantConditionalBranch::eliminate(&Stmt::IfStmt(new_if_stmt));
      }

    }
    Ok(vec!(stmt.clone()))
  }

  pub fn eliminate_acc(stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(ConstantConditionalBranch::eliminate(stmt)?);
    }
    Ok(result)
  }

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

}

impl FunctionOptimization for DeadCodeElimination {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    function.body = walker::walk_stmts(&function.body, DeadCodeElimination::eliminate)?;
    Ok(())
  }
}

impl FunctionOptimization for ConstantConditionalBranch {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    function.body = walker::walk_stmts(&function.body, ConstantConditionalBranch::eliminate)?;
    Ok(())
  }
}

// TODO We'll refine this a lot. Right now, it's hard coded.
pub fn run_standard_passes(file: &mut decl::TopLevelClass) -> Result<(), Error> {
  ConstantConditionalBranch.run_on_file(file)?;
  DeadCodeElimination.run_on_file(file)?;
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::stmt;
  use crate::gdscript::expr::Expr;
  use crate::gdscript::arglist::ArgList;

  #[test]
  fn dead_code_else_do_not_eliminate() {
    let stmt = stmt::if_else(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))), vec!(Stmt::ReturnStmt(Expr::from(2))));
    let decl = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt) };
    let mut transformed_decl = decl.clone();
    DeadCodeElimination.run_on_function(&mut transformed_decl).unwrap();
    assert_eq!(decl, transformed_decl);
  }

  #[test]
  fn dead_code_else_eliminate() {

    let stmt0 = stmt::if_else(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))), vec!(Stmt::PassStmt));
    let decl0 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt0) };

    let stmt1 = stmt::if_then(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))));
    let decl1 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt1) };

    let mut transformed_decl = decl0.clone();
    DeadCodeElimination.run_on_function(&mut transformed_decl).unwrap();
    assert_eq!(decl1, transformed_decl);
  }

}
