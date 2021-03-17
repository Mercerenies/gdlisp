
pub mod noop;
pub mod walker;

use crate::gdscript::decl;
use crate::gdscript::stmt::Stmt;
use crate::compile::error::Error;

pub struct DeadCodeElimination;

// Note: If optimization results in an error, the code is guaranteed
// to be in a valid, correct state. It may or may not be rolled back
// to the way it started, but it should perform equivalently at
// runtime.
pub trait FunctionOptimization {
  fn run(&self, function: &mut decl::FnDecl) -> Result<(), Error>;
}

impl DeadCodeElimination {

  pub fn eliminate(stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    let mut stmt = stmt.clone();
    match &mut stmt {
      Stmt::IfStmt(if_stmt) => {
        if let Some(else_clause) = &if_stmt.else_clause {
          if noop::is_code_seq_noop(&else_clause) {
            if_stmt.else_clause = None;
          }
        }
      }
      _ => {} ////
    }
    Ok(vec!(stmt))
  }

}

impl FunctionOptimization for DeadCodeElimination {
  fn run(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    function.body = walker::walk_stmts(&function.body, DeadCodeElimination::eliminate)?;
    Ok(())
  }
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
    DeadCodeElimination.run(&mut transformed_decl).unwrap();
    assert_eq!(decl, transformed_decl);
  }

  #[test]
  fn dead_code_else_eliminate() {

    let stmt0 = stmt::if_else(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))), vec!(Stmt::PassStmt));
    let decl0 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt0) };

    let stmt1 = stmt::if_then(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))));
    let decl1 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt1) };

    let mut transformed_decl = decl0.clone();
    DeadCodeElimination.run(&mut transformed_decl).unwrap();
    assert_eq!(decl1, transformed_decl);
  }

}
