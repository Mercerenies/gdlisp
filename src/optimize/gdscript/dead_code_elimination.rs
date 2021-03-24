
use crate::gdscript::stmt::Stmt;
use crate::compile::error::Error;
use super::StatementLevelPass;
use super::noop;

pub struct DeadCodeElimination;

// TODO Apply to code after a return
impl StatementLevelPass for DeadCodeElimination {

  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::optimize::gdscript::FunctionOptimization;
  use crate::gdscript::stmt;
  use crate::gdscript::expr::Expr;
  use crate::gdscript::decl;
  use crate::gdscript::arglist::ArgList;

  #[test]
  fn else_do_not_run_on_stmt() {
    /* (Change nothing)
     * if 0:
     *   return 1
     * else:
     *   return 2
     */
    let stmt = stmt::if_else(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))), vec!(Stmt::ReturnStmt(Expr::from(2))));
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

    let stmt0 = stmt::if_else(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))), vec!(Stmt::PassStmt));
    let decl0 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt0) };

    let stmt1 = stmt::if_then(Expr::from(0), vec!(Stmt::ReturnStmt(Expr::from(1))));
    let decl1 = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt1) };

    let mut transformed_decl = decl0.clone();
    DeadCodeElimination.run_on_function(&mut transformed_decl).unwrap();
    assert_eq!(decl1, transformed_decl);
  }

}
