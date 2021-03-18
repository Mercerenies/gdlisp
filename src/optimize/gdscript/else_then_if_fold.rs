
use crate::gdscript::stmt::Stmt;
use crate::compile::error::Error;
use super::StatementLevelPass;

pub struct ElseThenIfFold;

impl StatementLevelPass for ElseThenIfFold {
  fn eliminate(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    // If we have an else whose body is an if, we can flatten it.
    // This comes up when compiling cond sometimes.
    if let Stmt::IfStmt(if_stmt) = &stmt {
      if let Some(else_stmt) = &if_stmt.else_clause {
        if let [Stmt::IfStmt(inner_if_stmt)] = &else_stmt[..] {
          let mut new_if_stmt = if_stmt.clone();
          new_if_stmt.elif_clauses.push(inner_if_stmt.if_clause.clone());
          for elif in &inner_if_stmt.elif_clauses {
            new_if_stmt.elif_clauses.push(elif.clone());
          }
          new_if_stmt.else_clause = inner_if_stmt.else_clause.clone();
          return Ok(vec!(Stmt::IfStmt(new_if_stmt)));
        }
      }
    }
    Ok(vec!(stmt.clone()))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::Expr;
  use crate::gdscript::stmt;
  use crate::gdscript::decl;
  use crate::gdscript::arglist::ArgList;
  use crate::optimize::gdscript::FunctionOptimization;

  #[test]
  fn else_then_if_optimize_test() {
    /* (Merge else and if)
     * if 1:
     *   return 1
     * else:
     *   if 2:
     *     return 2
     */
    let inner_stmt = stmt::if_then(Expr::from(2), vec!(Stmt::ReturnStmt(Expr::from(2))));
    let stmt = stmt::if_else(Expr::from(1), vec!(Stmt::ReturnStmt(Expr::from(1))), vec!(inner_stmt));
    let mut decl = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt) };

    let desired_stmt = Stmt::IfStmt(stmt::IfStmt {
      if_clause: (Expr::from(1), vec!(Stmt::ReturnStmt(Expr::from(1)))),
      elif_clauses: vec!((Expr::from(2), vec!(Stmt::ReturnStmt(Expr::from(2))))),
      else_clause: None,
    });
    let desired_decl = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(desired_stmt) };

    ElseThenIfFold.run_on_function(&mut decl).unwrap();
    assert_eq!(decl, desired_decl);
  }

}
