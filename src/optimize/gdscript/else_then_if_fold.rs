
use crate::gdscript::stmt::{Stmt, StmtF};
use crate::compile::error::GDError;
use super::StatementLevelPass;

pub struct ElseThenIfFold;

impl StatementLevelPass for ElseThenIfFold {
  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, GDError> {
    // If we have an else whose body is an if, we can flatten it.
    // This comes up when compiling cond sometimes.
    if let StmtF::IfStmt(if_stmt) = &stmt.value {
      if let Some(else_stmt) = &if_stmt.else_clause {
        if let [Stmt { value: StmtF::IfStmt(inner_if_stmt), pos: _ }] = &else_stmt[..] {
          let mut new_if_stmt = if_stmt.clone();
          new_if_stmt.elif_clauses.push(inner_if_stmt.if_clause.clone());
          for elif in &inner_if_stmt.elif_clauses {
            new_if_stmt.elif_clauses.push(elif.clone());
          }
          new_if_stmt.else_clause = inner_if_stmt.else_clause.clone();
          return Ok(vec!(Stmt::new(StmtF::IfStmt(new_if_stmt), stmt.pos)));
        }
      }
    }
    Ok(vec!(stmt.clone()))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::{Expr, ExprF};
  use crate::gdscript::stmt;
  use crate::gdscript::decl;
  use crate::gdscript::arglist::ArgList;
  use crate::optimize::gdscript::FunctionOptimization;
  use crate::pipeline::source::SourceOffset;

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  #[test]
  fn else_then_if_optimize_test() {
    /* (Merge else and if)
     * if 1:
     *   return 1
     * else:
     *   if 2:
     *     return 2
     */
    let inner_stmt = stmt::if_then(e(ExprF::from(2)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(2))))), SourceOffset::default());
    let stmt = stmt::if_else(e(ExprF::from(1)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(1))))), vec!(inner_stmt), SourceOffset::default());
    let mut decl = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(stmt) };

    let desired_stmt = s(StmtF::IfStmt(stmt::IfStmt {
      if_clause: (e(ExprF::from(1)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(1)))))),
      elif_clauses: vec!((e(ExprF::from(2)), vec!(s(StmtF::ReturnStmt(e(ExprF::from(2))))))),
      else_clause: None,
    }));
    let desired_decl = decl::FnDecl { name: String::from("example"), args: ArgList::empty(), body: vec!(desired_stmt) };

    ElseThenIfFold.run_on_function(&mut decl).unwrap();
    assert_eq!(decl, desired_decl);
  }

}
