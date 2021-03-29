
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::{self, Expr};
use crate::gdscript::op;
use crate::compile::error::Error;
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
    Expr::from(expr::TernaryIf {
      true_case: Box::new(next.1),
      cond: Box::new(next.0),
      false_case: Box::new(acc),
    })
  }

  fn match_simple_assign<'a>(&self, stmts: &'a [Stmt]) -> Option<(&'a str, &'a Expr)> {
    if let [Stmt::Assign(lhs, op::AssignOp::Eq, rhs)] = stmts {
      if let Expr::Var(var_name) = &**lhs {
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
      return Some(Stmt::Assign(Box::new(Expr::var(var_name)), op::AssignOp::Eq, Box::new(final_expr)));
    }

    None
  }

}

impl StatementLevelPass for TernaryIfFold {
  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    if let Stmt::IfStmt(if_stmt) = &stmt {
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

  #[test]
  fn ternary_if_fold_test_1() {
    /*
     * if a:
     *   x = 1
     * else:
     *   x = 2
     */
    let stmt1 = Stmt::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a"), vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(1))))),
      elif_clauses: vec!(),
      else_clause: Some(vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(2))))),
    });
    let stmt2 = Stmt::Assign(
      Box::new(Expr::var("x")),
      op::AssignOp::Eq,
      Box::new(Expr::TernaryIf(expr::TernaryIf {
        true_case: Box::new(Expr::from(1)),
        cond: Box::new(Expr::var("a")),
        false_case: Box::new(Expr::from(2)),
      })),
    );
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
    let stmt1 = Stmt::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a"), vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(1))))),
      elif_clauses: vec!((Expr::var("b"), vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(2)))))),
      else_clause: Some(vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(3))))),
    });
    let stmt2 = Stmt::Assign(
      Box::new(Expr::var("x")),
      op::AssignOp::Eq,
      Box::new(Expr::TernaryIf(expr::TernaryIf {
        true_case: Box::new(Expr::from(1)),
        cond: Box::new(Expr::var("a")),
        false_case: Box::new(Expr::TernaryIf(expr::TernaryIf {
          true_case: Box::new(Expr::from(2)),
          cond: Box::new(Expr::var("b")),
          false_case: Box::new(Expr::from(3)),
        })),
      })),
    );
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
    let stmt1 = Stmt::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a"), vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(1))))),
      elif_clauses: vec!((Expr::var("b"), vec!(Stmt::Assign(Box::new(Expr::var("y")), op::AssignOp::Eq, Box::new(Expr::from(2)))))),
      else_clause: Some(vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(3))))),
    });
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
    let stmt1 = Stmt::IfStmt(stmt::IfStmt {
      if_clause: (Expr::var("a"), vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(1))))),
      elif_clauses: vec!((Expr::var("b"), vec!(Stmt::Assign(Box::new(Expr::var("x")), op::AssignOp::Eq, Box::new(Expr::from(2)))))),
      else_clause: None,
    });
    assert_eq!(TernaryIfFold.run_on_stmt(&stmt1).unwrap(), vec!(stmt1));
  }

}
