
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::gdscript::decl;
use crate::gdscript::op;
use crate::compile::error::Error;
use super::FunctionOptimization;
use super::constant;
use super::stmt_walker;
use super::assignment::{AssignmentStmt, AssignType};

pub struct RedundantAssignmentElimination;

/*
 * This catches situations like
 *
 *     var example = 0
 *     example = 10
 *
 * and replaces them with
 *
 *     var example = 10
 *
 * Specifically, any variable declaration or (possibly compound)
 * assignment (where the expression is non-stateful), followed by zero
 * or more non-stateful statements, followed by another assignment, is
 * considered redundant and can be culled down to one declaration /
 * assignment.
 */
impl RedundantAssignmentElimination {

  fn match_first_assign<'a>(&self, stmt: &'a Stmt) -> Option<AssignmentStmt<'a>> {
    AssignmentStmt::match_stmt(stmt)
  }

  // The second assignment (the one that makes the first redundant)
  // must be a Stmt::Assign. If it's a Stmt::VarDecl, then it's a
  // Godot error (I think? Might be a shadowed variable? Either way,
  // not our problem)
  fn match_second_assign<'a, 'b, 'c>(&'a self, name: &'b str, stmt: &'c Stmt) -> Option<&'c Expr> {
    if let Some(AssignmentStmt { assign_type, var_name, expr }) = AssignmentStmt::match_stmt(stmt) {
      if assign_type == AssignType::Assignment(op::AssignOp::Eq) && name == var_name {
        return Some(expr);
      }
    }
    None
  }

  fn rebuild_assignment(&self, assign_type: AssignType, var_name: &str, expr: &Expr) -> Stmt {
    AssignmentStmt { assign_type, var_name, expr }.into()
  }

  pub fn run_on_stmts(&self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    // Look for something to eliminate. If we find it, do the
    // elimination and call the function again. If not, we're done.
    for (index, stmt1) in stmts.iter().enumerate() {
      if let Some(AssignmentStmt { assign_type, var_name: name, expr: expr1 }) = self.match_first_assign(stmt1) {
        if !constant::expr_has_side_effects(expr1) {
          // Found a match. Keep going.
          for jndex in index+1..stmts.len() {
            let stmt2 = &stmts[jndex];
            if let Some(expr2) = self.match_second_assign(name, stmt2) {
              // Redundant assignment; cull
              let new_stmt = self.rebuild_assignment(assign_type.ensure_eq(), name, expr2);
              let mut new_stmts = stmts.to_vec();
              new_stmts.splice(index..=jndex, vec!(new_stmt).into_iter());
              return self.run_on_stmts(&new_stmts);
            } else if constant::stmt_has_side_effects(stmt2) {
              // If it's stateful, then we can't do anything
              break;
            }
          }
        }
      }
    }
    // Found nothing to do, so just pass through
    Ok(stmts.to_vec())
  }

}

impl FunctionOptimization for RedundantAssignmentElimination {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    function.body = stmt_walker::walk_stmts(&function.body, |x| self.run_on_stmts(x))?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::arglist::ArgList;

  #[test]
  fn redundant_assign_test_1() {
    /* (Eliminate after decl)
     * var foo = 1
     * foo = 2
     * foo = 3
     */

    let body0 = vec!(
      Stmt::VarDecl(String::from("foo"), Expr::from(1)),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(3))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0,
    };

    let body1 = vec!(
      Stmt::VarDecl(String::from("foo"), Expr::from(3)),
    );
    let func1 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body1,
    };

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_2() {
    /* (Eliminate after decl)
     * var foo = 1
     * "string"
     * null
     * foo = 2
     */

    let body0 = vec!(
      Stmt::VarDecl(String::from("foo"), Expr::from(1)),
      Stmt::Expr(Expr::from("string")),
      Stmt::Expr(Expr::null()),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0,
    };

    let body1 = vec!(
      Stmt::VarDecl(String::from("foo"), Expr::from(2)),
    );
    let func1 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body1,
    };

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_3() {
    /* (Eliminate after assign)
     * foo = 1
     * foo = 2
     */

    let body0 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(1))),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0,
    };

    let body1 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let func1 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body1,
    };

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_4() {
    /* (Eliminate after assign (compound))
     * foo += 1
     * foo = 2
     */

    let body0 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Add, Box::new(Expr::from(1))),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0,
    };

    let body1 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let func1 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body1,
    };

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_no_trigger_1() {
    /* (Do not eliminate if stateful in between)
     * foo = 1
     * fn()
     * foo = 2
     */

    let body0 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(1))),
      Stmt::Expr(Expr::Call(None, String::from("fn"), vec!())),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0.clone(),
    };
    let func1 = func0.clone();

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_no_trigger_2() {
    /* (Do not eliminate if stateful on assign)
     * foo = fn()
     * foo = 2
     */

    let body0 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::Call(None, String::from("fn"), vec!()))),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0.clone(),
    };
    let func1 = func0.clone();

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_no_trigger_3() {
    /* (Do not eliminate if different variables)
     * foo = 1
     * bar = 2
     */

    let body0 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(1))),
      Stmt::Assign(Box::new(Expr::var("bar")), op::AssignOp::Eq, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0.clone(),
    };
    let func1 = func0.clone();

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

  #[test]
  fn redundant_assign_test_no_trigger_4() {
    /* (Do not eliminate if second assignment is compound)
     * foo = 1
     * foo += 2
     */

    let body0 = vec!(
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Eq, Box::new(Expr::from(1))),
      Stmt::Assign(Box::new(Expr::var("foo")), op::AssignOp::Add, Box::new(Expr::from(2))),
    );
    let mut func0 = decl::FnDecl {
      name: String::from("example"),
      args: ArgList::empty(),
      body: body0.clone(),
    };
    let func1 = func0.clone();

    RedundantAssignmentElimination.run_on_function(&mut func0).unwrap();
    assert_eq!(func0, func1);
  }

}
