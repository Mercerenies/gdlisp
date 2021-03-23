
pub mod noop;
pub mod walker;
pub mod constant;
pub mod dead_code_elimination;
pub mod constant_conditional_branch;
pub mod else_then_if_fold;
pub mod basic_math_ops;

use crate::gdscript::decl::{self, Decl};
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::{self, Stmt};
use crate::compile::error::Error;

// Note: If optimization results in an error, the code is guaranteed
// to be in a valid, correct state. It may or may not be rolled back
// to the way it started, but it should perform equivalently at
// runtime.

pub trait ExpressionLevelPass {
  fn run_on_expr(&self, expr: &Expr) -> Result<Expr, Error>;
}

pub trait StatementLevelPass {
  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error>;
}

pub trait FunctionOptimization {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error>;
}

pub trait FileOptimization {
  fn run_on_file(&self, file: &mut decl::TopLevelClass) -> Result<(), Error>;
}

// TODO Note that expression-level optimizations won't run on
// ConstDecl, VarDecl, or EnumDecl expressions right now.
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
    Decl::VarDecl(_, _, _) | Decl::ConstDecl(_, _) | Decl::SignalDecl(_, _) | Decl::EnumDecl(_) => {
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

// A StatementLevelPass is just a local FunctionOptimization
impl<T> FunctionOptimization for T where T : StatementLevelPass {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    function.body = walker::walk_stmts(&function.body, |x| self.run_on_stmt(x))?;
    Ok(())
  }
}

// An ExpressionLevelPass can easily be realized as a StatementLevelPass
impl<T> StatementLevelPass for T where T : ExpressionLevelPass {
  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    let new_stmt = match stmt {
      Stmt::Expr(e) => {
        Stmt::Expr(self.run_on_expr(&e)?)
      }
      Stmt::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause }) => {
        let new_if_stmt = stmt::IfStmt {
          if_clause: (self.run_on_expr(&if_clause.0)?, if_clause.1.clone()),
          elif_clauses: (elif_clauses.iter().map(|(e, s)| Ok((self.run_on_expr(e)?, s.clone()))).collect::<Result<_, Error>>()?),
          else_clause: else_clause.clone(),
        };
        Stmt::IfStmt(new_if_stmt)
      }
      Stmt::ForLoop(stmt::ForLoop { iter_var, collection, body }) => {
        let new_for_loop = stmt::ForLoop {
          iter_var: iter_var.clone(),
          collection: self.run_on_expr(collection)?,
          body: body.clone(),
        };
        Stmt::ForLoop(new_for_loop)
      }
      Stmt::WhileLoop(stmt::WhileLoop { condition, body }) => {
        let new_while_loop = stmt::WhileLoop {
          condition: self.run_on_expr(condition)?,
          body: body.clone(),
        };
        Stmt::WhileLoop(new_while_loop)
      }
      Stmt::MatchStmt(expr, clauses) => {
        Stmt::MatchStmt(self.run_on_expr(expr)?, clauses.clone())
      }
      Stmt::VarDecl(name, expr) => {
        Stmt::VarDecl(name.clone(), self.run_on_expr(expr)?)
      }
      Stmt::ReturnStmt(expr) => {
        Stmt::ReturnStmt(self.run_on_expr(expr)?)
      }
      Stmt::Assign(lhs, op, rhs) => {
        let lhs = self.run_on_expr(&*lhs)?;
        let rhs = self.run_on_expr(&*rhs)?;
        Stmt::Assign(Box::new(lhs), *op, Box::new(rhs))
      }
      Stmt::PassStmt | Stmt::BreakStmt | Stmt::ContinueStmt => {
        stmt.clone()
      }
    };
    Ok(vec!(new_stmt))
  }
}

// TODO We'll refine this a lot. Right now, it's hard coded.
pub fn run_standard_passes(file: &mut decl::TopLevelClass) -> Result<(), Error> {
  constant_conditional_branch::ConstantConditionalBranch.run_on_file(file)?;
  dead_code_elimination::DeadCodeElimination.run_on_file(file)?;
  else_then_if_fold::ElseThenIfFold.run_on_file(file)?;
  basic_math_ops::BasicMathOps.run_on_file(file)?;
  Ok(())
}
