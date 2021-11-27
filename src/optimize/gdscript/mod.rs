
pub mod noop;
pub mod stmt_walker;
pub mod expr_walker;
pub mod constant;
pub mod variables;
pub mod assignment;
pub mod dead_code_elimination;
pub mod constant_conditional_branch;
pub mod else_then_if_fold;
pub mod basic_math_ops;
pub mod redundant_assignment_elimination;
pub mod direct_var_substitute;
pub mod dead_var_elimination;
pub mod ternary_if_fold;

use crate::gdscript::decl::{self, Decl, DeclF};
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
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
  fn run_on_init_function(&self, function: &mut decl::InitFnDecl) -> Result<(), Error>;
}

pub trait FileOptimization {
  fn run_on_file(&self, file: &mut decl::TopLevelClass) -> Result<(), Error>;
}

// TODO Note that expression-level optimizations won't run on
// ConstDecl, VarDecl, or EnumDecl expressions right now. Also on
// super args in InitFnDecl.
fn on_decl(opt: &impl FunctionOptimization, decl: &mut Decl) -> Result<(), Error> {
  match &mut decl.value {
    DeclF::FnDecl(_, fndecl) => {
      opt.run_on_function(fndecl)
    }
    DeclF::InitFnDecl(fndecl) => {
      opt.run_on_init_function(fndecl)
    }
    DeclF::ClassDecl(cdecl) => {
      for d in &mut cdecl.body {
        on_decl(opt, d)?;
      }
      Ok(())
    }
    DeclF::VarDecl(_, _, _, _) | DeclF::ConstDecl(_, _) | DeclF::SignalDecl(_, _) | DeclF::EnumDecl(_) => {
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
    function.body = stmt_walker::walk_stmts(&function.body, stmt_walker::on_each_stmt(|x| self.run_on_stmt(x)))?;
    Ok(())
  }
  fn run_on_init_function(&self, function: &mut decl::InitFnDecl) -> Result<(), Error> {
    function.body = stmt_walker::walk_stmts(&function.body, stmt_walker::on_each_stmt(|x| self.run_on_stmt(x)))?;
    Ok(())
  }
}

// An ExpressionLevelPass can easily be realized as a StatementLevelPass
impl<T> StatementLevelPass for T where T : ExpressionLevelPass {
  fn run_on_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    expr_walker::walk_expr(stmt, |e| self.run_on_expr(e))
  }
}

// TODO We'll refine this a lot. Right now, it's hard coded.
pub fn run_standard_passes(file: &mut decl::TopLevelClass) -> Result<(), Error> {

  // Run thrice, for good measure :)
  for _ in 0..3 {

    // Fold trivial if statements containing assignments into the variable
    ternary_if_fold::TernaryIfFold.run_on_file(file)?;
    redundant_assignment_elimination::RedundantAssignmentElimination.run_on_file(file)?;
    dead_var_elimination::DeadVarElimination.run_on_file(file)?;
    dead_code_elimination::DeadCodeElimination.run_on_file(file)?;

    // Simplify anything we can at the expression level.
    basic_math_ops::BasicMathOps.run_on_file(file)?;

    // Eliminate and fold conditionals
    else_then_if_fold::ElseThenIfFold.run_on_file(file)?;
    constant_conditional_branch::ConstantConditionalBranch.run_on_file(file)?;
    dead_code_elimination::DeadCodeElimination.run_on_file(file)?;

    // Get rid of unnecessary assignments
    redundant_assignment_elimination::RedundantAssignmentElimination.run_on_file(file)?;

    // Eliminate constant variables
    direct_var_substitute::DirectVarSubstitute.run_on_file(file)?;
    dead_var_elimination::DeadVarElimination.run_on_file(file)?;

    // Another conditional and dead code pass
    else_then_if_fold::ElseThenIfFold.run_on_file(file)?;
    constant_conditional_branch::ConstantConditionalBranch.run_on_file(file)?;
    dead_code_elimination::DeadCodeElimination.run_on_file(file)?;

    // Eliminate dead variables
    dead_var_elimination::DeadVarElimination.run_on_file(file)?;
    dead_code_elimination::DeadCodeElimination.run_on_file(file)?;

  }

  Ok(())
}
