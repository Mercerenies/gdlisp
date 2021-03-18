
pub mod noop;
pub mod walker;
pub mod constant;
pub mod dead_code_elimination;
pub mod constant_conditional_branch;
pub mod else_then_if_fold;

use crate::gdscript::decl::{self, Decl};
use crate::gdscript::stmt::Stmt;
use crate::compile::error::Error;

// Note: If optimization results in an error, the code is guaranteed
// to be in a valid, correct state. It may or may not be rolled back
// to the way it started, but it should perform equivalently at
// runtime.

pub trait StatementLevelPass {
  fn eliminate(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error>;
}

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

// A StatementLevelPass is just a local FunctionOptimization
impl<T> FunctionOptimization for T where T : StatementLevelPass {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    function.body = walker::walk_stmts(&function.body, |x| self.eliminate(x))?;
    Ok(())
  }
}

// TODO We'll refine this a lot. Right now, it's hard coded.
pub fn run_standard_passes(file: &mut decl::TopLevelClass) -> Result<(), Error> {
  constant_conditional_branch::ConstantConditionalBranch.run_on_file(file)?;
  dead_code_elimination::DeadCodeElimination.run_on_file(file)?;
  else_then_if_fold::ElseThenIfFold.run_on_file(file)?;
  Ok(())
}
