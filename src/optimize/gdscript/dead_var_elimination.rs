
use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::decl;
use crate::compile::error::Error;
use super::FunctionOptimization;
use super::stmt_walker;
use super::variables::get_variable_info;

pub struct DeadVarElimination;

impl DeadVarElimination {
  fn run_on_body(&self, stmts: &mut Vec<Stmt>) {
    let vars = get_variable_info(stmts);
    *stmts = stmt_walker::walk_stmts_ok(&stmts, stmt_walker::on_each_stmt_ok(|stmt| {
      if let StmtF::VarDecl(var_name, expr) = &stmt.value {
        if let Some(info) = vars.get(var_name) {
          if !info.is_ever_used() {
            return vec!(Stmt::expr(expr.clone()));
          }
        }
      }
      vec!(stmt.clone())
    }));
  }
}

/*
 * If a variable is never used, then we can omit it entirely.
 */
impl FunctionOptimization for DeadVarElimination {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    self.run_on_body(&mut function.body);
    Ok(())
  }
  fn run_on_init_function(&self, function: &mut decl::InitFnDecl) -> Result<(), Error> {
    self.run_on_body(&mut function.body);
    Ok(())
  }
}
