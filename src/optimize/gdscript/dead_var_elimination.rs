
use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::decl;
use crate::compile::error::Error;
use super::FunctionOptimization;
use super::stmt_walker;
use super::variables::get_variable_info;

pub struct DeadVarElimination;

/*
 * If a variable is never used, then we can omit it entirely.
 */
impl FunctionOptimization for DeadVarElimination {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    let vars = get_variable_info(&function.body);
    function.body = stmt_walker::walk_stmts_ok(&function.body, stmt_walker::on_each_stmt_ok(|stmt| {
      if let StmtF::VarDecl(var_name, expr) = &stmt.value {
        if let Some(info) = vars.get(var_name) {
          if !info.is_ever_used() {
            return vec!(Stmt::expr(expr.clone()));
          }
        }
      }
      vec!(stmt.clone())
    }));
    Ok(())
  }
}
