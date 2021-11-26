
use crate::gdscript::expr::ExprF;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl;
use crate::compile::error::Error;
use super::FunctionOptimization;
use super::constant;
use super::expr_walker;
use super::variables::get_variable_info;

pub struct DirectVarSubstitute;

// NOTE: This optimization assumes that a local variable is only
// declared once in a given function. I don't know Godot's exact
// scoping rules, but I know the GDLisp compiler ensures uniqueness of
// names in local variables. If we ever stop doing so, we need to
// alter this a bit.

impl DirectVarSubstitute {
  fn run_on_body(&self, stmts: &mut Vec<Stmt>) -> Result<(), Error> {
    let vars = get_variable_info(stmts);
    *stmts = expr_walker::walk_exprs_ok(stmts, |var_expr| {
      if let ExprF::Var(var_name) = &var_expr.value {
        if let Some(info) = vars.get(var_name) {
          if info.is_read_only() && constant::expr_is_constant(&info.value) {
            return info.value.clone();
          }
        }
      }
      var_expr.clone()
    });
    Ok(())
  }
}

/*
 * If a variable is declared and assigned a constant value, and if the
 * variable is never written to again, then we can simply use the
 * constant value anywhere we would use the variable.
 */
impl FunctionOptimization for DirectVarSubstitute {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    self.run_on_body(&mut function.body)
  }
  fn run_on_init_function(&self, function: &mut decl::InitFnDecl) -> Result<(), Error> {
    self.run_on_body(&mut function.body)
  }
}
