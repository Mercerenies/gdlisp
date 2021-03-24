
use crate::gdscript::expr::Expr;
use crate::gdscript::decl;
use crate::compile::error::Error;
use super::FunctionOptimization;
use super::constant;
use super::expr_walker;
use super::variables::{VarInfo, Access, get_variable_info};

pub struct DirectVarSubstitute;

// NOTE: This optimization assumes that a local variable is only
// declared once in a given function. I don't know Godot's exact
// scoping rules, but I know the GDLisp compiler ensures uniqueness of
// names in local variables. If we ever stop doing so, we need to
// alter this a bit.

/*
 * If a variable is declared and assigned a constant value, and if the
 * variable is never written to again, then we can simply use the
 * constant value anywhere we would use the variable.
 */
impl FunctionOptimization for DirectVarSubstitute {
  fn run_on_function(&self, function: &mut decl::FnDecl) -> Result<(), Error> {
    let vars = get_variable_info(&function.body);
    function.body = expr_walker::walk_exprs(&function.body, |var_expr| {
      if let Expr::Var(var_name) = var_expr {
        if let Some(VarInfo { access, value }) = vars.get(var_name) {
          if *access == Access::Read && constant::expr_is_constant(&value) {
            return Ok(value.clone());
          }
        }
      }
      Ok(var_expr.clone())
    })?;
    Ok(())
  }
}
