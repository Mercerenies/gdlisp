
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::gdscript::decl;
use crate::compile::error::Error;
use super::FunctionOptimization;
use super::constant;
use super::stmt_walker;
use super::expr_walker;

use std::collections::HashMap;

pub struct DirectVarSubstitute;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Access { Read, Write }

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
        if let Some((access, init_expr)) = vars.get(var_name) {
          if *access == Access::Read && constant::expr_is_constant(&init_expr) {
            return Ok(init_expr.clone());
          }
        }
      }
      Ok(var_expr.clone())
    })?;
    Ok(())
  }
}

fn get_variable_info(stmts: &[Stmt]) -> HashMap<String, (Access, Expr)> {
  let mut map = HashMap::new();
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|stmt| {
    match stmt {
      Stmt::VarDecl(s, e) => {
        map.entry(s.to_owned()).or_insert((Access::Read, e.clone()));
      }
      Stmt::Assign(s, _, _) => {
        if let Expr::Var(s) = &**s {
          map.entry(s.to_owned()).and_modify(|v| v.0 = Access::Write);
        }
      }
      _ => {}
    };
    Ok(vec!(stmt.clone())) // Pass through
  })).expect("Internal error in DirectVarSubstitute optimization"); // Cannot fail
  map
}
