
// Helper for getting information about variable usage in a block of code

use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use super::stmt_walker;
use super::expr_walker;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarInfo {
  pub value: Expr,
  pub read_count: u32,
  pub write_count: u32,
}

impl VarInfo {

  pub fn new(expr: Expr) -> VarInfo {
    VarInfo { value: expr, read_count: 0, write_count: 0 }
  }

  pub fn is_ever_used(&self) -> bool {
    self.read_count > 0 || self.write_count > 0
  }

  pub fn is_read_only(&self) -> bool {
    self.write_count == 0
  }

}

pub fn get_variable_info(stmts: &[Stmt]) -> HashMap<String, VarInfo> {
  let mut map = HashMap::new();

  // Read declarations
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|stmt| {
    if let Stmt::VarDecl(s, e) = stmt {
      map.insert(s.to_owned(), VarInfo::new(e.clone()));
    }
    Ok(vec!(stmt.clone())) // Pass through
  })).expect("Internal error in variables::get_variable_info"); // Cannot fail

  // Read modifications
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|stmt| {
    if let Stmt::Assign(s, _, _) = stmt {
      if let Expr::Var(s) = &**s {
        map.entry(s.to_owned()).and_modify(|v| v.write_count += 1);
      }
    }
    Ok(vec!(stmt.clone())) // Pass through
  })).expect("Internal error in variables::get_variable_info"); // Cannot fail

  // Read accesses
  expr_walker::walk_exprs(stmts, |expr| {
    if let Expr::Var(s) = expr {
      map.entry(s.to_owned()).and_modify(|v| v.read_count += 1);
    }
    Ok(expr.clone()) // Pass through
  }).expect("Internal error in variables::get_variable_info"); // Cannot fail

  map
}
