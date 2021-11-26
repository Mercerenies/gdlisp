
// Helper for getting information about variable usage in a block of code

use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::stmt::{Stmt, StmtF};
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
  stmt_walker::walk_stmts_ok(stmts, stmt_walker::on_each_stmt_ok(|stmt| {
    if let StmtF::VarDecl(s, e) = &stmt.value {
      map.insert(s.to_owned(), VarInfo::new(e.clone()));
    }
    vec!(stmt.clone()) // Pass through
  }));

  // Read modifications
  stmt_walker::walk_stmts_ok(stmts, stmt_walker::on_each_stmt_ok(|stmt| {
    if let StmtF::Assign(s, _, _) = &stmt.value {
      if let ExprF::Var(s) = &s.value {
        map.entry(s.to_owned()).and_modify(|v| v.write_count += 1);
      }
    }
    vec!(stmt.clone()) // Pass through
  }));

  // Read accesses
  expr_walker::walk_exprs_ok(stmts, |expr| {
    if let ExprF::Var(s) = &expr.value {
      map.entry(s.to_owned()).and_modify(|v| v.read_count += 1);
    }
    expr.clone() // Pass through
  });

  map
}
