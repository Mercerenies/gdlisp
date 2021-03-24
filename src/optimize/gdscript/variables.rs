
// Helper for getting information about variable usage in a block of code

use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use super::stmt_walker;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarInfo {
  pub access: Access,
  pub value: Expr,
}

// Note: No closed access because we're doing GDScript optimizations
// and GDScript doesn't have closures.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Access { Read, Write }

pub fn get_variable_info(stmts: &[Stmt]) -> HashMap<String, VarInfo> {
  let mut access_map = HashMap::new();
  let mut value_map = HashMap::new();
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|stmt| {
    match stmt {
      Stmt::VarDecl(s, e) => {
        value_map.insert(s.to_owned(), e.clone());
        access_map.entry(s.to_owned()).or_insert(Access::Read);
      }
      Stmt::Assign(s, _, _) => {
        if let Expr::Var(s) = &**s {
          access_map.insert(s.to_owned(), Access::Write);
        }
      }
      _ => {}
    };
    Ok(vec!(stmt.clone())) // Pass through
  })).expect("Internal error in variables::get_variable_info"); // Cannot fail
  let mut map = HashMap::new();
  for (k, access) in access_map {
    if let Some(value) = value_map.get(&k) {
      map.insert(k.to_owned(), VarInfo { access, value: value.to_owned() });
    }
  }
  map
}
