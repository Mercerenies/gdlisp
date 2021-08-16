
//! Factory functions for building some common declaration types.

use super::names::fresh::FreshNameGenerator;
use super::body::builder::StmtBuilder;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::pipeline::source::SourceOffset;

/// Appends (to the builder) a variable declaration statement.
///
/// The variable will have a generated name produced by `gen`. The
/// generated name is returned.
pub fn declare_var<'a>(gen: &mut FreshNameGenerator<'a>,
                       builder: &mut StmtBuilder,
                       prefix: &str,
                       value: Option<Expr>,
                       pos: SourceOffset)
                       -> String {
  let var_name = gen.generate_with(prefix);
  let value = value.unwrap_or(Expr::null(pos));
  builder.append(Stmt::var_decl(var_name.clone(), value, pos));
  var_name
}
