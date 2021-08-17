
//! Helpers for constructing GDLisp `Cell` objects and wrapping
//! variables in them.
//!
//! The GDLisp `Cell` class is a simple class with only one member
//! variable (called `contents`), and a single 1-argument constructor.
//!
//! Any local variable (including function parameters) may need to be
//! wrapped in a cell. A variable needs to be wrapped in a cell if its
//! [access type](crate::ir::access_type::AccessType) is
//! [`ClosedRW`](crate::ir::access_type::AccessType::ClosedRW). That
//! is, a variable needs to be wrapped in a cell if either of the
//! following is true.
//!
//! * We write to the variable inside of a closure that is strictly
//!   smaller than the scope of the variable.
//!
//! * We write to the variable anywhere AND we read from the variable
//!   inside a closure that is strictly smaller than the scope of the
//!   variable.
//!
//! For details on the motivation for these rules and specifics on how
//! their calculated, see the documentation for
//! [`AccessType`](crate::ir::access_type::AccessType).
//!
//! If we decide that a cell is warranted, then the variable will be
//! wrapped in a cell at construction time. For local variables, this
//! means that the variable's initializer is wrapped in a call to
//! `Cell.new`. For parameters, the first few lines of the function
//! will be assignments of the form `argument_name =
//! Cell.new(argument_name)`. In either case, all access or
//! assignments to these variables will be targeted at
//! `argument_name.contents`.
//!
//! The only exception to this is the construction of closures.
//! Closures are explicitly expected to share cells with their
//! enclosing scope, so when a closure is constructed, the constructor
//! arguments to that closure will be passed as cells directly.

use super::on_gdlisp_root;
use crate::pipeline::source::SourceOffset;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::compile::body::builder::StmtBuilder;

/// The name of the `Cell` class.
pub const CELL_CLASS: &str = "Cell";

/// The name of the sole field in the `Cell` class.
pub const CELL_CONTENTS: &str = "contents";

/// An expression representing the GDLisp `Cell` class.
pub fn cell_class(pos: SourceOffset) -> Expr {
  on_gdlisp_root(String::from("Cell"), pos)
}

/// Given a GDLisp expression, produce an expression which constructs
/// a cell containing it.
pub fn construct_cell(expr: Expr) -> Expr {
  let pos = expr.pos;
  Expr::call(Some(cell_class(pos)), "new", vec!(expr), pos)
}

/// Constructs an assignment statement, assigning the variable with
/// name `arg` to its own value, but wrapped in a cell.
pub fn wrap_var_in_cell(stmt_builder: &mut StmtBuilder, arg: &str, pos: SourceOffset) {
  let var = Expr::var(arg, pos);
  stmt_builder.append(Stmt::simple_assign(var.clone(), construct_cell(var), pos));
}
