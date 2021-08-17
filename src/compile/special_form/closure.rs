
//! Helpers for generating closures, for use in constructs such as
//! lambdas and lambda classes.

use crate::ir::expr::{Locals, Functions};
use crate::compile::symbol_table::local_var::VarScope;
use crate::compile::symbol_table::SymbolTable;

type IRArgList = crate::ir::arglist::ArgList;
type IRExpr = crate::ir::expr::Expr;

/// A `ClosureData` contains information about collections of
/// variables and functions to close over.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ClosureData {
  /// The collection of all local variables in-scope which need to be
  /// closed over.
  pub closure_vars: Locals,
  /// The collection of all local variables introduced in the
  /// closure's scope or a strictly larger one. This is similar to
  /// `closed_vars` and will always be a superset of that collection,
  /// but this collection also includes variables which are introduced
  /// as part of the closure's scope, most commonly arguments to the
  /// lambda which created the closure. Such variables do not need to
  /// be closed over (since they do not exist outside the closure),
  /// but it is still meaningful to ask what their `access_type` is
  /// and whether they need a GDLisp cell.
  pub all_vars: Locals,
  /// The collection of all local functions in-scope which need to be
  /// closed over.
  pub closure_fns: Functions,
}

/// A function consists of an argument list and a body expression.
/// This simple wrapper couples the two, so that we can pass them as a
/// pair to [`ClosureData`] methods.
#[derive(Clone, Debug)]
pub struct Function<'a, 'b> {
  pub args: &'a IRArgList,
  pub body: &'b IRExpr,
}

impl ClosureData {

  /// Purges globals from `self.closure_vars`, as though via the
  /// module-level function [`purge_globals`].
  ///
  /// Equivalent to `purge_globals(&mut self.closure_vars, table)`.
  pub fn purge_globals(&mut self, table: &SymbolTable) {
    purge_globals(&mut self.closure_vars, table)
  }

}

impl<'a, 'b> Function<'a, 'b> {

  /// Convenience function to construct a new `Function`.
  pub fn new(args: &'a IRArgList, body: &'b IRExpr) -> Self {
    Function { args, body }
  }

}

impl<'a, 'b> From<Function<'a, 'b>> for ClosureData {

  /// If we're constructing a simple lambda function, we can convert
  /// its [`Function`] value into a [`ClosureData`] in a well-defined
  /// way.
  fn from(function: Function<'a, 'b>) -> ClosureData {
    let (all_vars, closure_fns) = function.body.get_names();
    let mut closure_vars = all_vars.clone();
    for arg in function.args.iter_vars() {
      closure_vars.remove(arg);
    }
    ClosureData { closure_vars, all_vars, closure_fns }
  }

}

/// Removes all of the variables from `vars` whose scope (according to
/// the corresponding entry in `table`) is [`VarScope::GlobalVar`].
///
/// Lambdas are lifted to the file-level scope. A variable with scope
/// `VarScope::GlobalVar` is defined either at the file-level scope or
/// as a superglobal. In either case, the lambda will still have a
/// reference to the variable without any help, so we don't need to
/// close around those variables. This function removes from `vars`
/// the variables which it is unnecessary to explicitly create
/// closures around.
pub fn purge_globals(vars: &mut Locals, table: &SymbolTable) {
  vars.retain(|var, _| {
    table.get_var(var).map_or(true, |v| v.scope != VarScope::GlobalVar)
  });
}
