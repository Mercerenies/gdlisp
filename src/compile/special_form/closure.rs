
//! Helpers for generating closures, for use in constructs such as
//! lambdas and lambda classes.

use crate::ir::expr::{Locals, Functions, LambdaClass, LocalFnClause};
use crate::compile::symbol_table::local_var::VarScope;
use crate::compile::symbol_table::SymbolTable;
use crate::pipeline::source::SourceOffset;
use super::lambda::closure_fn_to_gd_var; // TODO Move this function over to this module

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

/// A simple wrapper around a collection of [`LocalFnClause`]. A
/// `labels` SCC is a collection of interconnected local function
/// clauses, which reference each other in a circular fashion. More
/// generally, this structure can be used for any collection of local
/// function clauses, regardless of referencing requirements.
#[repr(transparent)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LabelsComponent<'a, 'b>(pub &'a [&'b LocalFnClause]);

impl ClosureData {

  /// Purges globals from `self.closure_vars`, as though via the
  /// module-level function [`purge_globals`].
  ///
  /// Equivalent to `purge_globals(&mut self.closure_vars, table)`.
  pub fn purge_globals(&mut self, table: &SymbolTable) {
    purge_globals(&mut self.closure_vars, table)
  }

  /// Assuming `self` contains all of the relevant closure information
  /// on the GDLisp side, this function constructs a list of the
  /// necessary variables which need to be looked up on the GDScript
  /// side to construct or utilize the closure.
  ///
  /// Every variable in `self.closure_vars` and every function in
  /// `self.closure_fns` will be considered for inclusion in the
  /// result. Specifically, each variable will be looked up in `table`
  /// and, if the result has a
  /// [`LocalVar::simple_name`](crate::compile::symbol_table::local_var::LocalVar::simple_name),
  /// then it will be included in the returned translation vector.
  /// Likewise, each function is looked up in `table` and, if
  /// [`closure_fn_to_gd_var`] returns a name, then that name is added
  /// to the translation vector.
  ///
  /// `self.all_vars` is not considered during this calculation.
  ///
  /// This function can be used in two similar ways. By passing the
  /// lambda symbol table as `table` to this function, the resulting
  /// vector will contain the GDScript names used to refer to the
  /// closure variables from *within* the closure. This is useful for
  /// building the lambda's constructor function. By passing the
  /// enclosing outer table as `table`, on the other hand, the
  /// resulting vector will contain the names used to refer to the
  /// closure variables from the scope *surrounding* the closure. This
  /// is useful for building the expression that will *call* the
  /// lambda's constructor function.
  ///
  /// # Panics
  ///
  /// It is a precondition of this function that every name in
  /// `self.closure_vars` and in `self.closure_fns` appears in `table`
  /// under the appropriate namespace. If any names are missing, then
  /// this function will panic.
  pub fn to_gd_closure_vars(&self, table: &SymbolTable) -> Vec<String> {
    let mut gd_closure_vars = Vec::new();

    // Get closure variables
    for lisp_name in self.closure_vars.names() {
      let var = table.get_var(lisp_name).unwrap_or_else(|| {
        panic!("Internal error compiling lambda variable {}", lisp_name);
      });
      if let Some(name) = var.simple_name() {
        gd_closure_vars.push(name.to_owned());
      }
    }

    // Get closure functions (functions also get moved to the variable
    // namespace when closed around, since GDScript doesn't treat
    // function names as first-class objects)
    for lisp_name in self.closure_fns.names() {
      let (call, _) = table.get_fn(lisp_name).unwrap_or_else(|| {
        panic!("Internal error compiling lambda variable {}", lisp_name);
      });
      if let Some(var) = closure_fn_to_gd_var(call) {
        gd_closure_vars.push(var);
      }
    }

    gd_closure_vars
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

impl<'a> From<&'a LambdaClass> for ClosureData {

  /// A lambda class involves closing around any variables inside of
  /// it, similar to a lambda function. The lambda class case is
  /// somewhat more complex, as it consists of multiple functions and,
  /// in general, a `self` variable that gets implicitly overwritten
  /// by the class' `self`.
  fn from(class: &'a LambdaClass) -> ClosureData {
    let (mut closure_vars, mut closure_fns) = class.constructor_or_default(SourceOffset::from(0)).get_names();
    for d in &class.decls {
      let (decl_vars, decl_fns) = d.get_names();
      closure_vars.merge_with(decl_vars);
      closure_fns.merge_with(decl_fns);
    }
    closure_vars.remove("self"); // Don't close around self; we get a new self
    ClosureData { closure_vars: closure_vars.clone(), all_vars: closure_vars, closure_fns }
  }

}

impl<'a, 'b> From<LabelsComponent<'a, 'b>> for ClosureData {

  /// A strongly-connected component (SCC) of a `labels` clause is a
  /// collection of interconnected local function clauses. Those
  /// function clauses each have their own closure, and together they
  /// have a common closure for the object which encapsulates them.
  ///
  /// Note that all of the function names in the SCC are in scope for
  /// the duration of all of the function bodies, so the function
  /// names themselves will never appear in the resulting
  /// `closure_fns`.
  fn from(comp: LabelsComponent<'a, 'b>) -> ClosureData {
    let LabelsComponent(clauses) = comp;
    let mut closure_vars = Locals::new();
    let mut closure_fns = Functions::new();
    let mut all_vars = Locals::new();

    for clause in clauses {
      let (mut inner_vars, inner_fns) = clause.body.get_names();
      all_vars.merge_with(inner_vars.clone());
      for arg in clause.args.iter_vars() {
        inner_vars.remove(arg);
      }
      closure_vars.merge_with(inner_vars);
      closure_fns.merge_with(inner_fns);
    }

    // Function names are in scope for the duration of their own bodies
    for clause in clauses {
      closure_fns.remove(&clause.name);
    }

    ClosureData { closure_vars, all_vars, closure_fns }
  }

}

/// Removes all of the variables from `vars` whose scope (according to
/// the corresponding entry in `table`) is
/// [`VarScope::GlobalVar`](crate::compile::symbol_table::local_var::VarScope::GlobalVar).
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
