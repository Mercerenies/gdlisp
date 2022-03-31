
//! GDScript argument lists.
//!
//! The type [`ArgList`] defines a list of arguments as GDScript sees
//! them.
//!
//! See [`crate::ir::arglist`] for the companion module used during IR
//! analysis.

/// A list of arguments in a function declaration.
///
/// Currently, GDLisp only compiles to required GDScript arguments and
/// does not provide support for optional arguments on the GDScript
/// side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  /// The required arguments.
  pub args: Vec<String>,
}

// TODO Support default arguments

impl ArgList {

  /// An empty argument list.
  pub fn empty() -> ArgList {
    ArgList { args: vec!() }
  }

  /// A list of required arguments.
  pub fn required(args: Vec<String>) -> ArgList {
    ArgList { args }
  }

  /// Whether the argument list is empty.
  pub fn is_empty(&self) -> bool {
    self.args.is_empty()
  }

  /// Returns the list of arguments in GDScript form, i.e. as a
  /// comma-separated list of arguments.
  pub fn to_gd(&self) -> String {
    self.args.join(", ")
  }

}
