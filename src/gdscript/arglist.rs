
//! GDScript argument lists.
//!
//! The type [`ArgList`] defines a list of arguments as GDScript sees
//! them.
//!
//! See [`crate::ir::arglist`] for the companion module used during IR
//! analysis.

use std::convert::AsRef;
use std::mem::swap;

/// A list of arguments in a function declaration.
///
/// Currently, GDLisp only compiles to required GDScript arguments and
/// does not provide support for optional arguments on the GDScript
/// side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  /// The required arguments.
  args: Vec<String>,
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

  /// Insert required arguments at the beginning of this argument
  /// list.
  pub fn prepend_required(&mut self, required: impl Iterator<Item=String>) {
    let mut new_vec: Vec<_> = required.collect();
    swap(&mut new_vec, &mut self.args);
    self.args.extend(new_vec);
  }

  /// All of the arguments, required and optional alike, as an
  /// iterator.
  pub fn all_args_iter(&self) -> impl Iterator<Item=&str> {
    self.args.iter().map(AsRef::as_ref)
  }

  /// Returns the list of arguments in GDScript form, i.e. as a
  /// comma-separated list of arguments.
  pub fn to_gd(&self) -> String {
    self.args.join(", ")
  }

}
