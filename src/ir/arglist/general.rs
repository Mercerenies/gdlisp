
//! Provides the [`GeneralArgList`] type, for the most general type of
//! argument list available.

use super::vararg::VarArg;

/// `GeneralArgList` is the largest argument list type available in
/// this crate. It is not used directly in GDLisp and is instead
/// converted to one of the several more restrictive types after
/// parsing. Every single argument list in GDLisp is capable of being
/// converted losslessly to `GeneralArgList`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeneralArgList {
  /// The list of required arguments.
  pub required_args: Vec<GeneralArg>,
  /// The list of optional arguments.
  pub optional_args: Vec<GeneralArg>,
  /// The "rest" argument.
  pub rest_arg: Option<(GeneralArg, VarArg)>,
}

/// An argument in a [`GeneralArgList`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeneralArg {
  pub name: String,
}

impl GeneralArgList {

  /// An empty argument list, taking no arguments and accepting no
  /// "rest" argument.
  pub fn empty() -> GeneralArgList {
    GeneralArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: None,
    }
  }

}

impl GeneralArg {

  pub fn new(name: String) -> GeneralArg {
    GeneralArg { name }
  }

}
