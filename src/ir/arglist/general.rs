
//! Provides the [`GeneralArgList`] type, for the most general type of
//! argument list available.

use crate::sxp::ast::{ASTF, AST};
use crate::ir::special_form::access_slot::AccessSlotSyntax;
use super::vararg::VarArg;
use super::error::{ArgListParseError, ArgListParseErrorF};

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
  pub is_instance_field: bool,
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

  /// A simple argument, with no instance field modifiers.
  pub fn simple(name: String) -> GeneralArg {
    GeneralArg { name, is_instance_field: false }
  }

  /// Parse the argument from an AST of the form `name` or
  /// `(access-slot self name)`. Any other form will be rejected with
  /// an error.
  pub fn parse(ast: &AST) -> Result<GeneralArg, ArgListParseError> {
    if let ASTF::Symbol(name) = &ast.value {
      // Ordinary argument
      Ok(GeneralArg {
        name: name.to_owned(),
        is_instance_field: false,
      })
    } else {
      // Try to parse as `self:foobar`.
      let AccessSlotSyntax { object, slot_name } = parse_access_slot(ast)?;
      verify_self(object)?;
      Ok(GeneralArg {
        name: slot_name,
        is_instance_field: true,
      })
    }
  }

  /// Returns the name, if there are no adornments or modifiers
  /// applied. Returns an error otherwise. This is the retraction of
  /// [`GeneralArg::simple`].
  pub fn into_simple_name(self) -> Result<String, ArgListParseErrorF> {
    if self.is_instance_field {
      Err(ArgListParseErrorF::SimpleArgExpected)
    } else {
      Ok(self.name)
    }
  }

}

/// Equivalent to [`AccessSlotSyntax::parse_ast`], but with the error
/// type converted to [`ArgListParseError`]. The original error's
/// [`SourceOffset`](crate::pipeline::source::SourceOffset) will be
/// preserved.
fn parse_access_slot<'a>(ast: &'a AST) -> Result<AccessSlotSyntax<'a>, ArgListParseError> {
  AccessSlotSyntax::parse_ast(ast)
    .map_err(|err| ArgListParseError::new(ArgListParseErrorF::InvalidArgument(ast.clone()), err.pos))
}

/// Verifies that the AST is literally the symbol `self`.
fn verify_self(ast: &AST) -> Result<(), ArgListParseError> {
  if ast.value == ASTF::Symbol(String::from("self")) {
    Ok(())
  } else {
    Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(ast.clone()), ast.pos))
  }
}
