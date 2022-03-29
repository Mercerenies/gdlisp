
//! This module provides convenience functions for checking the length
//! of [`Vec`] vectors of values.
//!
//! This module is *not* a handler for the concept of an "argument
//! list" on a GDLisp function; for that, use [`crate::ir::arglist`]
//! or [`crate::gdscript::arglist`], depending on whether you're
//! concerned about the GDLisp or GDScript representation of an
//! argument list. This module, instead, is for situations where the
//! *compiler* is expecting some predetermined number of arguments,
//! and we'd like a convenient way to check that the argument count is
//! correct and return an error otherwise.

use super::error::{Error, ErrorF};
use crate::pipeline::source::SourceOffset;
use crate::sxp::ast::{AST, ASTF};

use std::fmt;

/// `Expecting` specifies how many arguments are expected. The two
/// bounds are always inclusive. If a function has no upper bound,
/// then [`usize::MAX`] can be used.
///
/// An `Expecting` instance where `maximum < minimum` will accept no
/// argument lists and always produce an error. No effort is made in
/// this module to prevent the construction of such instances.
///
/// See also [`ExpectedShape`], which deals with the expected shape of
/// a single argument, rather than the number of arguments.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Expecting {
  pub minimum: usize,
  pub maximum: usize,
}

/// `ExpectedShape` specifies the type of argument that is expected.
///
/// Whereas [`Expecting`] concerns itself with the number of
/// arguments, this enum concerns itself with the shape of an
/// individual argument.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExpectedShape {
  /// A variable declaration clause for a let expression.
  VarDecl,
  /// A macro declaration clause in a macrolet expression.
  MacroDecl,
  /// A function declaration clause in an flet expression or similar.
  FnDecl,
  /// A valid "extends" clause for a class declaration.
  SuperclassDecl,
  /// A special reference name. See
  /// [`crate::ir::special_form::special_ref_form`].
  SpecialRefValue,
  /// A symbol literal.
  Symbol,
  /// A string literal.
  String,
  /// A nonempty list literal.
  NonemptyList,
  /// A second argument to `yield`. The GDScript `yield` function is
  /// unique in that it has two optional arguments, but if one is
  /// supplied then the other becomes required. This special value
  /// provides an error message specific to that unique case.
  YieldArg,
}

impl Expecting {

  /// Convenience function for constructing general `Expecting`
  /// values.
  pub fn new(minimum: usize, maximum: usize) -> Expecting {
    Expecting { minimum, maximum }
  }

  /// Synonym for [`Expecting::new`].
  pub fn between(minimum: usize, maximum: usize) -> Expecting {
    Expecting::new(minimum, maximum)
  }

  /// An `Expecting` which demands a specific number of arguments.
  pub fn exactly(value: usize) -> Expecting {
    Expecting::new(value, value)
  }

  /// An `Expecting` with no upper bound.
  pub fn at_least(minimum: usize) -> Expecting {
    Expecting::new(minimum, usize::MAX)
  }

  /// An `Expecting` with no lower bound.
  pub fn at_most(maximum: usize) -> Expecting {
    Expecting::new(0, maximum)
  }

  /// Check that the number of arguments is within the bounds
  /// specified by `self`.
  pub fn contains(&self, args_count: usize) -> bool {
    args_count >= self.minimum && args_count <= self.maximum
  }

  /// Check that `self.contains(args_count)`, and if not, raise an
  /// appropriate [`ErrorF::WrongNumberArgs`] with the given `name`
  /// and `pos`.
  pub fn validate_amount(&self, name: &str, pos: SourceOffset, args_count: usize) -> Result<(), Error> {
    if self.contains(args_count) {
      Ok(())
    } else {
      Err(Error::new(ErrorF::WrongNumberArgs(String::from(name), *self, args_count), pos))
    }
  }

  /// Validate against the length of a slice.
  ///
  /// Equivalent to `self.validate_amount(name, pos, slice.len())`.
  pub fn validate<T>(&self, name: &str, pos: SourceOffset, slice: &[T]) -> Result<(), Error> {
    self.validate_amount(name, pos, slice.len())
  }

}

impl ExpectedShape {

  /// Extracts an [`ASTF::Symbol`], or reports an error if the [`AST`]
  /// is not a symbol.
  pub fn extract_symbol(form_name: &str, ast: AST) -> Result<String, Error> {
    let pos = ast.pos;
    match ast.value {
      ASTF::Symbol(s) => Ok(s),
      _ => Err(Error::new(ErrorF::InvalidArg(form_name.to_owned(), ast, ExpectedShape::Symbol), pos)),
    }
  }

  /// Extracts an [`ASTF::String`], or reports an error if the [`AST`]
  /// is not a string literal.
  pub fn extract_string(form_name: &str, ast: AST) -> Result<String, Error> {
    let pos = ast.pos;
    match ast.value {
      ASTF::String(s) => Ok(s),
      _ => Err(Error::new(ErrorF::InvalidArg(form_name.to_owned(), ast, ExpectedShape::String), pos)),
    }
  }

}

impl fmt::Display for Expecting {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.minimum > self.maximum {
      write!(f, "(no valid call)")
    } else if self.minimum == self.maximum {
      write!(f, "exactly {}", self.minimum)
    } else if self.maximum == usize::MAX {
      write!(f, "at least {}", self.minimum)
    } else if self.minimum == usize::MIN {
      write!(f, "at most {}", self.maximum)
    } else {
      write!(f, "{} to {}", self.minimum, self.maximum)
    }
  }
}

impl fmt::Display for ExpectedShape {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ExpectedShape::VarDecl => write!(f, "variable declaration"),
      ExpectedShape::MacroDecl => write!(f, "macro declaration"),
      ExpectedShape::FnDecl => write!(f, "function declaration"),
      ExpectedShape::SuperclassDecl => write!(f, "superclass declaration"),
      ExpectedShape::SpecialRefValue => write!(f, "special reference value"),
      ExpectedShape::Symbol => write!(f, "symbol"),
      ExpectedShape::String => write!(f, "string"),
      ExpectedShape::NonemptyList => write!(f, "nonempty list"),
      ExpectedShape::YieldArg => write!(f, "additional argument (yield takes 0 or 2 arguments)"),
    }
  }
}

/// Panic if the vector is nonempty. This method is provided for
/// symmetry with [`one`], [`two`], and [`three`].
///
/// This is intended to be used as a convenient destructuring method
/// *after* a call to [`Expecting::validate`].
pub fn zero<T>(args: Vec<T>) {
  assert!(!args.is_empty(), "Assertion violated in gdlisp::compile::args::zero");
}

/// Get the single element from the vector, panicking if the length is
/// wrong.
///
/// This is intended to be used as a convenient destructuring method
/// *after* a call to [`Expecting::validate`].
pub fn one<T>(mut args: Vec<T>) -> T {
  assert!(args.len() == 1, "Assertion violated in gdlisp::compile::args::one");
  args.pop().expect("Internal error in gdlisp::compile::args")
}

/// Get the two elements from the argstor, panicking if the length is
/// wrong.
///
/// This is intended to be used as a convenient destructuring method
/// *after* a call to [`Expecting::validate`].
pub fn two<T>(mut args: Vec<T>) -> (T, T) {
  assert!(args.len() == 2, "Assertion violated in gdlisp::compile::args::two");
  let y = args.pop().expect("Internal error in gdlisp::compile::args");
  let x = args.pop().expect("Internal error in gdlisp::compile::args");
  (x, y)
}

/// Get the three elements from the argstor, panicking if the length is
/// wrong.
///
/// This is intended to be used as a convenient destructuring method
/// *after* a call to [`Expecting::validate`].
pub fn three<T>(mut args: Vec<T>) -> (T, T, T) {
  assert!(args.len() == 3, "Assertion violated in gdlisp::compile::args::three");
  let z = args.pop().expect("Internal error in gdlisp::compile::args");
  let y = args.pop().expect("Internal error in gdlisp::compile::args");
  let x = args.pop().expect("Internal error in gdlisp::compile::args");
  (x, y, z)
}
