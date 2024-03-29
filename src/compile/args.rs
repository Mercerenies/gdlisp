// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

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

use super::error::{GDError, GDErrorF};
use crate::pipeline::source::SourceOffset;
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::literal::Literal;
use crate::ir::special_form::access_slot::ACCESS_SLOT_FORM_NAME;

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
  /// An integer literal.
  Integer,
  /// The empty list, as a literal.
  EmptyList,
  /// Equivalent to `EmptyList` but issues an error message of the
  /// form "expected end of list". This shape should be used in cases
  /// where the full list is not syntactically empty but the tail is
  /// expected to be, for clarity.
  EndOfList,
  /// A nonempty list literal.
  NonemptyList,
  /// A list literal containing a single element.
  SingletonList,
  /// A second argument to `yield`. The GDScript `yield` function is
  /// unique in that it has two optional arguments, but if one is
  /// supplied then the other becomes required. This special value
  /// provides an error message specific to that unique case.
  YieldArg,
  /// A valid instance function name, either a symbol or an expression
  /// of the form `(set name)` or `(get name)`, where `name` is an
  /// arbitrary symbol.
  InstanceFnName,
  /// A symbol, or a pair of symbols as a 2-element list.
  SymbolOrPairOfSymbols,
  /// The `access-slot` literal name.
  AccessSlotName,
}

impl Expecting {

  /// `Expecting` instance which expects no arguments at all.
  pub const NONE: Expecting = Expecting { minimum: 0, maximum: 0 };

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
  /// appropriate [`GDErrorF::WrongNumberArgs`] with the given `name`
  /// and `pos`.
  pub fn validate_amount(&self, name: &str, pos: SourceOffset, args_count: usize) -> Result<(), GDError> {
    if self.contains(args_count) {
      Ok(())
    } else {
      Err(GDError::new(GDErrorF::WrongNumberArgs(String::from(name), *self, args_count), pos))
    }
  }

  /// Validate against the length of a slice.
  ///
  /// Equivalent to `self.validate_amount(name, pos, slice.len())`.
  pub fn validate<T>(&self, name: &str, pos: SourceOffset, slice: &[T]) -> Result<(), GDError> {
    self.validate_amount(name, pos, slice.len())
  }

}

impl ExpectedShape {

  /// Extracts a [`Literal::Symbol`], or reports an error if the
  /// [`AST`] is not a symbol.
  pub fn extract_symbol(form_name: &str, ast: AST) -> Result<String, GDError> {
    let pos = ast.pos;
    match ast.value {
      ASTF::Atom(Literal::Symbol(s)) => Ok(s),
      _ => Err(GDError::new(GDErrorF::InvalidArg(form_name.to_owned(), ast, ExpectedShape::Symbol), pos)),
    }
  }

  /// Extracts a [`Literal::String`], or reports an error if the
  /// [`AST`] is not a string literal.
  pub fn extract_string(form_name: &str, ast: AST) -> Result<String, GDError> {
    let pos = ast.pos;
    match ast.value {
      ASTF::Atom(Literal::String(s)) => Ok(s),
      _ => Err(GDError::new(GDErrorF::InvalidArg(form_name.to_owned(), ast, ExpectedShape::String), pos)),
    }
  }

  /// Extracts a [`Literal::Int`], or reports an error if the [`AST`]
  /// is not an integer literal.
  pub fn extract_i32(form_name: &str, ast: AST) -> Result<i32, GDError> {
    let pos = ast.pos;
    match ast.value {
      ASTF::Atom(Literal::Int(n)) => Ok(n),
      _ => Err(GDError::new(GDErrorF::InvalidArg(form_name.to_owned(), ast, ExpectedShape::Integer), pos)),
    }
  }

  /// Validates that the vector is in fact empty. If it is, this
  /// function returns `()` harmlessly. If it is nonempty, then this
  /// function produces an appropriate error about the expected shape
  /// with [`ExpectedShape::EmptyList`].
  ///
  /// This function takes a `&[&AST]` to be compatible with the output
  /// of [`DottedExpr`](crate::sxp::dotted::DottedExpr).
  pub fn validate_empty(form_name: &str, lst: &[&AST], pos: SourceOffset) -> Result<(), GDError> {
    if lst.is_empty() {
      Ok(())
    } else {
      // Note: Report error as `lst[0].pos`, since that's where the proof of nonempty-ness started.
      let err_pos = lst[0].pos;
      let lst: Vec<_> = lst.iter().map(|x| (*x).to_owned()).collect();
      Err(GDError::new(GDErrorF::InvalidArg(form_name.to_owned(), AST::list(lst, pos), ExpectedShape::EmptyList), err_pos))
    }
  }

  /// Equivalent to [`validate_empty`](ExpectedShape::validate_empty)
  /// but produces [`ExpectedShape::EndOfList`] as error instead.
  ///
  /// This function takes a `&[&AST]` to be compatible with the output
  /// of [`DottedExpr`](crate::sxp::dotted::DottedExpr).
  pub fn validate_end_of_list(form_name: &str, lst: &[&AST], pos: SourceOffset) -> Result<(), GDError> {
    if lst.is_empty() {
      Ok(())
    } else {
      // Note: Report error as `lst[0].pos`, since that's where the proof of nonempty-ness started.
      let err_pos = lst[0].pos;
      let lst: Vec<_> = lst.iter().map(|x| (*x).to_owned()).collect();
      Err(GDError::new(GDErrorF::InvalidArg(form_name.to_owned(), AST::list(lst, pos), ExpectedShape::EndOfList), err_pos))
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
      ExpectedShape::Integer => write!(f, "integer"),
      ExpectedShape::EmptyList => write!(f, "empty list"),
      ExpectedShape::EndOfList => write!(f, "end of list"),
      ExpectedShape::NonemptyList => write!(f, "nonempty list"),
      ExpectedShape::SingletonList => write!(f, "singleton list"),
      ExpectedShape::YieldArg => write!(f, "additional argument (yield takes 0 or 2 arguments)"),
      ExpectedShape::InstanceFnName => write!(f, "instance function name"),
      ExpectedShape::SymbolOrPairOfSymbols => write!(f, "symbol or 2-element list of symbols"),
      ExpectedShape::AccessSlotName => write!(f, "the literal symbol '{}'", ACCESS_SLOT_FORM_NAME),
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
