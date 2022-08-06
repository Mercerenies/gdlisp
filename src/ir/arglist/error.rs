
//! Provides [`ArgListParseError`], the type of argument list parse
//! errors.

use crate::sxp::ast::AST;
use crate::pipeline::source::{Sourced, SourceOffset};

use std::fmt;

/// `ArgListParseErrorF` describes the types of errors that can occur
/// when parsing an [`AST`] argument list.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ArgListParseErrorF {
  /// An argument of some specific type was expected but something
  /// else was provided. (TODO Remove this in favor of more specific
  /// errors)
  InvalidArgument(AST),
  /// An `@` (or `self:`) argument was found in a non-constructor
  /// context.
  BadSelf(AST),
  /// An `&` directive was provided but the name was unknown.
  UnknownDirective(String),
  /// An `&` directive appeared in the wrong place in an argument
  /// list, such as attempting to specify `&opt` arguments after
  /// `&rest`.
  DirectiveOutOfOrder(String),
  /// A simple argument list with no directives was expected, but
  /// directives were used.
  SimpleArgListExpected,
  /// A simple (unmodified) argument was expected, but an instance
  /// field was named.
  SimpleArgExpected,
  /// A constructor argument list was expected, but directives were
  /// used.
  ConstructorArgListExpected,
}

/// An [`ArgListParseErrorF`] together with [`SourceOffset`] data.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArgListParseError {
  pub value: ArgListParseErrorF,
  pub pos: SourceOffset,
}

impl ArgListParseError {
  pub fn new(value: ArgListParseErrorF, pos: SourceOffset) -> ArgListParseError {
    ArgListParseError { value, pos }
  }
}

impl fmt::Display for ArgListParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ArgListParseErrorF::InvalidArgument(ast) => {
        write!(f, "Invalid arglist argument {}", ast)
      }
      ArgListParseErrorF::BadSelf(_) => {
        write!(f, "'@' arguments are not supported here")
      }
      ArgListParseErrorF::UnknownDirective(s) => {
        write!(f, "Unknown arglist directive {}", s)
      }
      ArgListParseErrorF::DirectiveOutOfOrder(s) => {
        write!(f, "Arglist directive appeared out of order {}", s)
      }
      ArgListParseErrorF::SimpleArgListExpected => {
        write!(f, "Only simple arglists are allowed in this context")
      }
      ArgListParseErrorF::SimpleArgExpected => {
        write!(f, "Only simple arguments (not instance variables) are allowed in this context")
      }
      ArgListParseErrorF::ConstructorArgListExpected => {
        write!(f, "Only constructor arglists are allowed in this context")
      }
    }
  }
}

impl Sourced for ArgListParseError {
  type Item = ArgListParseErrorF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &ArgListParseErrorF {
    &self.value
  }

}
