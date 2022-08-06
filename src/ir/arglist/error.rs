
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
  /// An `&` directive was provided but the name was unknown.
  UnknownDirective(String),
  /// An `&` directive appeared in the wrong place in an argument
  /// list, such as attempting to specify `&opt` arguments after
  /// `&rest`.
  DirectiveOutOfOrder(String),
  /// A simple argument list with no directives was expected, but
  /// directives were used.
  SimpleArgListExpected,
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
      ArgListParseErrorF::UnknownDirective(s) => {
        write!(f, "Unknown arglist directive {}", s)
      }
      ArgListParseErrorF::DirectiveOutOfOrder(s) => {
        write!(f, "Arglist directive appeared out of order {}", s)
      }
      ArgListParseErrorF::SimpleArgListExpected => {
        write!(f, "Only simple arglists are allowed in this context")
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
