
//! Errors that can occur as a result of GDLisp code.

use crate::sxp;
use crate::sxp::ast::AST;
use crate::ir::arglist::ArgListParseError;
use crate::ir::identifier::Namespace;
use crate::ir::import::{ImportDeclParseError, ImportNameResolutionError};
use crate::ir::modifier::{ParseError as ModifierParseError};
use crate::compile::symbol_table::local_var::VarNameIntoExtendsError;
use crate::runner::path::RPathBuf;
use crate::runner::macro_server::response;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::fmt;

// TODO TooFewArgs and TooManyArgs sometimes use the second argument
// to specify expected # of args and sometimes use it to specify the
// given # of args. Standardize this, likely by taking two arguments.

/// This type captures all errors that can occur during compilation of
/// GDLisp code.
///
/// This type does *not* include I/O errors, which are considered to
/// be outside of GDLisp's purview. This type also excludes parsing
/// errors, for which LALRPOP provides its own error types. See
/// [`crate::pipeline::error`] for an error type which includes this
/// one and is more general.
#[derive(PartialEq, Eq, Debug)]
pub enum ErrorF {
  DottedListError,
  ArgListParseError(ArgListParseError),
  ImportDeclParseError(ImportDeclParseError),
  CannotCall(AST),
  #[deprecated(note="Use WrongNumberArgs instead")]
  TooFewArgs(String, usize),
  #[deprecated(note="Use WrongNumberArgs instead")]
  TooManyArgs(String, usize),
  WrongNumberArgs(String, ArgErrorInfo),
  InvalidArg(String, AST, String), // Function, argument, expected
  NoSuchVar(String),
  NoSuchFn(String),
  NoSuchEnumValue(String, String),
  NoSuchMagic(String),
  UnknownDecl(AST),
  InvalidDecl(AST),
  UnquoteOutsideQuasiquote,
  UnquoteSplicedOutsideQuasiquote,
  BadUnquoteSpliced(AST),
  NoSuchFile(RPathBuf),
  AmbiguousNamespace(String),
  NotConstantEnough(String),
  CannotAssignTo(String),
  CannotExtend(String),
  ExportOnInnerClassVar(String),
  ResourceDoesNotExist(String),
  InvalidImportOnResource(String),
  GodotServerError(response::Failure),
  StaticConstructor,
  StaticMethodOnLambdaClass(String),
  ModifierParseError(ModifierParseError),
  MacroInMinimalistError(String),
  MacroBeforeDefinitionError(String),
}

#[derive(PartialEq, Eq, Debug)]
pub struct ArgErrorInfo {
  pub expected: usize,
  pub actual: usize,
}

/// Variant of [`ErrorF`] with source offset information. See
/// [`Sourced`].
#[derive(PartialEq, Eq, Debug)]
pub struct Error {
  pub value: ErrorF,
  pub pos: SourceOffset,
}

const INTERNAL_ERROR_NOTE: &str = "Note: Unless you're doing something really strange, you should probably report this as a compiler bug";

impl Error {

  pub fn new(value: ErrorF, pos: SourceOffset) -> Error {
    Error { value, pos }
  }

  pub fn from_value<T>(value: T, pos: SourceOffset) -> Error
  where ErrorF: From<T> {
    Error::new(ErrorF::from(value), pos)
  }

}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ErrorF::DottedListError => {
        write!(f, "Unexpected dotted list")
      }
      ErrorF::ArgListParseError(err) => {
        write!(f, "{}", err)
      }
      ErrorF::ImportDeclParseError(err) => {
        write!(f, "{}", err)
      }
      ErrorF::CannotCall(ast) => {
        write!(f, "Cannot make function call on expression {}", ast)
      }
      ErrorF::TooFewArgs(name, _) => {
        write!(f, "Too few arguments to call {}", name)
      }
      ErrorF::TooManyArgs(name, _) => {
        write!(f, "Too many arguments to call {}", name)
      }
      ErrorF::WrongNumberArgs(name, ArgErrorInfo { expected, actual }) => {
        write!(f, "Wrong number of arguments to call {}: expected {}, got {}", name, expected, actual)
      }
      ErrorF::InvalidArg(name, provided, expected) => {
        write!(f, "Invalid argument to {}, given {}, expecting {}", name, provided, expected)
      }
      ErrorF::NoSuchVar(name) => {
        write!(f, "No such variable {}", name)
      }
      ErrorF::NoSuchFn(name) => {
        write!(f, "No such function {}", name)
      }
      ErrorF::NoSuchEnumValue(name, subname) => {
        write!(f, "No such enum value {}:{}", name, subname)
      }
      ErrorF::NoSuchMagic(name) => {
        write!(f, "No such call magic {} ({})", name, INTERNAL_ERROR_NOTE)
      }
      ErrorF::UnknownDecl(ast) => {
        write!(f, "Unknown declaration {}", ast)
      }
      ErrorF::InvalidDecl(ast) => {
        write!(f, "Invalid declaration {}", ast)
      }
      ErrorF::UnquoteOutsideQuasiquote => {
        write!(f, "Unquote (,) can only be used inside quasiquote (`)")
      }
      ErrorF::UnquoteSplicedOutsideQuasiquote => {
        write!(f, "Spliced unquote (,.) can only be used inside quasiquote (`)")
      }
      ErrorF::BadUnquoteSpliced(ast) => {
        write!(f, "Spliced unquote (,.) does not make sense in this context: {}", ast)
      }
      ErrorF::NoSuchFile(p) => {
        write!(f, "Cannot locate file {}", p)
      }
      ErrorF::AmbiguousNamespace(s) => {
        write!(f, "Ambiguous namespace when importing {}", s)
      }
      ErrorF::NotConstantEnough(s) => {
        write!(f, "Expression for constant declaration {} is not constant enough", s)
      }
      ErrorF::CannotAssignTo(s) => {
        write!(f, "Cannot assign to immutable variable {}", s)
      }
      ErrorF::CannotExtend(s) => {
        write!(f, "Cannot extend expression {}", s)
      }
      ErrorF::ExportOnInnerClassVar(v) => {
        write!(f, "Export declarations can only be used on a file's main class, but one was found on {}", v)
      }
      ErrorF::ResourceDoesNotExist(s) => {
        write!(f, "Resource {} does not exist", s)
      }
      ErrorF::InvalidImportOnResource(s) => {
        write!(f, "Cannot use restricted or open import lists on resource import at {}", s)
      }
      ErrorF::GodotServerError(err) => {
        write!(f, "Error during Godot server task execution (error code {}): {}", err.error_code, err.error_string)
      }
      ErrorF::StaticConstructor => {
        write!(f, "Class constructors cannot be static")
      }
      ErrorF::StaticMethodOnLambdaClass(s) => {
        write!(f, "Static method {} is not allowed on anonymous class instance", s)
      }
      ErrorF::ModifierParseError(m) => {
        write!(f, "Modifier error: {}", m)
      }
      ErrorF::MacroInMinimalistError(m) => {
        write!(f, "Attempt to expand macro {} in minimalist file ({})", m, INTERNAL_ERROR_NOTE)
      }
      ErrorF::MacroBeforeDefinitionError(m) => {
        write!(f, "Attempt to use macro {} before definition was available", m)
      }
    }
  }
}

impl Sourced for Error {
  type Item = ErrorF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &ErrorF {
    &self.value
  }

}

impl From<sxp::dotted::TryFromDottedExprError> for ErrorF {
  fn from(_: sxp::dotted::TryFromDottedExprError) -> ErrorF {
    ErrorF::DottedListError
  }
}

impl From<sxp::dotted::TryFromDottedExprError> for Error {
  fn from(err: sxp::dotted::TryFromDottedExprError) -> Error {
    Error::new(ErrorF::DottedListError, err.pos)
  }
}

impl From<ArgListParseError> for ErrorF {
  fn from(err: ArgListParseError) -> ErrorF {
    ErrorF::ArgListParseError(err)
  }
}

impl From<ArgListParseError> for Error {
  fn from(err: ArgListParseError) -> Error {
    let pos = err.pos;
    Error::new(ErrorF::from(err), pos)
  }
}

impl From<ImportDeclParseError> for ErrorF {
  fn from(err: ImportDeclParseError) -> ErrorF {
    ErrorF::ImportDeclParseError(err)
  }
}

impl From<ImportNameResolutionError> for ErrorF {
  fn from(err: ImportNameResolutionError) -> ErrorF {
    match err {
      ImportNameResolutionError::UnknownName(id) => {
        match id.namespace {
          Namespace::Function => ErrorF::NoSuchFn(id.name),
          Namespace::Value => ErrorF::NoSuchVar(id.name),
        }
      }
      ImportNameResolutionError::AmbiguousNamespace(s) => {
        ErrorF::AmbiguousNamespace(s)
      }
    }
  }
}

impl From<VarNameIntoExtendsError> for ErrorF {
  fn from(err: VarNameIntoExtendsError) -> ErrorF {
    match err {
      VarNameIntoExtendsError::CannotExtendLocal(s) => {
        ErrorF::CannotExtend(s)
      }
      VarNameIntoExtendsError::CannotExtendCurrentFile(s) => {
        ErrorF::CannotExtend(s)
      }
    }
  }
}

impl From<response::Failure> for ErrorF {
  fn from(err: response::Failure) -> ErrorF {
    ErrorF::GodotServerError(err)
  }
}

impl From<ModifierParseError> for ErrorF {
  fn from(err: ModifierParseError) -> ErrorF {
    ErrorF::ModifierParseError(err)
  }
}

impl From<ModifierParseError> for Error {
  fn from(err: ModifierParseError) -> Error {
    let pos = err.pos;
    Error::new(ErrorF::from(err), pos)
  }
}
