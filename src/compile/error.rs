
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
#[derive(Debug)]
pub enum Error {
  DottedListError,
  ArgListParseError(ArgListParseError),
  ImportDeclParseError(ImportDeclParseError),
  CannotCall(AST),
  TooFewArgs(String, usize),
  TooManyArgs(String, usize),
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

const INTERNAL_ERROR_NOTE: &str = "Note: Unless you're doing something really strange, you should probably report this as a compiler bug";

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::DottedListError => {
        write!(f, "Unexpected dotted list")
      }
      Error::ArgListParseError(err) => {
        write!(f, "{}", err)
      }
      Error::ImportDeclParseError(err) => {
        write!(f, "{}", err)
      }
      Error::CannotCall(ast) => {
        write!(f, "Cannot make function call on expression {}", ast)
      }
      Error::TooFewArgs(name, _) => {
        write!(f, "Too few arguments to call {}", name)
      }
      Error::TooManyArgs(name, _) => {
        write!(f, "Too many arguments to call {}", name)
      }
      Error::InvalidArg(name, provided, expected) => {
        write!(f, "Invalid argument to {}, given {}, expecting {}", name, provided, expected)
      }
      Error::NoSuchVar(name) => {
        write!(f, "No such variable {}", name)
      }
      Error::NoSuchFn(name) => {
        write!(f, "No such function {}", name)
      }
      Error::NoSuchEnumValue(name, subname) => {
        write!(f, "No such enum value {}:{}", name, subname)
      }
      Error::NoSuchMagic(name) => {
        write!(f, "No such call magic {} ({})", name, INTERNAL_ERROR_NOTE)
      }
      Error::UnknownDecl(ast) => {
        write!(f, "Unknown declaration {}", ast)
      }
      Error::InvalidDecl(ast) => {
        write!(f, "Invalid declaration {}", ast)
      }
      Error::UnquoteOutsideQuasiquote => {
        write!(f, "Unquote (,) can only be used inside quasiquote (`)")
      }
      Error::UnquoteSplicedOutsideQuasiquote => {
        write!(f, "Spliced unquote (,.) can only be used inside quasiquote (`)")
      }
      Error::BadUnquoteSpliced(ast) => {
        write!(f, "Spliced unquote (,.) does not make sense in this context: {}", ast)
      }
      Error::NoSuchFile(p) => {
        write!(f, "Cannot locate file {}", p)
      }
      Error::AmbiguousNamespace(s) => {
        write!(f, "Ambiguous namespace when importing {}", s)
      }
      Error::NotConstantEnough(s) => {
        write!(f, "Expression for constant declaration {} is not constant enough", s)
      }
      Error::CannotAssignTo(s) => {
        write!(f, "Cannot assign to immutable variable {}", s)
      }
      Error::CannotExtend(s) => {
        write!(f, "Cannot extend expression {}", s)
      }
      Error::ExportOnInnerClassVar(v) => {
        write!(f, "Export declarations can only be used on a file's main class, but one was found on {}", v)
      }
      Error::ResourceDoesNotExist(s) => {
        write!(f, "Resource {} does not exist", s)
      }
      Error::InvalidImportOnResource(s) => {
        write!(f, "Cannot use restricted or open import lists on resource import at {}", s)
      }
      Error::GodotServerError(err) => {
        write!(f, "Error during Godot server task execution (error code {}): {}", err.error_code, err.error_string)
      }
      Error::StaticConstructor => {
        write!(f, "Class constructors cannot be static")
      }
      Error::StaticMethodOnLambdaClass(s) => {
        write!(f, "Static method {} is not allowed on anonymous class instance", s)
      }
      Error::ModifierParseError(m) => {
        write!(f, "Modifier error: {}", m)
      }
      Error::MacroInMinimalistError(m) => {
        write!(f, "Attempt to expand macro {} in minimalist file ({})", m, INTERNAL_ERROR_NOTE)
      }
      Error::MacroBeforeDefinitionError(m) => {
        write!(f, "Attempt to use macro {} before definition was available", m)
      }
    }
  }
}

impl From<sxp::dotted::TryFromDottedExprError> for Error {
  fn from(_: sxp::dotted::TryFromDottedExprError) -> Error {
    Error::DottedListError
  }
}

impl From<ArgListParseError> for Error {
  fn from(err: ArgListParseError) -> Error {
    Error::ArgListParseError(err)
  }
}

impl From<ImportDeclParseError> for Error {
  fn from(err: ImportDeclParseError) -> Error {
    Error::ImportDeclParseError(err)
  }
}

impl From<ImportNameResolutionError> for Error {
  fn from(err: ImportNameResolutionError) -> Error {
    match err {
      ImportNameResolutionError::UnknownName(id) => {
        match id.namespace {
          Namespace::Function => Error::NoSuchFn(id.name),
          Namespace::Value => Error::NoSuchVar(id.name),
        }
      }
      ImportNameResolutionError::AmbiguousNamespace(s) => {
        Error::AmbiguousNamespace(s)
      }
    }
  }
}

impl From<VarNameIntoExtendsError> for Error {
  fn from(err: VarNameIntoExtendsError) -> Error {
    match err {
      VarNameIntoExtendsError::CannotExtendLocal(s) => {
        Error::CannotExtend(s)
      }
      VarNameIntoExtendsError::CannotExtendCurrentFile(s) => {
        Error::CannotExtend(s)
      }
    }
  }
}

impl From<response::Failure> for Error {
  fn from(err: response::Failure) -> Error {
    Error::GodotServerError(err)
  }
}

impl From<ModifierParseError> for Error {
  fn from(err: ModifierParseError) -> Error {
    Error::ModifierParseError(err)
  }
}
