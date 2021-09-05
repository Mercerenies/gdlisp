
//! Errors that can occur as a result of GDLisp code.

use super::args::Expecting;
use crate::sxp;
use crate::sxp::ast::AST;
use crate::ir::arglist::ArgListParseError;
use crate::ir::identifier::Namespace;
use crate::ir::decl::DuplicateMainClassError;
use crate::ir::import::{ImportDeclParseError, ImportNameResolutionError};
use crate::ir::modifier::{ParseError as ModifierParseError};
use crate::compile::symbol_table::local_var::VarNameIntoExtendsError;
use crate::runner::path::RPathBuf;
use crate::runner::macro_server::response;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::fmt;

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
  /// A `DottedListError` indicates that a dotted list, such as `(1 2
  /// . 3)`, was encountered where a proper list, such as `(1 2 3)`,
  /// was expecting. `DottedListError` is almost always constructed
  /// via a [`From`] conversion from
  /// [`sxp::dotted::TryFromDottedExprError`].
  DottedListError,
  /// An error during parsing of an argument list. The sole argument
  /// to this constructor stores further information about what went
  /// wrong.
  ArgListParseError(ArgListParseError),
  /// An error during parsing of an import declaration. The sole
  /// argument to this constructor stores further information about
  /// what went wrong.
  ImportDeclParseError(ImportDeclParseError),
  /// Only a handful of `AST` shapes are valid callable objects, most
  /// namely symbols but also certain shapes of proper lists. If any
  /// non-callable `AST` is ever put into a position where it would be
  /// the head of a function call, then a `CannotCall` error shall be
  /// issued, with that `AST` as argument.
  CannotCall(AST),
  /// A function, macro, special form, or any other callable object
  /// was called with a different number of arguments than was
  /// expected.
  ///
  /// This error includes the name of the function, the expected
  /// (minimum and maximum) number of arguments, and the actual number
  /// of arguments provided.
  WrongNumberArgs(String, Expecting, usize),
  /// A function, macro, special form, or any other callable object
  /// was expecting a value of a particular type or shape but received
  /// something incompatible.
  ///
  /// This error includes the name of the callable object, the faulty
  /// argument that was passed, and a freeform text description of the
  /// sort of argument that was expected.
  InvalidArg(String, AST, String), // TODO Standardize the third argument in some kind of ADT.
  /// A name was referenced in the variable namespace, but no such
  /// name was found in the symbol table.
  NoSuchVar(String),
  /// A name was referenced in the function namespace, but no such
  /// name was found in the symbol table.
  NoSuchFn(String),
  /// An enumeration constant was subscripted, but the subscripted
  /// name does not exist in the enumeration.
  ///
  /// Note that this is a fairly basic check and can be circumvented
  /// with relative ease. This error is designed to catch the most
  /// obvious of typos and is not even close to a full static
  /// typechecker for enumeration values.
  NoSuchEnumValue(String, String),
  /// A call magic entity was referenced, but no entity with that name
  /// exists.
  ///
  /// In general, users of GDLisp should not be dealing directly with
  /// call magic, so if you see this error in real code, please
  /// consider reporting a bug to the compiler's issue tracker.
  NoSuchMagic(String),
  /// An `AST` appeared in declaration context, but its head could not
  /// be interpreted as a known declaration type.
  ///
  /// This error is only produced after any macro expansion is
  /// attempted, so if you were expecting to call a macro, then
  /// consider double-checking the name of the macro.
  UnknownDecl(AST),
  /// A declaration was incorrectly formatted.
  ///
  /// This is a *very* general-purpose catch-all error type, and other
  /// error types should always be favored if a more appropriate one
  /// exists. (TODO Reduce the use of this error, and even potentially
  /// remove it altogether)
  InvalidDecl(AST),
  /// An `unquote` expression appeared outside of a `quasiquote`.
  UnquoteOutsideQuasiquote,
  /// An `unquote-spliced` expression appeared outside of a
  /// `quasiquote`.
  UnquoteSplicedOutsideQuasiquote,
  /// An `unquote-spliced` expression appeared within a `quasiquote`
  /// but in a context where splicing does not make sense, such as in
  /// the cdr of a dotted list or as a part of a dictionary literal.
  BadUnquoteSpliced(AST),
  /// The current preload resolver failed to resolve the file with the
  /// given path.
  ///
  /// TODO Is this an internal-only error? Is there ever a reasonable
  /// time that a user should see this error?
  NoSuchFile(RPathBuf),
  /// An import declaration includes an explicit named import whose
  /// namespace is ambiguous.
  ///
  /// Normally, when importing particular names in a `use` statement,
  /// the namespace will be inferred. If either a function or a value
  /// exists with that name, then the matching namespace will be used
  /// in either case. However, if *both* a function and a value exist
  /// with the same name in the target module, then the import is
  /// ambiguous. Such imports can be disambiguated by specifying the
  /// namespace explicitly as follows.
  ///
  /// ```text
  /// (use "res://Example.lisp" ((my-example-value value) (my-example-function function)))
  /// (use "res://Example.lisp" ((my-example-value value as aliased-value) (my-example-function function as aliased-function)))
  /// ```
  AmbiguousNamespace(String),
  /// The constant variable with the given name was initialized with a
  /// value that does not meet the GDLisp requirements for a constant.
  ///
  /// Note that GDLisp is fairly strict in what is considers a valid
  /// constant. In particular, if your intention is to import an
  /// external file, GDScript users would declare a constant whose
  /// value is initialized to the result of a `preload` call, but in
  /// GDLisp, we use the special `use` declaration for this. Generally
  /// speaking, constants should be used for simple atomic values,
  /// such as numbers and strings, not for aliasing complex data from
  /// another source.
  NotConstantEnough(String),
  /// An attempt was made to assign to a non-assignable variable, such
  /// as a constant or the special `self` value in a class.
  CannotAssignTo(String),
  /// An attempt was made to extend a value which cannot be extended
  /// as a class.
  ///
  /// TODO Provide better info here than `String`, via the
  /// already-in-place [`VarNameIntoExtendsError`] infrastructure.
  CannotExtend(String),
  /// An `export` declaration was used on a variable in an inner
  /// class.
  ///
  /// `export` declarations only make sense on the main class of a
  /// file, as they are used for interfacing with existing Godot
  /// tooling which assumes the class is the main class.
  ExportOnInnerClassVar(String),
  /// This error type is no longer relevant and should not be used. It
  /// has been superseded by [`ErrorF::NoSuchFile`] and
  /// [`ErrorF::InvalidImportOnResource`].
  #[deprecated]
  ResourceDoesNotExist(String),
  /// This error is issued if the type of import declaration was
  /// incorrect for the given file. Namely, explicit and open import
  /// lists are only allowed for GDLisp source files, not for GDScript
  /// source files or other resource types.
  InvalidImportOnResource(String),
  /// An error occurred in communication with the Godot macro server.
  GodotServerError(response::Failure),
  /// A constructor `_init` function was labeled as static.
  /// Constructor functions can never be static.
  StaticConstructor,
  /// A static method, or other non-instance entity such as a
  /// constant, was declared on a lambda class (i.e. a class
  /// constructed in an expression via the `new` special form).
  ///
  /// There's no philosophical reason a lambda class cannot have
  /// static methods or constants, but due to various awkward
  /// implementation issues, together with the questionable utility of
  /// such a feature, the feature was outright banned. See Issue #30
  /// for more background.
  StaticOnLambdaClass(String),
  /// An error during parsing of a modifier or modifier list. The sole
  /// argument to this constructor stores further information about
  /// what went wrong.
  ModifierParseError(ModifierParseError),
  /// A macro call was encountered during a minimalist compilation.
  ///
  /// Generally, users should never be performing minimalist
  /// compilations on their own files, as this is an advanced feature
  /// intended for bootstrapping the standard library. As such, if you
  /// encounter this error, consider reporting a bug to GDLisp.
  MacroInMinimalistError(String),
  /// A macro call was encountered before the macro was defined.
  ///
  /// While GDLisp functions can be, generally speaking, called from
  /// before they're defined (subject to some constraints), macros
  /// are, for deeply technical reasons, only available after the
  /// point in the file where they've been defined. This error will
  /// arise if a macro call is performed at a point in the file above
  /// the point where the `defmacro` is actually written.
  MacroBeforeDefinitionError(String),
  /// Two or more main classes were declared in the same file.
  ///
  /// A file can have at most one main class. On the GDScript side,
  /// main classes will be compiled into the overarching class
  /// representing the file, and all other classes will get compiled
  /// into inner classes within that main class. If there's no main
  /// class, then a minimal stub inheriting from `Node` will be filled
  /// in in GDScript.
  DuplicateMainClass,
  /// A `sys/context-filename` call failed to resolve (using the
  /// current preload resolver) the current filename.
  ///
  /// If you ever encounter this error, please report it as a bug.
  ContextualFilenameUnresolved,
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
      ErrorF::WrongNumberArgs(name, expected, actual) => {
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
      ErrorF::StaticOnLambdaClass(s) => {
        write!(f, "Static name {} is not allowed on anonymous class instance", s)
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
      ErrorF::DuplicateMainClass => {
        write!(f, "File has two main classes") // TODO Would be nice to have the source offset of the *original* main class here as well.
      }
      ErrorF::ContextualFilenameUnresolved => {
        write!(f, "Could not resolve contextual filename of current file ({})", INTERNAL_ERROR_NOTE)
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
      VarNameIntoExtendsError::CannotExtendLazyValue(s) => {
        ErrorF::CannotExtend(s)
      }
      VarNameIntoExtendsError::CannotExtendNull => {
        ErrorF::CannotExtend(String::from("null"))
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

impl From<DuplicateMainClassError> for Error {
  fn from(err: DuplicateMainClassError) -> Error {
    Error::new(ErrorF::DuplicateMainClass, err.0)
  }
}
