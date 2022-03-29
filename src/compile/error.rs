
//! Errors that can occur as a result of GDLisp code.

use super::args::{Expecting, ExpectedShape};
use crate::sxp;
use crate::sxp::ast::AST;
use crate::ir::arglist::{ArgListParseError, ArgListParseErrorF};
use crate::ir::identifier::{Id, Namespace, ClassNamespace};
use crate::ir::decl::DuplicateMainClassError;
use crate::ir::import::{ImportDeclParseError, ImportNameResolutionError};
use crate::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use crate::ir::scope::error::ScopeError;
use crate::ir::depends::DependencyError;
use crate::compile::symbol_table::local_var::VarNameIntoExtendsError;
use crate::runner::path::RPathBuf;
use crate::runner::macro_server::response;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::fmt;

// TODO Rename this to GDError and the pipeline one to PError, to be
// completely unambiguous.

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
  InvalidArg(String, AST, ExpectedShape), // TODO Audit usage
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
  CannotExtend(VarNameIntoExtendsError),
  /// An `export` declaration was used on a variable in an inner
  /// class.
  ///
  /// `export` declarations only make sense on the main class of a
  /// file, as they are used for interfacing with existing Godot
  /// tooling which assumes the class is the main class.
  ExportOnInnerClassVar(String),
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
  /// Two or more constructors were defined in the same class or
  /// lambda class.
  DuplicateConstructor,
  /// The same name was declared twice in the same namespace and scope.
  DuplicateName(ClassNamespace, String),
  /// A getter was declared with an invalid argument list or modifier.
  BadGetterArguments(String),
  /// A setter was declared with an invalid argument list or modifier.
  BadSetterArguments(String),
  /// A field and either a getter or setter with the same name were
  /// declared in the same scope.
  FieldAccessorConflict(String),
  /// A `super` call was attempted in a situation where it makes no
  /// sense (such as outside of class scope).
  BadSuperCall(String),
  /// The "extends" clause for a `defclass` was incorrectly formatted.
  /// A `defclass` extends clause should be either a singleton list or
  /// an empty list.
  BadExtendsClause,
  /// A clause of `defenum` was incorrectly formatted. A `defenum`
  /// clause shall be a list of length either 1 or 2, indicating
  /// either a name or a name together with a value.
  BadEnumClause,
  /// The type of a `sys/declare` was invalid. The type of a
  /// `sys/declare` should be 'value', 'superglobal', 'function', or
  /// 'superfunction'.
  BadSysDeclare(String),
  /// An attempt was made to import a name from another file, but that
  /// name was not (publicly) declared in that file.
  UnknownImportedName(Id),
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

  /// Constructs a new error from an `ErrorF` and a source offset.
  pub fn new(value: ErrorF, pos: SourceOffset) -> Error {
    Error { value, pos }
  }

  /// Constructs a new error from a value compatible with `ErrorF` and
  /// a source offset.
  pub fn from_value<T>(value: T, pos: SourceOffset) -> Error
  where ErrorF: From<T> {
    Error::new(ErrorF::from(value), pos)
  }

  /// Gets the unique identifier of the error shape. See
  /// [`ErrorF::error_number`].
  pub fn error_number(&self) -> u32 {
    self.value.error_number()
  }

  /// Returns whether the error is internal to GDLisp. See
  /// [`ErrorF::is_internal`].
  pub fn is_internal(&self) -> bool {
    self.value.is_internal()
  }

}

impl ErrorF {

  /// Produces a unique numerical value representing this type of
  /// error, intended to make identifying the error easier for the
  /// user.
  pub fn error_number(&self) -> u32 {
    match self {
      // Note: Error code 0 is not used.
      ErrorF::DottedListError => 1,
      ErrorF::ArgListParseError(err) => {
        match &err.value {
          ArgListParseErrorF::InvalidArgument(_) => 2,
          ArgListParseErrorF::UnknownDirective(_) => 3,
          ArgListParseErrorF::DirectiveOutOfOrder(_) => 4,
          ArgListParseErrorF::SimpleArgListExpected => 5,
        }
      }
      ErrorF::ImportDeclParseError(err) => {
        match err {
          ImportDeclParseError::NoFilename => 6,
          ImportDeclParseError::BadFilename(_) => 7,
          ImportDeclParseError::InvalidPath(_) => 8,
          ImportDeclParseError::MalformedFunctionImport(_) => 9,
          ImportDeclParseError::InvalidEnding(_) => 10,
        }
      }
      ErrorF::CannotCall(_) => 11,
      ErrorF::WrongNumberArgs(_, _, _) => 12,
      ErrorF::InvalidArg(_, _, _) => 13,
      ErrorF::NoSuchVar(_) => 14,
      ErrorF::NoSuchFn(_) => 15,
      ErrorF::NoSuchEnumValue(_, _) => 16,
      ErrorF::NoSuchMagic(_) => 17,
      ErrorF::UnknownDecl(_) => 18,
      // NOTE: 19 belongs to the removed InvalidDecl, which has been
      // split into several different error types: InvalidArg,
      // BadExtendsClause, BadEnumClause, and BadSysDeclare.
      ErrorF::UnquoteOutsideQuasiquote => 20,
      ErrorF::UnquoteSplicedOutsideQuasiquote => 21,
      ErrorF::BadUnquoteSpliced(_) => 22,
      ErrorF::NoSuchFile(_) => 23,
      ErrorF::AmbiguousNamespace(_) => 24,
      ErrorF::NotConstantEnough(_) => 25,
      ErrorF::CannotAssignTo(_) => 26,
      ErrorF::CannotExtend(_) => 27,
      ErrorF::ExportOnInnerClassVar(_) => 28,
      // NOTE: 29 belongs to the removed ResourceDoesNotExist, which
      // has been replaced by NoSuchFile and InvalidImportOnResource.
      ErrorF::InvalidImportOnResource(_) => 30,
      ErrorF::GodotServerError(_) => 31,
      ErrorF::StaticConstructor => 32,
      ErrorF::StaticOnLambdaClass(_) => 33,
      ErrorF::ModifierParseError(err) => {
        match &err.value {
          ModifierParseErrorF::UniquenessError(_) => 34,
          ModifierParseErrorF::Expecting(_, _) => 35,
          ModifierParseErrorF::ExhaustedAlternatives => 36,
        }
      }
      ErrorF::MacroInMinimalistError(_) => 37,
      ErrorF::MacroBeforeDefinitionError(_) => 38,
      ErrorF::DuplicateMainClass => 39,
      ErrorF::ContextualFilenameUnresolved => 40,
      ErrorF::DuplicateConstructor => 41,
      ErrorF::DuplicateName(_, _) => 42,
      ErrorF::BadGetterArguments(_) => 43,
      ErrorF::BadSetterArguments(_) => 44,
      ErrorF::FieldAccessorConflict(_) => 45,
      ErrorF::BadSuperCall(_) => 46,
      ErrorF::BadExtendsClause => 47,
      ErrorF::BadEnumClause => 48,
      ErrorF::BadSysDeclare(_) => 49,
      ErrorF::UnknownImportedName(_) => 50,
    }
  }

  /// Returns whether or not the error is an internal GDLisp error.
  /// Internal GDLisp errors are those that the compiler emits which
  /// should generally never be seen by users. If a user encounters
  /// such an error, it is probably a bug in the GDLisp compiler, and
  /// such errors are displayed with a disclaimer indicating as much.
  pub fn is_internal(&self) -> bool {
    #[allow(clippy::match_like_matches_macro)] // Suggested alternative is quite lengthy on one line
    match self {
      ErrorF::NoSuchMagic(_) => true,
      ErrorF::MacroInMinimalistError(_) => true,
      ErrorF::ContextualFilenameUnresolved => true,
      _ => false,
    }
  }

}

impl fmt::Display for ErrorF {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Error {:04}: ", self.error_number())?;
    match self {
      ErrorF::DottedListError => {
        write!(f, "Unexpected dotted list")?;
      }
      ErrorF::ArgListParseError(err) => {
        write!(f, "Error parsing argument list: {}", err)?;
      }
      ErrorF::ImportDeclParseError(err) => {
        write!(f, "Error parsing import declaration: {}", err)?;
      }
      ErrorF::CannotCall(ast) => {
        write!(f, "Cannot make function call on expression {}", ast)?;
      }
      ErrorF::WrongNumberArgs(name, expected, actual) => {
        write!(f, "Wrong number of arguments to call {}: expected {}, got {}", name, expected, actual)?;
      }
      ErrorF::InvalidArg(name, provided, expected) => {
        write!(f, "Invalid argument to {}, given {}, expecting {}", name, provided, expected)?;
      }
      ErrorF::NoSuchVar(name) => {
        write!(f, "No such variable {}", name)?;
      }
      ErrorF::NoSuchFn(name) => {
        write!(f, "No such function {}", name)?;
      }
      ErrorF::NoSuchEnumValue(name, subname) => {
        write!(f, "No such enum value {}:{}", name, subname)?;
      }
      ErrorF::NoSuchMagic(name) => {
        write!(f, "No such call magic {}", name)?;
      }
      ErrorF::UnknownDecl(ast) => {
        write!(f, "Unknown declaration {}", ast)?;
      }
      ErrorF::UnquoteOutsideQuasiquote => {
        write!(f, "Unquote (,) can only be used inside quasiquote (`)")?;
      }
      ErrorF::UnquoteSplicedOutsideQuasiquote => {
        write!(f, "Spliced unquote (,.) can only be used inside quasiquote (`)")?;
      }
      ErrorF::BadUnquoteSpliced(ast) => {
        write!(f, "Spliced unquote (,.) does not make sense in this context: {}", ast)?;
      }
      ErrorF::NoSuchFile(p) => {
        write!(f, "Cannot locate file {}", p)?;
      }
      ErrorF::AmbiguousNamespace(s) => {
        write!(f, "Ambiguous namespace when importing {}", s)?;
      }
      ErrorF::NotConstantEnough(s) => {
        write!(f, "Expression for constant declaration {} is not constant enough", s)?;
      }
      ErrorF::CannotAssignTo(s) => {
        write!(f, "Cannot assign to immutable variable {}", s)?;
      }
      ErrorF::CannotExtend(err) => {
        write!(f, "{}", err)?;
      }
      ErrorF::ExportOnInnerClassVar(v) => {
        write!(f, "Export declarations can only be used on a file's main class, but one was found on {}", v)?;
      }
      ErrorF::InvalidImportOnResource(s) => {
        write!(f, "Cannot use restricted or open import lists on resource import at {}", s)?;
      }
      ErrorF::GodotServerError(err) => {
        write!(f, "Error during Godot server task execution (error code {}): {}", err.error_code, err.error_string)?;
      }
      ErrorF::StaticConstructor => {
        write!(f, "Class constructors cannot be static")?;
      }
      ErrorF::StaticOnLambdaClass(s) => {
        write!(f, "Static name {} is not allowed on anonymous class instance", s)?;
      }
      ErrorF::ModifierParseError(m) => {
        write!(f, "Modifier error: {}", m)?;
      }
      ErrorF::MacroInMinimalistError(m) => {
        write!(f, "Attempt to expand macro {} in minimalist file", m)?;
      }
      ErrorF::MacroBeforeDefinitionError(m) => {
        write!(f, "Attempt to use macro {} before definition was available", m)?;
      }
      ErrorF::DuplicateMainClass => {
        write!(f, "File has two main classes")?; // TODO Would be nice to have the source offset of the *original* main class here as well.
      }
      ErrorF::ContextualFilenameUnresolved => {
        write!(f, "Could not resolve contextual filename of current file")?;
      }
      ErrorF::DuplicateConstructor => {
        write!(f, "Class has two constructors")?; // TODO Would be nice to have the source offset of the *original* constructor here as well.
      }
      ErrorF::DuplicateName(ns, name) => {
        write!(f, "The {} '{}' was already declared in this scope", ns.name(), name)?; // TODO Would be nice to have the source offset of the *original* name here as well.
      }
      ErrorF::BadGetterArguments(field_name) => {
        write!(f, "The getter '{}' has a bad signature; getters must be 0-ary non-static functions", field_name)?;
      }
      ErrorF::BadSetterArguments(field_name) => {
        write!(f, "The setter '{}' has a bad signature; getters must be 1-ary non-static functions", field_name)?;
      }
      ErrorF::FieldAccessorConflict(field_name) => {
        write!(f, "The value '{}' was declared as both an instance variable and either a getter or a setter", field_name)?;
      }
      ErrorF::BadSuperCall(super_method) => {
        write!(f, "Cannot call superclass method '{}'; there is no superclass in the current scope", super_method)?;
      }
      ErrorF::BadExtendsClause => {
        write!(f, "Bad 'extends' clause; a class 'extends' clause should either be the empty list or a singleton list")?;
      }
      ErrorF::BadEnumClause => {
        write!(f, "Bad 'defenum' clause; expected a list of one or two elements")?;
      }
      ErrorF::BadSysDeclare(v) => {
        write!(f, "Bad 'sys/declare' type; expected 'value', 'superglobal', 'function', or 'superfunction', got {}", v)?;
      }
      ErrorF::UnknownImportedName(id) => {
        write!(f, "Unknown {} name '{}' in import", id.namespace.name(), &id.name)?;
      }
    }
    if self.is_internal() {
      write!(f, " ({})", INTERNAL_ERROR_NOTE)?;
    }
    Ok(())
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &self.value)
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
        ErrorF::UnknownImportedName(id)
      }
      ImportNameResolutionError::AmbiguousNamespace(s) => {
        ErrorF::AmbiguousNamespace(s)
      }
    }
  }
}

impl From<VarNameIntoExtendsError> for ErrorF {
  fn from(err: VarNameIntoExtendsError) -> ErrorF {
    ErrorF::CannotExtend(err)
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

impl From<ScopeError<ClassNamespace>> for Error {
  fn from(err: ScopeError<ClassNamespace>) -> Error {
    match err {
      ScopeError::DuplicateName(n, s, p) =>
        Error::new(ErrorF::DuplicateName(n, s), p),
    }
  }
}

impl From<ScopeError<Namespace>> for Error {
  fn from(err: ScopeError<Namespace>) -> Error {
    Error::from(
      ScopeError::<ClassNamespace>::from(err),
    )
  }
}

impl From<DependencyError> for Error {
  fn from(e: DependencyError) -> Error {
    match e {
      DependencyError::UnknownName(id, pos) => {
        let error_value = match id.namespace {
          Namespace::Function => ErrorF::NoSuchFn(id.name),
          Namespace::Value => ErrorF::NoSuchVar(id.name),
        };
        Error::new(error_value, pos)
      }
    }
  }
}
