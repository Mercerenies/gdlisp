
//! [`LocalVar`] and various helper structures for storing information
//! about variables.

use crate::ir::access_type::AccessType;
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::decl;
use crate::gdscript::literal::Literal;
use crate::gdscript::library::cell::CELL_CONTENTS;
use crate::pipeline::source::SourceOffset;

use std::borrow::ToOwned;
use std::convert::TryFrom;

/// All of the relevant information needed to understand a variable is
/// stored in `LocalVar`. Despite its name, this structure is used to
/// describe all names in the variable namespace in Godot, including
/// local variables, global constants, and class names.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LocalVar {
  /// The name of the variable.
  pub name: VarName,
  /// The broadest type of access that will ever be required of the
  /// variable.
  pub access_type: AccessType,
  /// The scope of the variable.
  pub scope: VarScope,
  /// Whether or not the variable can be assigned to.
  pub assignable: bool,
  /// In the case of global constants, the compiler often knows the
  /// constant's value at compile-time. We can use this information to
  /// provide optimizations, as well as possible warnings about
  /// incorrect usage of a value. If no value is known, then
  /// `value_hint` is simply `None`.
  pub value_hint: Option<ValueHint>,
}

/// `VarName` will eventually translate into an [`Expr`] (via
/// [`From::from`]). This type should be thought of as a spiritual
/// subtype of `Expr`, only supporting a strict subset of expressions
/// which are necessary for name translations.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VarName {
  /// A variable which is local to the current scope and can be seen
  /// unqualified.
  Local(String),
  /// A file-level constant defined in the current file.
  FileConstant(String),
  /// A superglobal name, such as built-in GDScript constants. These
  /// names will never be modified if imported.
  Superglobal(String),
  /// A file-level constant defined in another file and imported.
  ImportedConstant(Box<VarName>, String),
  /// The current file, as a constant value. This is semantically
  /// similar to a [`VarName::FileConstant`], but it is a special
  /// case, as `VarName::CurrentFile` is required to compile to a
  /// `load(...)` call rather than a simple name.
  CurrentFile(String),
  /// A lazy evaluated value, implemented as a 0-ary function call on
  /// the current file.
  LazyValue(String),
  /// A lazy evaluated value, implemented as a 0-ary function call on
  /// an imported file constant.
  ImportedLazyValue(Box<VarName>, String),
  /// A null value. This is used as a placeholder for things in the
  /// value namespace that don't have runtime presence, such as symbol
  /// macros.
  Null,
}

/// [`VarName`] can always be converted (without loss of information)
/// into an [`Expr`]. It can also sometimes be converted (via
/// [`TryFrom`]) into a [`ClassExtends`](decl::ClassExtends). This is
/// the error type that can result when the latter conversion fails.
#[derive(Clone, Debug)]
pub enum VarNameIntoExtendsError {
  /// It is not permitted to have a class extend a local variable.
  /// Even if the class is an anonymous class defined at local scope,
  /// there is no good semantic way to permit this behavior.
  CannotExtendLocal(String),
  /// It is not permitted to have a class extend the currently loading
  /// file. This would cause a cyclic load error in Godot.
  CannotExtendCurrentFile(String),
  /// It is not permitted to extend lazy-constructed values, as lazy
  /// values are implemented as 0-ary function calls, and function
  /// calls cannot be made in the "extends" clause of a class
  /// declaration.
  CannotExtendLazyValue(String),
  /// The null [`VarName`] is used in some places as a placeholder for
  /// values that don't exist at runtime (i.e. those fully handled
  /// during the IR macro expansion phase). Needless to say, we can't
  /// extend from a name that doesn't exist at runtime.
  CannotExtendNull,
}

/// The scope of a variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarScope {
  /// Global variables are available at least in the current file, if
  /// not everywhere. A global variable never needs to be closed over,
  /// since any closures will be lifted no further than the top of the
  /// current file.
  GlobalVar,
  /// Local variables are available in some scope strictly smaller
  /// than file-level. A local variable *does* need a closure if
  /// referenced in an inner lambda or anonymous class, as it may not
  /// be available outside the original scope of the variable.
  LocalVar,
}

/// A hint as to the value of a [`VarName`], if known. This
/// information can be used in optimizations or possible compiler
/// warnings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueHint {
  /// The variable refers to a class. Class names cannot be reassigned
  /// and are always valid as types.
  ClassName,
  /// The variable refers to a singleton object. Singleton objects
  /// cannot be reassigned.
  ObjectName,
  /// The variable refers to a GDScript literal value, whose exact
  /// value is already known.
  Literal(Literal),
  /// The variable refers to an enumeration declaration with the given
  /// names as enum cases. If this value hint is given, then `:` slot
  /// access can be checked at compile time to ensure that the name in
  /// question actually exists on the enum.
  Enum(Vec<String>),
  /// The variable is the result of a superglobal declaration.
  /// Currently, this is only available if explicitly requested via
  /// `sys/declare`.
  Superglobal,
  /// A symbol macro name. If we end up using one of these at runtime
  /// somehow, then it should be a hard error wherever possible.
  SymbolMacro,
}

/// A `ValueHintsTable` provides a means to look up a variable and get
/// a potential [`ValueHint`] for the variable. The most relevant
/// implementor of this trait is [`SymbolTable`](super::SymbolTable),
/// where the value hint is simply looked up in the current scope.
pub trait ValueHintsTable {
  /// Get a hint for the value of the variable with name `name`.
  /// Returns `None` if no hint is available.
  fn get_value_hint(&self, name: &str) -> Option<&ValueHint>;
}

/// `VacuousValueHintsTable` is a trivial implementation of
/// [`ValueHintsTable`] that never returns a hint.
pub struct VacuousValueHintsTable;

impl ValueHintsTable for VacuousValueHintsTable {
  fn get_value_hint(&self, _name: &str) -> Option<&ValueHint> {
    None
  }
}

impl LocalVar {

  /// A read-only (non-closure) local variable, as per
  /// [`LocalVar::local`].
  pub fn read(name: String) -> LocalVar {
    LocalVar::local(name, AccessType::Read)
  }

  /// A read-write (non-closure) local variable, as per
  /// [`LocalVar::local`].
  pub fn rw(name: String) -> LocalVar {
    LocalVar::local(name, AccessType::RW)
  }

  /// A closed read-write local variable, as per [`LocalVar::local`].
  pub fn closed_rw(name: String) -> LocalVar {
    LocalVar::local(name, AccessType::ClosedRW)
  }

  /// A local variable with the given access type.
  pub fn local(name: String, access_type: AccessType) -> LocalVar {
    LocalVar {
      name: VarName::Local(name),
      access_type: access_type,
      scope: VarScope::LocalVar,
      assignable: true,
      value_hint: None,
    }
  }

  /// A superglobal variable, available in all GDScript scopes, not
  /// just the current file. Superglobals are always
  /// [`AccessType::Read`] and are never `assignable`.
  pub fn superglobal(name: String) -> LocalVar {
    LocalVar {
      name: VarName::Superglobal(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  /// A file-level constant GDScript variable. File-level constants
  /// are always [`AccessType::Read`] and are never `assignable`.
  pub fn file_constant(name: String) -> LocalVar {
    LocalVar {
      name: VarName::FileConstant(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  /// A `LocalVar` representing the current file (whose name is
  /// `name`) via [`VarName::CurrentFile`]. The current file is always
  /// a non-`assignable` value with access type [`AccessType::Read`].
  pub fn current_file(name: String) -> LocalVar {
    LocalVar {
      name: VarName::CurrentFile(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  /// A file-level lazy value. File-level values are treated as
  /// constant, are always [`AccessType::Read`], and are never
  /// `assignable`.
  pub fn lazy_value(name: String) -> LocalVar {
    LocalVar {
      name: VarName::LazyValue(name),
      access_type: AccessType::Read,
      scope: VarScope::GlobalVar,
      assignable: false,
      value_hint: None,
    }
  }

  /// A `LocalVar` referencing the special GDScript `self` variable.
  /// `self` is a local, non-assignable variable. We give it access
  /// type [`AccessType::ClosedRead`], as the broadest possible access
  /// type `self` can use, since it cannot be written to but it could
  /// potentially be closed over.
  pub fn self_var() -> LocalVar { // TODO Should this be a special case?
    // Note: Cannot assign to self
    LocalVar::local(String::from("self"), AccessType::ClosedRead).no_assign()
  }

  /// Sets `self.assignable` to false and returns `self`. This method
  /// is intended to be used in a builder style.
  pub fn no_assign(mut self) -> Self {
    self.assignable = false;
    self
  }

  /// Sets the `self.value_hint` value and returns `self`. This method
  /// is intended to be used in a builder style.
  pub fn with_hint(mut self, value_hint: ValueHint) -> Self {
    self.value_hint = Some(value_hint);
    self
  }

  /// The simple, unqualified name of the variable, if it exists.
  /// Equivalent to `self.name.simple_name()`.
  pub fn simple_name(&self) -> Option<&str> {
    self.name.simple_name()
  }

  /// An `Expr` which references the value of this variable. If this
  /// variable requires a cell (`self.access_type.requires_cell()`),
  /// then this access expression contains the necessary subscripting
  /// to access the *contents* of the cell, not the cell itself.
  pub fn expr(&self, pos: SourceOffset) -> Expr {
    let inner: Expr = self.name.clone().into_expr(pos);
    if self.access_type.requires_cell() {
      Expr::new(ExprF::Attribute(Box::new(inner), CELL_CONTENTS.to_owned()), pos)
    } else {
      inner
    }
  }

  // TODO Put all of the declaration-site stuff here as well, like
  // .expr() for access, so we have it all in one place (i.e. the
  // difference between "var x = ..." and "var x = Cell.new(...)")

}

impl VarName {

  /// Helper function to produce a `load(...)` GDScript expression for
  /// the file named `filename`.
  pub fn load_expr(filename: String, pos: SourceOffset) -> Expr {
    Expr::call(None, "load", vec!(Expr::from_value(filename, pos)), pos)
  }

  /// `VarName` for a local variable.
  pub fn local(name: &str) -> VarName {
    VarName::Local(String::from(name))
  }

  /// `VarName` for a file-level constant.
  pub fn file_constant(name: &str) -> VarName {
    VarName::FileConstant(String::from(name))
  }

  /// `VarName` for a superglobal variable.
  pub fn superglobal(name: &str) -> VarName {
    VarName::Superglobal(String::from(name))
  }

  /// `VarName` for an imported constant name.
  pub fn imported_constant(orig_name: VarName, name: &str) -> VarName {
    VarName::ImportedConstant(Box::new(orig_name), String::from(name))
  }

  /// `VarName` for the current file.
  pub fn current_file(filename: &str) -> VarName {
    VarName::CurrentFile(String::from(filename))
  }

  /// Converts `self` to valid GDScript syntax. Equivalent to
  /// `self.clone().into_expr(pos).to_gd()`.
  pub fn to_gd(&self, pos: SourceOffset) -> String {
    self.clone().into_expr(pos).to_gd()
  }

  /// If `self` refers to a simple (unqualified) name, such as a local
  /// variable or an unimported constant defined in the current file,
  /// then this method returns the name. If `self` refers to a
  /// qualified or otherwise special name (such as a `load` on the
  /// current file), then this method returns `None`.
  pub fn simple_name(&self) -> Option<&str> {
    match self {
      VarName::Local(s) => Some(&s),
      VarName::FileConstant(s) => Some(&s),
      VarName::Superglobal(s) => Some(&s),
      VarName::ImportedConstant(_, _) => None,
      VarName::CurrentFile(_) => None,
      VarName::LazyValue(_) => None,
      VarName::ImportedLazyValue(_, _) => None,
      VarName::Null => None,
    }
  }

  /// Converts the `VarName` into an appropriate value to be called
  /// from another module.
  ///
  /// If a name `foo` is available at top-level scope `A.gd` and some
  /// file `B.gd` imports `A.gd` and calls the top-level preload
  /// constant `AConst`, then calling `foo.as_imported("AConst")` will
  /// convert the name to how it should be referenced from `B.gd`.
  pub fn into_imported(self, import_name: String) -> VarName {
    match self {
      VarName::Local(s) => {
        // To be honest, this case probably should never occur. So
        // we'll just pretend it's FileConstant.
        VarName::ImportedConstant(Box::new(VarName::FileConstant(import_name)), s)
      }
      VarName::FileConstant(s) => {
        // Import file constants by qualifying the name.
        VarName::ImportedConstant(Box::new(VarName::FileConstant(import_name)), s)
      }
      VarName::Superglobal(s) => {
        // Superglobals are always in scope and don't change on import.
        VarName::Superglobal(s)
      }
      VarName::ImportedConstant(lhs, s) => {
        // Import the constant transitively.
        let lhs = Box::new(lhs.into_imported(import_name));
        VarName::ImportedConstant(lhs, s)
      }
      VarName::CurrentFile(_) => {
        // The current file imports as the name of the import itself.
        VarName::FileConstant(import_name)
      }
      VarName::LazyValue(s) => {
        // Import the value by qualifying the name.
        VarName::ImportedLazyValue(Box::new(VarName::FileConstant(import_name)), s)
      }
      VarName::ImportedLazyValue(lhs, s) => {
        // Import the value transitively.
        let lhs = Box::new(lhs.into_imported(import_name));
        VarName::ImportedLazyValue(lhs, s)
      }
      VarName::Null => {
        // Null is already available everywhere.
        VarName::Null
      }
    }
  }

  /// [`VarName`] can always be converted into an [`Expr`]. `VarName`
  /// is, by definition, the subset of GDScript expressions suitable
  /// for variable name expansion.
  pub fn into_expr(self, pos: SourceOffset) -> Expr {
    match self {
      VarName::Local(s) => Expr::new(ExprF::Var(s), pos),
      VarName::FileConstant(s) => Expr::new(ExprF::Var(s), pos),
      VarName::Superglobal(s) => Expr::new(ExprF::Var(s), pos),
      VarName::ImportedConstant(lhs, s) => Expr::new(ExprF::Attribute(Box::new(lhs.into_expr(pos)), s), pos),
      VarName::CurrentFile(filename) => VarName::load_expr(filename, pos),
      VarName::LazyValue(s) => Expr::call(None, &s, vec!(), pos),
      VarName::ImportedLazyValue(lhs, s) => Expr::call(Some(lhs.into_expr(pos)), &s, vec!(), pos),
      VarName::Null => Expr::null(pos),
    }
  }

}

/// Classes extend from variables which have `VarName`. We can
/// (attempt to) convert from `VarName` to [`decl::ClassExtends`]. We
/// cannot, however, extend from local variables using this technique.
impl TryFrom<VarName> for decl::ClassExtends {
  type Error = VarNameIntoExtendsError;

  fn try_from(var_name: VarName) -> Result<decl::ClassExtends, VarNameIntoExtendsError> {
    match var_name {
      VarName::Local(s) => {
        Err(VarNameIntoExtendsError::CannotExtendLocal(s))
      }
      VarName::FileConstant(s) => {
        Ok(decl::ClassExtends::Qualified(vec!(s)))
      }
      VarName::Superglobal(s) => {
        Ok(decl::ClassExtends::Qualified(vec!(s)))
      }
      VarName::ImportedConstant(lhs, s) => {
        let decl::ClassExtends::Qualified(mut vec) = decl::ClassExtends::try_from(*lhs)?;
        vec.push(s);
        Ok(decl::ClassExtends::Qualified(vec))
      }
      VarName::CurrentFile(s) => {
        Err(VarNameIntoExtendsError::CannotExtendCurrentFile(s))
      }
      VarName::LazyValue(s) => {
        Err(VarNameIntoExtendsError::CannotExtendLazyValue(s))
      }
      VarName::ImportedLazyValue(_, s) => {
        Err(VarNameIntoExtendsError::CannotExtendLazyValue(s))
      }
      VarName::Null => {
        Err(VarNameIntoExtendsError::CannotExtendNull)
      }
    }
  }

}

impl ValueHint {

  /// Helper method for constructing a [`ValueHint::Enum`] from an
  /// iterator of unowned strings. Implicitly clones the strings and
  /// stores them in a `ValueHint::Enum`.
  pub fn enumeration<'a>(values: impl Iterator<Item=&'a str>) -> ValueHint {
    ValueHint::Enum(values.map(str::to_owned).collect())
  }

}
