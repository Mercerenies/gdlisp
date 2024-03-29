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

//! [`LocalVar`] and various helper structures for storing information
//! about variables.

use crate::ir::access_type::AccessType;
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::literal::Literal;
use crate::gdscript::library::cell::CELL_CONTENTS;
use crate::gdscript::class_extends::ClassExtends;
use crate::pipeline::source::SourceOffset;
use crate::compile::names;

use serde::{Serialize, Deserialize};

use std::borrow::ToOwned;
use std::convert::TryFrom;
use std::fmt;
use std::error::Error;
use std::ffi::OsStr;

/// All of the relevant information needed to understand a variable is
/// stored in `LocalVar`. Despite its name, this structure is used to
/// describe all names in the variable namespace in Godot, including
/// local variables, global constants, and class names.
#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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
#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum VarName {
  /// A variable which is local to the current scope and can be seen
  /// unqualified.
  Local(String),
  /// Similar to `Local`, with the caveat that the variable
  /// `OuterClassRef` refers to is an outer class reference. These are
  /// treated like local variables for most purposes but will be
  /// handled specially when used in nested closures.
  OuterClassRef(String),
  /// A file-level constant defined in the current file.
  FileConstant(String),
  /// A superglobal name, such as built-in GDScript constants. These
  /// names will never be modified if imported.
  Superglobal(String),
  /// A file-level constant defined in another file and imported.
  ImportedConstant(Box<VarName>, String),
  /// A file-level constant subscripted by a given constant numerical value.
  SubscriptedConstant(Box<VarName>, i32),
  /// The current file, as a constant value. This is semantically
  /// similar to a [`VarName::FileConstant`], but it is a special
  /// case, as `VarName::CurrentFile` is required to compile to a
  /// `load(...)` call rather than a simple name.
  CurrentFile(String),
  /// A `load` call on a file other than the current file.
  DirectLoad(String),
  /// A null value. This is used as a placeholder for things in the
  /// value namespace that don't have runtime presence, such as symbol
  /// macros.
  Null,
}

/// [`VarName`] can always be converted (without loss of information)
/// into an [`Expr`]. It can also sometimes be converted (via
/// [`TryFrom`]) into a [`ClassExtends`](ClassExtends). This is
/// the error type that can result when the latter conversion fails.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarNameIntoExtendsError {
  /// It is not permitted to have a class extend a local variable.
  /// Even if the class is an anonymous class defined at local scope,
  /// there is no good semantic way to permit this behavior.
  CannotExtendLocal(String),
  /// It is not permitted to have a class which extends a subscripted
  /// (i.e. `foo[bar]`) expression.
  CannotExtendSubscript(Box<VarName>, i32),
  /// It is not permitted to have a class extend the currently loading
  /// file. This would cause a cyclic load error in Godot.
  CannotExtendCurrentFile(String),
  /// It is not permitted to have a class extend a `load` function
  /// call.
  CannotExtendLoadDirective(String),
  /// The null [`VarName`] is used in some places as a placeholder for
  /// values that don't exist at runtime (i.e. those fully handled
  /// during the IR macro expansion phase). Needless to say, we can't
  /// extend from a name that doesn't exist at runtime.
  CannotExtendNull,
}

/// The scope of a variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
  /// The variable is a constant whose exact value may not be known.
  GlobalConstant,
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

  /// A local outer class reference variable with the given access
  /// type.
  pub fn outer_class_ref(name: String, access_type: AccessType) -> LocalVar {
    LocalVar {
      name: VarName::OuterClassRef(name),
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

  /// Replaces the simple, unqualified name of the variable. If the
  /// variable has no unqualified name, then this function does
  /// nothing.
  pub fn set_simple_name(&mut self, name: String) {
    match &mut self.name {
      VarName::Local(s) => {
        *s = name;
      }
      VarName::OuterClassRef(s) => {
        *s = name;
      }
      VarName::FileConstant(s) => {
        *s = name;
      }
      VarName::Superglobal(s) => {
        *s = name;
      }
      VarName::ImportedConstant(_, _) => {}
      VarName::SubscriptedConstant(_, _) => {}
      VarName::CurrentFile(_) => {}
      VarName::DirectLoad(_) => {}
      VarName::Null => {}
    }
  }

  /// Returns whether or not the variable name in question compiles to
  /// an expression which is valid as the right-hand side of a `const`
  /// in GDScript.
  ///
  /// This function is permitted to be overly conservative. That is,
  /// in situations where the variable name may or may not be valid,
  /// this function will return `false`. However, if this function
  /// returns `true`, then it *must* be valid as a `const` expression.
  pub fn is_valid_const_expr(&self) -> bool {
    match &self.name {
      VarName::Local(_) => false,
      VarName::OuterClassRef(_) => false,
      VarName::FileConstant(_) | VarName::ImportedConstant(_, _) | VarName::SubscriptedConstant(_, _) => {
        // If it's a top-level constant, use the value hint to figure
        // out which constant.
        match self.value_hint {
          None => false,
          Some(ValueHint::ClassName) => true,
          Some(ValueHint::ObjectName) => false, // Implemented as a 0-ary function call by GDLisp
          Some(ValueHint::Literal(_)) => true,
          Some(ValueHint::Enum(_)) => true,
          Some(ValueHint::GlobalConstant) => true,
          Some(ValueHint::Superglobal) => true,
          Some(ValueHint::SymbolMacro) => false, // How did we even get into this situation?
        }
      }
      VarName::Superglobal(_) => true,
      VarName::CurrentFile(_) => false,
      VarName::DirectLoad(_) => false,
      VarName::Null => true,
    }
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

  /// `VarName` for an outer class reference (local) variable.
  pub fn outer_class_ref(name: &str) -> VarName {
    VarName::OuterClassRef(String::from(name))
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
      VarName::Local(s) => Some(s),
      VarName::OuterClassRef(s) => Some(s),
      VarName::FileConstant(s) => Some(s),
      VarName::Superglobal(s) => Some(s),
      VarName::ImportedConstant(_, _) => None,
      VarName::SubscriptedConstant(_, _) => None,
      VarName::CurrentFile(_) => None,
      VarName::DirectLoad(_) => None,
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
    self.into_imported_var(VarName::FileConstant(import_name))
  }

  pub fn into_imported_var(self, import: VarName) -> VarName {
    match self {
      VarName::Local(s) => {
        // To be honest, this case probably should never occur. So
        // we'll just pretend it's FileConstant.
        VarName::ImportedConstant(Box::new(import), s)
      }
      VarName::OuterClassRef(s) => {
        // To be honest, this case probably should never occur. So
        // we'll just pretend it's FileConstant.
        VarName::ImportedConstant(Box::new(import), s)
      }
      VarName::FileConstant(s) => {
        // Import file constants by qualifying the name.
        VarName::ImportedConstant(Box::new(import), s)
      }
      VarName::Superglobal(s) => {
        // Superglobals are always in scope and don't change on import.
        VarName::Superglobal(s)
      }
      VarName::ImportedConstant(lhs, s) => {
        // Import the constant transitively.
        let lhs = Box::new(lhs.into_imported_var(import));
        VarName::ImportedConstant(lhs, s)
      }
      VarName::SubscriptedConstant(lhs, n) => {
        // Import the constant transitively.
        let lhs = Box::new(lhs.into_imported_var(import));
        VarName::SubscriptedConstant(lhs, n)
      }
      VarName::DirectLoad(filename) => {
        // A direct load does not change when imported. It still directly loads the same file.
        VarName::DirectLoad(filename)
      }
      VarName::CurrentFile(_) => {
        // The current file imports as the name of the import itself.
        import
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
      VarName::OuterClassRef(s) => Expr::new(ExprF::Var(s), pos),
      VarName::FileConstant(s) => Expr::new(ExprF::Var(s), pos),
      VarName::Superglobal(s) => Expr::new(ExprF::Var(s), pos),
      VarName::ImportedConstant(lhs, s) => Expr::new(ExprF::Attribute(Box::new(lhs.into_expr(pos)), s), pos),
      VarName::SubscriptedConstant(lhs, n) => Expr::new(ExprF::Subscript(Box::new(lhs.into_expr(pos)), Box::new(Expr::from_value(n, pos))), pos),
      VarName::DirectLoad(filename) => VarName::load_expr(filename, pos),
      VarName::CurrentFile(filename) => VarName::load_expr(filename, pos),
      VarName::Null => Expr::null(pos),
    }
  }

}

/// Classes extend from variables which have `VarName`. We can
/// (attempt to) convert from `VarName` to [`ClassExtends`]. We
/// cannot, however, extend from local variables using this technique.
impl TryFrom<VarName> for ClassExtends {
  type Error = VarNameIntoExtendsError;

  // Note: This will only succeed into ClassExtends::Qualified. It
  // will never produce any other alternative value for ClassExtends.
  // This behavior may change in the future, at which point we'll need
  // to change VarName::ImportedConstant to transitively handle that
  // (or at least err in a better way than panicking).
  fn try_from(var_name: VarName) -> Result<ClassExtends, VarNameIntoExtendsError> {
    match var_name {
      VarName::Local(s) => {
        Err(VarNameIntoExtendsError::CannotExtendLocal(s))
      }
      VarName::OuterClassRef(s) => {
        Err(VarNameIntoExtendsError::CannotExtendLocal(s))
      }
      VarName::FileConstant(s) => {
        Ok(ClassExtends::SimpleIdentifier(s))
      }
      VarName::Superglobal(s) => {
        Ok(ClassExtends::SimpleIdentifier(s))
      }
      VarName::ImportedConstant(lhs, s) => {
        ClassExtends::try_from(*lhs).map(|x| x.attribute(s))
      }
      VarName::SubscriptedConstant(lhs, n) => {
        Err(VarNameIntoExtendsError::CannotExtendSubscript(lhs, n))
      }
      VarName::DirectLoad(s) => {
        Err(VarNameIntoExtendsError::CannotExtendLoadDirective(s))
      }
      VarName::CurrentFile(s) => {
        Err(VarNameIntoExtendsError::CannotExtendCurrentFile(s))
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
    ValueHint::Enum(values.map(names::lisp_to_gd).collect())
  }

  /// The value hint for a resource with the given file extension. The
  /// file extension should *not* include the dot. If the file type
  /// cannot be inferred or if there isn't a value hint that
  /// represents the given file type, then `None` is returned.
  pub fn from_file_ext(file_extension: &OsStr) -> Option<ValueHint> {
    let file_extension = file_extension.to_ascii_lowercase();
    if file_extension == "gd" {
      Some(ValueHint::ClassName)
    } else {
      None
    }
  }

}

impl fmt::Display for VarNameIntoExtendsError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      VarNameIntoExtendsError::CannotExtendLocal(s) => {
        write!(f, "Cannot extend local variable {}", s)
      }
      VarNameIntoExtendsError::CannotExtendSubscript(expr, n) => {
        let expr = expr.clone().into_expr(SourceOffset(0)).subscript(Expr::from_value(*n, SourceOffset(0)), SourceOffset(0));
        write!(f, "Cannot extend reference to current file {}", expr.to_gd())
      }
      VarNameIntoExtendsError::CannotExtendCurrentFile(s) => {
        write!(f, "Cannot extend reference to current file {}", s)
      }
      VarNameIntoExtendsError::CannotExtendLoadDirective(s) => {
        write!(f, "Cannot extend direct load of file {}", s)
      }
      VarNameIntoExtendsError::CannotExtendNull => {
        write!(f, "Cannot extend null value")
      }
    }
  }
}

impl Error for VarNameIntoExtendsError {}

#[cfg(test)]
mod tests {
  use super::*;

  // TODO More

  fn qualified(mut name: Vec<&'static str>) -> ClassExtends {
    assert!(!name.is_empty(), "Empty identifier not allowed");
    let first = ClassExtends::SimpleIdentifier(name.remove(0).to_owned());
    name.into_iter().fold(first, |acc, name| acc.attribute(name))
  }

  #[test]
  fn extends_constant() {
    assert_eq!(
      ClassExtends::try_from(VarName::file_constant("Abc")),
      Ok(qualified(vec!("Abc"))),
    );
  }

  #[test]
  fn extends_superglobal() {
    assert_eq!(
      ClassExtends::try_from(VarName::superglobal("Node")),
      Ok(qualified(vec!("Node"))),
    );
  }

  #[test]
  fn extends_imported() {
    assert_eq!(
      ClassExtends::try_from(VarName::imported_constant(VarName::file_constant("Foo"), "MyClass")),
      Ok(qualified(vec!("Foo", "MyClass"))),
    );
  }

  #[test]
  fn cannot_extend_local() {
    assert_eq!(
      ClassExtends::try_from(VarName::local("abc")),
      Err(VarNameIntoExtendsError::CannotExtendLocal(String::from("abc"))),
    );
  }

  #[test]
  fn cannot_extend_subscripted() {
    assert_eq!(
      ClassExtends::try_from(VarName::SubscriptedConstant(Box::new(VarName::file_constant("abc")), 10)),
      Err(VarNameIntoExtendsError::CannotExtendSubscript(Box::new(VarName::file_constant("abc")), 10)),
    );
  }

  #[test]
  fn cannot_extend_current_file() {
    assert_eq!(
      ClassExtends::try_from(VarName::CurrentFile(String::from("Filename.gd"))),
      Err(VarNameIntoExtendsError::CannotExtendCurrentFile(String::from("Filename.gd"))),
    );
  }

  #[test]
  fn cannot_extend_null_value() {
    assert_eq!(
      ClassExtends::try_from(VarName::Null),
      Err(VarNameIntoExtendsError::CannotExtendNull),
    );
  }

  #[test]
  fn from_file_ext_test() {
    assert_eq!(ValueHint::from_file_ext(OsStr::new("GD")), Some(ValueHint::ClassName));
    assert_eq!(ValueHint::from_file_ext(OsStr::new("gd")), Some(ValueHint::ClassName));
    assert_eq!(ValueHint::from_file_ext(OsStr::new("tscn")), None);
    assert_eq!(ValueHint::from_file_ext(OsStr::new("png")), None);
    assert_eq!(ValueHint::from_file_ext(OsStr::new("abc")), None);
  }

}
