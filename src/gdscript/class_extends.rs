
//! Provides the [`ClassExtends`] type, for qualified names valid in
//! an `extends` clause.

use super::literal::Literal;

/// A descriptor of what class is being extended.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassExtends {
  /// A qualified name. `Qualified(vec!("foo", "bar", "baz"))`
  /// represents the name `foo.bar.baz`.
  Qualified(Vec<String>),
  StringLit(String), // TODO Test this
}

impl ClassExtends {

  /// A simple unqualified name.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::gdscript::class_extends::ClassExtends;
  /// let named = ClassExtends::named(String::from("Foobar"));
  /// assert_eq!(named, ClassExtends::Qualified(vec!(String::from("Foobar"))));
  /// ```
  pub fn named(name: String) -> ClassExtends {
    ClassExtends::Qualified(vec!(name))
  }

  /// Convert `self` to a string suitable as the tail end of a
  /// `extends` clause in GDScript.
  pub fn to_gd(&self) -> String {
    match self {
      ClassExtends::Qualified(names) => names.join("."),
      ClassExtends::StringLit(string) => Literal::String(string.to_owned()).to_gd(),
    }
  }

}
