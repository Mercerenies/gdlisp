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

//! Provides the [`ClassExtends`] type, for qualified names valid in
//! an `extends` clause.

use super::literal::Literal;

/// A descriptor of what class is being extended.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassExtends {
  /// A qualified name, separating the final identifier from the
  /// left-hand side by a dot.
  Qualified(Box<ClassExtends>, String),
  /// A simple, unquoted identifier.
  SimpleIdentifier(String),
  /// A string literal, referencing a known file name.
  StringLit(String),
}

impl ClassExtends {

  /// Qualifies the given `extends` name with an additional
  /// identifier.
  pub fn attribute(self, attr: impl Into<String>) -> ClassExtends {
    ClassExtends::Qualified(Box::new(self), attr.into())
  }

  /// Convert `self` to a string suitable as the tail end of a
  /// `extends` clause in GDScript.
  pub fn to_gd(&self) -> String {
    match self {
      ClassExtends::Qualified(lhs, rhs) => format!("{}.{}", lhs.to_gd(), rhs),
      ClassExtends::SimpleIdentifier(string) => string.to_owned(),
      ClassExtends::StringLit(string) => Literal::String(string.to_owned()).to_gd(),
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_simple_identifier() {
    let cls = ClassExtends::SimpleIdentifier(String::from("foobar"));
    assert_eq!(cls.to_gd(), "foobar");
  }

  #[test]
  fn test_string_literal() {
    let cls = ClassExtends::StringLit(String::from("abc"));
    assert_eq!(cls.to_gd(), "\"abc\"");
  }

  #[test]
  fn test_string_literal_escaped() {
    let cls = ClassExtends::StringLit(String::from(r#"abc"\def"#));
    assert_eq!(cls.to_gd(), r#""abc\"\\def""#);
  }

  #[test]
  fn test_qualified_identifier() {
    let cls = ClassExtends::SimpleIdentifier(String::from("foobar")).attribute("baz").attribute("xyz");
    assert_eq!(cls.to_gd(), "foobar.baz.xyz");
  }

  #[test]
  fn test_qualified_path() {
    let cls = ClassExtends::StringLit(String::from("foobar")).attribute("baz").attribute("xyz");
    assert_eq!(cls.to_gd(), "\"foobar\".baz.xyz");
  }

}
