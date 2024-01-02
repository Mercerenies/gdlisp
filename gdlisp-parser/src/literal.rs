// Copyright 2023, 2024 Silvio Mayolo
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

//! Literal values at the abstract syntax tree level.

use crate::string::insert_escapes;

use ordered_float::OrderedFloat;

use std::fmt;

/// A GDLisp literal AST. A literal is an AST that is not recursive,
/// i.e. does not contain any more ASTs inside of it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
  /// A nil value, or `()`. This is comparable to `null` in some
  /// languages but also functions as the empty list.
  Nil,
  /// A literal 32-bit integer value.
  Int(i32),
  /// A literal floating-point value. For AST purposes, we do not use
  /// standard IEEE comparison semantics and instead use
  /// [`OrderedFloat`], whose ordering and equality relations satisfy
  /// convenient abstract mathematical properties.
  Float(OrderedFloat<f32>),
  /// A literal string.
  String(String),
  /// A literal symbol.
  Symbol(String),
  /// A literal Boolean value.
  Bool(bool),
}

impl Literal {

  pub fn symbol<S>(s: S) -> Literal
  where String : From<S> {
    Literal::Symbol(s.into())
  }

  pub fn string<S>(s: S) -> Literal
  where String : From<S> {
    Literal::String(s.into())
  }

}

/// Pretty-print the atomic AST value, using a format compatible with
/// [`parser`](crate::parser).
impl fmt::Display for Literal {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Literal::Nil => write!(f, "()"),
      Literal::Int(n) => write!(f, "{}", n),
      Literal::Bool(true) => write!(f, "#t"),
      Literal::Bool(false) => write!(f, "#f"),
      Literal::Float(x) => write!(f, "{}", x),
      Literal::String(s) => write!(f, "\"{}\"", insert_escapes(s)),
      Literal::Symbol(s) => write!(f, "{}", s),
    }
  }

}

impl From<i32> for Literal {
  fn from(value: i32) -> Literal {
    Literal::Int(value)
  }
}

impl From<f32> for Literal {
  fn from(value: f32) -> Literal {
    Literal::Float(value.into())
  }
}

impl From<OrderedFloat<f32>> for Literal {
  fn from(value: OrderedFloat<f32>) -> Literal {
    Literal::Float(value)
  }
}

impl From<String> for Literal {
  fn from(value: String) -> Literal {
    Literal::String(value)
  }
}

impl<'a> From<&'a str> for Literal {
  fn from(value: &'a str) -> Literal {
    Literal::String(String::from(value))
  }
}

impl From<bool> for Literal {
  fn from(value: bool) -> Literal {
    Literal::Bool(value)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn basic_literal_output_test() {
    assert_eq!(Literal::Nil.to_string(), String::from("()"));
    assert_eq!(Literal::Int(99).to_string(), String::from("99"));
    assert_eq!(Literal::Bool(true).to_string(), String::from("#t"));
    assert_eq!(Literal::Bool(false).to_string(), String::from("#f"));
    assert_eq!(Literal::from(1.23).to_string(), String::from("1.23"));
  }

  #[test]
  fn symbol_output_test() {
    assert_eq!(Literal::symbol("foo").to_string(), String::from("foo"));
  }

  #[test]
  fn basic_string_output_test() {
    assert_eq!(Literal::from("").to_string(), String::from("\"\""));
    assert_eq!(Literal::from("foo").to_string(), String::from("\"foo\""));
  }

  #[test]
  fn escaped_string_output_test() {
    assert_eq!(Literal::from(r#"words in "quotes""#).to_string(), String::from(r#""words in \"quotes\"""#));
  }

}
