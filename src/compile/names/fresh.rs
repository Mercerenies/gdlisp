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

//! Provides the struct [`FreshNameGenerator`] for safely generating
//! unused names.

use super::generator::NameGenerator;

use json::{JsonValue, object};

use std::borrow::ToOwned;

/// A `FreshNameGenerator` retains state for generating names which
/// are guaranteed to be unused.
///
/// The name generator retains a list of strings which it considers
/// "reserved words". Those words will never be generated by the
/// generator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FreshNameGenerator {
  reserved: Vec<String>,
  index: u32,
}

// Currently a placeholder enum that has a single dummy error value
// right now. We can fill in more details later if we feel the need to
// be any more specific than "JSON parse failed".

/// Parse errors that can occur during parsing of JSON as a
/// [`FreshNameGenerator`].
#[derive(Clone, Debug)]
pub enum ParseError {
  MalformedInput,
}

impl FreshNameGenerator {

  /// Construct a new `FreshNameGenerator` given a collection
  /// `reserved` of reserved words. The words in `reserved` are
  /// guaranteed to never be generated by this name generator object.
  pub fn new(reserved: Vec<&str>) -> FreshNameGenerator {
    let reserved: Vec<_> = reserved.into_iter().map(|x| x.to_owned()).collect();
    FreshNameGenerator {
      reserved: reserved,
      index: 0,
    }
  }

  /// Convert the `FreshNameGenerator` to a JSON value, suitable for
  /// restoring the generator later with
  /// [`FreshNameGenerator::from_json`].
  pub fn to_json(&self) -> JsonValue {
    let reserved: Vec<_> = self.reserved.iter().map(|x| (**x).to_owned()).collect();
    object!{
      "reserved" => reserved,
      "index" => self.index,
    }
  }

  /// Produce a `FreshNameGenerator` from a JSON value.
  ///
  /// For any `gen: FreshNameGenerator`, it should be the case that
  /// `from_json(&gen.to_json())` should be an object
  /// indistinguishable from `gen` for all practical purposes.
  pub fn from_json(value: &JsonValue) -> Result<Self, ParseError> {
    let value = match value {
      JsonValue::Object(value) => value,
      _ => return Err(ParseError::MalformedInput),
    };

    // reserved
    let reserved = match value.get("reserved") {
      Some(JsonValue::Array(reserved)) => reserved,
      _ => return Err(ParseError::MalformedInput),
    };
    let reserved = reserved.iter()
      .map(|value| value.as_str().ok_or(ParseError::MalformedInput).map(|string| {
        string.to_owned()
      }))
      .collect::<Result<_, _>>()?;

    // index
    let index = match value.get("index") {
      Some(JsonValue::Number(index)) => u32::from(*index),
      _ => return Err(ParseError::MalformedInput),
    };

    Ok(FreshNameGenerator { reserved, index })
  }

}

impl NameGenerator for FreshNameGenerator {

  fn generate_with(&mut self, prefix: &str) -> String {
    let mut name: String;
    loop {
      name = format!("{}_{}", prefix, self.index);
      self.index += 1;
      if !self.reserved.contains(&name) {
        break;
      }
    }
    name
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn empty_generate() {
    let mut gen = FreshNameGenerator::new(vec!());
    assert_eq!(gen.generate(), "_G_0");
    assert_eq!(gen.generate(), "_G_1");
    assert_eq!(gen.generate(), "_G_2");
  }

  #[test]
  fn unrelated_generate() {
    let mut gen = FreshNameGenerator::new(vec!("foo", "bar", "these_names_change_nothing", "a99"));
    assert_eq!(gen.generate(), "_G_0");
    assert_eq!(gen.generate(), "_G_1");
    assert_eq!(gen.generate(), "_G_2");
  }

  #[test]
  fn conflicting_generate() {
    let mut gen = FreshNameGenerator::new(vec!("_G_1", "_G_3"));
    assert_eq!(gen.generate(), "_G_0");
    assert_eq!(gen.generate(), "_G_2");
    assert_eq!(gen.generate(), "_G_4");
  }

  #[test]
  fn custom_prefix() {
    let mut gen = FreshNameGenerator::new(vec!("foo_1", "foo_3", "_G_0"));
    assert_eq!(gen.generate_with("foo"), "foo_0");
    assert_eq!(gen.generate_with("foo"), "foo_2");
    assert_eq!(gen.generate_with("foo"), "foo_4");
  }

  #[test]
  fn through_json() {
    let mut gen = FreshNameGenerator::new(vec!("abc", "def", "foo_1", "_example_names"));
    assert_eq!(gen.generate_with("foo"), "foo_0");
    assert_eq!(gen.generate_with("foo"), "foo_2");
    assert_eq!(gen.index, 3);
    let json = gen.to_json();
    let gen1 = FreshNameGenerator::from_json(&json).unwrap();
    assert_eq!(gen, gen1);
  }

}
