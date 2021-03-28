
use json::{JsonValue, object};

use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FreshNameGenerator<'a> {
  reserved: Vec<Cow<'a, str>>,
  index: u32,
}

// Placeholder enum that has a single dummy error value right now. We
// can fill in more details later if we feel the need to be any more
// specific than "JSON parse failed".
#[derive(Clone, Debug)]
pub enum ParseError {
  MalformedInput,
}

impl<'a> FreshNameGenerator<'a> {

  pub const DEFAULT_PREFIX: &'static str = "_G";

  pub fn new(reserved: Vec<&'a str>) -> FreshNameGenerator<'a> {
    let reserved: Vec<_> = reserved.into_iter().map(|x| Cow::Borrowed(x)).collect();
    FreshNameGenerator {
      reserved: reserved,
      index: 0,
    }
  }

  pub fn generate(&mut self) -> String {
    self.generate_with(FreshNameGenerator::DEFAULT_PREFIX)
  }

  pub fn generate_with(&mut self, prefix: &str) -> String {
    let mut name: String;
    loop {
      name = format!("{}_{}", prefix, self.index);
      self.index += 1;
      if !self.reserved.contains(&Cow::Borrowed(&*name)) {
        break;
      }
    }
    name
  }

  pub fn to_json(&self) -> JsonValue {
    let reserved: Vec<_> = self.reserved.iter().map(|x| (**x).to_owned()).collect();
    object!{
      "reserved" => reserved,
      "index" => self.index,
    }
  }

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
    let reserved = reserved.into_iter()
      .map(|value| value.as_str().ok_or(ParseError::MalformedInput).map(|string| {
        Cow::Owned(string.to_owned())
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
