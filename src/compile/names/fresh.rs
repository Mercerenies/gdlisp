
pub struct FreshNameGenerator<'a> {
  reserved: Vec<&'a str>,
  prefix: String,
  index: u32,
}

impl<'a> FreshNameGenerator<'a> {

  pub fn default_prefix() -> String {
    String::from("_G")
  }

  pub fn with_prefix(reserved: Vec<&'a str>, prefix: String) -> FreshNameGenerator<'a> {
    FreshNameGenerator {
      reserved: reserved,
      prefix: prefix,
      index: 0,
    }
  }

  pub fn new(reserved: Vec<&'a str>) -> FreshNameGenerator<'a> {
    FreshNameGenerator::with_prefix(reserved, FreshNameGenerator::default_prefix())
  }

  pub fn generate(&mut self) -> String {
    let mut name: String;
    loop {
      name = format!("{}{}", self.prefix, self.index);
      self.index += 1;
      if !self.reserved.contains(&&*name) {
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
    assert_eq!(gen.generate(), "_G0");
    assert_eq!(gen.generate(), "_G1");
    assert_eq!(gen.generate(), "_G2");
  }

  #[test]
  fn unrelated_generate() {
    let mut gen = FreshNameGenerator::new(vec!("foo", "bar", "these_names_change_nothing", "a99"));
    assert_eq!(gen.generate(), "_G0");
    assert_eq!(gen.generate(), "_G1");
    assert_eq!(gen.generate(), "_G2");
  }

  #[test]
  fn conflicting_generate() {
    let mut gen = FreshNameGenerator::new(vec!("_G1", "_G3"));
    assert_eq!(gen.generate(), "_G0");
    assert_eq!(gen.generate(), "_G2");
    assert_eq!(gen.generate(), "_G4");
  }

  #[test]
  fn custom_prefix() {
    let mut gen = FreshNameGenerator::with_prefix(vec!("foo1", "foo3", "_G0"), String::from("foo"));
    assert_eq!(gen.generate(), "foo0");
    assert_eq!(gen.generate(), "foo2");
    assert_eq!(gen.generate(), "foo4");
  }

}
