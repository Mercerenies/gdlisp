
pub struct FreshNameGenerator<'a> {
  reserved: Vec<&'a str>,
  index: u32,
}

impl<'a> FreshNameGenerator<'a> {

  pub const DEFAULT_PREFIX: &'static str = "_G";

  pub fn new(reserved: Vec<&'a str>) -> FreshNameGenerator<'a> {
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

}
