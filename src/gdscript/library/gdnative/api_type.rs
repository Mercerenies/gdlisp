
//! The [`ApiType`] enum, indicating whether a GDScript class is in
//! the core library or intended for editor tooling.

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApiType {
  Core,
  Tools,
}

impl ApiType {
  pub const CORE: &'static str = "core";
  pub const TOOLS: &'static str = "tools";

  pub fn as_string(&self) -> &'static str {
    match self {
      ApiType::Core => ApiType::CORE,
      ApiType::Tools => ApiType::TOOLS,
    }
  }

}
