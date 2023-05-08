
use godot::prelude::{Dictionary, Variant};

use std::iter::FromIterator;

/// Rust-side adaptation of the `ScriptLanguage::ScriptTemplate` Godot
/// type. This struct also provides a mechanism to translate it into a
/// `Dictionary` compatible with
/// `ScriptLanguageExtension::get_built_in_templates`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScriptTemplate {
  pub inherit: String,
  pub name: String,
  pub description: String,
  pub content: String,
  pub id: i32,
  pub origin: TemplateLocation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i32)]
pub enum TemplateLocation {
  BuiltIn,
  Editor,
  Project,
}

impl ScriptTemplate {

  // TODO Can we use serde to automate some of this?
  pub fn into_dictionary(self) -> Dictionary {
    Dictionary::from_iter([
      ("inherit", Variant::from(self.inherit)),
      ("name", Variant::from(self.name)),
      ("description", Variant::from(self.description)),
      ("content", Variant::from(self.content)),
      ("id", Variant::from(self.id)),
      ("origin", Variant::from(self.origin as i32)),
    ])
  }

}

