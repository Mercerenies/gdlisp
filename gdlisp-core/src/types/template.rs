
use gdextension_serde::to_variant;

use godot::prelude::{Dictionary, FromGodot};
use serde::{Serialize, Deserialize};
use serde_repr::{Serialize_repr, Deserialize_repr};

/// Rust-side adaptation of the `ScriptLanguage::ScriptTemplate` Godot
/// type. This struct also provides a mechanism to translate it into a
/// `Dictionary` compatible with
/// `ScriptLanguageExtension::get_built_in_templates`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScriptTemplate {
  pub inherit: String,
  pub name: String,
  pub description: String,
  pub content: String,
  pub id: i32,
  pub origin: TemplateLocation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize_repr, Deserialize_repr)]
#[repr(i32)]
pub enum TemplateLocation {
  BuiltIn,
  Editor,
  Project,
}

impl ScriptTemplate {

  // TODO Can we use serde to automate some of this?
  pub fn into_dictionary(self) -> Dictionary {
    Dictionary::from_variant(&to_variant(&self).unwrap())
  }

}

