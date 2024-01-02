// Copyright 2024 Silvio Mayolo
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

