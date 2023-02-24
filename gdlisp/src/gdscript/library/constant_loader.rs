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

use super::gdnative::NativeClasses;
use crate::ir::decl::EnumDecl;
use crate::ir::export::Visibility;
use crate::ir::expr::Expr;
use crate::pipeline::source::SourceOffset;
use crate::util::prefix_matcher::PrefixMatcher;

use phf::{phf_map, phf_set};

use std::borrow::Borrow;
use std::collections::HashMap;

const ENUM_GROUPINGS_BY_PREFIX: phf::Map<&'static str, &'static str> = phf_map! {
  "BUTTON_" => "Mouse",
  "CORNER_" => "Corner",
  "ERR_" => "Err",
  "HALIGN_" => "HAlign",
  "JOY_" => "Joy",
  "KEY_" => "Key",
  "KEY_MASK_" => "KeyMask",
  "MARGIN_" => "Margin",
  "METHOD_FLAG_" => "MethodFlag",
  "MIDI_MESSAGE_" => "MidiMessage",
  "OP_" => "Op",
  "PROPERTY_HINT_" => "PropertyHint",
  "PROPERTY_USAGE_" => "PropertyUsage",
  "TYPE_" => "Type",
  "VALIGN_" => "VAlign",
};

// Constants which are known in GDScript but which do not belong to an
// enum.
const ENUM_BLACKLIST: phf::Set<&'static str> = phf_set! { "SPKEY" };

// Names which do not follow the usual prefixing rules.
const ENUM_CUSTOM_NAMES: phf::Map<&'static str, &'static str> = phf_map! {
  "OK" => "Err",
  "FAILED" => "Err",
  "METHOD_FLAGS_DEFAULT" => "MethodFlag",
  "HORIZONTAL" => "Orientation",
  "VERTICAL" => "Orientation",
};

lazy_static! {

  static ref ENUM_GROUPINGS_TABLE: PrefixMatcher<'static> =
    PrefixMatcher::build(ENUM_GROUPINGS_BY_PREFIX.keys());

}

/// An enumeration of GDScript-side constants. This can be converted
/// (via [`From::from`]) into an [`EnumDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstantEnum {
  pub name: String,
  pub clauses: Vec<(String, String)>,
  pub pos: SourceOffset,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum EnumGrouping {
  Blacklist,
  Unidentified,
  // Matches the enum with the given enumeration name and item name.
  Matched(String, String),
}

pub fn get_all_constants(classes: &NativeClasses) -> impl Iterator<Item=&str> {
  let global_constants = classes.get_global_constants();
  global_constants.constants.keys().map(String::borrow)
}

pub fn get_all_constant_enums(classes: &NativeClasses, pos: SourceOffset) -> Vec<ConstantEnum> {
  get_all_constant_enums_with_leftovers(classes, pos).0
}

fn get_all_constant_enums_with_leftovers(classes: &NativeClasses, pos: SourceOffset) -> (Vec<ConstantEnum>, Vec<&str>) {
  let table = &ENUM_GROUPINGS_TABLE;
  let mut enums: HashMap<String, ConstantEnum> = HashMap::new();
  let mut unidentified: Vec<&str> = Vec::new();

  for constant_name in get_all_constants(classes) {
    match identify_enum_grouping(table, constant_name) {
      EnumGrouping::Blacklist => {
        // Skip entirely, we are aware of it and don't want it in an
        // enum.
      }
      EnumGrouping::Unidentified => {
        // Unidentified
        unidentified.push(constant_name);
      }
      EnumGrouping::Matched(enum_name, enum_item_name) => {
        let enum_name_clone = enum_name.clone();
        let enum_entry: &mut ConstantEnum = enums.entry(enum_name).or_insert_with(|| {
          ConstantEnum {
            name: enum_name_clone,
            clauses: Vec::new(),
            pos: pos,
          }
        });
        enum_entry.clauses.push((enum_item_name, constant_name.to_owned()));
      }
    }
  }

  // For the sake of consistency, always return the enums (and their
  // entries) in alphabetical order
  let mut enums: Vec<_> = enums.into_values().collect();
  enums.sort_unstable_by(|a, b| a.name.cmp(&b.name));
  for constant_enum in &mut enums {
    constant_enum.clauses.sort_unstable_by(|a, b| a.0.cmp(&b.0));
  }

  (enums, unidentified)

}

fn identify_enum_grouping(table: &PrefixMatcher<'_>, constant_name: &str) -> EnumGrouping {
  if ENUM_BLACKLIST.contains(constant_name) {
    // Blacklist, omit entirely
    EnumGrouping::Blacklist
  } else if let Some(custom_name) = ENUM_CUSTOM_NAMES.get(constant_name) {
    // Match in custom names list
    EnumGrouping::Matched((*custom_name).to_owned(), constant_name.to_owned())
  } else if let Some(prefix) = table.identify_prefix(constant_name) {
    // Match in prefix rules
    let enum_name = ENUM_GROUPINGS_BY_PREFIX[prefix];
    let enum_item_name = constant_name.strip_prefix(prefix).expect("Prefix did not match");
    EnumGrouping::Matched(enum_name.to_owned(), enum_item_name.to_owned())
  } else {
    // No match
    EnumGrouping::Unidentified
  }
}

impl From<ConstantEnum> for EnumDecl {
  fn from(constant_enum: ConstantEnum) -> EnumDecl {
    let pos = constant_enum.pos;
    let clauses: Vec<(String, Option<Expr>)> =
      constant_enum.clauses.into_iter()
      .map(|(name, value)| (name, Some(Expr::var(value, pos))))
      .collect();
    EnumDecl {
      visibility: Visibility::Public,
      name: constant_enum.name,
      clauses: clauses,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  // We should be able to classify, in some form or another, every
  // single constant in GDScript. If there's one that's unknown (for
  // instance, because we updated Godot versions and didn't add it),
  // then this test will fail and will let us know.
  #[test]
  fn all_constants_classified_test() {
    let native_classes = NativeClasses::get_api_from_godot().unwrap();
    let (_, leftover_constants) = get_all_constant_enums_with_leftovers(&native_classes, SourceOffset(0));
    assert!(leftover_constants.is_empty(), "The following constants were leftover after classification: {:?}", leftover_constants);
  }

}
