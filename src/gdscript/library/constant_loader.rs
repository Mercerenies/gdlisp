
use super::gdnative::NativeClasses;
use crate::ir::decl::EnumDecl;
use crate::ir::export::Visibility;
use crate::ir::expr::{Expr, ExprF};
use crate::pipeline::source::SourceOffset;

use phf::phf_map;

use std::borrow::Borrow;

const ENUM_GROUPINGS_BY_PREFIX: phf::Map<&'static str, &'static str> = phf_map! {
  "BUTTON_" => "Button",
  "CORNER_" => "Corner",
  "ERR_" => "Err",
  "HALIGN_" => "HAlign",
  "JOY_" => "Joy",
  "KEY_" => "Key",
  "MARGIN_" => "Margin",
  "METHOD_FLAG_" => "MethodFlag",
  "MIDI_MESSAGE_" => "MidiMessage",
  "OP_" => "Op",
  "PROPERTY_HINT_" => "PropertyHint",
  "PROPERTY_USAGE_" => "PropertyUsage",
  "TYPE_" => "Type",
  "VALIGN_" => "VAlign",
};

/// An enumeration of GDScript-side constants. This can be converted
/// (via [`From::from`]) into an [`EnumDecl`].
pub struct ConstantEnum {
  pub name: String,
  pub clauses: Vec<(String, String)>,
  pub pos: SourceOffset,
}

pub fn get_all_constants(classes: &NativeClasses) -> impl Iterator<Item=&str> {
  let global_constants = classes.get_global_constants();
  global_constants.constants.keys().map(String::borrow)
}

pub fn get_all_constant_enums(classes: &NativeClasses) -> Vec<ConstantEnum> {
  get_all_constant_enums_with_leftovers(classes).0
}

fn get_all_constant_enums_with_leftovers(classes: &NativeClasses) -> (Vec<ConstantEnum>, Vec<&str>) {
  todo!()
}

impl From<ConstantEnum> for EnumDecl {
  fn from(constant_enum: ConstantEnum) -> EnumDecl {
    let pos = constant_enum.pos;
    let clauses: Vec<(String, Option<Expr>)> =
      constant_enum.clauses.into_iter()
      .map(|(name, value)| (name, Some(Expr::new(ExprF::LocalVar(value), pos))))
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
    let (_, leftover_constants) = get_all_constant_enums_with_leftovers(&native_classes);
    assert!(leftover_constants.is_empty(), "The following constants were leftover after classification: {:?}", leftover_constants);
  }

}
