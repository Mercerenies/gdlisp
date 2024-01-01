
use gdextension_serde::to_variant;

use gd_rehearse::itest::*;
use godot::prelude::*;

#[gditest]
fn serialize_bool_test() {
  let variant = to_variant(&false).unwrap();
  assert_eq!(variant.get_type(), VariantType::Bool);
  assert_eq!(variant.to::<bool>(), false);

  let variant = to_variant(&true).unwrap();
  assert_eq!(variant.get_type(), VariantType::Bool);
  assert_eq!(variant.to::<bool>(), true);
}
