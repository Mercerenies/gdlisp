
use gdextension_serde::to_variant;

use gd_rehearse::itest::*;
use godot::prelude::*;
use serde::Serialize;

use std::ffi::CString;
use std::collections::HashMap;

#[derive(Serialize)]
struct UnitStruct;

#[derive(Serialize)]
struct NewtypeStruct<T>(T);

#[derive(Serialize)]
struct RegularStruct {
  foo: i64,
  bar: String,
}

#[derive(Serialize)]
enum MyEnum {
  UnitEnum,
  NewtypeEnum(i64),
  StructEnum { x: i64 },
}

#[gditest]
fn serialize_unit_test() {
  let variant = to_variant(&()).unwrap();
  assert_eq!(variant.get_type(), VariantType::Nil);
}

#[gditest]
fn serialize_bool_test() {
  let variant = to_variant(&false).unwrap();
  assert_eq!(variant.get_type(), VariantType::Bool);
  assert_eq!(variant.to::<bool>(), false);

  let variant = to_variant(&true).unwrap();
  assert_eq!(variant.get_type(), VariantType::Bool);
  assert_eq!(variant.to::<bool>(), true);
}

#[gditest]
fn serialize_signed_int_test() {
  let variant = to_variant(&(10i8)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 10);

  let variant = to_variant(&(7i16)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 7);

  let variant = to_variant(&(0i32)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 0);

  let variant = to_variant(&(-31i64)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), -31);

  let variant = to_variant(&(31i64)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 31);
}

#[gditest]
fn serialize_unsigned_int_test() {
  let variant = to_variant(&(10u8)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 10);

  let variant = to_variant(&(7u16)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 7);

  let variant = to_variant(&(0u32)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 0);

  let variant = to_variant(&(31u64)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 31);
}

#[gditest]
fn serialize_float_test() {
  let variant = to_variant(&(5.5f32)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Float);
  assert_eq!(variant.to::<f64>(), 5.5);

  let variant = to_variant(&(-5.5f64)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Float);
  assert_eq!(variant.to::<f64>(), -5.5);
}

#[gditest]
fn serialize_str_test() {
  let variant = to_variant(&"abc def").unwrap();
  assert_eq!(variant.get_type(), VariantType::String);
  assert_eq!(variant.to::<String>(), String::from("abc def"));
}

#[gditest]
fn serialize_char_test() {
  let variant = to_variant(&'a').unwrap();
  assert_eq!(variant.get_type(), VariantType::String);
  assert_eq!(variant.to::<String>(), String::from("a"));
}

#[gditest]
fn serialize_byte_array_test() {
  // Serde serializes CStr as a byte array.
  let bytes = CString::new("abcd").unwrap();
  let variant = to_variant(&bytes).unwrap();
  assert_eq!(variant.get_type(), VariantType::PackedByteArray);
  let godot_bytes = variant.to::<PackedByteArray>();
  assert_eq!(godot_bytes.as_slice(), &[0x61, 0x62, 0x63, 0x64]);
}

#[gditest]
fn serialize_array_test() {
  let vector = vec![10, 20, 30];
  let variant = to_variant(&vector).unwrap();
  assert_eq!(variant.get_type(), VariantType::Array);
  let godot_array = variant.to::<VariantArray>();
  assert_eq!(godot_array.len(), 3);
  assert_eq!(godot_array.get(0).to::<i64>(), 10);
  assert_eq!(godot_array.get(1).to::<i64>(), 20);
  assert_eq!(godot_array.get(2).to::<i64>(), 30);
}

#[gditest]
fn serialize_tuple_test() {
  let tuple = (10i64, 20i64, 30i64);
  let variant = to_variant(&tuple).unwrap();
  assert_eq!(variant.get_type(), VariantType::Array);
  let godot_array = variant.to::<VariantArray>();
  assert_eq!(godot_array.len(), 3);
  assert_eq!(godot_array.get(0).to::<i64>(), 10);
  assert_eq!(godot_array.get(1).to::<i64>(), 20);
  assert_eq!(godot_array.get(2).to::<i64>(), 30);
}

#[gditest]
fn serialize_map_test() {
  let map = {
    let mut map = HashMap::<String, i64>::new();
    map.insert(String::from("a"), 10);
    map.insert(String::from("b"), 100);
    map.insert(String::from("c"), -100);
    map
  };
  let variant = to_variant(&map).unwrap();
  assert_eq!(variant.get_type(), VariantType::Dictionary);
  let dictionary = variant.to::<Dictionary>();
  let expected_dictionary = {
    let mut dictionary = Dictionary::new();
    dictionary.insert("a", 10);
    dictionary.insert("b", 100);
    dictionary.insert("c", -100);
    dictionary
  };
  assert_eq!(dictionary, expected_dictionary);
}

#[gditest]
fn serialize_option_test() {
  let variant = to_variant(&None::<i64>).unwrap();
  assert_eq!(variant.get_type(), VariantType::Nil);

  let variant = to_variant(&Some(1i64)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 1);

  // Nested options are flattened
  let variant = to_variant(&Some(Some(1i64))).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 1);
}

#[gditest]
fn serialize_unit_struct_test() {
  let variant = to_variant(&UnitStruct).unwrap();
  assert_eq!(variant.get_type(), VariantType::Nil);
}

#[gditest]
fn serialize_newtype_struct_test() {
  let variant = to_variant(&NewtypeStruct(88i32)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Int);
  assert_eq!(variant.to::<i64>(), 88);

  let variant = to_variant(&NewtypeStruct("ABC")).unwrap();
  assert_eq!(variant.get_type(), VariantType::String);
  assert_eq!(variant.to::<String>(), String::from("ABC"));
}

#[gditest]
fn serialize_struct_test() {
  let my_struct = RegularStruct { foo: 10, bar: String::from("bar_value") };
  let variant = to_variant(&my_struct).unwrap();
  assert_eq!(variant.get_type(), VariantType::Dictionary);
  let dictionary = variant.to::<Dictionary>();
  assert_eq!(dictionary.len(), 2);
  assert_eq!(dictionary.get("foo").unwrap().to::<i64>(), 10);
  assert_eq!(dictionary.get("bar").unwrap().to::<String>(), String::from("bar_value"));
}

#[gditest]
fn serialize_unit_enum_test() {
  let variant = to_variant(&MyEnum::UnitEnum).unwrap();
  assert_eq!(variant.get_type(), VariantType::String);
  assert_eq!(variant.to::<String>(), String::from("UnitEnum"));
}

#[gditest]
fn serialize_newtype_enum_test() {
  let variant = to_variant(&MyEnum::NewtypeEnum(10)).unwrap();
  assert_eq!(variant.get_type(), VariantType::Dictionary);
  let dict = variant.to::<Dictionary>();
  let expected_dict = {
    let mut expected_dict = Dictionary::new();
    expected_dict.insert("NewtypeEnum", 10i64);
    expected_dict
  };
  assert_eq!(dict, expected_dict);
}

#[gditest]
fn serialize_struct_enum_test() {
  let my_value = MyEnum::StructEnum { x: 9 };
  let variant = to_variant(&my_value).unwrap();
  assert_eq!(variant.get_type(), VariantType::Dictionary);
  let dictionary = variant.to::<Dictionary>();
  assert_eq!(dictionary.len(), 1);
  let inner_dictionary = dictionary.get("StructEnum").unwrap().to::<Dictionary>();
  assert_eq!(inner_dictionary.len(), 1);
  assert_eq!(inner_dictionary.get("x").unwrap().to::<i64>(), 9);
}
