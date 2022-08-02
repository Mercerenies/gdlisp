
//! Helper constants for the collection of reserved words in GDScript.

use crate::gdscript::library::gdnative;

use std::collections::HashSet;
use std::borrow::Cow;

/// All of the words which have special syntactic meaning in GDScript.
///
/// Pulled from `godot/modules/gdscript_editor.cpp`.
const GDSCRIPT_KEYWORDS: [&str; 49] = [
  "if", "elif", "else", "for", "while", "match", "break",
  "continue", "pass", "return", "class", "class_name", "extends",
  "is", "as", "self", "tool", "signal", "func", "static", "const",
  "enum", "var", "onready", "export", "setget", "breakpoint", "preload",
  "yield", "assert", "remote", "master", "slave", "puppet", "remotesync",
  "mastersync", "puppetsync", "sync", "not", "and", "or", "in",
  // TODO This will be a separate case in a moment :)
  "typeof", "str", "printerr", "printraw", "print_debug", "prints", "printt",
];

/// The GDScript top-level global constant names which are not
/// included in `api.json`.
///
/// Pulled from `godot/modules/gdscript_editor.cpp`.
const GLOBAL_CONSTANTS: [&str; 6] = [
  "TAU", "INF", "NAN", "PI", "true", "false",
];

/// The types in GDScript whose names are considered reserved.
///
/// Pulled from `godot/modules/gdscript_editor.cpp`.
const NAMED_PRIMITIVE_TYPES: [&str; 27] = [
  "null", "bool", "int", "float", "String", "Vector2", "Rect2", "Vector3", "Transform2D",
  "Plane", "Quat", "AABB", "Basis", "Transform", "Color", "NodePath", "RID", "Object",
  "Array", "Dictionary", "PoolByteArray", "PoolIntArray", "PoolRealArray", "PoolStringArray",
  "PoolVector2Array", "PoolVector3Array", "PoolColorArray",
];

fn get_all_reserved_words() -> HashSet<Cow<'static, str>> {
  let api = gdnative::get_api_from_godot().expect("Could not read GDNative API from Godot binary");
  let mut set: HashSet<Cow<'static, str>> = HashSet::with_capacity(100);

  // GDScript keywords (hard-coded into GDLisp above)
  set.extend(GDSCRIPT_KEYWORDS.iter().map(|x| Cow::Borrowed(*x)));

  // Global constants defined in the GlobalConstants class
  let global_constants =
    api.get(gdnative::GLOBAL_CONSTANTS_CLASS).expect("Could not read global constants from GDNative API");
  set.extend(global_constants.constants.keys().map(|x| Cow::Owned(x.clone())));

  // Extra global constants
  set.extend(GLOBAL_CONSTANTS.iter().map(|x| Cow::Borrowed(*x)));

  // Named primitive types
  set.extend(NAMED_PRIMITIVE_TYPES.iter().map(|x| Cow::Borrowed(*x)));

  set
}

lazy_static! {

  /// All of the reserved words, as a hash set which can be checked
  /// for membership efficiently.
  pub static ref RESERVED_WORDS: HashSet<Cow<'static, str>> =
    get_all_reserved_words();

}
