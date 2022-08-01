
//! Helper constants for the collection of reserved words in GDScript.

use std::collections::HashSet;
use std::borrow::Cow;

/// All of the words which have special syntactic meaning in GDScript.
const GDSCRIPT_KEYWORDS: [&str; 40] = [
  "if", "elif", "else", "for", "while", "match", "break",
  "continue", "pass", "return", "class", "class_name", "extends",
  "is", "as", "self", "tool", "signal", "func", "static", "const",
  "enum", "var", "onready", "export", "setget", "breakpoint", "preload",
  "yield", "assert", "remote", "master", "puppet", "remotesync", "mastersync",
  "puppetsync", "not", "and", "or",
  // NOTE: This will be a separate case in a moment :)
  "typeof",
];

/// The GDScript top-level global constant names.
const GLOBAL_CONSTANTS: [&str; 5] = [
  "TAU", "INF", "NAN", "PI", "SPKEY",
];

/// The types in GDScript whose names are considered reserved.
const NAMED_TYPES: [&str; 23] = [
  "Array", "Dictionary", "PoolByteArray", "PoolIntArray", "PoolRealArray", "PoolStringArray",
  "PoolVector2Array", "PoolVector3Array", "PoolColorArray", "String", "Vector2",
  "Rect2", "Vector3", "Transform2D", "Plane", "Quat", "AABB", "Basis", "Transform",
  "Color", "NodePath", "RID", "Object",
];

fn get_all_reserved_words() -> HashSet<Cow<'static, str>> {
  let mut set: HashSet<Cow<'static, str>> = HashSet::with_capacity(100);
  set.extend(GDSCRIPT_KEYWORDS.iter().map(|x| Cow::Borrowed(*x)));
  set.extend(GLOBAL_CONSTANTS.iter().map(|x| Cow::Borrowed(*x)));
  set.extend(NAMED_TYPES.iter().map(|x| Cow::Borrowed(*x)));
  set
}

lazy_static! {

  /// All of the reserved words, as a hash set which can be checked
  /// for membership efficiently.
  pub static ref RESERVED_WORDS: HashSet<Cow<'static, str>> =
    get_all_reserved_words();

}
