
//! Helpers for managing GDLisp and GDScript names.

use phf::{phf_map};

pub mod fresh;

// TODO More translations
const TRANSLATIONS: phf::Map<char, &'static str> = phf_map! {
  '-' => "_",
  '<' => "_LT_",
  '>' => "_GT_",
  '=' => "_EQ_",
};

const KNOWN_GDSCRIPT_KEYWORDS: &[&str] = &[
  "if", "elif", "else", "for", "while", "match", "break",
  "continue", "pass", "return", "class", "class_name", "extends",
  "is", "as", "self", "tool", "signal", "func", "static", "const",
  "enum", "var", "onready", "export", "setget", "breakpoint", "preload",
  "yield", "assert", "remote", "master", "puppet", "remotesync", "mastersync",
  "puppetsync", "TAU", "INF", "NAN", "not", "and", "or", "Array",
  "Dictionary", "PoolByteArray", "PoolIntArray", "PoolRealArray", "PoolStringArray",
  "PoolVector2Array", "PoolVector3Array", "PoolColorArray", "String", "Vector2",
  "Rect2", "Vector3", "Transform2D", "Plane", "Quat", "AABB", "Basis", "Transform",
  "Color", "NodePath", "RID", "Object",
];

/// Whether `ch` is a valid GDScript identifier character.
///
/// Valid GDScript identifier characters are: uppercase and lowercase
/// ASCII letters, the digits 0-9, and underscore. Equivalently, this
/// function returns true if the character satisfies the regex
/// `[A-Za-z0-9_]`.
pub fn is_valid_gd_char(ch: char) -> bool {
  ch.is_digit(36) || ch == '_'
}

/// Converts `name` to a GDScript-friendly name.
///
/// First, if `name` is a known GDScript keyword (such as `if` or
/// `preload`), then an underscore is prefixed and the name is
/// returned. No further modifications are necessary.
///
/// Second, if the name begins with a digit, then an underscore is
/// prefixed before continuing to the next steps.
///
/// Finally, for every character which is *not* a valid GDScript
/// character (under [`is_valid_gd_char`]), a transformation takes
/// place to translate that character. The following transformations
/// are tried, in order.
///
/// 1. The sequence `->` is replaced by `_to_`.
///
/// 2. A question mark `?` at the end of an identifier is replaced by
/// `is_` at the beginning.
///
/// 3. `-`, `<`, `>`, and `=` are converted to, respectively, `_`,
/// `_LT_`, `_GT_`, `_EQ_`.
///
/// 4. All other characters are converted to `_uXXXX` where `XXXX` is
/// the hex value (minimum four digits) of the Unicode code point for
/// the character. If the character lies outside the basic
/// multilingual plane, then all digits will be printed.
///
/// # Examples
///
/// ```
/// # use gdlisp::compile::names::lisp_to_gd;
/// // No escaping necessary
/// assert_eq!(lisp_to_gd("foobar"), "foobar");
/// assert_eq!(lisp_to_gd("_private0"), "_private0");
/// assert_eq!(lisp_to_gd("xposition3"), "xposition3");
/// assert_eq!(lisp_to_gd("a0_0_EEe"), "a0_0_EEe");
///
/// // Leading digit must be escaped
/// assert_eq!(lisp_to_gd("3e"), "_3e");
/// assert_eq!(lisp_to_gd("2_"), "_2_");
///
/// // Special transformation rules
/// assert_eq!(lisp_to_gd("foo->bar"), "foo_to_bar");
/// assert_eq!(lisp_to_gd("failure?"), "is_failure");
///
/// // Known character translations
/// assert_eq!(lisp_to_gd("my-identifier"), "my_identifier");
/// assert_eq!(lisp_to_gd("<=>"), "_LT__EQ__GT_");
///
/// // General codepoints
/// assert_eq!(lisp_to_gd("w~~w"), "w_u007E_u007Ew");
/// assert_eq!(lisp_to_gd("α"), "_u03B1");
/// assert_eq!(lisp_to_gd("😃"), "_u1F603");
///
/// // Known GDScript keywords
/// assert_eq!(lisp_to_gd("preload"), "_preload");
/// assert_eq!(lisp_to_gd("assert"), "_assert");
/// ```
pub fn lisp_to_gd(name: &str) -> String {
  // Escape known GDScript keywords
  if KNOWN_GDSCRIPT_KEYWORDS.iter().any(|kw| *kw == name) {
    return format!("_{}", name);
  }
  let length = name.chars().count();
  let mut result = String::with_capacity(2 * length);
  let mut iter = name.chars().peekable();
  let mut first = true;
  while let Some(ch) = iter.next() {
    if is_valid_gd_char(ch) {
      // Special exception if it's the first character and a digit.
      // Otherwise, leave it as is.
      if first && ch.is_digit(10) {
        result.push('_');
      }
      result.push(ch);
    } else {
      let next = iter.peek();
      match (ch, next) {
        ('-', Some('>')) => {
          iter.next();
          result.push_str("_to_");
        }
        ('?', None) => {
          result = format!("is_{}", result);
        }
        (_, _) => {
          if let Some(s) = TRANSLATIONS.get(&ch) {
            result.push_str(s);
          } else {
            result.push_str(&format!("_u{:04X}", ch as u32));
          }
        }
      }
    }
    first = false;
  }
  result
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn special_cases() {
    assert_eq!(lisp_to_gd("bar->baz"), "bar_to_baz");
    assert_eq!(lisp_to_gd("success?"), "is_success");
    assert_eq!(lisp_to_gd("->->?"), "is__to__to_");
  }

  #[test]
  fn translations() {
    assert_eq!(lisp_to_gd("foo-bar"), "foo_bar");
    assert_eq!(lisp_to_gd("foo-bar_baz"), "foo_bar_baz");
    assert_eq!(lisp_to_gd(">>="), "_GT__GT__EQ_");
  }

  #[test]
  fn keywords() {
    assert_eq!(lisp_to_gd("if"), "_if");
    assert_eq!(lisp_to_gd("while"), "_while");
    assert_eq!(lisp_to_gd("While"), "While"); // No translation necessary
    //assert_eq!(lisp_to_gd("PI"), "_PI"); // TODO This
  }

}
