
//! Helpers for managing GDLisp and GDScript names.

use phf::{phf_map};

pub mod contextual;
pub mod fresh;
pub mod generator;

// Note: Many of these translations are based on the similar
// translations performed by the Scala compiler.
//
// https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/util/NameTransformer.scala
const TRANSLATIONS: phf::Map<char, &'static str> = phf_map! {
  '-' => "_",
  '<' => "_LT_",
  '>' => "_GT_",
  '=' => "_EQ_",
  '~' => "_TILDE_",
  '!' => "_BANG_",
  '#' => "_HASH_",
  '%' => "_PERCENT_",
  '^' => "_UP_",
  '&' => "_AMP_",
  '|' => "_BAR_",
  '*' => "_TIMES_",
  '/' => "_DIV_",
  '+' => "_PLUS_",
  ':' => "_COLON_",
  '\\' => "_BSLASH_",
  '?' => "_QMARK_",
  '@' => "_AT_",
  '$' => "_DSIGN_",
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
  "Color", "NodePath", "RID", "Object", "PI", "SPKEY", "typeof",
];
// TODO Add all of the @GDScript built-in names here, since we (based on cursory testing) cannot override those names locally and expect anything good to come of it.

/// A `NameTrans` is stored information about how a given GDLisp name
/// translates into a GDScript name.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameTrans {
  pub lisp_name: String,
  pub gd_name: String,
}

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
/// assert_eq!(lisp_to_gd("w~~w"), "w_TILDE__TILDE_w");
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
    format!("_{}", name)
  } else {
    lisp_to_gd_bare(name)
  }
}

/// This implements the same transformations as [`lisp_to_gd`], except
/// that known GDScript keywords are not underscore-escaped. See the
/// documentation for that function for details on the translations
/// which take place.
///
/// This function should be used sparingly, as it can result in
/// generating GDScript code that attempts to use names like `if` as
/// an identifier, which is obviously an error. However, it can also
/// be used to bypass the usual keyword-escaping rules, to allow
/// direct calls to the GDScript `Vector2` or `Dictionary` types,
/// which are normally barred in GDLisp.
pub fn lisp_to_gd_bare(name: &str) -> String {
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
    assert_eq!(lisp_to_gd("~a~"), "_TILDE_a_TILDE_");
    assert_eq!(lisp_to_gd("!"), "_BANG_");
    assert_eq!(lisp_to_gd("+1+"), "_PLUS_1_PLUS_");
    assert_eq!(lisp_to_gd("1^2"), "_1_UP_2");
    assert_eq!(lisp_to_gd("a&&b"), "a_AMP__AMP_b");
    assert_eq!(lisp_to_gd("a||b"), "a_BAR__BAR_b");
    assert_eq!(lisp_to_gd("a*b"), "a_TIMES_b");
    assert_eq!(lisp_to_gd("a/b"), "a_DIV_b");
    assert_eq!(lisp_to_gd("a+b"), "a_PLUS_b");
    assert_eq!(lisp_to_gd("keyword:"), "keyword_COLON_");
    assert_eq!(lisp_to_gd("\\"), "_BSLASH_");
    assert_eq!(lisp_to_gd("?-not-at-end"), "_QMARK__not_at_end");
    assert_eq!(lisp_to_gd("@annotation"), "_AT_annotation");
    assert_eq!(lisp_to_gd("$jquery"), "_DSIGN_jquery");
  }

  #[test]
  fn starts_with_number() {
    assert_eq!(lisp_to_gd("99"), "_99");
    assert_eq!(lisp_to_gd("d99"), "d99");
  }

  #[test]
  fn keywords() {
    assert_eq!(lisp_to_gd("if"), "_if");
    assert_eq!(lisp_to_gd("while"), "_while");
    assert_eq!(lisp_to_gd("While"), "While"); // No translation necessary
    assert_eq!(lisp_to_gd("PI"), "_PI");
  }

  #[test]
  fn special_cases_bare() {
    assert_eq!(lisp_to_gd_bare("bar->baz"), "bar_to_baz");
    assert_eq!(lisp_to_gd_bare("success?"), "is_success");
    assert_eq!(lisp_to_gd_bare("->->?"), "is__to__to_");
  }

  #[test]
  fn translations_bare() {
    assert_eq!(lisp_to_gd_bare("foo-bar"), "foo_bar");
    assert_eq!(lisp_to_gd_bare("foo-bar_baz"), "foo_bar_baz");
    assert_eq!(lisp_to_gd_bare(">>="), "_GT__GT__EQ_");
  }

  #[test]
  fn keywords_bare() {
    assert_eq!(lisp_to_gd_bare("if"), "if");
    assert_eq!(lisp_to_gd_bare("while"), "while");
    assert_eq!(lisp_to_gd_bare("While"), "While"); // No translation necessary
    assert_eq!(lisp_to_gd_bare("PI"), "PI");
  }

}
