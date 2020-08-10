
use phf::{phf_map};

// TODO More translations
const TRANSLATIONS: phf::Map<char, &'static str> = phf_map! {
  '-' => "_",
  '<' => "_LT_",
  '>' => "_GT_",
  '=' => "_EQ_",
};

pub fn is_valid_gd_char(ch: char) -> bool {
  ch.is_digit(36) || ch == '_'
}

pub fn lisp_to_gd(name: &str) -> String {
  // TODO Escape known GDScript keywords
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
  fn noop_escapes() {
    assert_eq!(lisp_to_gd("foobar"), "foobar");
    assert_eq!(lisp_to_gd("_private0"), "_private0");
    assert_eq!(lisp_to_gd("xposition3"), "xposition3");
    assert_eq!(lisp_to_gd("a0_0_EEe"), "a0_0_EEe");
  }

  #[test]
  fn leading_digit() {
    assert_eq!(lisp_to_gd("3e"), "_3e");
    assert_eq!(lisp_to_gd("2_"), "_2_");
  }

  #[test]
  fn special_cases() {
    assert_eq!(lisp_to_gd("foo->bar"), "foo_to_bar");
    assert_eq!(lisp_to_gd("failure?"), "is_failure");
    assert_eq!(lisp_to_gd("->->?"), "is__to__to_");
  }

  #[test]
  fn translations() {
    assert_eq!(lisp_to_gd("foo-bar"), "foo_bar");
    assert_eq!(lisp_to_gd("foo-bar_baz"), "foo_bar_baz");
    assert_eq!(lisp_to_gd("<=>"), "_LT__EQ__GT_");
  }

  #[test]
  fn general_codepoints() {
    assert_eq!(lisp_to_gd("w~~w"), "w_u007E_u007Ew");
    assert_eq!(lisp_to_gd("α"), "_u03B1");
  }

}
