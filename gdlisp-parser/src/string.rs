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

//! Helper functions for working with strings within
//! [`AST`](super::ast::AST) values.

use phf::{phf_map};

use std::collections::HashMap;
use std::fmt::Write;

// Note: \u{...} is a special case and is not listed here.
const ESCAPE_SEQUENCES: phf::Map<char, char> = phf_map! {
  'n' => '\n',
  't' => '\t',
  'r' => '\r',
  'a' => '\x07',
  'b' => '\x08',
  'f' => '\x0C',
  'v' => '\x0B',
  '"' => '"',
  '\'' => '\'',
  '\\' => '\\',
};

/// The type of errors produced by [`parse_escapes`].
#[derive(Debug)]
pub enum Error {
  /// An escape sequence `\` was not terminated.
  UnfinishedEscape,
  /// An escape sequence `\` was not recognized.
  InvalidEscape(char),
  /// A Unicode escape sequence `\u` was invalid.
  InvalidUnicodeEscape,
}

/// Given a string, parse escape sequences beginning with `\` and
/// produce a string containing the result.
///
/// Note that our `\u{...}` syntax is distinct from (but compatible
/// with) GDScript's. GDLisp supports two different Unicode escapes.
/// The first, identical to GDScript, consists of `\u` followed by
/// exactly four hexadecimal digits. This is capable of supporting any
/// character in the basic multilingual plane. The second consists of
/// `\u` followed by one or more hexadecimal digits contained in
/// mandatory curly braces. This is capable of representing *any*
/// Unicode character.
///
/// # Examples
///
/// ```
/// # use gdlisp_parser::string::parse_escapes;
/// assert_eq!(parse_escapes(r#"foobar"#).unwrap(), "foobar");
/// assert_eq!(parse_escapes(r#"foo\"\"bar\\"#).unwrap(), "foo\"\"bar\\");
/// assert_eq!(parse_escapes(r#"\u03B1\u03b1\u{3b1}\u{3B1}\u{000000003b1}"#).unwrap(), "ααααα");
/// ```
pub fn parse_escapes(input: &str) -> Result<String, Error> {
  let length = input.chars().count();
  let mut result = String::with_capacity(length);
  let mut iter = input.chars();
  while let Some(ch) = iter.next() {
    if ch == '\\' {
      match iter.next() {
        Some('u') => {
          parse_unicode_sequence(&mut iter, &mut result)?;
        }
        Some(esc) => {
          if let Some(value_char) = ESCAPE_SEQUENCES.get(&esc) {
            result.push(*value_char);
          } else {
            return Err(Error::InvalidEscape(esc));
          }
        }
        None => {
          return Err(Error::UnfinishedEscape);
        }
      }
    } else {
      result.push(ch);
    }
  }
  Ok(result)
}

/// Parses a Unicode escape sequence. It is assumed that this function
/// will be called immediately *after* parsing `\u` in the iterator.
/// This function can parse either four hexadecimal digits or a
/// brace-enclosed collection of at least one hexadecimal character.
///
/// In either case, the hexadecimal digits are case insensitive.
fn parse_unicode_sequence(iter: &mut impl Iterator<Item=char>,
                          dest: &mut impl Write) -> Result<(), Error> {
  let digits = match iter.next() {
    Some(first) if is_hex_digit(first) => {
      // A sequence of exactly four hexadecimal digits.
      let mut digits: String = String::with_capacity(4);
      digits.push(first);
      for _ in 0..3 {
        let next_char = iter.next().ok_or(Error::InvalidUnicodeEscape)?;
        if !is_hex_digit(next_char) {
          return Err(Error::InvalidUnicodeEscape);
        }
        digits.push(next_char);
      }
      digits
    }
    Some('{') => {
      let mut digits: String = String::with_capacity(8);
      loop {
        let next_char = iter.next().ok_or(Error::InvalidUnicodeEscape)?;
        if next_char == '}' {
          break;
        } else if !is_hex_digit(next_char) {
          return Err(Error::InvalidUnicodeEscape);
        }
        digits.push(next_char);
      }
      if digits.is_empty() {
        return Err(Error::InvalidUnicodeEscape);
      }
      digits
    }
    _ => {
      return Err(Error::InvalidUnicodeEscape);
    }
  };
  let codepoint = u32::from_str_radix(&digits, 16).expect("Internal error in parse_unicode_sequence");
  let ch = char::from_u32(codepoint).ok_or(Error::InvalidUnicodeEscape)?;
  write!(dest, "{}", ch).expect("Internal error in parse_unicode_sequence");
  Ok(())
}

/// Returns true if the character is a hexadecimal digit
/// (case-insensitive).
fn is_hex_digit(digit: char) -> bool {
  ('0'..='9').contains(&digit) ||
    ('a'..='f').contains(&digit) ||
    ('A'..='F').contains(&digit)
}

/// Given a string, insert Godot-style escape sequences to make the
/// string valid as a GDScript string literal. This involves escaping
/// any control characters whose scalar values are less than 32, as
/// well as quotes and backslashes. Characters outside of the standard
/// ASCII range may or may not be escaped.
///
/// ```
/// # use gdlisp_parser::string::insert_escapes;
/// assert_eq!(insert_escapes(r#"foobar"#), r#"foobar"#);
/// assert_eq!(insert_escapes(r#"a"b'c"#), r#"a\"b\'c"#);
/// assert_eq!(insert_escapes(r#"A\B"#), r#"A\\B"#);
/// assert_eq!(insert_escapes("\n"), "\\n");
/// assert_eq!(insert_escapes("α"), "α");
/// assert_eq!(insert_escapes("😀"), "😀");
/// assert_eq!(insert_escapes("\x06"), r#"\u0006"#);
/// ```
pub fn insert_escapes(input: &str) -> String {
  let mut result = String::with_capacity(input.len() + 16); // Just a bit of extra space for escape sequences.
  let reverse_map = reverse_escape_map();
  for ch in input.chars() {
    if should_escape(ch) {
      // Escape it
      match reverse_map.get(&ch) {
        None => {
          // Fall back to \uXXXX
          write!(result, "\\u{:04X}", ch as u32).expect("Failed to write to local string");
        }
        Some(esc) => {
          // Built-in escape sequence
          result.push_str(esc);
        }
      }
    } else {
      // Leave it
      result.push(ch);
    }
  }
  result
}

fn should_escape(ch: char) -> bool {
  ((ch as u32) < 32) || (ch == '"') || (ch == '\'') || (ch == '\\')
}

fn reverse_escape_map() -> HashMap<char, String> {
  let mut map: HashMap<char, String> = HashMap::new();
  for (escape_char, value_char) in &ESCAPE_SEQUENCES {
    map.insert(*value_char, format!("\\{}", escape_char));
  }
  map
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn successful_escaping_test() {
    assert_eq!(parse_escapes(r#"I said, \"foobar\"."#).unwrap(), "I said, \"foobar\".");
    assert_eq!(parse_escapes(r#"\'\'\'"#).unwrap(), "\'\'\'");
    assert_eq!(parse_escapes(r#"\\\\"#).unwrap(), "\\\\");
    assert_eq!(parse_escapes(r#"\a\b\n"#).unwrap(), "\x07\x08\n");
    assert_eq!(parse_escapes(r#"\f\v\r\t"#).unwrap(), "\x0C\x0B\r\t");
  }

  #[test]
  fn failed_escaping_test() {
    assert!(parse_escapes(r#"\"#).is_err());
    assert!(parse_escapes(r#"\I"#).is_err());
    assert!(parse_escapes(r#"\u"#).is_err());
    assert!(parse_escapes(r#"\u{00"#).is_err());
    assert!(parse_escapes(r#"\u13zz"#).is_err());
    assert!(parse_escapes(r#"\u{}"#).is_err());
    assert!(parse_escapes(r#"\u{potato}"#).is_err());
  }

}
