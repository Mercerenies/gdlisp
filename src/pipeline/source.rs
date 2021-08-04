
//! Wrapper struct and helpers for keeping track of positions in the
//! source code.
//!
//! All source positions always refer to the original GDLisp source
//! file, never to the position in an intermediate representation.

use std::fmt;

/// A `SourceOffset` is really just a [`usize`] representing a byte
/// offset into the original file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct SourceOffset(pub usize);

/// A `SourcePos` indicates a line and column number in a file, where
/// all offsets are indicated in *characters*.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourcePos {
  pub line: usize,
  pub column: usize,
}

/// Returns the position of all of the newlines in the source text.
/// For the purposes of this function, CARRIAGE RETURN (U+001D) and
/// NEWLINE (U+001A) are considered to be newlines. Additionally, a
/// CRLF (U+001D U+001A) sequence is considered to be only a single
/// newline.
pub fn find_newlines(source: &str) -> Vec<SourceOffset> {
  let mut result = Vec::new();

  let mut prev = None;
  for (idx, ch) in source.bytes().enumerate() {
    if ch == 13 {
      result.push(SourceOffset(idx));
    } else if ch == 10 {
      // An LF after a CR doesn't count
      if prev != Some(13) {
        result.push(SourceOffset(idx));
      }
    }
    prev = Some(ch)
  }

  result
}

impl From<usize> for SourceOffset {
  fn from(x: usize) -> SourceOffset {
    SourceOffset(x)
  }
}

impl fmt::Display for SourceOffset {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_newlines() {
    let s1 = "abcdef";
    assert_eq!(find_newlines(s1), vec!());

    let s2 = "abcdef\nghi";
    assert_eq!(find_newlines(s2), vec!(SourceOffset(6)));

    let s3 = "abcdef\nghi\njkl";
    assert_eq!(find_newlines(s3), vec!(SourceOffset(6), SourceOffset(10)));

    let s4 = "abcdef\nghi\rjkl";
    assert_eq!(find_newlines(s4), vec!(SourceOffset(6), SourceOffset(10)));

    let s5 = "abcdef\nghi\r\njkl";
    assert_eq!(find_newlines(s5), vec!(SourceOffset(6), SourceOffset(10)));

    let s6 = "abcdef\n\nghi\r\njkl";
    assert_eq!(find_newlines(s6), vec!(SourceOffset(6), SourceOffset(7), SourceOffset(11)));

    let s7 = "abcdef\n\nghi\r\n\rjkl";
    assert_eq!(find_newlines(s7), vec!(SourceOffset(6), SourceOffset(7), SourceOffset(11), SourceOffset(13)));

    let s8 = "abcdef\n\nghi\r\n\r\njkl";
    assert_eq!(find_newlines(s8), vec!(SourceOffset(6), SourceOffset(7), SourceOffset(11), SourceOffset(13)));
  }

}
