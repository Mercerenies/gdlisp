
//! Wrapper struct and helpers for keeping track of positions in the
//! source code.
//!
//! All source positions always refer to the original GDLisp source
//! file, never to the position in an intermediate representation.

use crate::util;

use std::fmt;
use std::io::{self, BufReader};
use std::path::Path;
use std::fs::File;

/// A `SourceOffset` is really just a [`usize`] representing a
/// 0-indexed byte offset into the original file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct SourceOffset(pub usize);

/// A `SourcePos` indicates a line and column number in a file, where
/// all offsets are indicated in *characters*. Source position lines
/// and columns are always 1-indexed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourcePos {
  pub line: usize,
  pub column: usize,
}

/// A `SourcedValue` is a view of a [`Sourced`] instance, with its
/// [`SourceOffset`] converted to a [`SourcePos`] by looking at the
/// original material from which the offset was constructed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourcedValue<'a, T: Sourced> {
  source: &'a T,
  error_pos: SourcePos,
}

/// Trait for types which contain [source offset](SourceOffset)
/// information.
///
/// Several data structures in GDLisp take the following form. There
/// is one type `FooF` which represents some abstract syntax tree.
/// `FooF` values contain `Foo` values, while `Foo` is a struct that
/// is roughly isomorphic to `(FooF, SourceOffset)`. For any such
/// `Foo` type, this trait applies and provides the mechanism to get
/// the underlying data structure and the source offset.
pub trait Sourced {
  /// The type of value underlying this data structure. In the `Foo`
  /// example in the trait description, this would be `FooF`.
  type Item;

  /// Gets the [`SourceOffset`] for the position `self` starts at.
  fn get_source(&self) -> SourceOffset;

  /// Gets the value underlying `self`.
  fn get_value(&self) -> &Self::Item;

}

impl<'a, T: Sourced> SourcedValue<'a, T> {

  /// Constructs a `SourcedValue` view of `value`, assuming
  /// `value.get_source()` is a reference to a position in the string
  /// `source_code`.
  pub fn new(value: &'a T, source_code: &str) -> SourcedValue<'a, T> {
    let error_offset = value.get_source();
    let error_pos = SourcePos::from_offset(error_offset, source_code);
    SourcedValue { source: value, error_pos }
  }

  /// Constructs a `SourcedValue` view of `value`, assuming
  /// `value.get_source()` is a reference to a position in the file
  /// indicated. Returns an IO error in case of error reading the
  /// file.
  pub fn from_file<P>(value: &'a T, source_file: &P) -> io::Result<SourcedValue<'a, T>>
  where P: AsRef<Path> + ?Sized {
    let mut input_file = BufReader::new(File::open(source_file)?);
    let file_contents = util::read_to_end(&mut input_file)?;
    Ok(SourcedValue::new(value, &file_contents))
  }

  pub fn get_source_pos(&self) -> SourcePos {
    self.error_pos
  }

}

impl<'a, T: Sourced> Sourced for SourcedValue<'a, T> {
  type Item = T::Item;

  fn get_source(&self) -> SourceOffset {
    self.source.get_source()
  }

  fn get_value(&self) -> &Self::Item {
    self.source.get_value()
  }

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

impl SourcePos {

  /// Constructs a SourcePos representing the given line and column
  /// number.
  pub fn new(line: usize, column: usize) -> SourcePos {
    SourcePos { line, column }
  }

  /// Converts `offset` into a [`SourcePos`] representing a position
  /// in the given string of text `source`.
  pub fn from_offset(offset: SourceOffset, source: &str) -> SourcePos {
    let lines = {
      let mut lines = find_newlines(source);
      lines.push(SourceOffset(usize::MAX)); // Implicitly assume there's a final newline at the end of time.
      lines
    };

    let mut line_number = 1;
    let mut column_number = 1;

    for (idx, ch) in source.char_indices() {
      if idx >= offset.0 {
        break
      }
      if lines[line_number - 1].0 <= idx {
        line_number += 1;
        column_number = 1;
      } else if ch != '\n' { // Windows compatibility (don't advance on CRLF)
        column_number += 1;
      }
    }

    SourcePos::new(line_number, column_number)
  }

}

impl From<usize> for SourceOffset {
  fn from(x: usize) -> SourceOffset {
    SourceOffset(x)
  }
}

impl From<SourceOffset> for usize {
  fn from(x: SourceOffset) -> usize {
    x.0
  }
}

impl fmt::Display for SourceOffset {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl fmt::Display for SourcePos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "line {} column {}", self.line, self.column)
  }
}

impl<'a, T: Sourced + fmt::Display> fmt::Display for SourcedValue<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "On {}: {}", self.error_pos, self.source)
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

  #[test]
  fn test_offset() {

    assert_eq!(SourcePos::from_offset(SourceOffset(0), "abc"), SourcePos::new(1, 1));
    assert_eq!(SourcePos::from_offset(SourceOffset(1), "abc"), SourcePos::new(1, 2));
    assert_eq!(SourcePos::from_offset(SourceOffset(2), "abc"), SourcePos::new(1, 3));
    assert_eq!(SourcePos::from_offset(SourceOffset(3), "abc"), SourcePos::new(1, 4));

    assert_eq!(SourcePos::from_offset(SourceOffset(0), "abc\ndef"), SourcePos::new(1, 1));
    assert_eq!(SourcePos::from_offset(SourceOffset(1), "abc\ndef"), SourcePos::new(1, 2));
    assert_eq!(SourcePos::from_offset(SourceOffset(2), "abc\ndef"), SourcePos::new(1, 3));
    assert_eq!(SourcePos::from_offset(SourceOffset(3), "abc\ndef"), SourcePos::new(1, 4));
    assert_eq!(SourcePos::from_offset(SourceOffset(4), "abc\ndef"), SourcePos::new(2, 1));
    assert_eq!(SourcePos::from_offset(SourceOffset(5), "abc\ndef"), SourcePos::new(2, 2));
    assert_eq!(SourcePos::from_offset(SourceOffset(6), "abc\ndef"), SourcePos::new(2, 3));
    assert_eq!(SourcePos::from_offset(SourceOffset(7), "abc\ndef"), SourcePos::new(2, 4));

    assert_eq!(SourcePos::from_offset(SourceOffset(0), "abc\r\ndef"), SourcePos::new(1, 1));
    assert_eq!(SourcePos::from_offset(SourceOffset(1), "abc\r\ndef"), SourcePos::new(1, 2));
    assert_eq!(SourcePos::from_offset(SourceOffset(2), "abc\r\ndef"), SourcePos::new(1, 3));
    assert_eq!(SourcePos::from_offset(SourceOffset(3), "abc\r\ndef"), SourcePos::new(1, 4));
    assert_eq!(SourcePos::from_offset(SourceOffset(4), "abc\r\ndef"), SourcePos::new(2, 1));
    assert_eq!(SourcePos::from_offset(SourceOffset(5), "abc\r\ndef"), SourcePos::new(2, 1));
    assert_eq!(SourcePos::from_offset(SourceOffset(6), "abc\r\ndef"), SourcePos::new(2, 2));
    assert_eq!(SourcePos::from_offset(SourceOffset(7), "abc\r\ndef"), SourcePos::new(2, 3));
    assert_eq!(SourcePos::from_offset(SourceOffset(8), "abc\r\ndef"), SourcePos::new(2, 4));

  }

}
