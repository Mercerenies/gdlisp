
//! Helper functions for working with strings within
//! [`AST`](super::ast::AST) values.

/// The type of errors produced by [`parse_escapes`].
#[derive(Debug)]
pub enum Error {
  /// An escape sequence `\` was not terminated.
  UnfinishedEscape,
  /// An escape sequence `\` was not recognized.
  InvalidEscape(char),
}

/// Given a string, parse escape sequences beginning with `\` and
/// produce a string containing the result.
///
/// # Examples
///
/// ```
/// # use gdlisp::sxp::string::parse_escapes;
/// assert_eq!(parse_escapes(r#"foobar"#).unwrap(), "foobar");
/// assert_eq!(parse_escapes(r#"foo\"\"bar\\"#).unwrap(), "foo\"\"bar\\");
/// ```
pub fn parse_escapes(input: &str) -> Result<String, Error> {
  let length = input.chars().count();
  let mut result = String::with_capacity(length);
  let mut iter = input.chars();
  while let Some(ch) = iter.next() {
    if ch == '\\' {
      match iter.next() {
        Some('\\') => result.push('\\'),
        Some('\"') => result.push('\"'),
        Some(esc) => return Err(Error::InvalidEscape(esc)),
        None => return Err(Error::UnfinishedEscape),
      }
    } else {
      result.push(ch);
    }
  }
  Ok(result)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn failed_escaping() {
    assert!(parse_escapes(r#"\"#).is_err());
    assert!(parse_escapes(r#"\I"#).is_err());
  }

}
