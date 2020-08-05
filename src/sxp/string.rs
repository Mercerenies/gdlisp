
#[derive(Debug)]
pub enum Error {
  UnfinishedEscape,
  InvalidEscape(char),
}

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
  fn escaping() {
    assert_eq!(parse_escapes(r#"foobar"#).unwrap(), "foobar");
    assert_eq!(parse_escapes(r#"foo\"\"bar\\"#).unwrap(), "foo\"\"bar\\");
  }

  #[test]
  fn failed_escaping() {
    assert!(parse_escapes(r#"\"#).is_err());
    assert!(parse_escapes(r#"\I"#).is_err());
  }

}
