
// An RPathBuf consists of a PathBuf together with a specifier

use std::path::{PathBuf, Path};
use std::convert::TryFrom;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RPathBuf {
  source: PathSrc,
  path: PathBuf,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PathSrc { User, Res, Absolute }

#[derive(Clone, Copy, Debug)]
pub enum TryFromRPathBufError {
  ExpectedRelative, ExpectedAbsolute
}

impl RPathBuf {

  pub fn new(source: PathSrc, path: PathBuf) -> Result<RPathBuf, TryFromRPathBufError> {
    if source.should_be_relative_path() == path.is_relative() {
      Ok(RPathBuf { source, path })
    } else if source.should_be_relative_path() {
      Err(TryFromRPathBufError::ExpectedRelative)
    } else {
      Err(TryFromRPathBufError::ExpectedAbsolute)
    }
  }

  pub fn source(&self) -> PathSrc {
    self.source
  }

  pub fn path(&self) -> &Path {
    &self.path
  }

}

impl PathSrc {

  pub fn should_be_relative_path(self) -> bool {
    self != PathSrc::Absolute
  }

}

impl TryFrom<String> for RPathBuf {
  type Error = TryFromRPathBufError;

  fn try_from(path: String) -> Result<RPathBuf, TryFromRPathBufError> {
    if let Some(s) = path.strip_prefix("res://") {
      RPathBuf::new(PathSrc::Res, PathBuf::from_str(s).unwrap()) // Infallible
    } else if let Some(s) = path.strip_prefix("user://") {
      RPathBuf::new(PathSrc::User, PathBuf::from_str(s).unwrap()) // Infallible
    } else {
      // Assume it's an absolute path
      RPathBuf::new(PathSrc::Absolute, PathBuf::from(path))
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_rpathbuf_new() {
    assert!(RPathBuf::new(PathSrc::Absolute, PathBuf::from_str("/a/b").unwrap()).is_ok());
    assert!(RPathBuf::new(PathSrc::Res, PathBuf::from_str("a/b").unwrap()).is_ok());
    assert!(RPathBuf::new(PathSrc::User, PathBuf::from_str("a/b").unwrap()).is_ok());
    assert!(RPathBuf::new(PathSrc::Absolute, PathBuf::from_str("a/b").unwrap()).is_err());
    assert!(RPathBuf::new(PathSrc::Res, PathBuf::from_str("/a/b").unwrap()).is_err());
    assert!(RPathBuf::new(PathSrc::User, PathBuf::from_str("/a/b").unwrap()).is_err());
  }

  #[test]
  fn test_rpathbuf_parse() {

    let res = RPathBuf::try_from(String::from("res://foo/bar"));
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.source(), PathSrc::Res);
    assert_eq!(res.path(), PathBuf::from_str("foo/bar").unwrap());

    let user = RPathBuf::try_from(String::from("user://foo/bar"));
    assert!(user.is_ok());
    let user = user.unwrap();
    assert_eq!(user.source(), PathSrc::User);
    assert_eq!(user.path(), PathBuf::from_str("foo/bar").unwrap());

    let abs = RPathBuf::try_from(String::from("/foo/bar"));
    assert!(abs.is_ok());
    let abs = abs.unwrap();
    assert_eq!(abs.source(), PathSrc::Absolute);
    assert_eq!(abs.path(), PathBuf::from_str("/foo/bar").unwrap());
  }

}
