
// An RPathBuf consists of a PathBuf together with a specifier

use std::path::{PathBuf, Path, Components};
use std::convert::TryFrom;
use std::str::FromStr;
use std::fmt;
use std::ffi::OsStr;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RPathBuf {
  source: PathSrc,
  path: PathBuf,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

  pub fn path_mut(&mut self) -> &mut PathBuf {
    &mut self.path
  }

  pub fn components(&self) -> Components<'_> {
    self.path().components()
  }

  pub fn extension(&self) -> Option<&OsStr> {
    self.path().extension()
  }

  pub fn components_no_root(&self) -> Components<'_> {
    let mut comp = self.components();
    if self.path().has_root() {
      let _ignore_root = comp.next();
    }
    comp
  }

  pub fn path_to_string<P : AsRef<Path> + ?Sized>(path: &P) -> String {
    path.as_ref().to_string_lossy().replace("\\", "/")
  }

}

impl fmt::Display for RPathBuf {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let path = RPathBuf::path_to_string(self.path());
    write!(f, "{}{}", self.source().prefix(), path)
  }
}

impl PathSrc {

  pub fn should_be_relative_path(self) -> bool {
    self != PathSrc::Absolute
  }

  pub fn prefix(self) -> &'static str {
    match self {
      PathSrc::User => "user://",
      PathSrc::Res => "res://",
      PathSrc::Absolute => "",
    }
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

  #[test]
  fn test_rpathbuf_display() {
    assert_eq!(RPathBuf::try_from(String::from("res://foo/bar")).unwrap().to_string(), "res://foo/bar");
    assert_eq!(RPathBuf::try_from(String::from("user://foo/bar")).unwrap().to_string(), "user://foo/bar");
    assert_eq!(RPathBuf::try_from(String::from("/foo/bar")).unwrap().to_string(), "/foo/bar");
  }

}
