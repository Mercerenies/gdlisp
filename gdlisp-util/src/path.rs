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

//! Structure for representing a Godot resource path.
//!
//! Godot resource paths are represented using a special syntax. There
//! are three types of paths that can be represented in this way.
//!
//! 1. **Absolute paths** have no prefix and are written as-is. These
//! paths start from the file-system root and refer to a specific
//! location.
//!
//! 2. **Resource paths** begin with `res://` and represent a path
//! relative to the project directory.
//!
//! 3. **User paths** begin with `user://` and represent a path
//! relative to some user-local storage directory, usually used for
//! save files or the like.
//!
//! This module defines [`RPathBuf`], a [`PathBuf`]-like structure
//! which represents the above file path formats.

// An RPathBuf consists of a PathBuf together with a specifier

use std::path::{PathBuf, Path, Components};
use std::convert::TryFrom;
use std::str::FromStr;
use std::fmt;
use std::ffi::OsStr;

/// An `RPathBuf` represents a file path using Godot's format.
///
/// See the [module-level documentation](crate::runner::path) for more
/// details.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RPathBuf {
  source: PathSrc,
  path: PathBuf,
}

/// An [`RPathBuf`] can refer to an absolute path, a resource path, or
/// a user path.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PathSrc {
  /// A user path, relative to some user-local storage directory.
  User,
  /// A resource path, relative to the project direction.
  Res,
  /// An absolute path, from the file system root directory.
  Absolute,
}

/// Errors which can occur when constructing an [`RPathBuf`] object.
#[derive(Clone, Copy, Debug)]
pub enum TryFromRPathBufError {
  /// A relative path was expected (based on the [`PathSrc`]) but an
  /// absolute one was provided.
  ExpectedRelative,
  /// An absolute path was expected (based on the [`PathSrc`]) but a
  /// relative one was provided.
  ExpectedAbsolute,
}

impl RPathBuf {

  /// Construct an `RPathBuf` from a source and a file path.
  ///
  /// Note that `path` should be a relative path if and only if
  /// [`source.should_be_relative_path()`](PathSrc::should_be_relative_path)
  /// is true. If this condition is violated, then an error will be
  /// returned from this function.
  pub fn new(source: PathSrc, path: PathBuf) -> Result<RPathBuf, TryFromRPathBufError> {
    if source.should_be_relative_path() == path.is_relative() {
      Ok(RPathBuf { source, path })
    } else if source.should_be_relative_path() {
      Err(TryFromRPathBufError::ExpectedRelative)
    } else {
      Err(TryFromRPathBufError::ExpectedAbsolute)
    }
  }

  /// Gets the path's source type.
  pub fn source(&self) -> PathSrc {
    self.source
  }

  /// Gets the file path.
  pub fn path(&self) -> &Path {
    &self.path
  }

  /// Gets the file path, mutably.
  ///
  /// Callers of this function are expected to maintain the
  /// constructor's precondition, namely that the path is relative if
  /// and only if [`PathSrc::should_be_relative_path`] is true.
  pub fn path_mut(&mut self) -> &mut PathBuf {
    &mut self.path
  }

  /// Consumes `self` and produce its file path as a [`PathBuf`].
  pub fn into_path(self) -> PathBuf {
    self.path
  }

  /// Returns the path's components. Equivalent to
  /// `self.path().components()`.
  pub fn components(&self) -> Components<'_> {
    self.path().components()
  }

  /// Returns the path's extension. Equivalent to
  /// `self.path().extension()`.
  pub fn extension(&self) -> Option<&OsStr> {
    self.path().extension()
  }

  /// Returns the path's components, like [`RPathBuf::components`].
  /// However, if the path is an absolute path, then the root `/` will
  /// be stripped from the components.
  pub fn components_no_root(&self) -> Components<'_> {
    let mut comp = self.components();
    if self.path().has_root() {
      let _ignore_root = comp.next();
    }
    comp
  }

  /// Return the path as a string.
  ///
  /// Forward slashes will always be used as the path delimiter, even
  /// on Windows. This is consistent with Godot's behavior.
  pub fn path_to_string<P : AsRef<Path> + ?Sized>(path: &P) -> String {
    path.as_ref().to_string_lossy().replace('\\', "/")
  }

}

/// Displays the path as a string, using Godot's `user://` and
/// `res://` syntax as appropriate. Absolute paths will be shown
/// verbatim with no prefix.
impl fmt::Display for RPathBuf {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let path = RPathBuf::path_to_string(self.path());
    write!(f, "{}{}", self.source().prefix(), path)
  }
}

impl PathSrc {

  /// Returns whether a path with this source type should be a
  /// relative path.
  ///
  /// All source types except [`PathSrc::Absolute`] should refer to
  /// relative paths.
  pub fn should_be_relative_path(self) -> bool {
    self != PathSrc::Absolute
  }

  /// Returns the Godot-style prefix (`user://` or `res://`) for the
  /// path type.
  ///
  /// In the case of [`PathSrc::Absolute`], an empty string is
  /// returned, as no prefix is necessary in this case.
  pub fn prefix(self) -> &'static str {
    match self {
      PathSrc::User => "user://",
      PathSrc::Res => "res://",
      PathSrc::Absolute => "",
    }
  }

}

/// Converts a Godot-style import string (potentially beginning with
/// `res://` or `user://`) into the appropriate type of [`RPathBuf`]
/// object.
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
