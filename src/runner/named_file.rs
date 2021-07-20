
//! Defines the [`NamedFile`] trait and some trivial implementations.

use tempfile::NamedTempFile;

use std::path::{Path, PathBuf};
use std::io;
use std::fs::File;
use std::borrow::ToOwned;

/// Instances of `NamedFile` are file-like objects which have a path.
/// Any object which has a reasonable notion of "file path" can
/// implement this trait.
pub trait NamedFile : io::Write {

  /// Returns the path of the file represented by this object.
  fn path(&self) -> &Path;

}

/// A `SimpleNamedFile` is a [`NamedFile`] which simply contains a
/// path and the file with no additional behavior. This struct is most
/// useful in situations where [`NamedTempFile`]-like semantics are
/// desired, but we wish to persist the relevant file.
pub struct SimpleNamedFile(PathBuf, File);

impl SimpleNamedFile {

  /// Construct a `SimpleNamedFile`. This will attempt to open the
  /// file for writing, and in case of failure will return the
  /// appropriate IO error.
  pub fn create<P : AsRef<Path>>(path: P) -> io::Result<SimpleNamedFile> {
    let buf = path.as_ref().to_owned();
    File::create(path).map(|f| SimpleNamedFile(buf, f))
  }

}

impl io::Write for SimpleNamedFile {

  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.1.write(buf)
  }

  fn flush(&mut self) -> io::Result<()> {
    self.1.flush()
  }

}

impl NamedFile for SimpleNamedFile {
  fn path(&self) -> &Path {
    self.0.as_ref()
  }
}

impl NamedFile for NamedTempFile {
  fn path(&self) -> &Path {
    NamedTempFile::path(self)
  }
}
