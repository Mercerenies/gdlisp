
#![deprecated]

//! This module provides [`TemporaryCopy`], for temporarily copying
//! files.
//!
//! The copy will be deleted when the `TemporaryCopy` instance is
//! dropped.
//!
//! This is currently unused and should be regarded as deprecated.
//! It's not remotely thread-safe.

// I forgot how horribly non-thread-safe this is, so I just leave the
// copied file there now. This used to be used in macro_server.

use std::fs::{copy, remove_file};
use std::path::{PathBuf, Path};
use std::io;

/// At construction, a `TemporaryCopy` makes a copy of the source file
/// at the destination. When the `TemporaryCopy` is later dropped, the
/// destination file is deleted.
pub struct TemporaryCopy {
  src: PathBuf,
  dest: PathBuf,
}

impl TemporaryCopy {

  /// Make a copy of `src` at `dest` and return a `TemporaryCopy`
  /// which, when dropped, will delete the copy.
  ///
  /// In case of any IO errors, the `TemporaryCopy` will not be
  /// constructed.
  pub fn new(src: PathBuf, dest: PathBuf) -> io::Result<TemporaryCopy> {
    copy(&src, &dest).map(|_| {
      TemporaryCopy { src, dest }
    })
  }

  /// Get the source file.
  pub fn source(&self) -> &Path {
    &self.src
  }

  /// Get the destination file.
  pub fn destination(&self) -> &Path {
    &self.dest
  }

}

/// Delete the destination file, suppressing any IO errors that result
/// from the delete operation.
impl Drop for TemporaryCopy {

  fn drop(&mut self) {
    // I can't propagate the error because we're in Drop, so if the
    // removal fails, just ignore it and move on.
    let _ = remove_file(&self.dest);
  }

}
