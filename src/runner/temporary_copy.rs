
// Just a small helper; copies a file and then deletes the copy when dropped.

// NOTE: This is currently unused. I forgot how horribly
// non-thread-safe this is, so I just leave the copied file there now.
// This used to be used in macro_server.

use std::fs::{copy, remove_file};
use std::path::{PathBuf, Path};
use std::io;

pub struct TemporaryCopy {
  src: PathBuf,
  dest: PathBuf,
}

impl TemporaryCopy {

  pub fn new(src: PathBuf, dest: PathBuf) -> io::Result<TemporaryCopy> {
    copy(&src, &dest).map(|_| {
      TemporaryCopy { src, dest }
    })
  }

  pub fn source(&self) -> &Path {
    &self.src
  }

  pub fn destination(&self) -> &Path {
    &self.dest
  }

}

impl Drop for TemporaryCopy {

  fn drop(&mut self) {
    // I can't propagate the error because we're in Drop, so if the
    // removal fails, just ignore it and move on.
    let _ = remove_file(&self.dest);
  }

}
