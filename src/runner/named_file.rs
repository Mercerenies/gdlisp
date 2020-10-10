
use tempfile::NamedTempFile;

use std::path::{Path, PathBuf};
use std::io;
use std::fs::File;
use std::borrow::ToOwned;

pub trait NamedFile : io::Write {
  fn path(&self) -> &Path;
}

pub struct SimpleNamedFile(PathBuf, File);

impl SimpleNamedFile {

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
