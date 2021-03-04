
// Some infrastructure for spoofing files to test import syntax.

use gdlisp::pipeline::resolver::NameResolver;

use std::collections::HashMap;
use std::path::Path;
use std::io::{self, Read, Write};
use std::cmp::min;

// Behaves like the read instance on &[u8] but takes ownership of its
// string.
#[derive(Clone)]
struct StringReader {
  string: String,
  position: usize
}

#[derive(Default)]
pub struct MockFileLoader {
  files: HashMap<String, String>,
}

impl MockFileLoader {

  pub fn new() -> MockFileLoader {
    MockFileLoader::default()
  }

  pub fn add_file(&mut self, name: &str, contents: &str) {
    self.files.insert(name.to_owned(), contents.to_owned());
  }

}

impl StringReader {

  pub fn new(s: String) -> StringReader {
    StringReader { string: s, position: 0 }
  }

}

impl Read for StringReader {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    let bytes = self.string.as_bytes();
    let to_copy = min(buf.len(), bytes.len() - self.position);
    buf[0..to_copy].copy_from_slice(&bytes[self.position..self.position+to_copy]);
    self.position += to_copy;
    Ok(to_copy)
  }
}

impl NameResolver for MockFileLoader {

  fn resolve_input_path(&self, filename: &Path) -> io::Result<Box<dyn Read>> {
    let filename = filename.file_name().unwrap().to_string_lossy().to_owned();
    let file_contents = StringReader::new(self.files.get(&*filename).unwrap().to_owned());
    Ok(Box::new(file_contents))
  }

  fn resolve_output_path(&self, _filename: &Path) -> io::Result<Box<dyn Write>> {
    Ok(Box::new(io::sink()))
  }

}
