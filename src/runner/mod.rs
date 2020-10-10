
pub mod named_file;

use std::process::{Command, Stdio};
use std::path::Path;
use std::io;

pub fn run_with_file<P : AsRef<Path>>(path: P) -> io::Result<String> {
  let mut cmd =
    Command::new("godot")
    .stderr(Stdio::inherit())
    .stdout(Stdio::piped());
  panic!("TBA"); /////
}
