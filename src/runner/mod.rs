
pub mod named_file;
pub mod into_gd_file;

use into_gd_file::IntoGDFile;

use tempfile::{Builder, NamedTempFile};

use std::process::{Command, Stdio};
use std::path::Path;
use std::io::{self, Write};

pub fn run_with_file<P : AsRef<Path>>(path: P) -> io::Result<String> {
  let out =
    Command::new("godot")
    .arg("-s")
    .arg(path.as_ref().as_os_str())
    .arg("-q")
    .stderr(Stdio::inherit())
    .stdout(Stdio::piped())
    .output()?;
  let text = String::from_utf8_lossy(&out.stdout);
  Ok(text.into())
}

fn make_tmp_file() -> io::Result<NamedTempFile> {
  Builder::new()
    .prefix("__gdlisp_test")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile()
}

fn make_tmp_file_in<P : AsRef<Path>>(dir: P) -> io::Result<NamedTempFile> {
  Builder::new()
    .prefix("__gdlisp_test")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile_in(dir)
}

pub fn run_with_temporary<T>(data: &T) -> io::Result<String>
where T : IntoGDFile + ?Sized {
  let mut tmp = make_tmp_file()?;
  data.write_to_gd(&mut tmp)?;
  tmp.flush()?;
  run_with_file(tmp.path())
}

pub fn run_with_temporary_in<T, P>(data: &T, dir: P) -> io::Result<String>
where T : IntoGDFile + ?Sized,
      P : AsRef<Path> {
  let mut tmp = make_tmp_file_in(dir)?;
  data.write_to_gd(&mut tmp)?;
  tmp.flush()?;
  run_with_file(tmp.path())
}

#[cfg(test)]
mod tests {
  use super::*;

  // Note: These tests are ignored by default right now, since there's
  // no way to run Godot windowless on Linux yet, so running these
  // pops up with a Godot window, which is annoying when just
  // routinely trying to run through the tests. If
  // https://github.com/godotengine/godot/pull/42276 gets merged into
  // Godot, I'll be able to start running these all the time.

  #[test]
  #[ignore]
  fn run_minimal_test() {
    run_with_temporary(r#"
extends SceneTree

func _init():
    pass
"#).unwrap();
  }

  #[test]
  #[ignore]
  fn run_printing_test() {
    let out = run_with_temporary(r#"
extends SceneTree

func _init():
    print("999_SAMPLE_OUTPUT_STRING_999")
"#).unwrap();
    assert!(out.contains("999_SAMPLE_OUTPUT_STRING_999"));
  }

}
