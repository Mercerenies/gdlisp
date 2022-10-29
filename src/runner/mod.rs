
//! Mechanisms for running Godot code in child processes.
//!
//! The functions defined in this module are general-purpose functions
//! for running arbitrary Godot code in child processes. The submodule
//! [`macro_server`] provides the more specific use-case of starting
//! up a Godot process as a server and communicating with it via TCP.

pub mod godot;
pub mod into_gd_file;
pub mod macro_server;
pub mod named_file;
pub mod path;
pub mod version;

use godot::GodotCommand;

use tempfile::{Builder, NamedTempFile};

use std::process::{Command, Stdio};
use std::path::Path;
use std::io::{self, Seek, SeekFrom};
use std::fmt;
use std::error::Error;

/// A type containing string output from both `stdout` and `stderr`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Output {
  pub stdout: String,
  pub stderr: String,
}

#[derive(Debug, Clone)]
pub struct JsonApiError;

impl fmt::Display for JsonApiError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "error reading JSON API from Godot binary")
  }
}

impl Error for JsonApiError {}

/// Runs a Godot process, with `path` as a project path.
///
/// `path` should be the path to a directory which contains a
/// `project.godot` file (note that `path` should point to the
/// directory, *not* the `project.godot` file itself). This function
/// will wait until the child terminates before returning.
///
/// Anything printed to stderr will be passed through to the parent
/// stderr, and stdout will be collected into a `String` and then
/// returned. Anything in stdout which is not valid UTF-8 will be
/// replaced with `U+FFFD`.
pub fn run_project<P : AsRef<Path>>(path: P) -> io::Result<String> { // TODO Consider returning Cow?
  let out =
    Command::new("godot")
    .arg("--no-window")
    .arg("--path")
    .arg(path.as_ref().as_os_str())
    .stderr(Stdio::inherit())
    .stdout(Stdio::piped())
    .output()?;
  let text = String::from_utf8_lossy(&out.stdout);
  Ok(text.into())
}

/// Identical to [`run_project`] except that `stderr` is also captured
/// and returned. The two return values are `stdout` and `stderr`, respectively.
pub fn run_project_capturing_stderr<P : AsRef<Path>>(path: P) -> io::Result<Output> { // TODO Consider returning Cow?
  let out =
    Command::new("godot")
    .arg("--no-window")
    .arg("--path")
    .arg(path.as_ref().as_os_str())
    .stderr(Stdio::piped())
    .stdout(Stdio::piped())
    .output()?;
  let stdout = String::from(String::from_utf8_lossy(&out.stdout));
  let stderr = String::from(String::from_utf8_lossy(&out.stderr));
  Ok(Output { stdout, stderr })
}

/// Runs a Godot process to dump the JSON API for GDNative to a
/// (newly-created) temporary file. This method returns the temporary
/// file object. The corresponding file will be deleted when the
/// returned value is dropped.
pub fn dump_json_api() -> io::Result<NamedTempFile> {
  let mut file = make_tmp_file()?;
  let status = GodotCommand::base()
    .quiet()
    .arg("--gdnative-generate-json-api")
    .arg(file.path())
    .status()?;
  if !status.success() {
    return Err(io::Error::new(io::ErrorKind::Other, JsonApiError));
  }
  // Not sure if this is necessary, but better safe than sorry. Ensure
  // that the seek position of the opened file handle is at 0.
  file.seek(SeekFrom::Start(0))?;
  Ok(file)
}

fn make_tmp_file() -> io::Result<NamedTempFile> {
  Builder::new()
    .prefix("__gdlisp_test")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile()
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::runner::into_gd_file::IntoGDFile;
  use std::io::Write;

  fn run_with_temporary(data: &str) -> io::Result<String> {
    let mut tmp = make_tmp_file()?;
    data.write_to_gd(&mut tmp)?;
    tmp.flush()?;

    let out =
      GodotCommand::base()
      .script_file(tmp.path())
      .quit_after_one()
      .output()?;
    let text = String::from_utf8_lossy(&out.stdout);
    Ok(text.into())
  }

  #[test]
  fn run_minimal_test() {
    run_with_temporary(r#"
extends SceneTree

func _init():
    pass
"#).unwrap();
  }

  #[test]
  fn run_printing_test() {
    let out = run_with_temporary(r#"
extends SceneTree

func _init():
    print("999_SAMPLE_OUTPUT_STRING_999")
"#).unwrap();
    assert!(out.contains("999_SAMPLE_OUTPUT_STRING_999"));
  }

  #[test]
  fn dump_json_api_test() {
    // Checks that dump_json_api completes successfully and produces
    // output. It does *not* check the contents of the output. That is
    // tested over in `crate::gdscript::library::gdnative`.
    let mut tempfile = dump_json_api().unwrap();
    let file_len = tempfile.seek(SeekFrom::End(0)).unwrap();
    assert!(file_len > 0);
  }

}
