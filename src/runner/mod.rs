
//! Mechanisms for running Godot code in child processes.
//!
//! The functions defined in this module are general-purpose functions
//! for running arbitrary Godot code in child processes. The submodule
//! [`macro_server`] provides the more specific use-case of starting
//! up a Godot process as a server and communicating with it via TCP.

pub mod named_file;
pub mod into_gd_file;
pub mod macro_server;
pub mod temporary_copy;
pub mod path;
pub mod version;

use into_gd_file::IntoGDFile;

use tempfile::{Builder, NamedTempFile};

use std::process::{Command, Stdio, Child};
use std::path::Path;
use std::io::{self, Write, Seek, SeekFrom};
use std::ffi::OsStr;

/// Runs a Godot process, with `path` as a script file. The script
/// file should inherit from `SceneTree` or `MainLoop`. This function
/// will wait until the child terminates before returning.
///
/// Anything printed to stderr will be passed through to the parent
/// stderr, and stdout will be collected into a `String` and then
/// returned. Anything in stdout which is not valid UTF-8 will be
/// replaced with `U+FFFD`.
pub fn run_with_file<P : AsRef<Path>>(path: P) -> io::Result<String> {
  let out =
    Command::new("godot")
    .arg("--no-window")
    .arg("-s")
    .arg(path.as_ref().as_os_str())
    .arg("-q")
    .stderr(Stdio::inherit())
    .stdout(Stdio::piped())
    .output()?;
  let text = String::from_utf8_lossy(&out.stdout);
  Ok(text.into())
}

/// Runs a Godot process, with `path` as a project path.
///
/// `path` should be the path to a directory which contains a
/// `project.godot` file (note that `path` should point to the
/// directory, *not* the `project.godot` file itself). `env` shall be
/// an iterator of environment variables to be set locally for the
/// child process. This function returns as soon as the child process
/// spawns. stdout will be suppressed, and stderr will be inherited by
/// the parent process. The [`Child`] instance is returned.
pub fn run_project_process<P, I, K, V>(path: P, env: I) -> io::Result<Child>
where P : AsRef<Path>,
      I : Iterator<Item=(K, V)>,
      K : AsRef<OsStr>,
      V : AsRef<OsStr> {
  Command::new("godot")
    .arg("--no-window")
    .arg("--path")
    .arg(path.as_ref().as_os_str())
    .envs(env)
    .stderr(Stdio::inherit())
    .stdout(Stdio::null())
    .spawn()
}

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
pub fn run_project<P : AsRef<Path>>(path: P) -> io::Result<String> {
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

/// Runs a Godot process to dump the JSON API for GDNative to a
/// (newly-created) temporary file. This method returns the temporary
/// file object. The corresponding file will be deleted when the
/// returned value is dropped.
pub fn dump_json_api() -> io::Result<NamedTempFile> {
  let mut file = make_tmp_file()?;
  Command::new("godot")
    .arg("--no-window")
    .arg("--quiet")
    .arg("--gdnative-generate-json-api")
    .arg(file.path())
    .status()?;
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

fn make_tmp_file_in<P : AsRef<Path>>(dir: P) -> io::Result<NamedTempFile> {
  Builder::new()
    .prefix("__gdlisp_test")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile_in(dir)
}

/// Given an [`IntoGDFile`] such as
/// [`TopLevelClass`](crate::gdscript::decl::TopLevelClass), this
/// function constructs a temporary file, dumps the contents of `data`
/// to that file, and then runs it with [`run_with_file`].
pub fn run_with_temporary<T>(data: &T) -> io::Result<String>
where T : IntoGDFile + ?Sized {
  let mut tmp = make_tmp_file()?;
  data.write_to_gd(&mut tmp)?;
  tmp.flush()?;
  run_with_file(tmp.path())
}

/// Given an [`IntoGDFile`] such as
/// [`TopLevelClass`](crate::gdscript::decl::TopLevelClass), this
/// function constructs a temporary file in the directory `dir`, dumps
/// the contents of `data` to that file, and then runs it with
/// [`run_with_file`].
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
