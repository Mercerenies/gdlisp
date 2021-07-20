
//! Helper to get the Godot version.

use std::process::{Command, Stdio};
use std::io;

/// Get the version of Godot that is present on the system path.
///
/// The format of the resulting string is dependent on Godot and
/// should generally be used as an informative message to the user,
/// not compared to implement a feature check.
///
/// In the case of IO error (including the situation where no
/// executable named `godot` is on the system path), returns an
/// appropriate error.
pub fn get_godot_version() -> io::Result<String> {
  let output =
    Command::new("godot")
    .arg("--version")
    .stderr(Stdio::null())
    .stdout(Stdio::piped())
    .output()?;
  let mut s = String::from_utf8_lossy(&output.stdout).into_owned();
  s.pop(); // Remove newline from end
  Ok(s)
}
