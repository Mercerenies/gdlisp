
use std::process::{Command, Stdio};
use std::io;

// Helper to get the Godot version

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
