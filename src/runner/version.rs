
//! Helper to get the Godot version.

use std::process::{Command, Stdio};
use std::io;
use std::collections::VecDeque;
use std::str::FromStr;

/// A version for a piece of software, in Semantic Versioning.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
  pub major: i32,
  pub minor: i32,
  pub patch: i32,
}

/// Version information, together with modifiers applied after the
/// base Semantic Versioning information.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct VersionInfo {
  pub version: Version,
  pub modifiers: Vec<String>,
}

impl Version {

  /// Constructs a version object representing version 0.0.0 of
  /// software.
  pub fn new() -> Self {
    Version::default()
  }

  /// Converts the version number into a single integer value which
  /// represents the version information. This is *not* a one-to-one
  /// mapping. It is intended to be a human-readable number
  /// representing the same version, which preserves the [`Version`]
  /// order for reasonable values.
  pub fn into_i32(self) -> i32 {
    self.major * 10000 + self.minor * 100 + self.patch
  }

}

impl VersionInfo {

  /// An empty version info object, corresponding to the version
  /// string `"0.0.0"`.
  pub fn new() -> Self {
    VersionInfo::default()
  }

  /// Given a Godot version string, parse it as a [`VersionInfo`]
  /// object. This method will not fail. Any missing fields will be
  /// initialized to an appropriate default (zero in the case of
  /// version numbers).
  pub fn parse(version_string: &str) -> VersionInfo {
    let mut atoms: VecDeque<&str> = version_string.split('.').collect();

    // Try to identify major, minor, patch version
    let major = pop_version_number(&mut atoms).unwrap_or(0);
    let minor = pop_version_number(&mut atoms).unwrap_or(0);
    let patch = pop_version_number(&mut atoms).unwrap_or(0);

    let version = Version { major, minor, patch };
    VersionInfo { version, modifiers: atoms.into_iter().map(str::to_owned).collect() }
  }

}

/// Get the version of Godot that is present on the system path.
///
/// The format of the resulting string is dependent on Godot and
/// should generally be used as an informative message to the user,
/// not compared to implement a feature check.
///
/// In the case of IO error (including the situation where no
/// executable named `godot` is on the system path), returns an
/// appropriate error.
pub fn get_godot_version_as_string() -> io::Result<String> {
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

/// Get the version of Godot that is present on the system path,
/// parsing it as a version string.
pub fn get_godot_version() -> io::Result<VersionInfo> {
  let version_string = get_godot_version_as_string()?;
  Ok(VersionInfo::parse(&version_string))
}

fn pop_version_number(atoms: &mut VecDeque<&str>) -> Option<i32> {
  atoms.pop_front().and_then(|atom| i32::from_str(atom).ok())
}
