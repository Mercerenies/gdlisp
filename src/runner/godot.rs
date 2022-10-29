
//! Provides a DSL for executing arbitrary Godot subprocesses.

use std::process::{Command, Stdio, Output, Child, ExitStatus};
use std::path::Path;
use std::ffi::OsStr;
use std::io;

/// `GodotCommand` is a wrapper around [`Command`] specifically
/// designed for running Godot processes. This structure implements
/// much of the same interface as `Command`, as well as adding some
/// Godot-specific functions.
///
/// `GodotCommand` instances are created either using
/// [`raw`](GodotCommand::raw) or [`base`](GodotCommand::base).
#[derive(Debug)]
pub struct GodotCommand {
  command: Command,
}

impl GodotCommand {

  /// A raw [`GodotCommand`]. This is the simplest way to create a
  /// Godot command. It provides *no* command line arguments or
  /// special stream handling, leaving everything at the [`Command`]
  /// defaults.
  ///
  /// Equivalent to `Command::new("godot")`, except wrapped as a
  /// `GodotCommand`.
  ///
  /// For most GDLisp use cases, [`base`](GodotCommand::base) should
  /// be preferred, as it implements common sense defaults on top of
  /// `raw`.
  pub fn raw() -> Self {
    GodotCommand {
      command: Command::new("godot"),
    }
  }

  /// Constructs a [`GodotCommand`] with sensible defaults. This
  /// includes passing the `--no-window` argument to Godot, as well as
  /// setting stderr to [`Stdio::inherit`] and stdout to
  /// [`Stdio::piped`].
  ///
  /// For a version of [`GodotCommand`] with no defaults set, see
  /// [`raw`](GodotCommand::raw).
  pub fn base() -> Self {
    let mut instance = Self::raw();
    instance
      .no_window()
      .stderr(Stdio::inherit())
      .stdout(Stdio::piped());
    instance
  }

  /// Adds an argument to pass to Godot.
  ///
  /// See [`Command::arg`].
  pub fn arg(&mut self, arg_text: impl AsRef<OsStr>) -> &mut Self {
    self.command.arg(arg_text);
    self
  }

  /// Adds the `--no-window` option to the command line arguments.
  ///
  /// This is already included if `self` was created with
  /// [`GodotCommand::base`], so it's only necessary for commands
  /// created with [`GodotCommand::raw`].
  pub fn no_window(&mut self) -> &mut Self {
    self.arg("--no-window")
  }

  /// Adds a command line argument indicating to the Godot process
  /// that the script at the given file path should be run.
  pub fn script_file(&mut self, filename: &Path) -> &mut Self {
    self.arg("-s").arg(filename)
  }

  /// Adds a command line argument indicating to the Godot process
  /// that the Godot project in the given directory should be run.
  /// There must be a `project.godot` file in the directory.
  ///
  /// Note that the path argument to `project_dir` should be a
  /// *directory*, not the `project.godot` file itself.
  pub fn project_dir(&mut self, directory_name: &Path) -> &mut Self {
    self.arg("--path").arg(directory_name)
  }

  /// Adds the `--quit` option to the command line arguments,
  /// indicating that the process should quit after one iteration.
  pub fn quit_after_one(&mut self) -> &mut Self {
    self.arg("--quit")
  }

  /// Adds the `--quiet` option to the command line arguments.
  pub fn quiet(&mut self) -> &mut Self {
    self.arg("--quiet")
  }

  /// Adds a single environment variable that will be visible to the
  /// Godot process.
  ///
  /// See [`Command::env`].
  pub fn env<K, V>(&mut self, key: K, val: V) -> &mut Self
  where K: AsRef<OsStr>,
        V: AsRef<OsStr> {
    self.command.env(key, val);
    self
  }

  /// Adds environment variables that will be visible to the Godot
  /// process.
  ///
  /// See [`Command::envs`].
  pub fn envs<I, K, V>(&mut self, vars: I) -> &mut Self
  where I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr> {
    self.command.envs(vars);
    self
  }

  /// Sets a handler for the standard error stream.
  ///
  /// See [`Command::stderr`].
  pub fn stderr(&mut self, cfg: impl Into<Stdio>) -> &mut Self {
    self.command.stderr(cfg);
    self
  }

  /// Sets a handler for the standard output stream.
  ///
  /// See [`Command::stdout`].
  pub fn stdout(&mut self, cfg: impl Into<Stdio>) -> &mut Self {
    self.command.stdout(cfg);
    self
  }

  /// Sets a handler for the standard input stream.
  ///
  /// See [`Command::stdin`].
  pub fn stdin(&mut self, cfg: impl Into<Stdio>) -> &mut Self {
    self.command.stdin(cfg);
    self
  }

  /// Executes the command, waits on it to terminate, and then
  /// collects all of its output.
  ///
  /// See [`Command::output`].
  pub fn output(&mut self) -> io::Result<Output> {
    self.command.output()
  }

  /// Executes the command, waits on it to terminate, and then
  /// returns its exit status.
  ///
  /// See [`Command::status`].
  pub fn status(&mut self) -> io::Result<ExitStatus> {
    self.command.status()
  }

  /// Executes the command, returning a [`Child`] instance
  /// immediately.
  ///
  /// See [`Command::spawn`].
  pub fn spawn(&mut self) -> io::Result<Child> {
    self.command.spawn()
  }

}

impl From<GodotCommand> for Command {
  fn from(godot_command: GodotCommand) -> Command {
    godot_command.command
  }
}
