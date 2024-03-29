// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! This module provides functionality to parse command line
//! arguments.
//!
//! Generally speaking, the entrypoint to this module will be
//! [`parse_args`].

use getopts::{Options, ParsingStyle};

const PROGRAM_DESCRIPTION: &str =
  r#"The GDLisp compiler. If invoked with filenames, compiles the GDLisp sources
at the given path. If given a directory, compiles all GDLisp source files in the
Godot project at that directory. If given *no* file or directory arguments, invokes
the GDLisp REPL."#;

/// This structure contains information about the command line
/// arguments passed to the compiler. It is usually constructed via
/// [`parse_args`].
#[derive(Debug, Clone, Default)]
pub struct CommandLineArgs {

  /// The input file provided, or `None` if none was provided.
  pub input_file: Option<String>,

  /// Whether `--help` was provided.
  pub help_message: bool,

  /// Whether the internal `--compile-stdlib` was provided.
  pub compile_stdlib_flag: bool,

  /// Whether the `--legacy-repl` command was provided.
  pub legacy_repl_flag: bool,

}

impl CommandLineArgs {

  /// Construct a new defaulted [`CommandLineArgs`].
  pub fn new() -> CommandLineArgs {
    CommandLineArgs::default()
  }

  /// Construct a [`CommandLineArgs`] which indicates that the user
  /// would like to see the help message.
  pub fn help() -> CommandLineArgs {
    let mut inst = CommandLineArgs::new();
    inst.help_message = true;
    inst
  }

}

/// The [`Options`] which are used for parsing GDLisp command line
/// arguments.
pub fn options() -> Options {
  let mut opts = Options::new();
  opts
    .parsing_style(ParsingStyle::FloatingFrees)
    .long_only(false)
    .optflag("", "help", "Display usage information")
    .optflag("", "compile-stdlib", "Compile the GDLisp standard library")
    .optflag("", "legacy-repl", "Run the old-style GDLisp REPL which compiles rather than executing (legacy)");
  opts
}

/// Parse the arguments and return an appropriate [`CommandLineArgs`]
/// instance.
///
/// If any parsing error occurs, then [`CommandLineArgs::help()`] is
/// returned instead. If you would like to do your own error-handling,
/// consider calling [`options`] directly.
pub fn parse_args(args: &[String]) -> CommandLineArgs {
  match options().parse(args) {
    Err(_) => {
      CommandLineArgs::help()
    }
    Ok(parsed) => {
      let mut result = CommandLineArgs::new();

      result.help_message = parsed.opt_present("help");
      result.compile_stdlib_flag = parsed.opt_present("compile-stdlib");
      result.legacy_repl_flag = parsed.opt_present("legacy-repl");
      result.input_file = parsed.free.first().cloned();

      result
    }
  }
}

/// Helper function to show the GDLisp compiler help message.
pub fn show_help_message(program: &str) {
  let opts = options();
  let brief = format!("Usage: {} [FILE] [options]\n\n{}", program, PROGRAM_DESCRIPTION);
  print!("{}", opts.usage(&brief));
}
