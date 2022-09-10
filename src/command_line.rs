
//! This module provides functionality to parse command line
//! arguments.
//!
//! Generally speaking, the entrypoint to this module will be
//! [`parse_args`].

use getopts::{Options, ParsingStyle};

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
  let brief = format!("Usage: {} [FILE] [options]", program);
  print!("{}", opts.usage(&brief));
}
