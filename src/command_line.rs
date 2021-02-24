
// Command line argument parsing

use getopts::{Options, ParsingStyle};

#[derive(Debug, Clone, Default)]
pub struct CommandLineArgs {
  pub input_file: Option<String>,
  pub help_message: bool,
}

impl CommandLineArgs {

  pub fn new() -> CommandLineArgs {
    CommandLineArgs::default()
  }

  pub fn help() -> CommandLineArgs {
    let mut inst = CommandLineArgs::new();
    inst.help_message = true;
    inst
  }

}

pub fn options() -> Options {
  let mut opts = Options::new();
  opts
    .parsing_style(ParsingStyle::FloatingFrees)
    .long_only(false)
    .optflag("", "help", "Display usage information");
  opts
}

pub fn parse_args(args: &[String]) -> CommandLineArgs {
  match options().parse(args) {
    Err(_) => {
      CommandLineArgs::help()
    }
    Ok(parsed) => {
      let mut result = CommandLineArgs::new();

      result.help_message = parsed.opt_present("help");
      result.input_file = parsed.free.first().cloned();

      result
    }
  }
}

pub fn show_help_message(program: &str) {
  let opts = options();
  let brief = format!("Usage: {} [FILE] [options]", program);
  print!("{}", opts.usage(&brief));
}
