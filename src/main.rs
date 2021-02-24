
extern crate lalrpop_util;
extern crate gdlisp;

/*
pub mod sxp;
pub mod compile;
pub mod gdscript;
mod parser_test;

lalrpop_mod!(pub parser);
*/

use gdlisp::gdscript::decl;
use gdlisp::command_line::{parse_args, show_help_message};
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::config::ProjectConfig;
use gdlisp::pipeline::loader::FileLoader;

use std::io::{self, BufRead};
use std::env;
use std::path::{PathBuf, Path};
use std::str::FromStr;

fn run_pseudo_repl() {
  let stdin = io::stdin();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from_str(".").unwrap() }); // Infallible

  for line in stdin.lock().lines() {
    match pipeline.compile_code("(eval)", &line.unwrap()) {
      Err(err) => {
        println!("Error: {:?}", err);
      }
      Ok(trans) => {
        let cls = decl::TopLevelClass::from(trans);
        print!("{}", cls.to_gd());
      }
    }
  }

}

fn compile_file<P : AsRef<Path> + ?Sized>(input: &P) {
  let input = input.as_ref();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: input.parent().unwrap_or(input).to_owned() });
  pipeline.load_file(input.file_name().unwrap()).unwrap();
}

fn main() {
  let args: Vec<_> = env::args().collect();
  let program_name = &args[0];
  let parsed_args = parse_args(&args[1..]);
  if parsed_args.help_message {
    show_help_message(&program_name);
  } else if let Some(input) = parsed_args.input_file {
    compile_file(&input);
  } else {
    run_pseudo_repl();
  }
}
