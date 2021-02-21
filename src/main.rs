
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
use gdlisp::pipeline;

use std::io::{self, Write, BufRead, BufWriter, BufReader};
use std::env;
use std::fs;

fn run_pseudo_repl() {
  let stdin = io::stdin();

  for line in stdin.lock().lines() {
    match pipeline::compile_code("(eval)", &line.unwrap()) {
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

fn compile_file(input: &str, output: Option<&str>) {
  let mut output_target: Box<dyn Write> = output.map_or(Box::new(io::stdout()), |name| Box::new(fs::File::create(name).unwrap()));
  let mut output_target: BufWriter<&mut dyn Write> = BufWriter::new(output_target.by_ref());

  let mut input_target = BufReader::new(fs::File::open(input).unwrap());
  pipeline::compile_file(input, &mut input_target, &mut output_target).unwrap();
}

fn main() {
  let args: Vec<_> = env::args().collect();
  let program_name = &args[0];
  let parsed_args = parse_args(&args[1..]);
  if parsed_args.help_message {
    show_help_message(&program_name);
  } else if let Some(input) = parsed_args.input_file {
    compile_file(&input, parsed_args.output_file.as_deref());
  } else {
    run_pseudo_repl();
  }
}
