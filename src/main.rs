
extern crate lalrpop_util;
extern crate gdlisp;

/*
pub mod sxp;
pub mod compile;
pub mod gdscript;
mod parser_test;

lalrpop_mod!(pub parser);
*/

use gdlisp::ir;
use gdlisp::compile::Compiler;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::CodeBuilder;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::parser;
use gdlisp::gdscript::library;
use gdlisp::gdscript::decl;
use gdlisp::command_line::{parse_args, show_help_message};
use gdlisp::pipeline;

use std::io::{self, Write, BufRead, BufWriter, BufReader};
use std::env;
use std::fs;

fn run_pseudo_repl() {
  let stdin = io::stdin();
  let parser = parser::SomeASTParser::new();
  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()));
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  for line in stdin.lock().lines() {
    let str = line.unwrap();
    match parser.parse(&str) {
      Err(err) => println!("Error: {}", err),
      Ok(value) => {
        println!("{}", value);
        match ir::compile_toplevel(&value) {
          Err(err) => println!("Error: {:?}", err),
          Ok(value) => {
            let mut tmp = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
            match compiler.compile_decls(&mut tmp, &table, &value) {
              Err(err) => println!("Error: {:?}", err),
              Ok(()) => {
                let result = tmp.build();
                print!("{}", result.to_gd());
              }
            }
          }
        }
      }
    }
  }

}

fn compile_file(input: &str, output: Option<&str>) {
  let mut output_target: Box<dyn Write> = output.map_or(Box::new(io::stdout()), |name| Box::new(fs::File::create(name).unwrap()));
  let mut output_target: BufWriter<&mut dyn Write> = BufWriter::new(output_target.by_ref());

  let mut input_target = BufReader::new(fs::File::open(input).unwrap());
  pipeline::compile_file(&mut input_target, &mut output_target).unwrap();
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
