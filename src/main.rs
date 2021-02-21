
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
use gdlisp::sxp::ast;
use gdlisp::command_line::{parse_args, show_help_message};

use std::io::{self, Write, BufRead, BufWriter};
use std::env;
use std::fs;

fn run_pseudo_repl() {
  let stdin = io::stdin();
  let parser = parser::ASTParser::new();
  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()));
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  for line in stdin.lock().lines() {
    let str = line.unwrap();
    match parser.parse(&str) {
      Err(err) => println!("Error: {}", err),
      Ok(value) => {
        println!("{}", value);
        // If it doesn't already look like a list of declarations,
        // make it a singleton list. (We're trying to guess what the
        // user wants here, so it won't be perfect)
        let value =
          match value {
            ast::AST::Cons(ref a, _) if matches!(**a, ast::AST::Cons(_, _)) => value,
            _ => ast::cons(value, ast::AST::Nil),
          };
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

  let input_contents = fs::read_to_string(input).unwrap();
  let parser = parser::SomeASTParser::new();
  let ast = parser.parse(&input_contents).unwrap();

  let mut compiler = Compiler::new(FreshNameGenerator::new(ast.all_symbols()));
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  let ir = ir::compile_toplevel(&ast).unwrap();
  let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
  compiler.compile_decls(&mut builder, &table, &ir).unwrap();
  let result = builder.build();
  write!(output_target, "{}", result.to_gd()).unwrap();

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
