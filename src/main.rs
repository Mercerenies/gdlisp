
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

use std::io::{self, BufRead};

fn main() {
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
