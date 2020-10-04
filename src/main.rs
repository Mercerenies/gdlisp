
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
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::parser;
use gdlisp::gdscript::library;

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
        match ir::compile_expr(&value) {
          Err(err) => println!("Error: {:?}", err),
          Ok(value) => {
            let mut tmp = StmtBuilder::new();
            match compiler.compile_stmt(&mut tmp, &mut table, &mut stmt_wrapper::Return, &value) {
              Err(err) => println!("Error: {:?}", err),
              Ok(()) => {
                let (stmts, helpers) = tmp.build();
                helpers.into_iter().for_each(|decl| print!("{}", decl.to_gd(0)));
                stmts.into_iter().for_each(|stmt| print!("{}", stmt.to_gd(0)));
              }
            }
          }
        }
      }
    }
  }

}
