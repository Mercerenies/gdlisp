
extern crate lalrpop_util;
extern crate gdlisp;

/*
pub mod sxp;
pub mod compile;
pub mod gdscript;
mod parser_test;

lalrpop_mod!(pub parser);
*/

use gdlisp::compile::Compiler;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::parser;
use gdlisp::gdscript::{stmt, expr};

use std::io::{self, BufRead};

fn as_return(expr: expr::Expr) -> stmt::Stmt {
  stmt::Stmt::ReturnStmt(expr)
}

fn main() {
  let stdin = io::stdin();
  let parser = parser::ASTParser::new();
  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()));

  for line in stdin.lock().lines() {
    let str = line.unwrap();
    match parser.parse(&str) {
      Err(err) => println!("Error: {}", err),
      Ok(value) => {
        println!("{}", value);
        let mut tmp = StmtBuilder::new();
        match compiler.compile_statement(&mut tmp, as_return, &value) {
          Err(err) => println!("Error: {:?}", err),
          Ok(()) => {
            // TODO Print helpers too
            let (stmts, _) = tmp.build();
            stmts.into_iter().for_each(|stmt| print!("{}", stmt.to_gd(0)));
          }
        }
      }
    }
  }

}
