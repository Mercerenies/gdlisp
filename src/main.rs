
#[macro_use] extern crate lalrpop_util;

pub mod sxp;
pub mod compile;
pub mod gdscript;
mod parser_test;

lalrpop_mod!(parser);

use compile::Compiler;
use compile::names::fresh::FreshNameGenerator;
use compile::body::builder::StmtBuilder;

use std::io::{self, BufRead};

fn as_return(expr: gdscript::expr::Expr) -> gdscript::stmt::Stmt {
  gdscript::stmt::Stmt::ReturnStmt(expr)
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
