
#[macro_use] extern crate lalrpop_util;

pub mod sxp;
pub mod compile;
pub mod gdscript;
mod parser_test;

lalrpop_mod!(parser);

use std::io::{self, BufRead};

fn main() {
  let stdin = io::stdin();
  let parser = parser::ASTParser::new();
  for line in stdin.lock().lines() {
    let str = line.unwrap();
    match parser.parse(&str) {
      Err(err) => println!("Error: {}", err),
      Ok(value) => println!("{}", value),
    }
  }
}
