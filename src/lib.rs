
#![allow(clippy::redundant_field_names)]
#![allow(non_shorthand_field_patterns)]

#[macro_use] extern crate lalrpop_util;
#[macro_use] extern crate lazy_static;

pub mod sxp;
pub mod compile;
pub mod ir;
pub mod gdscript;
pub mod runner;
pub mod graph;
pub mod util;
pub mod command_line;
pub mod pipeline;
pub mod optimize;
pub mod repl;
mod parser_test;

lalrpop_mod!(#[allow(clippy::all)] pub parser);

lazy_static! {

  pub static ref AST_PARSER: parser::ASTParser =
    parser::ASTParser::new();

  pub static ref SOME_AST_PARSER: parser::SomeASTParser =
    parser::SomeASTParser::new();

}
