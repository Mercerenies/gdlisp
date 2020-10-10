
#[macro_use] extern crate lalrpop_util;

pub mod sxp;
pub mod compile;
pub mod ir;
pub mod gdscript;
pub mod runner;
mod parser_test;

lalrpop_mod!(pub parser);

