
#![allow(clippy::redundant_field_names)]
#![allow(non_shorthand_field_patterns)]

#[macro_use] extern crate lalrpop_util;

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
mod parser_test;

lalrpop_mod!(#[allow(clippy::all)] pub parser);

