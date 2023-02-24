// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

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
