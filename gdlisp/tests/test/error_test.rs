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

extern crate gdlisp;

use gdlisp::compile::symbol_table::local_var::VarNameIntoExtendsError;
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::sxp::ast::AST;

use super::common::*;

#[test]
pub fn bad_call_compile_test_1() {
  assert_eq!(
    parse_compile_and_output_err("(5 6)"),
    Err(PError::from(GDError::new(GDErrorF::CannotCall(AST::from_value(5, SourceOffset(1))), SourceOffset(1)))),
  );
}

#[test]
pub fn bad_call_compile_test_2() {
  assert_eq!(
    parse_compile_and_output_err("(() 6)"),
    Err(PError::from(GDError::new(GDErrorF::CannotCall(AST::nil(SourceOffset(1))), SourceOffset(1)))),
  );
}

#[test]
pub fn cannot_extend_local() {
  assert_eq!(
    parse_compile_and_output_err(r#"(let ((x 1)) (let ((y (new x))) y))"#),
    Err(PError::from(GDError::from_value(VarNameIntoExtendsError::CannotExtendLocal(String::from("x")), SourceOffset(22)))),
  );
}

#[test]
pub fn cannot_extend_current_file() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass MyMainClass () main) (defclass MySubclass (MyMainClass)))"#),
    Err(PError::from(GDError::from_value(VarNameIntoExtendsError::CannotExtendCurrentFile(String::from("res://TEST.gd")), SourceOffset(33)))),
  );
}
