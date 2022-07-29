
extern crate gdlisp;

use gdlisp::compile::symbol_table::local_var::VarNameIntoExtendsError;
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::{Error as PError};
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
    parse_compile_decl_err(r#"((let ((x 1)) (let ((y (new x))) y)))"#),
    Err(PError::from(GDError::from_value(VarNameIntoExtendsError::CannotExtendLocal(String::from("x")), SourceOffset(23)))),
  );
}

#[test]
pub fn cannot_extend_current_file() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass MyMainClass () main) (defclass MySubclass (MyMainClass)))"#),
    Err(PError::from(GDError::from_value(VarNameIntoExtendsError::CannotExtendCurrentFile(String::from("res://TEST.gd")), SourceOffset(33)))),
  );
}
