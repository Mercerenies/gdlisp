
extern crate gdlisp;

use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
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
