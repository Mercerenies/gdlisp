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

use super::common::*;
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::ir::loops::error::LoopPrimitiveError;

#[test]
pub fn while_tests() {
  assert_eq!(parse_compile_and_output("(while 1)"), "while 1:\n    pass\nreturn null\n");
  assert_eq!(parse_compile_and_output("(while (foo) (foo1 0) (foo2 0 0))"), "while foo():\n    foo1(0)\n    foo2(0, 0)\nreturn null\n");
  assert_eq!(parse_compile_and_output("(foo1 (while (bar)))"), "while bar():\n    pass\nreturn foo1(null)\n");
}

#[test]
pub fn compound_while_tests() {
  // If expressions cannot be compiled into a single GDScript
  // expression, so this forces the while loop to use the "compound"
  // form.
  assert_eq!(parse_compile_and_output("(while (if 1 2 3) (foo))"), "while true:\n    var _cond = null\n    if 1:\n        _cond = 2\n    else:\n        if true:\n            _cond = 3\n        else:\n            _cond = null\n    if !_cond:\n        break\n    foo()\nreturn null\n")
}

#[test]
pub fn while_test_with_break() {
  assert_eq!(parse_compile_and_output("(while 1 (break))"), r#"while 1:
    break
return null
"#);
}

#[test]
pub fn while_test_with_continue() {
  assert_eq!(parse_compile_and_output("(while 1 (continue))"), r#"while 1:
    continue
return null
"#);
}

#[test]
pub fn while_test_with_break_in_if() {
  assert_eq!(parse_compile_and_output("(while 1 (if 1 (break) (continue)))"), r#"while 1:
    if 1:
        break
    else:
        if true:
            continue
        else:
            pass
return null
"#);
}

#[test]
pub fn while_test_with_break_in_if_cond() {
  assert_eq!(parse_compile_and_output("(while (if 1 (break) (continue)) (foo))"), r#"while true:
    var _cond = null
    if 1:
        break
        _cond = null
    else:
        if true:
            continue
            _cond = null
        else:
            _cond = null
    if !_cond:
        break
    foo()
return null
"#);
}

#[test]
pub fn bad_break_test() {
  assert_eq!(parse_compile_and_output_err("(break)"),
             Err(PError::from(LoopPrimitiveError::break_error(SourceOffset(0)))));
}

#[test]
pub fn bad_continue_test() {
  assert_eq!(parse_compile_and_output_err("(continue)"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(0)))));
}

#[test]
pub fn bad_break_in_plain_lambda_test() {
  assert_eq!(parse_compile_and_output_err("(lambda () (break))"),
             Err(PError::from(LoopPrimitiveError::break_error(SourceOffset(11)))));
}

#[test]
pub fn bad_continue_in_plain_lambda_test() {
  assert_eq!(parse_compile_and_output_err("(lambda () (continue))"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(11)))));
}

#[test]
pub fn bad_break_in_loop_lambda_test() {
  assert_eq!(parse_compile_and_output_err("(while 1 (lambda () (break)))"),
             Err(PError::from(LoopPrimitiveError::break_error(SourceOffset(20)).in_closure())));
}

#[test]
pub fn bad_continue_in_loop_lambda_test() {
  assert_eq!(parse_compile_and_output_err("(while 1 (lambda () (continue)))"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(20)).in_closure())));
}

#[test]
pub fn bad_continue_in_loop_lambda_class_test() {
  assert_eq!(parse_compile_and_output_err("(while 1 (new Reference (defn foo () (continue))))"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(37)).in_closure())));
}
