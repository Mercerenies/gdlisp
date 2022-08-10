
use super::common::*;
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::ir::loops::error::LoopPrimitiveError;

#[test]
fn for_loop_test() {
  let output = parse_and_run(r#"
    ((for variable 3 (print variable)))
  "#);
  assert_eq!(output, "\n0\n1\n2\n");
}

#[test]
pub fn for_tests() {
  assert_eq!(parse_compile_and_output("(for x 1)"), "for x in 1:\n    pass\nreturn null\n");
  assert_eq!(parse_compile_and_output("(for x 1 2)"), "for x in 1:\n    pass\nreturn null\n");
  assert_eq!(parse_compile_and_output("(for x 1 (foo))"), "for x in 1:\n    foo()\nreturn null\n");
  assert_eq!(parse_compile_and_output("(for x-y 1 (foo))"), "for x_y in 1:\n    foo()\nreturn null\n");
}

#[test]
pub fn for_test_with_break() {
  assert_eq!(parse_compile_and_output("(for i 1 (break))"), r#"for i in 1:
    break
return null
"#);
}

#[test]
pub fn for_test_with_continue() {
  assert_eq!(parse_compile_and_output("(for i 1 (continue))"), r#"for i in 1:
    continue
return null
"#);
}

#[test]
pub fn bad_break_in_for_loop_lambda_test() {
  assert_eq!(parse_compile_and_output_err("(for i 1 (lambda () (break)))"),
             Err(PError::from(LoopPrimitiveError::break_error(SourceOffset(20)).in_closure())));
}

#[test]
pub fn bad_continue_in_for_loop_lambda_test() {
  assert_eq!(parse_compile_and_output_err("(for i 1 (lambda () (continue)))"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(20)).in_closure())));
}

#[test]
pub fn bad_continue_in_for_loop_lambda_class_test() {
  assert_eq!(parse_compile_and_output_err("(for i 1 (new Reference (defn foo () (continue))))"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(37)).in_closure())));
}

#[test]
pub fn bad_continue_in_for_loop_iter_test() {
  assert_eq!(parse_compile_and_output_err("(for i (continue) 2)"),
             Err(PError::from(LoopPrimitiveError::continue_error(SourceOffset(7)))));
}
