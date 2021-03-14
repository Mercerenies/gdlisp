
use super::common::{parse_compile_and_output, parse_compile_decl};

#[test]
pub fn expr_tests() {
  assert_eq!(parse_compile_and_output("100"), "return 100\n");
  assert_eq!(parse_compile_and_output("(progn 100 200 300)"), "return 300\n");
  assert_eq!(parse_compile_and_output("()"), "return GDLisp.Nil\n");
}

#[test]
pub fn progn_tests() {
  assert_eq!(parse_compile_and_output("(progn (foo) (bar) (foo))"), "foo()\nbar()\nreturn foo()\n");
  assert_eq!(parse_compile_and_output("(progn)"), "return GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(progn (progn))"), "return GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(progn ())"), "return GDLisp.Nil\n");
}

#[test]
#[should_panic]
pub fn nonexistent_assignment_test() {
  parse_compile_and_output("(setq nonexistent-var 0)");
}

#[test]
pub fn assignment_test() {

  // No cell; only accessed directly
  let result0 = parse_compile_and_output("(let ((x 1)) (setq x 2))");
  assert_eq!(result0, "var x_0 = 1\nx_0 = 2\nreturn x_0\n");

  // No cell; only accessed directly
  let result1 = parse_compile_and_output("(let ((x 1)) (setq x 2) 3)");
  assert_eq!(result1, "var x_0 = 1\nx_0 = 2\nreturn 3\n");

  // Cell; accessed inside lambda
  let result2 = parse_compile_and_output("(let ((x 1)) (lambda () (setq x 2)))");
  assert_eq!(result2, "var x_0 = GDLisp.Cell.new(1)\nreturn _LambdaBlock_1.new(x_0)\n");

  // Cell; accessed both inside and outside lambda
  let result3 = parse_compile_and_output("(let ((x 1)) (lambda () (setq x 2)) (setq x 3))");
  assert_eq!(result3, "var x_0 = GDLisp.Cell.new(1)\nx_0.contents = 3\nreturn x_0.contents\n");

  // No cell; read-only
  let result4 = parse_compile_and_output("(let ((x 1)) (lambda () x) x)");
  assert_eq!(result4, "var x_0 = 1\nreturn x_0\n");

  // Cell; closure and access separately
  let result5 = parse_compile_and_output("(let ((x 1)) (lambda () (setq x 1)) x)");
  assert_eq!(result5, "var x_0 = GDLisp.Cell.new(1)\nreturn x_0.contents\n");

}

#[test]
#[should_panic]
pub fn assign_to_self_test() {
  parse_compile_decl("((defclass Foo (Node) (defn _init () (setq self 1))))");
}

#[test]
#[should_panic]
pub fn assign_to_const_test() {
  parse_compile_decl("((defconst CONSTANT 1) (setq CONSTANT 2))");
}
