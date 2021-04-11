
use super::common::{parse_compile_and_output, parse_compile_decl};

#[test]
pub fn expr_tests() {
  assert_eq!(parse_compile_and_output("100"), "return 100\n");
  assert_eq!(parse_compile_and_output("(progn 100 200 300)"), "return 300\n");
  assert_eq!(parse_compile_and_output("()"), "return null\n");
}

#[test]
pub fn progn_tests() {
  assert_eq!(parse_compile_and_output("(progn (foo) (bar) (foo))"), "foo()\nbar()\nreturn foo()\n");
  assert_eq!(parse_compile_and_output("(progn)"), "return null\n");
  assert_eq!(parse_compile_and_output("(progn (progn))"), "return null\n");
  assert_eq!(parse_compile_and_output("(progn ())"), "return null\n");
}

#[test]
#[should_panic]
pub fn nonexistent_assignment_test() {
  parse_compile_and_output("(set nonexistent-var 0)");
}

#[test]
pub fn assignment_test() {

  // No cell; only accessed directly
  let result0 = parse_compile_and_output("(let ((x 1)) (set x 2))");
  assert_eq!(result0, "var x_0 = 1\nx_0 = 2\nreturn x_0\n");

  // No cell; only accessed directly
  let result1 = parse_compile_and_output("(let ((x 1)) (set x 2) 3)");
  assert_eq!(result1, "var x_0 = 1\nx_0 = 2\nreturn 3\n");

  // Cell; accessed inside lambda
  let result2 = parse_compile_and_output("(let ((x 1)) (lambda () (set x 2)))");
  assert_eq!(result2, "var x_0 = GDLisp.Cell.new(1)\nreturn _LambdaBlock_1.new(x_0)\n");

  // Cell; accessed both inside and outside lambda
  let result3 = parse_compile_and_output("(let ((x 1)) (lambda () (set x 2)) (set x 3))");
  assert_eq!(result3, "var x_0 = GDLisp.Cell.new(1)\nx_0.contents = 3\nreturn x_0.contents\n");

  // No cell; read-only
  let result4 = parse_compile_and_output("(let ((x 1)) (lambda () x) x)");
  assert_eq!(result4, "var x_0 = 1\nreturn x_0\n");

  // Cell; closure and access separately
  let result5 = parse_compile_and_output("(let ((x 1)) (lambda () (set x 1)) x)");
  assert_eq!(result5, "var x_0 = GDLisp.Cell.new(1)\nreturn x_0.contents\n");

}

#[test]
#[should_panic]
pub fn assign_to_self_test() {
  parse_compile_decl("((defclass Foo (Node) (defn _init () (set self 1))))");
}

#[test]
#[should_panic]
pub fn assign_to_const_test() {
  parse_compile_decl("((defconst CONSTANT 1) (set CONSTANT 2))");
}

#[test]
pub fn weird_name_test() {
  assert_eq!(parse_compile_and_output("(let ((a-b-c 1)) (a-b-c:d-e-f))"),
             "var a_b_c_0 = 1\nreturn a_b_c_0.d_e_f()\n");
}

#[test]
pub fn assign_to_slot_test() {
  assert_eq!(parse_compile_and_output("(let ((x 1)) (set x:foo 100) 2)"),
             "var x_0 = 1\nx_0.foo = 100\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(let ((x 1)) (set x:foo 100))"),
             "var x_0 = 1\nx_0.foo = 100\nreturn x_0.foo\n");
  assert_eq!(parse_compile_and_output("(flet ((f () 1)) (set (f):foo 100) 2)"),
             "_flet_0().foo = 100\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(flet ((f () 1)) (set (f):foo 100))"),
             "var _assign_1 = _flet_0()\n_assign_1.foo = 100\nreturn _assign_1.foo\n");
}
