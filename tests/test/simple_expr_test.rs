
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

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
pub fn nonexistent_assignment_test() {
  assert_eq!(
    parse_compile_and_output_err("(set nonexistent-var 0)"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("nonexistent-var")), SourceOffset(5)))),
  );
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
pub fn custom_assign_test() {
  let result = parse_compile_decl("((defn set-foo (c a b)) (set (foo 1 2) 3))");
  assert_eq!(result, r#"extends Reference
static func set_foo(c_0, a_1, b_2):
    return null
static func run():
    return set_foo(3, 1, 2)
"#);
}

#[test]
pub fn slot_assign_test_1() {
  let result = parse_compile_and_output("(let ((a 1)) (set a:b 3))");
  assert_eq!(result, "var a_0 = 1\na_0.b = 3\nreturn 3\n");
}

#[test]
pub fn slot_assign_test_2() {
  let result = parse_compile_and_output("(let ((a 1)) (set (access-slot a b) 3))");
  assert_eq!(result, "var a_0 = 1\na_0.b = 3\nreturn 3\n");
}

#[test]
pub fn slot_assign_test_3() {
  let result = parse_compile_and_output("(progn (let ((a 1)) (set (access-slot a b) 3)) 0)");
  assert_eq!(result, "var a_0 = 1\na_0.b = 3\nreturn 0\n");
}

#[test]
pub fn slot_assign_test_4() {
  let result = parse_compile_and_output("(let ((a 1)) (set (access-slot a b) (a:foo)))");
  assert_eq!(result, "var a_0 = 1\nvar _assign_1 = a_0.foo()\na_0.b = _assign_1\nreturn _assign_1\n");
}

#[test]
pub fn slot_assign_test_5() {
  let result = parse_compile_and_output("(progn (let ((a 1)) (set (access-slot a b) (a:foo))) 0)");
  assert_eq!(result, "var a_0 = 1\na_0.b = a_0.foo()\nreturn 0\n");
}

#[test]
pub fn assign_to_self_test() {
  assert_eq!(
    parse_compile_decl_err("((defclass Foo (Node) (defn _init () (set self 1))))"),
    Err(PError::from(GDError::new(GDErrorF::CannotAssignTo(String::from("self")), SourceOffset(47)))),
  );
}

#[test]
pub fn assign_to_const_test() {
  assert_eq!(
    parse_compile_decl_err("((defconst CONSTANT 1) (set CONSTANT 2))"),
    Err(PError::from(GDError::new(GDErrorF::CannotAssignTo(String::from("CONSTANT")), SourceOffset(37)))),
  );
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
             "var x_0 = 1\nx_0.foo = 100\nreturn 100\n");
  assert_eq!(parse_compile_and_output("(flet ((f () 1)) (set (f):foo 100) 2)"),
             "_flet_0().foo = 100\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(flet ((f () 1)) (set (f):foo 100))"),
             "_flet_0().foo = 100\nreturn 100\n");
}


#[test]
#[ignore]
pub fn assign_to_cons_test() {
  assert_eq!(parse_and_run("((let ((x (cons nil nil))) (set (car x) 10) (set (cdr x) 20) (print (car x)) (print (cdr x))))"),
             "\n10\n20\n");
}

#[test]
pub fn sys_this_file_test() {
  assert_eq!(parse_compile_and_output("(sys/special-ref this-file)"),
             "return load(\"res://TEST.gd\")\n");
}

#[test]
pub fn this_file_test() {
  assert_eq!(parse_compile_and_output("(this-file)"),
             "return load(\"res://TEST.gd\")\n");
}

#[test]
#[ignore]
pub fn sys_this_file_run_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (print (foo 124))
     (print ((sys/special-ref this-file):foo 124)))"#);
  assert_eq!(output, "\n248\n248\n");
}

#[test]
#[ignore]
pub fn sys_this_file_run_in_macro_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (defmacro baz () '((sys/special-ref this-file):foo 124))
     (print (baz)))"#);
  assert_eq!(output, "\n248\n");
}

#[test]
#[ignore]
pub fn this_file_run_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (print (foo 124))
     (print ((this-file):foo 124)))"#);
  assert_eq!(output, "\n248\n248\n");
}

#[test]
#[ignore]
pub fn this_file_run_in_macro_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (defmacro baz () '((this-file):foo 124))
     (print (baz)))"#);
  assert_eq!(output, "\n248\n");
}

#[test]
#[ignore]
pub fn assign_to_vec_test_1() {
  let output = parse_and_run(r#"
    ((let ((v (vector 1 1 1)))
       (print v:x)
       (set v:x 10)
       (print v:x)))
  "#);
  assert_eq!(output, "\n1\n10\n");
}

#[test]
#[ignore]
pub fn assign_to_vec_test_2() {
  let output = parse_and_run(r#"
    ((let ((v (vector 1 1 1)))
       (print v:x)
       (funcall (lambda () (set v:x 10)))
       (print v:x)))
  "#);
  assert_eq!(output, "\n1\n10\n");
}
