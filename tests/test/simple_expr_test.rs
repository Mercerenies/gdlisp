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

use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::ir::expr::Expr;

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
  assert_eq!(result0, "var x = 1\nx = 2\nreturn x\n");

  // No cell; only accessed directly
  let result1 = parse_compile_and_output("(let ((x 1)) (set x 2) 3)");
  assert_eq!(result1, "var x = 1\nx = 2\nreturn 3\n");

  // Cell; accessed inside lambda
  let result2 = parse_compile_and_output("(let ((x 1)) (lambda () (set x 2)))");
  assert_eq!(result2, "var x = GDLisp.Cell.new(1)\nreturn _LambdaBlock.new(x)\n");

  // Cell; accessed both inside and outside lambda
  let result3 = parse_compile_and_output("(let ((x 1)) (lambda () (set x 2)) (set x 3))");
  assert_eq!(result3, "var x = GDLisp.Cell.new(1)\nx.contents = 3\nreturn x.contents\n");

  // No cell; read-only
  let result4 = parse_compile_and_output("(let ((x 1)) (lambda () x) x)");
  assert_eq!(result4, "var x = 1\nreturn x\n");

  // Cell; closure and access separately
  let result5 = parse_compile_and_output("(let ((x 1)) (lambda () (set x 1)) x)");
  assert_eq!(result5, "var x = GDLisp.Cell.new(1)\nreturn x.contents\n");

}

#[test]
pub fn custom_assign_test() {
  let result = parse_compile_decl("((defn set-foo (c a b)) (defn example () (set (foo 1 2) 3)))");
  assert_eq!(result, r#"extends Reference


static func set_foo(c, a, b):
    return null


static func example():
    return set_foo(3, 1, 2)
"#);
}

#[test]
pub fn expr_at_toplevel_test() {
  // Note: We're using `parse_compile_decl_err` here, not
  // `parse_compile_and_output_err`, so the system is *expecting*
  // declarations. We gave it an expression and expect it to come back
  // with an `ExprAtTopLevel` error.
  let result = parse_compile_decl_err("((+ 1 1))");
  assert_eq!(
    result,
    Err(PError::from(GDError::new(
      GDErrorF::ExprAtTopLevel(
        Expr::call("+", vec!(Expr::from_value(1, SourceOffset(4)), Expr::from_value(1, SourceOffset(6))), SourceOffset(1)),
      ),
      SourceOffset(1),
    ))),
  );
}

#[test]
pub fn slot_assign_test_1() {
  let result = parse_compile_and_output("(let ((a 1)) (set a:b 3))");
  assert_eq!(result, "var a = 1\na.b = 3\nreturn 3\n");
}

#[test]
pub fn slot_assign_test_2() {
  let result = parse_compile_and_output("(let ((a 1)) (set (access-slot a b) 3))");
  assert_eq!(result, "var a = 1\na.b = 3\nreturn 3\n");
}

#[test]
pub fn slot_assign_test_3() {
  let result = parse_compile_and_output("(progn (let ((a 1)) (set (access-slot a b) 3)) 0)");
  assert_eq!(result, "var a = 1\na.b = 3\nreturn 0\n");
}

#[test]
pub fn slot_assign_test_4() {
  let result = parse_compile_and_output("(let ((a 1)) (set (access-slot a b) (a:foo)))");
  assert_eq!(result, "var a = 1\nvar _assign = a.foo()\na.b = _assign\nreturn _assign\n");
}

#[test]
pub fn slot_assign_test_5() {
  let result = parse_compile_and_output("(progn (let ((a 1)) (set (access-slot a b) (a:foo))) 0)");
  assert_eq!(result, "var a = 1\na.b = a.foo()\nreturn 0\n");
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
    parse_compile_decl_err("((defconst CONSTANT 1) (defn foo () (set CONSTANT 2)))"),
    Err(PError::from(GDError::new(GDErrorF::CannotAssignTo(String::from("CONSTANT")), SourceOffset(50)))),
  );
}

#[test]
pub fn weird_name_test() {
  assert_eq!(parse_compile_and_output("(let ((a-b-c 1)) (a-b-c:d-e-f))"),
             "var a_b_c = 1\nreturn a_b_c.d_e_f()\n");
}

#[test]
pub fn assign_to_slot_test() {
  assert_eq!(parse_compile_and_output("(let ((x 1)) (set x:foo 100) 2)"),
             "var x = 1\nx.foo = 100\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(let ((x 1)) (set x:foo 100))"),
             "var x = 1\nx.foo = 100\nreturn 100\n");
  assert_eq!(parse_compile_and_output("(flet ((f () 1)) (set (f):foo 100) 2)"),
             "_flet().foo = 100\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(flet ((f () 1)) (set (f):foo 100))"),
             "_flet().foo = 100\nreturn 100\n");
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
pub fn sys_this_file_run_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (print (foo 124))
     (print ((sys/special-ref this-file):foo 124)))"#);
  assert_eq!(output, "\n248\n248\n");
}

#[test]
pub fn sys_this_file_run_in_macro_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (defmacro baz () '((sys/special-ref this-file):foo 124))
     (print (baz)))"#);
  assert_eq!(output, "\n248\n");
}

#[test]
pub fn this_file_run_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (print (foo 124))
     (print ((this-file):foo 124)))"#);
  assert_eq!(output, "\n248\n248\n");
}

#[test]
pub fn this_file_run_in_macro_test() {
  let output = parse_and_run(r#"
    ((defn foo (x) (* x 2))
     (defmacro baz () '((this-file):foo 124))
     (print (baz)))"#);
  assert_eq!(output, "\n248\n");
}

#[test]
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
pub fn assign_to_vec_test_2() {
  let output = parse_and_run(r#"
    ((let ((v (vector 1 1 1)))
       (print v:x)
       (funcall (lambda () (set v:x 10)))
       (print v:x)))
  "#);
  assert_eq!(output, "\n1\n10\n");
}

#[test]
pub fn compound_assignment_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (update a (+ 2)))"), r#"var a = 1
a = a + 2
return a
"#);
}

#[test]
pub fn compound_assignment_run_test_1() {
  assert_eq!(parse_and_run("((let ((a 1)) (update a (+ 2)) (print a)))"), "\n3\n");
}

#[test]
pub fn compound_assignment_run_test_2() {
  assert_eq!(parse_and_run("((defn foo (x) (* x 2)) (let ((a 9)) (update a foo) (print a)))"), "\n18\n");
}
