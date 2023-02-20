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

use super::common::{parse_compile_and_output_h, parse_and_run};

#[test]
fn simple_flet_test_1() {
  let output = parse_and_run("((flet ((f (x) x)) (print (f 1))))");
  assert_eq!(output, "\n1\n");
}

#[test]
fn simple_flet_test_2() {
  let output = parse_and_run("((flet ((f (x) (* x 2))) (print (f 1)) (print (f 2))))");
  assert_eq!(output, "\n2\n4\n");
}

#[test]
fn simple_flet_test_3() {
  let output = parse_and_run("((defn foo (x) (* x 2)) (flet ((f (x) (foo x))) (print (f 1)) (print (f 2))))");
  assert_eq!(output, "\n2\n4\n");
}

#[test]
fn closed_flet_test_1() {
  let output = parse_and_run(r#"
    ((let ((x 0))
       (flet ((f () (set x (+ x 1))))
         (print (f))
         (print (f)))))"#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
fn closed_flet_test_2() {
  let output = parse_and_run(r#"
    ((let ((g (let ((x 0))
                (flet ((f () (set x (+ x 1))))
                  (lambda () (f))))))
       (print (funcall g))
       (print (funcall g))))
  "#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
fn nested_flet_test() {
  let output = parse_and_run(r#"
    ((let ((g (let ((x 0))
                (flet ((f () (set x (+ x 1))))
                  (flet ((g () (f)))
                    (function g))))))
       (print (funcall g))
       (print (funcall g))))
  "#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
pub fn semiglobal_flet_test() {

  let result0 = parse_compile_and_output_h("(flet ((f (x) (+ x 1))) (f 10))");
  assert_eq!(result0.0, "return _flet(10)\n");
  assert_eq!(result0.1, "static func _flet(x):\n    return x + 1\n");

}

#[test]
pub fn semiglobal_flet_test_indirect() {

  let result0 = parse_compile_and_output_h("(flet ((f (x) (+ x 1))) (funcall (function f) 10))");
  assert_eq!(result0.0, "return GDLisp.funcall(_FunctionRefBlock.new(), GDLisp.cons(10, null))\n");
  assert_eq!(result0.1, r#"static func _flet(x):
    return x + 1


class _FunctionRefBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func(arg0):
        return load("res://TEST.gd")._flet(arg0)

    func call_funcv(args):
        var required_0 = null
        if args == null:
            push_error("Not enough arguments")
        else:
            required_0 = args.car
            args = args.cdr
        if args == null:
            return call_func(required_0)
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn local_flet_test() {

  let result0 = parse_compile_and_output_h(r#"
    (let ((x 1))
      (flet ((f () (+ x 1)))
        (f)))
  "#);
  assert_eq!(result0.0, "var x = 1\nvar _flet = _LambdaBlock.new(x)\nreturn _flet.call_func()\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:

    var x

    func _init(x):
        self.x = x
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        return x + 1

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn local_flet_test_indirect() {

  let result0 = parse_compile_and_output_h(r#"
    (let ((x 1))
      (flet ((f () (+ x 1)))
        (funcall (function f))))
  "#);
  assert_eq!(result0.0, "var x = 1\nvar _flet = _LambdaBlock.new(x)\nreturn GDLisp.funcall(_flet, null)\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:

    var x

    func _init(x):
        self.x = x
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        return x + 1

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn local_flet_closure_test() {
  let result0 = parse_compile_and_output_h(r#"
    (let ((x 1))
      (flet ((f () x))
        (let ((g (lambda () (f))))
          (funcall g))))
  "#);
  assert_eq!(result0.0, "var x = 1\nvar _flet = _LambdaBlock.new(x)\nvar g = _LambdaBlock_0.new(_flet)\nreturn GDLisp.funcall(g, null)\n");
}
