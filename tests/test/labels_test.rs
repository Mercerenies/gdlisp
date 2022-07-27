
use super::common::{parse_compile_and_output_h, parse_and_run};

#[test]
fn simple_labels_test_1() {
  let output = parse_and_run("((labels ((f (x) x)) (print (f 1))))");
  assert_eq!(output, "\n1\n");
}

#[test]
fn labels_test_outer_scope_static_1() {
  let output = parse_and_run("((defn foo (x) (+ x 10)) (labels ((f (x) (if #f (f x) (foo x)))) (print (f 1))))");
  assert_eq!(output, "\n11\n");
}

#[test]
fn labels_test_outer_scope_static_2() {
  let output = parse_and_run("((defn foo (x) (+ x 10)) (labels ((f (x) (if #f (f x) (foo x)))) (print (funcall #'f 1))))");
  assert_eq!(output, "\n11\n");
}

#[test]
fn labels_test_outer_scope_static_3() {
  let output = parse_and_run("((defn foo (x y) (+ x y)) (let ((y 1)) (labels ((f (x) (foo x y))) (print (f 5)))))");
  assert_eq!(output, "\n6\n");
}

#[test]
fn labels_test_outer_scope_static_4() {
  let output = parse_and_run("((defn foo (x y) (+ x y)) (let ((y 1)) (labels ((f (x) (foo x y))) (print (funcall #'f 5)))))");
  assert_eq!(output, "\n6\n");
}

#[test]
fn labels_test_outer_scope_static_5() {
  let output = parse_and_run("((defn foo (x y) (+ x y)) (labels ((f (x) (foo x 1))) (print (f 5))))");
  assert_eq!(output, "\n6\n");
}

#[test]
fn labels_test_outer_scope_static_6() {
  let output = parse_and_run("((defn foo (x y) (+ x y)) (labels ((f (x) (foo x 1))) (print (funcall #'f 5))))");
  assert_eq!(output, "\n6\n");
}

#[test]
pub fn semiglobal_labels_test() {

  let result0 = parse_compile_and_output_h("(labels ((f (x) (+ x 1))) (f 10))");
  assert_eq!(result0.0, "return _flet_0(10)\n");
  assert_eq!(result0.1, "static func _flet_0(x_1):\n    return x_1 + 1\n");

}

#[test]
pub fn semiglobal_labels_test_indirect() {

  let result0 = parse_compile_and_output_h("(labels ((f (x) (+ x 1))) (funcall (function f) 10))");
  assert_eq!(result0.0, "return GDLisp.funcall(_FunctionRefBlock_2.new(), GDLisp.Cons.new(10, null))\n");
  assert_eq!(result0.1, r#"static func _flet_0(x_1):
    return x_1 + 1
class _FunctionRefBlock_2 extends GDLisp.Function:
    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func(arg0):
        return load("res://TEST.gd")._flet_0(arg0)
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
pub fn recursive_single_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x))) (f 1))");
  assert_eq!(result0.0, "var _locals = _Labels_2.new()\nreturn _locals._fn_f_0(1)\n");
  assert_eq!(result0.1, r#"class _Labels_2 extends Reference:
    func _init():
        pass
    func _fn_f_0(x_1):
        return _fn_f_0(x_1)
"#);
}

#[test]
pub fn recursive_double_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (g x)) (g (x) (f x))) (g (f 1)))");
  assert_eq!(result0.0, "var _locals = _Labels_4.new()\nreturn _locals._fn_g_1(_locals._fn_f_0(1))\n");
  assert_eq!(result0.1, r#"class _Labels_4 extends Reference:
    func _init():
        pass
    func _fn_f_0(x_2):
        return _fn_g_1(x_2)
    func _fn_g_1(x_3):
        return _fn_f_0(x_3)
"#);
}

#[test]
pub fn recursive_single_with_extra_beginning_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f (g x))) (g (x) 10)) (f 1))");
  assert_eq!(result0.0, "var _locals = _Labels_5.new()\nreturn _locals._fn_f_3(1)\n");
  assert_eq!(result0.1, r#"static func _flet_0(x_1):
    return 10
class _Labels_5 extends Reference:
    func _init():
        pass
    func _fn_f_3(x_4):
        return _fn_f_3(__gdlisp_outer_class_2._flet_0(x_4))
    var __gdlisp_outer_class_2 = load("res://TEST.gd")
"#);
}

#[test]
pub fn recursive_single_with_extra_end_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x)) (g (x) (f x))) (g 1))");
  assert_eq!(result0.0, "var _locals = _Labels_2.new()\nvar _flet_5 = _LambdaBlock_4.new(_locals)\nreturn _flet_5.call_func(1)\n");
  assert_eq!(result0.1, r#"class _Labels_2 extends Reference:
    func _init():
        pass
    func _fn_f_0(x_1):
        return _fn_f_0(x_1)
class _LambdaBlock_4 extends GDLisp.Function:
    var _locals
    func _init(_locals):
        self._locals = _locals
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func(x_3):
        return _locals._fn_f_0(x_3)
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
pub fn recursive_single_indirect_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x))) (funcall (function f) 1))");
  assert_eq!(result0.0, "var _locals = _Labels_2.new()\nreturn GDLisp.funcall(_FunctionRefBlock_3.new(_locals), GDLisp.Cons.new(1, null))\n");
  assert_eq!(result0.1, r#"class _Labels_2 extends Reference:
    func _init():
        pass
    func _fn_f_0(x_1):
        return _fn_f_0(x_1)
class _FunctionRefBlock_3 extends GDLisp.Function:
    var _locals
    func _init(_locals):
        self._locals = _locals
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func(arg0):
        return _locals._fn_f_0(arg0)
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
