
use super::common::{parse_compile_and_output_h, parse_and_run, parse_compile_decl};

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
  assert_eq!(result0.1, "static func _flet_0(x):\n    return x + 1\n");

}

#[test]
pub fn semiglobal_labels_test_indirect() {

  let result0 = parse_compile_and_output_h("(labels ((f (x) (+ x 1))) (funcall (function f) 10))");
  assert_eq!(result0.0, "return GDLisp.funcall(_FunctionRefBlock.new(), GDLisp.Cons.new(10, null))\n");
  assert_eq!(result0.1, r#"static func _flet_0(x):
    return x + 1
class _FunctionRefBlock extends GDLisp.Function:
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
  assert_eq!(result0.0, "var _locals = _Labels.new()\nreturn _locals._fn_f_0(1)\n");
  assert_eq!(result0.1, r#"class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_f_0(x)
"#);
}

#[test]
pub fn recursive_single_labels_with_contrived_local_name_test() {
  let result = parse_compile_decl("((defconst x 0) (let ((x 1)) (labels ((f (x_0) (f x_0) (f x))) (f 1))))");
  assert_eq!(result, r#"extends Reference
const x = 0
class _Labels extends Reference:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
    func _fn_f_0(x_0_0):
        _fn_f_0(x_0_0)
        return _fn_f_0(x_0)
static func run():
    var x_0 = 1
    var _locals = _Labels.new(x_0)
    return _locals._fn_f_0(1)
"#);
}

#[test]
pub fn recursive_double_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (g x)) (g (x) (f x))) (g (f 1)))");
  assert_eq!(result0.0, "var _locals = _Labels.new()\nreturn _locals._fn_g_1(_locals._fn_f_0(1))\n");
  assert_eq!(result0.1, r#"class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_g_1(x)
    func _fn_g_1(x):
        return _fn_f_0(x)
"#);
}

#[test]
pub fn recursive_single_with_extra_beginning_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f (g x))) (g (x) 10)) (f 1))");
  assert_eq!(result0.0, "var _locals = _Labels.new()\nreturn _locals._fn_f_2(1)\n");
  assert_eq!(result0.1, r#"static func _flet_0(x):
    return 10
class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_2(x):
        return _fn_f_2(__gdlisp_outer_class_1._flet_0(x))
    var __gdlisp_outer_class_1 = load("res://TEST.gd")
"#);
}

#[test]
pub fn recursive_single_with_extra_end_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x)) (g (x) (f x))) (g 1))");
  assert_eq!(result0.0, "var _locals = _Labels.new()\nvar _flet = _LambdaBlock.new(_locals)\nreturn _flet.call_func(1)\n");
  assert_eq!(result0.1, r#"class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_f_0(x)
class _LambdaBlock extends GDLisp.Function:
    var _locals
    func _init(_locals):
        self._locals = _locals
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func(x):
        return _locals._fn_f_0(x)
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
  assert_eq!(result0.0, "var _locals = _Labels.new()\nreturn GDLisp.funcall(_FunctionRefBlock.new(_locals), GDLisp.Cons.new(1, null))\n");
  assert_eq!(result0.1, r#"class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_f_0(x)
class _FunctionRefBlock extends GDLisp.Function:
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

#[test]
pub fn recursive_single_labels_decl_test_1() {
  let result = parse_compile_decl("((labels ((f (x) (f x))) (f 1)))");
  assert_eq!(result, r#"extends Reference
class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_f_0(x)
static func run():
    var _locals = _Labels.new()
    return _locals._fn_f_0(1)
"#);
}

#[test]
pub fn recursive_single_labels_decl_test_2() {
  // Contrived introduction of a constant named _locals. The
  // contextual name generator should detect this and work around it.
  let result = parse_compile_decl("((defconst _locals 0) (labels ((f (x) (f x))) (f 1)))");
  assert_eq!(result, r#"extends Reference
const _locals = 0
class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_f_0(x)
static func run():
    var _locals_0 = _Labels.new()
    return _locals_0._fn_f_0(1)
"#);
}

#[test]
pub fn contrived_nested_labels_test() {
  // Contrived introduction of a constant named _locals. The
  // contextual name generator should detect this and work around it.
  let result = parse_compile_decl(r#"((labels ((f (x) (f x)))
                                        (labels ((g (x) (f x) (g x)))
                                          (f 1)
                                          (g 2)))
                                      (labels ((f (x) (f x)))
                                        (labels ((g (x) (f x) (g x)))
                                          (f 1)
                                          (g 2))))"#);
  assert_eq!(result, r#"extends Reference
class _Labels extends Reference:
    func _init():
        pass
    func _fn_f_0(x):
        return _fn_f_0(x)
class _Labels_0 extends Reference:
    var _locals
    func _init(_locals):
        self._locals = _locals
    func _fn_g_1(x):
        _locals._fn_f_0(x)
        return _fn_g_1(x)
class _Labels_1 extends Reference:
    func _init():
        pass
    func _fn_f_2(x):
        return _fn_f_2(x)
class _Labels_2 extends Reference:
    var _locals_1
    func _init(_locals_1):
        self._locals_1 = _locals_1
    func _fn_g_3(x):
        _locals_1._fn_f_2(x)
        return _fn_g_3(x)
static func run():
    var _locals = _Labels.new()
    var _locals_0 = _Labels_0.new(_locals)
    _locals._fn_f_0(1)
    _locals_0._fn_g_1(2)
    var _locals_1 = _Labels_1.new()
    var _locals_2 = _Labels_2.new(_locals_1)
    _locals_1._fn_f_2(1)
    return _locals_2._fn_g_3(2)
"#);
}
