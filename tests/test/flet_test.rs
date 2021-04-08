
use super::common::{parse_compile_and_output_h, parse_and_run};

#[test]
#[ignore]
fn simple_flet_test_1() {
  let output = parse_and_run("((flet ((f (x) x)) (print (f 1))))");
  assert_eq!(output, "\n1\n");
}

#[test]
#[ignore]
fn simple_flet_test_2() {
  let output = parse_and_run("((flet ((f (x) (* x 2))) (print (f 1)) (print (f 2))))");
  assert_eq!(output, "\n2\n4\n");
}

#[test]
#[ignore]
fn simple_flet_test_3() {
  let output = parse_and_run("((defn foo (x) (* x 2)) (flet ((f (x) (foo x))) (print (f 1)) (print (f 2))))");
  assert_eq!(output, "\n2\n4\n");
}

#[test]
#[ignore]
fn closed_flet_test_1() {
  let output = parse_and_run(r#"
    ((let ((x 0))
       (flet ((f () (setq x (+ x 1))))
         (print (f))
         (print (f)))))"#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
#[ignore]
fn closed_flet_test_2() {
  let output = parse_and_run(r#"
    ((let ((g (let ((x 0))
                (flet ((f () (setq x (+ x 1))))
                  (lambda () (f))))))
       (print (funcall g))
       (print (funcall g))))
  "#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
#[ignore]
fn nested_flet_test() {
  let output = parse_and_run(r#"
    ((let ((g (let ((x 0))
                (flet ((f () (setq x (+ x 1))))
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
  assert_eq!(result0.0, "return _flet_0(10)\n");
  assert_eq!(result0.1, "static func _flet_0(x_1):\n    return x_1 + 1\n");

}

#[test]
pub fn semiglobal_flet_test_indirect() {

  let result0 = parse_compile_and_output_h("(flet ((f (x) (+ x 1))) (funcall (function f) 10))");
  assert_eq!(result0.0, "return GDLisp.funcall(_FunctionRefBlock_2.new(), GDLisp.Cons.new(10, null))\n");
  assert_eq!(result0.1, r#"static func _flet_0(x_1):
    return x_1 + 1
class _FunctionRefBlock_2 extends GDLisp.Function:
    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func(arg0):
        return _flet_0(arg0)
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
  assert_eq!(result0.0, "var x_0 = 1\nvar _flet_2 = _LambdaBlock_1.new(x_0)\nreturn _flet_2.call_func()\n");
  assert_eq!(result0.1, r#"class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return x_0 + 1
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
  assert_eq!(result0.0, "var x_0 = 1\nvar _flet_2 = _LambdaBlock_1.new(x_0)\nreturn GDLisp.funcall(_flet_2, null)\n");
  assert_eq!(result0.1, r#"class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return x_0 + 1
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
  assert_eq!(result0.0, "var x_0 = 1\nvar _flet_2 = _LambdaBlock_1.new(x_0)\nvar g_4 = _LambdaBlock_3.new(_flet_2)\nreturn GDLisp.funcall(g_4, null)\n");
}
