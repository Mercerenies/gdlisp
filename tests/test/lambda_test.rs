
use super::common::{parse_compile_and_output, parse_compile_and_output_h, parse_and_run};

#[test]
#[ignore]
fn simple_lambda_running_test() {
  let output = parse_and_run("((print (funcall (lambda (a) a) 1)))");
  assert_eq!(output, "\n1\n");
}

#[test]
#[ignore]
fn closure_lambda_running_test() {
  let output = parse_and_run(r#"
  ((let ((a (let ((x 99))
              (let ((f (lambda (z) x)))
                f))))
    (print (funcall a ()))))
  "#);
  assert_eq!(output, "\n99\n");
}

#[test]
#[ignore]
fn modifying_closure_test() {
  let output = parse_and_run(r#"
  ((let ((a 0))
    (funcall (lambda () (setq a 1)))
    (print a)))
  "#);
  assert_eq!(output, "\n1\n");
}

#[test]
#[ignore]
fn modifying_closure_inside_lambda_test() {
  // Basically, modifying_closure_test stuffed inside an extra lambda layer and then called.
  let output = parse_and_run(r#"
  ((let ((f (lambda (x) (funcall (lambda () (setq x 1))) x)))
    (print (funcall f 0))))
  "#);
  assert_eq!(output, "\n1\n");
}

#[test]
#[ignore]
fn lambda_access_outer_function_test() {
  let output = parse_and_run(r#"
  ((defn foo () 100)
   (let ((f (lambda () (foo))))
     (print (funcall f))))
  "#);
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn basic_lambda_test() {

  let result0 = parse_compile_and_output_h("(lambda ())");
  assert_eq!(result0.0, "return _LambdaBlock_0.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func():\n        return null\n    func call_funcv(args):\n        if args == null:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

  let result1 = parse_compile_and_output_h("(lambda (a) a)");
  assert_eq!(result1.0, "return _LambdaBlock_1.new()\n");
  assert_eq!(result1.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args == null:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args == null:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

  let result2 = parse_compile_and_output_h("(progn (lambda (a) a) 1)");
  assert_eq!(result2.0, "return 1\n");
  assert_eq!(result2.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args == null:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args == null:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () a))");
  assert_eq!(result0.0, "var a_0 = null\nreturn _LambdaBlock_1.new(a_0)\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    var a_0\n    func _init(a_0):\n        self.a_0 = a_0\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func():\n        return a_0\n    func call_funcv(args):\n        if args == null:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn non_closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () (let (a) a)))");
  assert_eq!(result0.0, "var a_0 = null\nreturn _LambdaBlock_2.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_2 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func():\n        var a_1 = null\n        return a_1\n    func call_funcv(args):\n        if args == null:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn basic_funcall_test() {
  assert_eq!(parse_compile_and_output("(funcall 1)"), "return GDLisp.funcall(1, null)\n");
  assert_eq!(parse_compile_and_output("(progn (funcall 1) 2)"), "GDLisp.funcall(1, null)\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(funcall 1 2 3)"), "return GDLisp.funcall(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, null)))\n");
}

#[test]
pub fn funcall_lambda_test() {

  let result0 = parse_compile_and_output_h("(let ((f (lambda (a) a))) (funcall f 100))");
  assert_eq!(result0.0, "var f_2 = _LambdaBlock_1.new()\nreturn GDLisp.funcall(f_2, GDLisp.Cons.new(100, null))\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args == null:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args == null:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");
}

#[test]
pub fn function_ref_test() {
  let result0 = parse_compile_and_output_h("(function foo1)");
  assert_eq!(result0.0, "return _FunctionRefBlock_0.new()\n");
  assert_eq!(result0.1, "class _FunctionRefBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func(arg0):\n        return foo1(arg0)\n    func call_funcv(args):\n        var required_0 = null\n        if args == null:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args == null:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

  let result1 = parse_compile_and_output_h("#'foo1");
  assert_eq!(result1.0, "return _FunctionRefBlock_0.new()\n");
  assert_eq!(result1.1, "class _FunctionRefBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = 0\n    func call_func(arg0):\n        return foo1(arg0)\n    func call_funcv(args):\n        var required_0 = null\n        if args == null:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args == null:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");
}
