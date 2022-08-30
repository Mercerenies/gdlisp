
use super::common::{parse_compile_and_output, parse_compile_and_output_h, parse_and_run};

#[test]
fn simple_lambda_running_test() {
  let output = parse_and_run("((print (funcall (lambda (a) a) 1)))");
  assert_eq!(output, "\n1\n");
}

#[test]
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
fn modifying_closure_test() {
  let output = parse_and_run(r#"
  ((let ((a 0))
    (funcall (lambda () (set a 1)))
    (print a)))
  "#);
  assert_eq!(output, "\n1\n");
}

#[test]
fn modifying_closure_inside_lambda_test() {
  // Basically, modifying_closure_test stuffed inside an extra lambda layer and then called.
  let output = parse_and_run(r#"
  ((let ((f (lambda (x) (funcall (lambda () (set x 1))) x)))
    (print (funcall f 0))))
  "#);
  assert_eq!(output, "\n1\n");
}

#[test]
fn lambda_access_outer_function_test() {
  let output = parse_and_run(r#"
  ((defn foo () 100)
   (let ((f (lambda () (foo))))
     (print (funcall f))))
  "#);
  assert_eq!(output, "\n100\n");
}

#[test]
fn simple_function_ref_run_test() {
  let output = parse_and_run(r#"
  ((defn foo () 100)
   (let ((f #'foo))
     (print (funcall f))))
  "#);
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn basic_lambda_test() {

  let result0 = parse_compile_and_output_h("(lambda ())");
  assert_eq!(result0.0, "return _LambdaBlock.new()\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        return null

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

  let result1 = parse_compile_and_output_h("(lambda (a) a)");
  assert_eq!(result1.0, "return _LambdaBlock.new()\n");
  assert_eq!(result1.1, r#"class _LambdaBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func(a):
        return a

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

  let result2 = parse_compile_and_output_h("(progn (lambda (a) a) 1)");
  assert_eq!(result2.0, "return 1\n");
  assert_eq!(result2.1, r#"class _LambdaBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func(a):
        return a

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
pub fn closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () a))");
  assert_eq!(result0.0, "var a = null\nreturn _LambdaBlock.new(a)\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:

    var a

    func _init(a):
        self.a = a
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        return a

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn non_closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () (let (a) a)))");
  assert_eq!(result0.0, "var a = null\nreturn _LambdaBlock.new()\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        var a = null
        return a

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

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
  assert_eq!(result0.0, "var f = _LambdaBlock.new()\nreturn GDLisp.funcall(f, GDLisp.Cons.new(100, null))\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func(a):
        return a

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
pub fn function_ref_test() {
  let result0 = parse_compile_and_output_h("(function foo1)");
  assert_eq!(result0.0, "return _FunctionRefBlock.new()\n");
  assert_eq!(result0.1, r#"class _FunctionRefBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func(arg0):
        return load("res://TEST.gd").foo1(arg0)

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

  let result1 = parse_compile_and_output_h("#'foo1");
  assert_eq!(result1.0, "return _FunctionRefBlock.new()\n");
  assert_eq!(result1.1, r#"class _FunctionRefBlock extends GDLisp.Function:

    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func(arg0):
        return load("res://TEST.gd").foo1(arg0)

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
