
use super::common::{parse_compile_and_output, parse_compile_and_output_h};

#[test]
pub fn let_tests() {
  assert_eq!(parse_compile_and_output("(let () 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(let (a) 1)"), "var a_0 = null\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a)) 1)"), "var a_0 = null\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (foo1 a))"), "var a_0 = 1\nreturn foo1(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a 1) (b 2)) (foo2 a b))"), "var a_0 = 1\nvar b_1 = 2\nreturn foo2(a_0, b_1)\n");
  assert_eq!(parse_compile_and_output("(let ((a (foo) (bar))) (foo1 a))"), "foo()\nvar a_0 = bar()\nreturn foo1(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a) b) 1)"), "var a_0 = null\nvar b_1 = null\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let (a (b)) 1)"), "var a_0 = null\nvar b_1 = null\nreturn 1\n");
}

#[test]
pub fn let_name_trans_tests() {
  assert_eq!(parse_compile_and_output("(let ((a-b 1)) a-b)"), "var a_b_0 = 1\nreturn a_b_0\n");
}

#[test]
pub fn var_shadowing() {
  assert_eq!(parse_compile_and_output("(let ((a)) (let ((a a)) a))"), "var a_0 = null\nvar a_1 = a_0\nreturn a_1\n");
}

#[test]
pub fn inline_if_in_let_test() {
  assert_eq!(parse_compile_and_output("(let ((a (if (foo) (bar) (foo)))) a)"), "var _if_0 = null\nif foo():\n    _if_0 = bar()\nelse:\n    _if_0 = foo()\nvar a_1 = _if_0\nreturn a_1\n");
}

#[test]
pub fn closure_var_test() {
  let result0 = parse_compile_and_output_h("(lambda () foobar)");
  assert_eq!(result0.0, "return _LambdaBlock_0.new(foobar)\n");
  assert_eq!(result0.1, r#"class _LambdaBlock_0 extends GDLisp.Function:
    var foobar
    func _init(foobar):
        self.foobar = foobar
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return foobar
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);


  let result1 = parse_compile_and_output_h("(lambda () glob)");
  assert_eq!(result1.0, "return _LambdaBlock_0.new()\n");
  assert_eq!(result1.1, r#"class _LambdaBlock_0 extends GDLisp.Function:
    func _init():
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return glob
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
"#);
}
