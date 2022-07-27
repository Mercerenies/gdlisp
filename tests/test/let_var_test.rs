
use super::common::{parse_compile_and_output, parse_compile_and_output_h};

#[test]
pub fn let_tests() {
  assert_eq!(parse_compile_and_output("(let () 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(let (a) 1)"), "var a = null\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a)) 1)"), "var a = null\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (foo1 a))"), "var a = 1\nreturn foo1(a)\n");
  assert_eq!(parse_compile_and_output("(let ((a 1) (b 2)) (foo2 a b))"), "var a = 1\nvar b = 2\nreturn foo2(a, b)\n");
  assert_eq!(parse_compile_and_output("(let ((a (foo) (bar))) (foo1 a))"), "foo()\nvar a = bar()\nreturn foo1(a)\n");
  assert_eq!(parse_compile_and_output("(let ((a) b) 1)"), "var a = null\nvar b = null\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let (a (b)) 1)"), "var a = null\nvar b = null\nreturn 1\n");
}

#[test]
pub fn let_name_trans_tests() {
  assert_eq!(parse_compile_and_output("(let ((a-b 1)) a-b)"), "var a_b = 1\nreturn a_b\n");
}

#[test]
pub fn var_shadowing() {
  assert_eq!(parse_compile_and_output("(let ((a)) (let ((a a)) a))"), "var a = null\nvar a_0 = a\nreturn a_0\n");
}

#[test]
pub fn inline_if_in_let_test() {
  assert_eq!(parse_compile_and_output("(let ((a (if (foo) (bar) (foo)))) a)"), "var _cond = null\nif foo():\n    _cond = bar()\nelse:\n    if true:\n        _cond = foo()\n    else:\n        _cond = null\nvar a = _cond\nreturn a\n");
}

#[test]
pub fn closure_var_test() {
  let result0 = parse_compile_and_output_h("(lambda () foobar)");
  assert_eq!(result0.0, "return _LambdaBlock.new(foobar)\n");
  assert_eq!(result0.1, r#"class _LambdaBlock extends GDLisp.Function:
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
  assert_eq!(result1.0, "return _LambdaBlock.new()\n");
  assert_eq!(result1.1, r#"class _LambdaBlock extends GDLisp.Function:
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

#[test]
pub fn let_star_test_1() {
  assert_eq!(parse_compile_and_output("(let* ((a 1) (b a)) b)"), "var a = 1\nvar b = a\nreturn b\n");
}

#[test]
pub fn let_star_test_2() {
  assert_eq!(parse_compile_and_output("(let* ((a 1) (a a) (a a)) a)"),
             "var a = 1\nvar a_0 = a\nvar a_1 = a_0\nreturn a_1\n");
}
