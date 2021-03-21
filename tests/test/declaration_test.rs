
extern crate gdlisp;

use super::common::parse_compile_decl;

#[test]
pub fn empty_file_test() {
  assert_eq!(parse_compile_decl("()"), "extends Reference\nstatic func run():\n    return null\n");
}

#[test]
pub fn simple_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) x))"),
             "extends Reference\nstatic func foo(x_0):\n    return x_0\nstatic func run():\n    return null\n");
}

#[test]
pub fn lambda_in_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () x) x))"), r#"extends Reference
class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return x_0
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
static func foo(x_0):
    return x_0
static func run():
    return null
"#);
}

#[test]
pub fn closed_rw_in_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () (setq x 1)) x))"), r#"extends Reference
class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        x_0.contents = 1
        return x_0.contents
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
static func foo(x_0):
    x_0 = GDLisp.Cell.new(x_0)
    return x_0.contents
static func run():
    return null
"#);
}

#[test]
pub fn mutually_recursive_test() {
  assert_eq!(parse_compile_decl("((defn foo () (bar)) (defn bar () (foo)))"), r#"extends Reference
static func foo():
    return bar()
static func bar():
    return foo()
static func run():
    return null
"#);
}

#[test]
#[should_panic]
pub fn nonexistent_function_test() {
  parse_compile_decl("((defn foo () (bar)))");
}

#[test]
pub fn progn_decl_test() {
  assert_eq!(parse_compile_decl("((progn (progn (defn foo () ()) (defn bar () ()))))"), r#"extends Reference
static func foo():
    return null
static func bar():
    return null
static func run():
    return null
"#);
}
