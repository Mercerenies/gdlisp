
extern crate gdlisp;

use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

use super::common::{parse_compile_decl, parse_compile_decl_err};

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
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () (set x 1)) x))"), r#"extends Reference
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
pub fn nonexistent_function_test() {
  assert_eq!(
    parse_compile_decl_err("((defn foo () (bar)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("bar")), SourceOffset(14)))),
  );
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

#[test]
pub fn declare_value_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare value x) x)"), r#"extends Reference
static func run():
    return x
"#);
}

#[test]
pub fn declare_value_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal x) x)"), r#"extends Reference
static func run():
    return x
"#);
}

#[test]
pub fn declare_value_test_3() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal x) (defconst y x))"), r#"extends Reference
const y = x
static func run():
    return null
"#);
}

#[test]
pub fn declare_value_non_const_test() {
  assert_eq!(
    parse_compile_decl_err("((sys/declare value x) (defconst y x))"),
    Err(PError::from(GDError::new(GDErrorF::NotConstantEnough(String::from("y")), SourceOffset(35)))),
  );
}

#[test]
pub fn declare_function_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare function f ()) (f))"), r#"extends Reference
static func run():
    return f()
"#);
}

#[test]
pub fn declare_function_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare function f (a &opt b)) (f 1) (f 1 2))"), r#"extends Reference
static func run():
    f(1, null)
    return f(1, 2)
"#);
}

#[test]
pub fn declare_function_test_3() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction f (a &opt b)) (f 1) (f 1 2))"), r#"extends Reference
static func run():
    f(1, null)
    return f(1, 2)
"#);
}

#[test]
pub fn nonsense_modifier_function_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () public private 1))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(21)))),
  );
}
