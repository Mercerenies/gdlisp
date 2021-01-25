
extern crate gdlisp;

use gdlisp::compile::Compiler;
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use gdlisp::parser;
use gdlisp::ir;
use gdlisp::gdscript::library;

fn bind_helper_symbols(table: &mut SymbolTable) {
  // Binds a few helper names to the symbol table for the sake of
  // debugging.
  table.set_fn(String::from("foo"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("foo")));
  table.set_fn(String::from("foo1"), FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, String::from("foo1")));
  table.set_fn(String::from("foo2"), FnCall::unqualified(FnSpecs::new(2, 0, false), FnScope::Global, String::from("foo2")));
  table.set_fn(String::from("bar"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("bar")));
  table.set_var(String::from("foobar"), String::from("foobar"));
}

// TODO Currently, this panics if it fails. This is okay-ish, since
// it's only being used for tests. But once we unify all of our errors
// (so we can represent parse errors and compile errors with one
// common type), we should return a Result<...> here.
fn parse_compile_and_output(input: &str) -> String {
  parse_compile_and_output_h(input).0
}

fn parse_compile_and_output_h(input: &str) -> (String, String) {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
  let mut table = SymbolTable::new();
  bind_helper_symbols(&mut table);
  library::bind_builtins(&mut table);

  let mut builder = StmtBuilder::new();
  let value = ir::compile_expr(&value).unwrap();
  let () = compiler.compile_stmt(&mut builder, &mut table, &mut stmt_wrapper::Return, &value).unwrap();
  let (stmts, helpers) = builder.build();
  let a = stmts.into_iter().map(|stmt| stmt.to_gd(0)).collect::<String>();
  let b = helpers.into_iter().map(|decl| decl.to_gd(0)).collect::<String>();
  (a, b)
}

#[test]
pub fn expr_tests() {
  assert_eq!(parse_compile_and_output("100"), "return 100\n");
  assert_eq!(parse_compile_and_output("(progn 100 200 300)"), "return 300\n");
  assert_eq!(parse_compile_and_output("()"), "return GDLisp.Nil\n");
}

#[test]
pub fn progn_tests() {
  assert_eq!(parse_compile_and_output("(progn (foo) (bar) (foo))"), "foo()\nbar()\nreturn foo()\n");
  assert_eq!(parse_compile_and_output("(progn)"), "return GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(progn (progn))"), "return GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(progn ())"), "return GDLisp.Nil\n");
}

#[test]
pub fn if_tests_expr() {
  assert_eq!(parse_compile_and_output("(if 1 2 3)"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = 2\nelse:\n    _if_0 = 3\nreturn _if_0\n");
  assert_eq!(parse_compile_and_output("(if 1 2)"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = 2\nelse:\n    _if_0 = GDLisp.Nil\nreturn _if_0\n");
  assert_eq!(parse_compile_and_output("(if 1 2 ())"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = 2\nelse:\n    _if_0 = GDLisp.Nil\nreturn _if_0\n");
  //assert_eq!(parse_compile_and_output("(if _if_0 2 ())"), "var _if_1 = GDLisp.Nil\nif _if_0:\n    _if_1 = 2\nelse:\n    _if_1 = GDLisp.Nil\nreturn _if_1\n"); // Variable scoping issues; can't do this one right now
  assert_eq!(parse_compile_and_output("(if 1 (foo) (bar))"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = foo()\nelse:\n    _if_0 = bar()\nreturn _if_0\n");
}

#[test]
pub fn if_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (if 1 2 3) 1)"), "if 1:\n    pass\nelse:\n    pass\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(progn (if 1 (foo) (bar)) 1)"), "if 1:\n    foo()\nelse:\n    bar()\nreturn 1\n");
}

#[test]
pub fn cond_tests_expr() {
  assert_eq!(parse_compile_and_output("(cond ((bar) (foo)) (foobar (bar)))"), "var _cond_0 = GDLisp.Nil\nif bar():\n    _cond_0 = foo()\nelse:\n    if foobar:\n        _cond_0 = bar()\n    else:\n        _cond_0 = GDLisp.Nil\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((bar) (foo)) (foobar (bar))) 1)"), "if bar():\n    foo()\nelse:\n    if foobar:\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_abbr_expr() {
  assert_eq!(parse_compile_and_output("(cond ((foo) (foo)) ((bar)))"), "var _cond_0 = GDLisp.Nil\nif foo():\n    _cond_0 = foo()\nelse:\n    var _cond_1 = bar()\n    if _cond_1:\n        _cond_0 = _cond_1\n    else:\n        _cond_0 = GDLisp.Nil\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_abbr_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((foo) (foo)) ((bar))) 1)"), "if foo():\n    foo()\nelse:\n    var _cond_0 = bar()\n    if _cond_0:\n        pass\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn let_tests() {
  assert_eq!(parse_compile_and_output("(let () 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(let (a) 1)"), "var a_0 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a)) 1)"), "var a_0 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (foo1 a))"), "var a_0 = 1\nreturn foo1(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a 1) (b 2)) (foo2 a b))"), "var a_0 = 1\nvar b_1 = 2\nreturn foo2(a_0, b_1)\n");
  assert_eq!(parse_compile_and_output("(let ((a (foo) (bar))) (foo1 a))"), "foo()\nvar a_0 = bar()\nreturn foo1(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a) b) 1)"), "var a_0 = GDLisp.Nil\nvar b_1 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let (a (b)) 1)"), "var a_0 = GDLisp.Nil\nvar b_1 = GDLisp.Nil\nreturn 1\n");
}

#[test]
pub fn var_shadowing() {
  assert_eq!(parse_compile_and_output("(let ((a)) (let ((a a)) a))"), "var a_0 = GDLisp.Nil\nvar a_1 = a_0\nreturn a_1\n");
}

#[test]
pub fn inline_if_in_let_test() {
  assert_eq!(parse_compile_and_output("(let ((a (if (foo) (bar) (foo)))) a)"), "var _if_0 = GDLisp.Nil\nif foo():\n    _if_0 = bar()\nelse:\n    _if_0 = foo()\nvar a_1 = _if_0\nreturn a_1\n");
}

#[test]
pub fn basic_lambda_test() {

  let result0 = parse_compile_and_output_h("(lambda ())");
  assert_eq!(result0.0, "return _LambdaBlock_0.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func():\n        return GDLisp.Nil\n    func call_funcv(args):\n        if args is GDLisp.NilClass:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

  let result1 = parse_compile_and_output_h("(lambda (a) a)");
  assert_eq!(result1.0, "return _LambdaBlock_1.new()\n");
  assert_eq!(result1.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

  let result2 = parse_compile_and_output_h("(progn (lambda (a) a) 1)");
  assert_eq!(result2.0, "return 1\n");
  assert_eq!(result2.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () a))");
  assert_eq!(result0.0, "var a_0 = GDLisp.Nil\nreturn _LambdaBlock_1.new(a_0)\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    var a_0\n    func _init(a_0):\n        self.a_0 = a_0\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func():\n        return a_0\n    func call_funcv(args):\n        if args is GDLisp.NilClass:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn non_closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () (let (a) a)))");
  assert_eq!(result0.0, "var a_0 = GDLisp.Nil\nreturn _LambdaBlock_2.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_2 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func():\n        var a_1 = GDLisp.Nil\n        return a_1\n    func call_funcv(args):\n        if args is GDLisp.NilClass:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn basic_funcall_test() {
  assert_eq!(parse_compile_and_output("(funcall 1)"), "return GDLisp.funcall(1, GDLisp.Nil)\n");
  assert_eq!(parse_compile_and_output("(progn (funcall 1) 2)"), "GDLisp.funcall(1, GDLisp.Nil)\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(funcall 1 2 3)"), "return GDLisp.funcall(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, GDLisp.Nil)))\n");
}

#[test]
pub fn funcall_lambda_test() {

  let result0 = parse_compile_and_output_h("(let ((f (lambda (a) a))) (funcall f 100))");
  assert_eq!(result0.0, "var f_2 = _LambdaBlock_1.new()\nreturn GDLisp.funcall(f_2, GDLisp.Cons.new(100, GDLisp.Nil))\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");
}

#[test]
pub fn simple_builtin_test() {
  assert_eq!(parse_compile_and_output("(cons 1 2)"), "return GDLisp.Cons.new(1, 2)\n");
  assert_eq!(parse_compile_and_output("(cons 1 (cons 2 3))"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, 3))\n");
}

#[test]
pub fn function_ref_test() {
  let result0 = parse_compile_and_output_h("(function foo1)");
  assert_eq!(result0.0, "return _FunctionRefBlock_0.new()\n");
  assert_eq!(result0.1, "class _FunctionRefBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(arg0):\n        return foo1(arg0)\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");
}
