
extern crate gdlisp;

use gdlisp::compile::Compiler;
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::compile::symbol_table::concrete::ConcreteTable;
use gdlisp::parser;

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
  let mut table = ConcreteTable::new();

  let mut builder = StmtBuilder::new();
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
  assert_eq!(parse_compile_and_output("(progn (a) (b) (c))"), "a()\nb()\nreturn c()\n");
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
  assert_eq!(parse_compile_and_output("(if 1 (a) (b))"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = a()\nelse:\n    _if_0 = b()\nreturn _if_0\n");
}

#[test]
pub fn if_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (if 1 2 3) 1)"), "if 1:\n    pass\nelse:\n    pass\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(progn (if 1 (a) (b)) 1)"), "if 1:\n    a()\nelse:\n    b()\nreturn 1\n");
}

#[test]
pub fn cond_tests_expr() {
  assert_eq!(parse_compile_and_output("(cond ((a) (foo)) ((b) (bar)))"), "var _cond_0 = GDLisp.Nil\nif a():\n    _cond_0 = foo()\nelse:\n    if b():\n        _cond_0 = bar()\n    else:\n        _cond_0 = GDLisp.Nil\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((a) (foo)) ((b) (bar))) 1)"), "if a():\n    foo()\nelse:\n    if b():\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_abbr_expr() {
  assert_eq!(parse_compile_and_output("(cond ((a) (foo)) ((bar)))"), "var _cond_0 = GDLisp.Nil\nif a():\n    _cond_0 = foo()\nelse:\n    var _cond_1 = bar()\n    if _cond_1:\n        _cond_0 = _cond_1\n    else:\n        _cond_0 = GDLisp.Nil\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_abbr_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((a) (foo)) ((bar))) 1)"), "if a():\n    foo()\nelse:\n    var _cond_0 = bar()\n    if _cond_0:\n        pass\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn let_tests() {
  assert_eq!(parse_compile_and_output("(let () 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(let (a) 1)"), "var a_0 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a)) 1)"), "var a_0 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (foo a))"), "var a_0 = 1\nreturn foo(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a 1) (b 2)) (foo a b))"), "var a_0 = 1\nvar b_1 = 2\nreturn foo(a_0, b_1)\n");
  assert_eq!(parse_compile_and_output("(let ((a (foo) (bar))) (baz a))"), "foo()\nvar a_0 = bar()\nreturn baz(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a) b) 1)"), "var a_0 = GDLisp.Nil\nvar b_1 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let (a (b)) 1)"), "var a_0 = GDLisp.Nil\nvar b_1 = GDLisp.Nil\nreturn 1\n");
}

#[test]
pub fn var_shadowing() {
  assert_eq!(parse_compile_and_output("(let ((a)) (let ((a a)) a))"), "var a_0 = GDLisp.Nil\nvar a_1 = a_0\nreturn a_1\n");
}

#[test]
pub fn inline_if_in_let_test() {
  assert_eq!(parse_compile_and_output("(let ((a (if (foo) (bar) (baz)))) a)"), "var _if_0 = GDLisp.Nil\nif foo():\n    _if_0 = bar()\nelse:\n    _if_0 = baz()\nvar a_1 = _if_0\nreturn a_1\n");
}

#[test]
pub fn basic_lambda_test() {

  let result0 = parse_compile_and_output_h("(lambda ())");
  assert_eq!(result0.0, "return _LambdaBlock_0.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_0 extends Reference:\n    func call_func():\n        return GDLisp.Nil\n");

  let result1 = parse_compile_and_output_h("(lambda (a) a)");
  assert_eq!(result1.0, "return _LambdaBlock_1.new()\n");
  assert_eq!(result1.1, "class _LambdaBlock_1 extends Reference:\n    func call_func(a_0):\n        return a_0\n");

  let result2 = parse_compile_and_output_h("(progn (lambda (a) a) 1)");
  assert_eq!(result2.0, "return 1\n");
  assert_eq!(result2.1, "class _LambdaBlock_1 extends Reference:\n    func call_func(a_0):\n        return a_0\n");

}

#[test]
pub fn basic_funcall_test() {
  assert_eq!(parse_compile_and_output("(funcall 1)"), "return 1.call_func()\n");
  assert_eq!(parse_compile_and_output("(progn (funcall 1) 2)"), "1.call_func()\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(funcall 1 2 3)"), "return 1.call_func(2, 3)\n");
}

#[test]
pub fn funcall_lambda_test() {

  let result0 = parse_compile_and_output_h("(let ((f (lambda (a) a))) (funcall f 3))");
  assert_eq!(result0.0, "var f_2 = _LambdaBlock_1.new()\nreturn f_2.call_func(3)\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends Reference:\n    func call_func(a_0):\n        return a_0\n");
}
