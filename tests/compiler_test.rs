
extern crate gdlisp;

use gdlisp::compile::Compiler;
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::parser;

// TODO Currently, this panics if it fails. This is okay-ish, since
// it's only being used for tests. But once we unify all of our errors
// (so we can represent parse errors and compile errors with one
// common type), we should return a Result<...> here.
fn parse_compile_and_output(input: &str) -> String {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));

  let mut builder = StmtBuilder::new();
  let () = compiler.compile_stmt(&mut builder, &mut stmt_wrapper::Return, &value).unwrap();
  // TODO Print helpers here, too
  let (stmts, _) = builder.build();
  stmts.into_iter().map(|stmt| stmt.to_gd(0)).collect::<String>()
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
