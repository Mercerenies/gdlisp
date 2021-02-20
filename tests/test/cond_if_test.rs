
use super::common::{parse_compile_and_output, parse_and_run};

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
#[ignore]
fn if_test_1() {
  let output = parse_and_run(r#"
  ((let ((x 1))
    (print (if x 10 20))))
  "#);
  assert_eq!(output, "\n10\n");
}

#[test]
#[ignore]
fn if_test_2() {
  let output = parse_and_run(r#"
  ((let ((x 0))
    (print (if x 10 20))))
  "#);
  assert_eq!(output, "\n20\n");
}
