
use super::common::{parse_compile_and_output, parse_and_run};

#[test]
pub fn if_tests_expr() {
  assert_eq!(parse_compile_and_output("(if 1 2 3)"), "var _cond = null\nif 1:\n    _cond = 2\nelse:\n    if true:\n        _cond = 3\n    else:\n        _cond = null\nreturn _cond\n");
  assert_eq!(parse_compile_and_output("(if 1 2)"), "var _cond = null\nif 1:\n    _cond = 2\nelse:\n    if true:\n        _cond = null\n    else:\n        _cond = null\nreturn _cond\n");
  assert_eq!(parse_compile_and_output("(if 1 2 ())"), "var _cond = null\nif 1:\n    _cond = 2\nelse:\n    if true:\n        _cond = null\n    else:\n        _cond = null\nreturn _cond\n");
  assert_eq!(parse_compile_and_output("(if 1 (foo) (bar))"), "var _cond = null\nif 1:\n    _cond = foo()\nelse:\n    if true:\n        _cond = bar()\n    else:\n        _cond = null\nreturn _cond\n");
}

#[test]
pub fn if_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (if 1 2 3) 1)"), "if 1:\n    pass\nelse:\n    if true:\n        pass\n    else:\n        pass\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(progn (if 1 (foo) (bar)) 1)"), "if 1:\n    foo()\nelse:\n    if true:\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_expr() {
  assert_eq!(parse_compile_and_output("(cond ((bar) (foo)) (foobar (bar)))"), "var _cond = null\nif bar():\n    _cond = foo()\nelse:\n    if foobar:\n        _cond = bar()\n    else:\n        _cond = null\nreturn _cond\n");
}

#[test]
pub fn cond_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((bar) (foo)) (foobar (bar))) 1)"), "if bar():\n    foo()\nelse:\n    if foobar:\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_abbr_expr() {
  assert_eq!(parse_compile_and_output("(cond ((foo) (foo)) ((bar)))"), "var _cond = null\nif foo():\n    _cond = foo()\nelse:\n    var _cond_0 = bar()\n    if _cond_0:\n        _cond = _cond_0\n    else:\n        _cond = null\nreturn _cond\n");
}

#[test]
pub fn cond_tests_abbr_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((foo) (foo)) ((bar))) 1)"), "if foo():\n    foo()\nelse:\n    var _cond = bar()\n    if _cond:\n        pass\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn or_test() {
  let result0 = parse_compile_and_output("(or 1 2 3)");
  assert_eq!(result0, r#"var _cond = null
var _cond_1 = 1
if _cond_1:
    _cond = _cond_1
else:
    var _cond_0 = 2
    if _cond_0:
        _cond = _cond_0
    else:
        if true:
            _cond = 3
        else:
            _cond = null
return _cond
"#);
}

#[test]
pub fn and_test() {
  let result0 = parse_compile_and_output("(and 1 2 3)");
  assert_eq!(result0, r#"var _cond = null
if !1:
    _cond = false
else:
    if !2:
        _cond = false
    else:
        if true:
            _cond = 3
        else:
            _cond = null
return _cond
"#);
}

#[test]
pub fn when_test() {
  let result0 = parse_compile_and_output("(when 1 (foo) (bar))");
  assert_eq!(result0, r#"var _cond = null
if 1:
    foo()
    _cond = bar()
else:
    _cond = null
return _cond
"#);
}

#[test]
pub fn unless_test() {
  let result0 = parse_compile_and_output("(unless 1 (foo) (bar))");
  assert_eq!(result0, r#"var _cond = null
if 1:
    _cond = null
else:
    if true:
        foo()
        _cond = bar()
    else:
        _cond = null
return _cond
"#);
}

#[test]
fn if_test_1() {
  let output = parse_and_run(r#"
  ((let ((x 1))
    (print (if x 10 20))))
  "#);
  assert_eq!(output, "\n10\n");
}

#[test]
fn if_test_2() {
  let output = parse_and_run(r#"
  ((let ((x 0))
    (print (if x 10 20))))
  "#);
  assert_eq!(output, "\n20\n");
}
