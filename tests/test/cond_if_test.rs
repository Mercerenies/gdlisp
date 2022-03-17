
use super::common::{parse_compile_and_output, parse_and_run};

#[test]
pub fn if_tests_expr() {
  assert_eq!(parse_compile_and_output("(if 1 2 3)"), "var _cond_0 = null\nif 1:\n    _cond_0 = 2\nelse:\n    if true:\n        _cond_0 = 3\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
  assert_eq!(parse_compile_and_output("(if 1 2)"), "var _cond_0 = null\nif 1:\n    _cond_0 = 2\nelse:\n    if true:\n        _cond_0 = null\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
  assert_eq!(parse_compile_and_output("(if 1 2 ())"), "var _cond_0 = null\nif 1:\n    _cond_0 = 2\nelse:\n    if true:\n        _cond_0 = null\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
  assert_eq!(parse_compile_and_output("(if 1 (foo) (bar))"), "var _cond_0 = null\nif 1:\n    _cond_0 = foo()\nelse:\n    if true:\n        _cond_0 = bar()\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
}

#[test]
pub fn if_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (if 1 2 3) 1)"), "if 1:\n    pass\nelse:\n    if true:\n        pass\n    else:\n        pass\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(progn (if 1 (foo) (bar)) 1)"), "if 1:\n    foo()\nelse:\n    if true:\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_expr() {
  assert_eq!(parse_compile_and_output("(cond ((bar) (foo)) (foobar (bar)))"), "var _cond_0 = null\nif bar():\n    _cond_0 = foo()\nelse:\n    if foobar:\n        _cond_0 = bar()\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((bar) (foo)) (foobar (bar))) 1)"), "if bar():\n    foo()\nelse:\n    if foobar:\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_abbr_expr() {
  assert_eq!(parse_compile_and_output("(cond ((foo) (foo)) ((bar)))"), "var _cond_0 = null\nif foo():\n    _cond_0 = foo()\nelse:\n    var _cond_1 = bar()\n    if _cond_1:\n        _cond_0 = _cond_1\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_abbr_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((foo) (foo)) ((bar))) 1)"), "if foo():\n    foo()\nelse:\n    var _cond_0 = bar()\n    if _cond_0:\n        pass\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn or_test() {
  let result0 = parse_compile_and_output("(or 1 2 3)");
  assert_eq!(result0, r#"var _cond_0 = null
var _cond_2 = 1
if _cond_2:
    _cond_0 = _cond_2
else:
    var _cond_1 = 2
    if _cond_1:
        _cond_0 = _cond_1
    else:
        if true:
            _cond_0 = 3
        else:
            _cond_0 = null
return _cond_0
"#);
}

#[test]
pub fn and_test() {
  let result0 = parse_compile_and_output("(and 1 2 3)");
  assert_eq!(result0, r#"var _cond_0 = null
if !1:
    _cond_0 = false
else:
    if !2:
        _cond_0 = false
    else:
        if true:
            _cond_0 = 3
        else:
            _cond_0 = null
return _cond_0
"#);
}

#[test]
pub fn when_test() {
  let result0 = parse_compile_and_output("(when 1 (foo) (bar))");
  assert_eq!(result0, r#"var _cond_0 = null
if 1:
    foo()
    _cond_0 = bar()
else:
    _cond_0 = null
return _cond_0
"#);
}

#[test]
pub fn unless_test() {
  let result0 = parse_compile_and_output("(unless 1 (foo) (bar))");
  assert_eq!(result0, r#"var _cond_0 = null
if 1:
    _cond_0 = null
else:
    if true:
        foo()
        _cond_0 = bar()
    else:
        _cond_0 = null
return _cond_0
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
