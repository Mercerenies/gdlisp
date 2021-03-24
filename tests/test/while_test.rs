
use super::common::parse_compile_and_output;

#[test]
pub fn while_tests() {
  assert_eq!(parse_compile_and_output("(while 1)"), "while 1:\n    pass\nreturn null\n");
  assert_eq!(parse_compile_and_output("(while (foo) (foo1 0) (foo2 0 0))"), "while foo():\n    foo1(0)\n    foo2(0, 0)\nreturn null\n");
  assert_eq!(parse_compile_and_output("(foo1 (while (bar)))"), "while bar():\n    pass\nreturn foo1(null)\n");
}

#[test]
pub fn compound_while_tests() {
  // If expressions cannot be compiled into a single GDScript
  // expression, so this forces the while loop to use the "compound"
  // form.
  assert_eq!(parse_compile_and_output("(while (if 1 2 3) (foo))"), "while true:\n    var _cond_0 = null\n    if 1:\n        _cond_0 = 2\n    else:\n        if true:\n            _cond_0 = 3\n        else:\n            _cond_0 = null\n    if _cond_0:\n        break\n    foo()\nreturn null\n")
}
