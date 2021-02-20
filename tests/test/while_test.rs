
use super::common::parse_compile_and_output;

#[test]
pub fn while_tests() {
  assert_eq!(parse_compile_and_output("(while 1)"), "while 1:\n    pass\nreturn GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(while (foo) (foo1 0) (foo2 0 0))"), "while foo():\n    foo1(0)\n    foo2(0, 0)\nreturn GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(foo1 (while (bar)))"), "while bar():\n    pass\nreturn foo1(GDLisp.Nil)\n");
}

#[test]
pub fn compound_while_tests() {
  // If expressions cannot be compiled into a single GDScript
  // expression, so this forces the while loop to use the "compound"
  // form.
  assert_eq!(parse_compile_and_output("(while (if 1 2 3) (foo))"), "while true:\n    var _if_0 = GDLisp.Nil\n    if 1:\n        _if_0 = 2\n    else:\n        _if_0 = 3\n    if _if_0:\n        break\n    foo()\nreturn GDLisp.Nil\n")
}
