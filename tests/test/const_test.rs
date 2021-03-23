
use super::common::{parse_compile_decl, parse_and_run};

#[test]
pub fn const_test() {
  assert_eq!(parse_compile_decl("((defconst A 10))"), "extends Reference\nconst A = 10\nstatic func run():\n    return null\n");
  assert_eq!(parse_compile_decl("((defconst A \"foo\"))"), "extends Reference\nconst A = \"foo\"\nstatic func run():\n    return null\n");
}

#[test]
#[should_panic]
pub fn const_test_nonconst_1() {
  parse_compile_decl("((defconst B ()))");
}

#[test]
#[should_panic]
pub fn const_test_nonconst_2() {
  parse_compile_decl("((defconst B (list->array 1)))");
}

#[test]
#[ignore]
pub fn const_test_run() {
  let output = parse_and_run(r#"
  ((defconst A 100) (print A))
  "#);
  assert_eq!(output, "\n100\n");
}

#[test]
#[ignore]
pub fn builtin_const_test() {
  // I don't care what this outputs; I just want to know that Godot
  // recognizes all of the constants I'm compiling these to.
  parse_and_run(r#"([Int Null TYPE_MAX PI NodePath PoolStringArray
                     Mouse:LEFT Margin:BOTTOM Corner:TOP_RIGHT
                     Orientation:VERTICAL HAlign:LEFT VAlign:TOP])"#);
}
