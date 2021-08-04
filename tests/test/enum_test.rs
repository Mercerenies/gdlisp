
extern crate gdlisp;

use gdlisp::compile::error::{Error as GDError};
use gdlisp::pipeline::error::{Error as PError};

use super::common::*;

#[test]
pub fn empty_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum))"), "extends Reference\nenum MyEnum {\n}\nstatic func run():\n    return null\n");
}

#[test]
pub fn unvalued_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum A B C))"), "extends Reference\nenum MyEnum {\n    A,\n    B,\n    C,\n}\nstatic func run():\n    return null\n");
}

#[test]
pub fn valued_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum (A 1) (B 2) (C 3)))"), "extends Reference\nenum MyEnum {\n    A = 1,\n    B = 2,\n    C = 3,\n}\nstatic func run():\n    return null\n");
}

#[test]
pub fn mixed_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum A (B 2) (C 3)))"), "extends Reference\nenum MyEnum {\n    A,\n    B = 2,\n    C = 3,\n}\nstatic func run():\n    return null\n");
}

#[test]
#[ignore]
pub fn enum_runner_test() {
  let result = parse_and_run("((defenum MyEnum (A 1) (B 2) (C 3)) (print MyEnum:A) (print MyEnum:B) (print MyEnum:C))");
  assert_eq!(result, "\n1\n2\n3\n");
}

#[test]
pub fn invalid_enum_test() {
  assert_eq!(
    parse_compile_decl_err("((defenum MyEnum (A 1) (B 2) (C 3)) MyEnum:D)"),
    Err(PError::from(GDError::NoSuchEnumValue(String::from("MyEnum"), String::from("D")))),
  );
}

#[test]
pub fn builtin_enum_test() {
  assert_eq!(parse_compile_decl("(Mouse:LEFT)"), "extends Reference\nstatic func run():\n    return GDLisp.Mouse.LEFT\n");
}
