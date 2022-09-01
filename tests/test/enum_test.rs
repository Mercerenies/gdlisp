
extern crate gdlisp;

use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn empty_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum))"), r#"extends Reference


enum MyEnum {
}
"#);
}

#[test]
pub fn unvalued_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum A B C))"), r#"extends Reference


enum MyEnum {
    A,
    B,
    C,
}
"#);
}

#[test]
pub fn valued_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum (A 1) (B 2) (C 3)))"), r#"extends Reference


enum MyEnum {
    A = 1,
    B = 2,
    C = 3,
}
"#);
}

#[test]
pub fn mixed_enum_test() {
  assert_eq!(parse_compile_decl("((defenum MyEnum A (B 2) (C 3)))"), r#"extends Reference


enum MyEnum {
    A,
    B = 2,
    C = 3,
}
"#);
}

#[test]
pub fn enum_runner_test() {
  let result = parse_and_run("((defenum MyEnum (A 1) (B 2) (C 3)) (print MyEnum:A) (print MyEnum:B) (print MyEnum:C))");
  assert_eq!(result, "\n1\n2\n3\n");
}

#[test]
pub fn invalid_enum_test() {
  assert_eq!(
    parse_compile_decl_err("((defenum MyEnum (A 1) (B 2) (C 3)) (defn foo () MyEnum:D))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchEnumValue(String::from("MyEnum"), String::from("D")), SourceOffset(49)))),
  );
}

#[test]
pub fn builtin_enum_test() {
  assert_eq!(parse_compile_decl("((defn foo () Mouse:LEFT))"), r#"extends Reference


static func foo():
    return GDLisp.Mouse.LEFT
"#);
}
