
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn const_test() {
  assert_eq!(parse_compile_decl("((defconst A 10))"), "extends Reference\nconst A = 10\nstatic func run():\n    return null\n");
  assert_eq!(parse_compile_decl("((defconst A \"foo\"))"), "extends Reference\nconst A = \"foo\"\nstatic func run():\n    return null\n");
}

#[test]
pub fn const_test_nonconst() {
  assert_eq!(
    parse_compile_decl_err("((defconst B (list->array 1)))"),
    Err(PError::from(GDError::new(GDErrorF::NotConstantEnough(String::from("B")), SourceOffset(13)))),
  );
}

#[test]
pub fn const_test_nonconst_in_class() {
  assert_eq!(
    parse_compile_decl_err("((defclass Foo (Reference) (defconst B (list->array 1))))"),
    Err(PError::from(GDError::new(GDErrorF::NotConstantEnough(String::from("B")), SourceOffset(39)))),
  );
}

#[test]
pub fn const_test_nonconst_in_enum() {
  assert_eq!(
    parse_compile_decl_err("((defenum Foo (A (list->array 1))))"),
    Err(PError::from(GDError::new(GDErrorF::NotConstantEnough(String::from("A")), SourceOffset(17)))),
  );
}

#[test]
pub fn const_test_run() {
  let output = parse_and_run(r#"
  ((defconst A 100) (print A))
  "#);
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn builtin_const_test() {
  // I don't care what this outputs; I just want to know that Godot
  // recognizes all of the constants I'm compiling these to.
  parse_and_run(r#"([Int Null TYPE_MAX PI NodePath PoolStringArray
                     Mouse:LEFT Margin:BOTTOM Corner:TOP_RIGHT
                     Orientation:VERTICAL HAlign:LEFT VAlign:TOP
                     Key:A Key:THORN Key:EXCLAM Key:KP_8 Key:KP-9
                     KeyMask:CTRL Joy:BUTTON-13 Joy:R3 Joy:ANALOG-R2
                     MIDIMessage:NOTE_ON PI SPKEY TYPE_ARRAY])"#);
}
