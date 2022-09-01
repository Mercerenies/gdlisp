
use super::common::*;

#[test]
pub fn basic_string_lit_tests() {
  assert_eq!(parse_compile_and_output(r#""a""#), "return \"a\"\n");
  assert_eq!(parse_compile_and_output(r#""ab\"cd""#), "return \"ab\\\"cd\"\n");
  assert_eq!(parse_compile_and_output(r#""α""#), "return \"α\"\n");
  assert_eq!(parse_compile_and_output("\"α\nβ\""), "return \"α\\nβ\"\n");
}

#[test]
pub fn basic_node_path_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo (Reference) main (defn foo () $a)))"#),
             r#"extends Reference


func _init():
    pass


func foo():
    return $a
"#);
}

#[test]
pub fn basic_node_path_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo (Reference) main (defn foo () $"a")))"#),
             r#"extends Reference


func _init():
    pass


func foo():
    return $a
"#);
}

#[test]
pub fn basic_node_path_test_3() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo (Reference) main (defn foo () $b-c)))"#),
             r#"extends Reference


func _init():
    pass


func foo():
    return $"b-c"
"#);
}

#[test]
pub fn basic_node_path_test_4() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo (Reference) main (defn foo () $"b c")))"#),
             r#"extends Reference


func _init():
    pass


func foo():
    return $"b c"
"#);
}

#[test]
pub fn basic_node_path_test_5() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo (Reference) main (defn foo () $"5\nc")))"#),
             r#"extends Reference


func _init():
    pass


func foo():
    return $"5\nc"
"#);
}

#[test]
pub fn basic_node_path_test_6() {
  // Note: \n is a *literal* newline in the code here, which will be
  // converted to a "backslash-n" sequence in GDScript.
  assert_eq!(parse_compile_decl("((defclass Foo (Reference) main (defn foo () $\"5\nc\")))"),
             r#"extends Reference


func _init():
    pass


func foo():
    return $"5\nc"
"#);
}
