
extern crate gdlisp;

use super::common::parse_compile_decl;

#[test]
pub fn empty_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node)))"), "extends Reference\nclass ClassName extends Node:\n    func _init():\n        return GDLisp.Nil\nstatic func run():\n    return GDLisp.Nil\n");
}

#[test]
pub fn simple_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y)) (defn foo () 2)))"), "extends Reference\nclass ClassName extends Node:\n    func _init(y_0):\n        return GDLisp.Nil\n    var x\n    func foo():\n        return 2\nstatic func run():\n    return GDLisp.Nil\n");
}

#[test]
pub fn member_var_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn get-x () self:x)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        return GDLisp.Nil
    var x
    func get_x():
        return self.x
static func run():
    return GDLisp.Nil
"#);
}
