
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

#[test]
pub fn member_var_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init(x_0):
        self.x = x_0
        return self.x
    var x
    func get_x():
        return self.x
static func run():
    return GDLisp.Nil
"#);
}

#[test]
pub fn simple_self_closure_class_test() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node) (defn test () (lambda () self))))"),
             r#"extends Reference
class _LambdaBlock_1 extends GDLisp.Function:
    var _self_0
    func _init(_self_0):
        self._self_0 = _self_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return _self_0
    func call_funcv(args):
        if args is GDLisp.NilClass:
            return call_func()
        else:
            push_error("Too many arguments")
class Foo extends Node:
    func _init():
        return GDLisp.Nil
    func test():
        return _LambdaBlock_1.new(self)
static func run():
    return GDLisp.Nil
"#);
}

#[test]
pub fn labels_self_closure_class_test() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node) (defn test () (labels ((foo (x) (foo self))) (foo 76)))))"),
             r#"extends Reference
class _Labels_0 extends Reference:
    var _self_1
    func _init(_self_1):
        self._self_1 = _self_1
    func _fn_foo_3(x_4):
        return _fn_foo_3(_self_1)
class Foo extends Node:
    func _init():
        return GDLisp.Nil
    func test():
        var _locals_2 = _Labels_0.new(self)
        return _locals_2._fn_foo_3(76)
static func run():
    return GDLisp.Nil
"#);
}
