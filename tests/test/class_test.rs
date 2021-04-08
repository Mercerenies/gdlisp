
extern crate gdlisp;

use super::common::{parse_compile_decl, parse_and_run};

#[test]
pub fn empty_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node)))"), "extends Reference\nclass ClassName extends Node:\n    func _init():\n        pass\nstatic func run():\n    return null\n");
}

#[test]
pub fn simple_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y)) (defn foo () 2)))"), "extends Reference\nclass ClassName extends Node:\n    func _init(y_0):\n        pass\n    var x\n    func foo():\n        return 2\nstatic func run():\n    return null\n");
}

#[test]
pub fn member_var_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn get-x () self:x)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    var x
    func get_x():
        return self.x
static func run():
    return null
"#);
}

#[test]
pub fn member_var_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init(x_0):
        self.x = x_0
    var x
    func get_x():
        return self.x
static func run():
    return null
"#);
}

#[test]
pub fn member_var_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x 999) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init(x_0):
        self.x = x_0
    var x = 999
    func get_x():
        return self.x
static func run():
    return null
"#);
}

#[test]
pub fn member_var_class_test_4() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) main (defvar x (export int 1 2)) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))"),
             r#"extends Node
func _init(x_0):
    self.x = x_0
export(int, 1, 2) var x
func get_x():
    return self.x
static func run():
    return null
"#);
}

#[test]
pub fn member_var_class_test_5() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String "foo" "bar")) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))"#),
             r#"extends Node
func _init(x_0):
    self.x = x_0
export(String, "foo", "bar") var x = "foo"
func get_x():
    return self.x
static func run():
    return null
"#);
}

#[test]
pub fn member_var_class_test_6() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvars x y z)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    var x
    var y
    var z
static func run():
    return null
"#);
}

#[test]
#[should_panic]
pub fn bad_member_var_class_test_1() {
  parse_compile_decl("((defclass ClassName (Node) (defvar x (if 1 2 3)) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))");
}

#[test]
#[should_panic]
pub fn bad_member_var_class_test_2() {
  // Can't have export on inner class
  parse_compile_decl("((defclass ClassName (Node) (defvar x (export int)) (defn _init (x) (setq self:x x)) (defn get-x () self:x)))");
}

#[test]
#[should_panic]
pub fn bad_self_static_ref_class_test() {
  // Can't reference self from static context
  parse_compile_decl("((defclass ClassName (Node) (defn get-self () static self)))");
}

#[test]
#[should_panic]
pub fn bad_member_const_class_test() {
  // Consts must be initialized
  parse_compile_decl("((defclass ClassName (Node) (defconst x)))");
}

#[test]
pub fn signal_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defsignal my-signal)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    signal my_signal
static func run():
    return null
"#);
}

#[test]
pub fn signal_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defsignal my-signal ())))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    signal my_signal
static func run():
    return null
"#);
}

#[test]
pub fn signal_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defsignal my-signal (foo bar))))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    signal my_signal(foo, bar)
static func run():
    return null
"#);
}

#[test]
pub fn const_in_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defconst x 1)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    const x = 1
static func run():
    return null
"#);
}

#[test]
pub fn const_in_main_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) main (defconst x 1)))"),
             r#"extends Node
func _init():
    pass
const x = 1
static func run():
    return null
"#);
}

#[test]
pub fn static_in_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defn foo () static 1)))"),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    static func foo():
        return 1
static func run():
    return null
"#);
}

#[test]
pub fn static_in_main_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) main (defn foo () static 1)))"),
             r#"extends Node
func _init():
    pass
static func foo():
    return 1
static func run():
    return null
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
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
class Foo extends Node:
    func _init():
        pass
    func test():
        return _LambdaBlock_1.new(self)
static func run():
    return null
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
        pass
    func test():
        var _locals_2 = _Labels_0.new(self)
        return _locals_2._fn_foo_3(76)
static func run():
    return null
"#);
}

#[test]
#[ignore]
pub fn simple_self_run_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init (x)
         (setq self:x x))
       (defn double ()
         (* self:x 2)))
     (let ((foo (Foo:new 100)))
       (print (foo:double))
       (setq foo:x 101)
       (print (foo:double))))
  "#), "\n200\n202\n");
}

#[test]
#[ignore]
pub fn self_with_closure_run_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init ()
         (setq self:x 1))
       (defn increment ()
         (lambda ()
           (setq self:x (+ self:x 1)))))
     (let ((fn (let ((tmp (Foo:new))) (tmp:increment))))
       (print (funcall fn))
       (print (funcall fn))
       (print (funcall fn))))
  "#), "\n2\n3\n4\n");
}

#[test]
#[ignore]
pub fn macro_in_class_test_1() {
  assert_eq!(parse_compile_decl(r#"
    ((defmacro add-one (x)
       (+ x 1))
     (defn example (x) x)
     (defclass Foo (Reference)
       (defn _init ()
         (example (add-one 2)))))"#),
             r#"extends Reference
static func add_one(x_0):
    return x_0 + 1
static func example(x_1):
    return x_1
class Foo extends Reference:
    func _init():
        example(3)
static func run():
    return null
"#);
}

#[test]
#[ignore]
pub fn macro_in_class_test_2() {
  assert_eq!(parse_and_run(r#"
    ((defmacro declare-function (name)
       `(defn ,name () 99))
     (defclass Foo (Reference)
       (declare-function fn1)
       (declare-function fn2))
     (let ((foo (Foo:new)))
       (print (foo:fn1))
       (print (foo:fn2))))"#),
             "\n99\n99\n");
}

#[test]
#[ignore]
pub fn macro_in_class_test_3() {
  assert_eq!(parse_and_run(r#"
    ((defmacro declare-functions ()
       '(progn (defn a () 1) (defn b () 2)))
     (defclass Foo (Reference)
       (declare-functions))
     (let ((foo (Foo:new)))
       (print (foo:a))
       (print (foo:b))))"#),
             "\n1\n2\n");
}

#[test]
#[ignore]
pub fn macro_uses_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x))
     (defmacro through-foo ()
       (let ((foo (Foo:new)))
         (setq foo:x 5)
         foo:x))
     (print (through-foo)))"#),
             "\n5\n");
}

#[test]
#[ignore]
pub fn reference_static_test_1() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defclass Foo (Node2D) (defn example () (foo))))"),
             r#"extends Reference
static func foo():
    return null
class Foo extends Node2D:
    func _init():
        pass
    func example():
        return load("res://TEST.gd").foo()
static func run():
    return null
"#);
}

#[test]
#[ignore]
pub fn reference_static_test_2() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defclass Foo (Node2D) main (defn example () (foo))))"),
             r#"extends Node2D
static func foo():
    return null
func _init():
    pass
func example():
    return foo()
static func run():
    return null
"#);
}

#[test]
#[ignore]
pub fn reference_static_test_3() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defclass Foo (Node2D) (defn example () static (foo))))"),
             r#"extends Reference
static func foo():
    return null
class Foo extends Node2D:
    func _init():
        pass
    static func example():
        return load("res://TEST.gd").foo()
static func run():
    return null
"#);
}

#[test]
#[ignore]
pub fn reference_static_test_4() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defclass Foo (Node2D) main (defn example () static (foo))))"),
             r#"extends Node2D
static func foo():
    return null
func _init():
    pass
static func example():
    return foo()
static func run():
    return null
"#);
}

#[test]
pub fn main_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node2D) main))"),
             r#"extends Node2D
func _init():
    pass
static func run():
    return null
"#);
}

#[test]
pub fn main_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node2D) main (defn foo () 1)))"),
             r#"extends Node2D
func _init():
    pass
func foo():
    return 1
static func run():
    return null
"#);
}

#[test]
pub fn main_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node2D) main (defn foo () 1)) Foo)"),
             r#"extends Node2D
func _init():
    pass
func foo():
    return 1
static func run():
    return load("res://TEST.gd")
"#);
}

#[test]
#[ignore]
pub fn macro_uses_main_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference) main
       (defvar x))
     (defmacro through-foo ()
       (let ((foo (Foo:new)))
         (setq foo:x 5)
         foo:x))
     (print (through-foo)))"#),
             "\n5\n");
}

#[test]
#[ignore]
pub fn reference_to_const_in_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defconst CONSTANT 100))
     (print Foo:CONSTANT)
     (print (Foo:new):CONSTANT))"#),
             "\n100\n100\n");
}

#[test]
#[ignore]
pub fn reference_to_static_in_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defn foo () static 98))
     (print (Foo:foo))
     (print ((Foo:new):foo)))"#),
             "\n98\n98\n");
}

#[test]
#[ignore]
pub fn reference_to_outer_in_class_test_1() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defclass Foo (Reference)
       (defn foo () (outer)))
     (print ((Foo:new):foo)))"#);
  assert_eq!(output, "\n100\n");
}

#[test]
#[ignore]
pub fn reference_to_outer_in_class_test_2() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defclass Foo (Reference)
       (defn foo () static (outer)))
     (print ((Foo:new):foo))
     (print (Foo:foo)))"#);
  assert_eq!(output, "\n100\n100\n");
}

#[test]
#[ignore]
pub fn reference_to_outer_in_class_test_3() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defclass Foo (Reference) main
       (defn foo () (outer)))
     (print ((Foo:new):foo)))"#);
  assert_eq!(output, "\n100\n");
}

#[test]
#[ignore]
pub fn reference_to_outer_in_class_test_4() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defclass Foo (Reference) main
       (defn foo () static (outer)))
     (print ((Foo:new):foo))
     (print (Foo:foo)))"#);
  assert_eq!(output, "\n100\n100\n");
}

#[test]
pub fn get_node_on_self_class_test() {
  assert_eq!(parse_compile_decl(r#"
    ((defclass Foo (Spatial) main
       (defn test () $Target/Node)))
    "#), r#"extends Spatial
func _init():
    pass
func test():
    return self.get_node("Target/Node")
static func run():
    return null
"#);
}

#[test]
pub fn get_node_on_explicit_target_class_test() {
  assert_eq!(parse_compile_decl(r#"
    ((defclass Foo (Spatial) main
       (defn test (x) x:$Target/Node)))
    "#), r#"extends Spatial
func _init():
    pass
func test(x_0):
    return x_0.get_node("Target/Node")
static func run():
    return null
"#);
}
