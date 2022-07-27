
extern crate gdlisp;

use gdlisp::ir::identifier::ClassNamespace;
use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::args::Expecting;
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn empty_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node)))"), "extends Reference\nclass ClassName extends Node:\n    func _init():\n        pass\nstatic func run():\n    return null\n");
}

#[test]
pub fn simple_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y)) (defn foo () 2)))"), "extends Reference\nclass ClassName extends Node:\n    func _init(y_0):\n        pass\n    var x\n    func foo():\n        return 2\nstatic func run():\n    return null\n");
}

#[test]
pub fn simple_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName () (defvar x) (defn _init (y)) (defn foo () 2)))"), "extends Reference\nclass ClassName extends Reference:\n    func _init(y_0):\n        pass\n    var x\n    func foo():\n        return 2\nstatic func run():\n    return null\n");
}

#[test]
pub fn parent_constructor_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y) (super y y)) (defn foo () 2)))"), "extends Reference\nclass ClassName extends Node:\n    func _init(y_0).(y_0, y_0):\n        pass\n    var x\n    func foo():\n        return 2\nstatic func run():\n    return null\n");
}

#[test]
pub fn parent_constructor_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y) (super (progn y))) (defn foo () 2)))"), "extends Reference\nclass ClassName extends Node:\n    func _init(y_0).(y_0):\n        pass\n    var x\n    func foo():\n        return 2\nstatic func run():\n    return null\n");
}

#[test]
pub fn parent_constructor_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y) (super (if y 1 2))) (defn foo () 2)))"), r#"extends Reference
class _LambdaBlock_3 extends GDLisp.Function:
    var y_0
    func _init(y_0):
        self.y_0 = y_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        var _cond_2 = null
        if y_0:
            _cond_2 = 1
        else:
            if true:
                _cond_2 = 2
            else:
                _cond_2 = null
        return _cond_2
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
class ClassName extends Node:
    func _init(y_0).(GDLisp.sys_DIV_funcall(_LambdaBlock_3.new(y_0), null)):
        pass
    var x
    func foo():
        return 2
static func run():
    return null
"#);
}

#[test]
pub fn parent_constructor_class_running_test() {
  assert_eq!(parse_and_run("((defclass A (Reference) (defvar x) (defn _init (x) (set self:x x))) (defclass B (A) (defn _init (y) (super (if y 1 2)))) (print (B:new #t):x))"), "\n1\n");
}

#[test]
pub fn super_call_in_class_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn foo () (super:foo))))"#),
             r#"extends Reference
class Foo extends Reference:
    func _init():
        pass
    func foo():
        return .foo()
static func run():
    return null
"#);
}

#[test]
pub fn super_call_in_class_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn foo () (super:bar)) (defn bar () (super:foo))))"#),
             r#"extends Reference
class Foo extends Reference:
    func _init():
        pass
    func foo():
        return .bar()
    func bar():
        return .foo()
static func run():
    return null
"#);
}

#[test]
pub fn super_call_in_class_test_3() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () main (defn foo () (super:foo)) (defn bar () (super:foo))))"#),
             r#"extends Reference
func _init():
    pass
func foo():
    return .foo()
func bar():
    return .foo()
static func run():
    return null
"#);
}

#[test]
pub fn super_call_in_class_test_4() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn _init () (super:foo))))"#),
             r#"extends Reference
class Foo extends Reference:
    func _init():
        .foo()
static func run():
    return null
"#);
}

#[test]
pub fn super_call_closed_in_class_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn foo () (lambda () (super:foo)))))"#),
             r#"extends Reference
class _LambdaBlock_2 extends GDLisp.Function:
    var _self_0
    func _init(_self_0):
        self._self_0 = _self_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return _self_0.__gdlisp_super_1()
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
class Foo extends Reference:
    func _init():
        pass
    func foo():
        return _LambdaBlock_2.new(self)
    func __gdlisp_super_1():
        return .foo()
static func run():
    return null
"#);
}

#[test]
pub fn super_call_closed_in_class_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn foo () (lambda () (super:foo) (super:bar)))))"#),
             r#"extends Reference
class _LambdaBlock_3 extends GDLisp.Function:
    var _self_0
    func _init(_self_0):
        self._self_0 = _self_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        _self_0.__gdlisp_super_1()
        return _self_0.__gdlisp_super_2()
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
class Foo extends Reference:
    func _init():
        pass
    func foo():
        return _LambdaBlock_3.new(self)
    func __gdlisp_super_1():
        return .foo()
    func __gdlisp_super_2():
        return .bar()
static func run():
    return null
"#);
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
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
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
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x 999) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
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
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) main (defvar x (export int 1 2)) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
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
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String "foo" "bar")) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"#),
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
pub fn member_var_class_test_7() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String "foo" "bar")) (defn _init (x) (set @x x)) (defn get-x () @x)))"#),
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
pub fn ready_member_var_class_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" onready)))"#),
             r#"extends Node
func _init():
    pass
onready var x = "foo"
static func run():
    return null
"#);
}

#[test]
pub fn ready_member_var_class_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String) onready)))"#),
             r#"extends Node
func _init():
    pass
export(String) onready var x = "foo"
static func run():
    return null
"#);
}

#[test]
pub fn complicated_member_var_class_test() {
  // Note: We use _cond_2 here because _cond_1 disappears during the
  // failed compilation of (if 1 2 3) via compile_simple_expr.
  assert_eq!(
    parse_compile_decl("((defclass ClassName (Node) main (defvar x (if 1 2 3)) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
    r#"extends Node
func _init(x_0):
    var _cond_2 = null
    if 1:
        _cond_2 = 2
    else:
        if true:
            _cond_2 = 3
        else:
            _cond_2 = null
    self.x = _cond_2
    self.x = x_0
var x
func get_x():
    return self.x
static func run():
    return null
"#);
}

#[test]
pub fn complicated_ready_member_var_class_test() {
  // Note: We use _cond_2 here because _cond_1 disappears during the
  // failed compilation of (if 1 2 3) via compile_simple_expr.
  assert_eq!(
    parse_compile_decl("((defclass ClassName (Node) main (defvar x (if 1 2 3) onready) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
    r#"extends Node
func _init(x_0):
    self.x = x_0
var x
func get_x():
    return self.x
func _ready():
    var _cond_2 = null
    if 1:
        _cond_2 = 2
    else:
        if true:
            _cond_2 = 3
        else:
            _cond_2 = null
    self.x = _cond_2
static func run():
    return null
"#);
}

#[test]
pub fn bad_member_var_class_test_1() {
  // Can't have export on inner class
  assert_eq!(
    parse_compile_decl_err("((defclass ClassName (Node) (defvar x (export int)) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
    Err(PError::from(GDError::new(GDErrorF::ExportOnInnerClassVar(String::from("x")), SourceOffset(28)))),
  );
}

#[test]
pub fn bad_self_static_ref_class_test() {
  // Can't reference self from static context
  assert_eq!(
    parse_compile_decl_err("((defclass ClassName (Node) (defn get-self () static self)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("self")), SourceOffset(53)))),
  );
}

#[test]
pub fn bad_member_const_class_test() {
  // Consts must be initialized
  let result = parse_compile_decl_err("((defclass ClassName (Node) (defconst x)))");
  assert_eq!(
    result,
    Err(PError::GDError(GDError::new(GDErrorF::WrongNumberArgs(String::from("defconst"), Expecting::exactly(2), 1), SourceOffset(28)))),
  );
}

#[test]
pub fn bad_static_constructor_class_test() {
  // Constructors cannot be static
  assert_eq!(
    parse_compile_decl_err("((defclass ClassName (Node) (defn _init () static)))"),
    Err(PError::from(GDError::new(GDErrorF::StaticConstructor, SourceOffset(29)))),
  );
}

#[test]
pub fn bad_super_in_instance_function_test() {
  // Can't have super in non-constructor method
  assert_eq!(
    parse_compile_decl_err("((defclass ClassName (Node) (defn foo () (super 1))))"),
    Err(PError::from(GDError::new(GDErrorF::BadSuperCall(String::from("(init)")), SourceOffset(42)))),
  );
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
class _Labels extends Reference:
    var _self_0
    func _init(_self_0):
        self._self_0 = _self_0
    func _fn_foo_1(x_2):
        return _fn_foo_1(_self_0)
class Foo extends Node:
    func _init():
        pass
    func test():
        var _locals = _Labels.new(self)
        return _locals._fn_foo_1(76)
static func run():
    return null
"#);
}

#[test]
pub fn labels_self_closure_class_with_contrived_const_test() {
  assert_eq!(parse_compile_decl("((defconst _Labels 10) (defclass Foo (Node) (defn test () (labels ((foo (x) (foo self))) (foo 76)))))"),
             r#"extends Reference
const _Labels = 10
class _Labels_0 extends Reference:
    var _self_0
    func _init(_self_0):
        self._self_0 = _self_0
    func _fn_foo_1(x_2):
        return _fn_foo_1(_self_0)
class Foo extends Node:
    func _init():
        pass
    func test():
        var _locals = _Labels_0.new(self)
        return _locals._fn_foo_1(76)
static func run():
    return null
"#);
}

#[test]
pub fn simple_self_run_class_test_1() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init (x)
         (set self:x x))
       (defn double ()
         (* self:x 2)))
     (let ((foo (Foo:new 100)))
       (print (foo:double))
       (set foo:x 101)
       (print (foo:double))))
  "#), "\n200\n202\n");
}

#[test]
pub fn simple_self_run_class_test_2() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init (x)
         (set @x x))
       (defn double ()
         (* @x 2)))
     (let ((foo (Foo:new 100)))
       (print (foo:double))
       (set foo:x 101)
       (print (foo:double))))
  "#), "\n200\n202\n");
}

#[test]
pub fn simple_self_run_class_test_3() {
  // Mixing self and @
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init (x)
         (set @x x))
       (defn double ()
         (* self:x 2)))
     (let ((foo (Foo:new 100)))
       (print (foo:double))
       (set foo:x 101)
       (print (foo:double))))
  "#), "\n200\n202\n");
}

#[test]
pub fn self_with_closure_run_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init ()
         (set self:x 1))
       (defn increment ()
         (lambda ()
           (set self:x (+ self:x 1)))))
     (let ((fn (let ((tmp (Foo:new))) (tmp:increment))))
       (print (funcall fn))
       (print (funcall fn))
       (print (funcall fn))))
  "#), "\n2\n3\n4\n");
}

#[test]
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
        __gdlisp_outer_class_2.example(3)
    var __gdlisp_outer_class_2 = load("res://TEST.gd")
static func run():
    return null
"#);
}

#[test]
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
pub fn macro_uses_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x))
     (defmacro through-foo ()
       (let ((foo (Foo:new)))
         (set foo:x 5)
         foo:x))
     (print (through-foo)))"#),
             "\n5\n");
}

#[test]
pub fn reference_static_test_1() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defclass Foo (Node2D) (defn example () (foo))))"),
             r#"extends Reference
static func foo():
    return null
class Foo extends Node2D:
    func _init():
        pass
    func example():
        return __gdlisp_outer_class_0.foo()
    var __gdlisp_outer_class_0 = load("res://TEST.gd")
static func run():
    return null
"#);
}

#[test]
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
pub fn main_class_test_4() {
  assert_eq!(parse_compile_decl("((defclass Foo () main))"),
             r#"extends Reference
func _init():
    pass
static func run():
    return null
"#);
}

#[test]
pub fn macro_uses_main_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference) main
       (defvar x))
     (defmacro through-foo ()
       (let ((foo (Foo:new)))
         (set foo:x 5)
         foo:x))
     (print (through-foo)))"#),
             "\n5\n");
}

#[test]
pub fn reference_to_const_in_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defconst CONSTANT 100))
     (print Foo:CONSTANT)
     (print (Foo:new):CONSTANT))"#),
             "\n100\n100\n");
}

#[test]
pub fn reference_to_static_in_class_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defn foo () static 98))
     (print (Foo:foo))
     (print ((Foo:new):foo)))"#),
             "\n98\n98\n");
}

#[test]
pub fn reference_to_outer_in_class_test_1() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defclass Foo (Reference)
       (defn foo () (outer)))
     (print ((Foo:new):foo)))"#);
  assert_eq!(output, "\n100\n");
}

#[test]
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
pub fn reference_to_outer_in_class_test_3() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defclass Foo (Reference) main
       (defn foo () (outer)))
     (print ((Foo:new):foo)))"#);
  assert_eq!(output, "\n100\n");
}

#[test]
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
pub fn constructor_with_parent_class_test_1() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference) (defn _init (x) (print x)))
     (defclass Bar (Foo) (defn _init (x) (super (+ x 1))))
     (Bar:new 10))"#);
  assert_eq!(output, "\n11\n");
}

#[test]
pub fn constructor_with_parent_class_test_2() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference) (defn _init (x) (print x)))
     (defclass Bar (Foo) (defn _init (x) (super (if (> x 10) (+ x 1) (- x 1)))))
     (Bar:new 20)
     (Bar:new 3))"#);
  assert_eq!(output, "\n21\n2\n");
}

#[test]
pub fn constructor_with_parent_class_test_3() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference) (defvar z) (defn _init (z) (set self:z z)))
     (defclass Bar (Foo) (defn _init () (super self)))
     (let ((bar (Bar:new)))
       (print (= bar bar:z))
       (set bar:z nil)))"#);
  assert_eq!(output, "\nTrue\n");
}

#[test]
pub fn constructor_with_parent_class_test_4() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference) (defvar z) (defn _init (z) (set self:z z)))
     (defclass Bar (Foo) (defn _init (x) (super (lambda (y) (+ x y)))))
     (let ((bar (Bar:new 64)))
       (print (funcall bar:z 1))
       (print (funcall bar:z -1))))"#);
  assert_eq!(output, "\n65\n63\n");
}

#[test]
pub fn constructor_with_parent_class_test_5() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference) (defvar z) (defn _init (z) (set self:z z)))
     (defclass Bar (Foo) (defn _init (x) (super (flet ((f (y) (+ x y))) #'f))))
     (let ((bar (Bar:new 64)))
       (print (funcall bar:z 1))
       (print (funcall bar:z -1))))"#);
  assert_eq!(output, "\n65\n63\n");
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
    return $Target/Node
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

#[test]
pub fn nonsense_modifier_class_test_1() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo (Node) main main))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("main")), SourceOffset(27)))),
  );
}

#[test]
pub fn nonsense_modifier_class_test_2() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo (Node) public public))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(29)))),
  );
}

#[test]
pub fn nonsense_modifier_class_test_3() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo (Node) public private))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(29)))),
  );
}

#[test]
pub fn nonsense_modifier_class_test_4() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo (Node) public (defn example () static static)))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("static")), SourceOffset(53)))),
  );
}

#[test]
pub fn duplicate_main_class_test() {
  assert_eq!(
    parse_compile_decl_err("((defclass Foo (Node) main) (defclass Bar (Node) main))"),
    Err(PError::from(GDError::new(GDErrorF::DuplicateMainClass, SourceOffset(29)))),
  );
}

#[test]
pub fn no_self_in_scope_test() {
  assert_eq!(
    parse_compile_and_output_err("@test"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("self")), SourceOffset(0)))),
  );
}

#[test]
pub fn duplicate_constructor_test() {
  assert_eq!(
    parse_compile_decl_err("((defclass Foo (Node) (defn _init ()) (defn _init ())))"),
    Err(PError::from(GDError::new(GDErrorF::DuplicateConstructor, SourceOffset(39)))),
  );
}

#[test]
pub fn class_setget_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node)
                                      (defvar x)))"#),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    var x
static func run():
    return null
"#);
}

#[test]
pub fn class_setget_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node)
                                      (defn (get x) () 10)))"#),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    func __gdlisp_get_x():
        return 10
    var x setget ,__gdlisp_get_x
static func run():
    return null
"#);
}

#[test]
pub fn class_setget_test_3() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node)
                                      (defn (set x) (a))))"#),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    func __gdlisp_set_x(a_0):
        return null
    var x setget __gdlisp_set_x
static func run():
    return null
"#);
}

#[test]
pub fn class_setget_test_4() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node)
                                      (defn (set x) (a))
                                      (defn (get x) () 10)))"#),
             r#"extends Reference
class ClassName extends Node:
    func _init():
        pass
    func __gdlisp_set_x(a_0):
        return null
    func __gdlisp_get_x():
        return 10
    var x setget __gdlisp_set_x, __gdlisp_get_x
static func run():
    return null
"#);
}

#[test]
pub fn class_setget_test_5() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main
                                      (defn (set x) (a))
                                      (defn (get x) () 10)))"#),
             r#"extends Node
func _init():
    pass
func __gdlisp_set_x(a_0):
    return null
func __gdlisp_get_x():
    return 10
var x setget __gdlisp_set_x, __gdlisp_get_x
static func run():
    return null
"#);
}

#[test]
pub fn class_setget_conflict_test_1() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defn (set x) (a))
                                         (defn (set x) (b))))"#),
             Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("__gdlisp_set_x")), SourceOffset(130)))));
}

#[test]
pub fn class_setget_conflict_test_2() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defn (get x) ())
                                         (defn (get x) ())))"#),
             Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("__gdlisp_get_x")), SourceOffset(129)))));
}

#[test]
pub fn class_setget_conflict_test_3() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defvar x)
                                         (defn (get x) ())))"#),
             Err(PError::from(GDError::new(GDErrorF::FieldAccessorConflict(String::from("x")), SourceOffset(70)))));
}

#[test]
pub fn class_setget_conflict_test_4() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defvar x)
                                         (defn (set x) (a))))"#),
             Err(PError::from(GDError::new(GDErrorF::FieldAccessorConflict(String::from("x")), SourceOffset(70)))));
}

#[test]
pub fn class_setget_bad_signature_1() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defn (get x) (a))))"#),
             Err(PError::from(GDError::new(GDErrorF::BadGetterArguments(String::from("x")), SourceOffset(70)))));
}

#[test]
pub fn class_setget_bad_signature_2() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defn (get x) () static)))"#),
             Err(PError::from(GDError::new(GDErrorF::BadGetterArguments(String::from("x")), SourceOffset(70)))));
}

#[test]
pub fn class_setget_bad_signature_3() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defn (set x) (a) static)))"#),
             Err(PError::from(GDError::new(GDErrorF::BadSetterArguments(String::from("x")), SourceOffset(70)))));
}

#[test]
pub fn class_setget_bad_signature_4() {
  assert_eq!(parse_compile_decl_err(r#"((defclass ClassName (Node)
                                         (defn (set x) ())))"#),
             Err(PError::from(GDError::new(GDErrorF::BadSetterArguments(String::from("x")), SourceOffset(70)))));
}

#[test]
pub fn class_setget_runner_test_1() {
  assert_eq!(parse_and_run(r#"((defclass ClassName (Reference)
                                 (defvar private-field)
                                 (defn _init ()
                                   (set @private-field 1))
                                 (defn (set x) (a) (set @private-field a))
                                 (defn (get x) () @private-field))
                               (let ((foo (ClassName:new)))
                                 (set foo:x 92)
                                 (print foo:x)))"#),
             "\n92\n");
}

#[test]
pub fn class_setget_runner_test_2() {
  assert_eq!(parse_and_run(r#"((defclass ClassName (Reference) main
                                 (defvar private-field)
                                 (defn _init ()
                                   (set @private-field 1))
                                 (defn (set x) (a) (set @private-field a))
                                 (defn (get x) () @private-field))
                               (let ((foo (ClassName:new)))
                                 (set foo:x 92)
                                 (print foo:x)))"#),
             "\n92\n");
}

#[test]
pub fn super_call_runner_test_1() {
  assert_eq!(parse_and_run(r#"((defclass Foo (Reference)
                                 (defn foo ()
                                   99))
                               (defclass Bar (Foo)
                                 (defn foo ()
                                   101)
                                 (defn call-foo ()
                                   (super:foo)))
                               (let ((bar (Bar:new)))
                                 (print (bar:foo))
                                 (print (bar:call-foo))))"#),
             "\n101\n99\n");
}

#[test]
pub fn super_call_runner_test_2() {
  assert_eq!(parse_and_run(r#"((defclass Foo (Reference)
                                 (defn foo ()
                                   99))
                               (defclass Bar (Foo)
                                 (defn foo ()
                                   101)
                                 (defn call-foo ()
                                   (lambda () (super:foo))))
                               (let* ((bar (Bar:new))
                                      (delegator (bar:call-foo)))
                                 (print (bar:foo))
                                 (print (funcall delegator))))"#),
             "\n101\n99\n");
}

#[test]
pub fn super_call_runner_test_3() {
  assert_eq!(parse_and_run(r#"((defclass Foo (Reference)
                                 (defvar x)
                                 (defn _init (x)
                                   (set @x x))
                                 (defn foo ()
                                   99))
                               (defclass Bar (Foo)
                                 (defn _init ()
                                   (super (lambda () (super:foo))))
                                 (defn foo ()
                                   101))
                               (let* ((bar (Bar:new)))
                                 (print (bar:foo))
                                 (print (funcall bar:x))))"#),
             "\n101\n99\n");
}

#[test]
pub fn bad_super_call_test() {
  assert_eq!(
    parse_compile_decl_err("((defn foo () (super:foo)))"),
    Err(PError::from(GDError::new(GDErrorF::BadSuperCall(String::from("foo")), SourceOffset(14)))),
  );
}
