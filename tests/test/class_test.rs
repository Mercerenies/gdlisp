
extern crate gdlisp;

use gdlisp::ir::identifier::ClassNamespace;
use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::args::Expecting;
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn empty_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node)))"), r#"extends Reference


class ClassName extends Node:

    func _init():
        pass
"#);
}

#[test]
pub fn simple_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y)) (defn foo () 2)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init(y):
        pass

    var x

    func foo():
        return 2
"#);
}

#[test]
pub fn simple_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName () (defvar x) (defn _init (y)) (defn foo () 2)))"),
             r#"extends Reference


class ClassName extends Reference:

    func _init(y):
        pass

    var x

    func foo():
        return 2
"#);
}

#[test]
pub fn parent_constructor_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y) (super y y)) (defn foo () 2)))"), r#"extends Reference


class ClassName extends Node:

    func _init(y).(y, y):
        pass

    var x

    func foo():
        return 2
"#);
}

#[test]
pub fn parent_constructor_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y) (super (progn y))) (defn foo () 2)))"), r#"extends Reference


class ClassName extends Node:

    func _init(y).(y):
        pass

    var x

    func foo():
        return 2
"#);
}

#[test]
pub fn parent_constructor_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (y) (super (if y 1 2))) (defn foo () 2)))"), r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

    var y

    func _init(y):
        self.y = y
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        var _cond = null
        if y:
            _cond = 1
        else:
            if true:
                _cond = 2
            else:
                _cond = null
        return _cond

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")


class ClassName extends Node:

    func _init(y).(GDLisp.sys_DIV_funcall(_LambdaBlock.new(y), null)):
        pass

    var x

    func foo():
        return 2
"#);
}

#[test]
pub fn parent_constructor_class_test_4() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (@x y) (super (if y 1 2))) (defn foo () 2)))"), r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

    var y

    func _init(y):
        self.y = y
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        var _cond = null
        if y:
            _cond = 1
        else:
            if true:
                _cond = 2
            else:
                _cond = null
        return _cond

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")


class ClassName extends Node:

    func _init(x_0, y).(GDLisp.sys_DIV_funcall(_LambdaBlock.new(y), null)):
        self.x = x_0

    var x

    func foo():
        return 2
"#);
}

#[test]
pub fn parent_constructor_class_test_5() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x-x) (defn _init (@x-x y) (super (if y 1 2))) (defn foo () 2)))"), r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

    var y

    func _init(y):
        self.y = y
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        var _cond = null
        if y:
            _cond = 1
        else:
            if true:
                _cond = 2
            else:
                _cond = null
        return _cond

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")


class ClassName extends Node:

    func _init(x_x_0, y).(GDLisp.sys_DIV_funcall(_LambdaBlock.new(y), null)):
        self.x_x = x_x_0

    var x_x

    func foo():
        return 2
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
"#);
}

#[test]
pub fn super_call_in_class_test_4() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn _init () (super:foo))))"#),
             r#"extends Reference


class Foo extends Reference:

    func _init():
        .foo()
"#);
}

#[test]
pub fn super_call_closed_in_class_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn foo () (lambda () (super:foo)))))"#),
             r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

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
        return _LambdaBlock.new(self)

    func __gdlisp_super_1():
        return .foo()
"#);
}

#[test]
pub fn super_call_closed_in_class_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass Foo () (defn foo () (lambda () (super:foo) (super:bar)))))"#),
             r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

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
        return _LambdaBlock.new(self)

    func __gdlisp_super_1():
        return .foo()

    func __gdlisp_super_2():
        return .bar()
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
"#);
}

#[test]
pub fn member_var_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init(x):
        self.x = x

    var x

    func get_x():
        return self.x
"#);
}

#[test]
pub fn member_var_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvar x 999) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init(x):
        self.x = x

    var x = 999

    func get_x():
        return self.x
"#);
}

#[test]
pub fn member_var_class_test_4() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) main (defvar x (export int 1 2)) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
             r#"extends Node


func _init(x):
    self.x = x


export(int, 1, 2) var x


func get_x():
    return self.x
"#);
}

#[test]
pub fn member_var_class_test_5() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String "foo" "bar")) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"#),
             r#"extends Node


func _init(x):
    self.x = x


export(String, "foo", "bar") var x = "foo"


func get_x():
    return self.x
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
"#);
}

#[test]
pub fn member_var_class_test_7() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String "foo" "bar")) (defn _init (x) (set @x x)) (defn get-x () @x)))"#),
             r#"extends Node


func _init(x):
    self.x = x


export(String, "foo", "bar") var x = "foo"


func get_x():
    return self.x
"#);
}

#[test]
pub fn init_member_var_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvars x y z) (defn _init (@x))))"),
             r#"extends Reference


class ClassName extends Node:

    func _init(x_0):
        self.x = x_0

    var x
    var y
    var z
"#);
}

#[test]
pub fn init_member_var_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvars x y) (defvar z 10) (defn _init (@x @y))))"),
             r#"extends Reference


class ClassName extends Node:

    func _init(x_0, y_1):
        self.x = x_0
        self.y = y_1

    var x
    var y
    var z = 10
"#);
}

#[test]
pub fn init_member_var_class_test_3() {
  // sys/split is just to force the variable initialization to go into
  // the constructor rather than be inline on the 'var' line.
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defvars x y) (defvar z (sys/split 3)) (defn _init (@x @y))))"),
             r#"extends Reference


class ClassName extends Node:

    func _init(x_0, y_1):
        var _split = 3
        self.z = _split
        self.x = x_0
        self.y = y_1

    var x
    var y
    var z
"#);
}

#[test]
pub fn ready_member_var_class_test_1() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" onready)))"#),
             r#"extends Node


func _init():
    pass


onready var x = "foo"
"#);
}

#[test]
pub fn ready_member_var_class_test_2() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main (defvar x "foo" (export String) onready)))"#),
             r#"extends Node


func _init():
    pass


export(String) onready var x = "foo"
"#);
}

#[test]
pub fn complicated_member_var_class_test() {
  assert_eq!(
    parse_compile_decl("((defclass ClassName (Node) main (defvar x (if 1 2 3)) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
    r#"extends Node


func _init(x):
    var _cond = null
    if 1:
        _cond = 2
    else:
        if true:
            _cond = 3
        else:
            _cond = null
    self.x = _cond
    self.x = x


var x


func get_x():
    return self.x
"#);
}

#[test]
pub fn complicated_ready_member_var_class_test() {
  assert_eq!(
    parse_compile_decl("((defclass ClassName (Node) main (defvar x (if 1 2 3) onready) (defn _init (x) (set self:x x)) (defn get-x () self:x)))"),
    r#"extends Node


func _init(x):
    self.x = x


var x


func get_x():
    return self.x


func _ready():
    var _cond = null
    if 1:
        _cond = 2
    else:
        if true:
            _cond = 3
        else:
            _cond = null
    self.x = _cond
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
pub fn bad_nullargs_constructor_class_test() {
  // Constructors cannot be sys/nullargs
  assert_eq!(
    parse_compile_decl_err("((defclass ClassName (Node) (defn _init () sys/nullargs)))"),
    Err(PError::from(GDError::new(GDErrorF::NullargsConstructor, SourceOffset(29)))),
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
"#);
}

#[test]
pub fn signal_class_test_4() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defsignal my-signal (foo bar)) (defsignal my-other-signal)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init():
        pass

    signal my_signal(foo, bar)
    signal my_other_signal
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
"#);
}

#[test]
pub fn const_in_main_class_test() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) main (defconst x 1)))"),
             r#"extends Node


func _init():
    pass


const x = 1
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
"#);
}

#[test]
pub fn nullargs_in_class_test_1() {
  // No effect since there are no arguments.
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defn foo () sys/nullargs 1)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init():
        pass

    func foo():
        return 1
"#);
}

#[test]
pub fn nullargs_in_class_test_2() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defn foo (x y z) sys/nullargs 1)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init():
        pass

    func foo(x = null, y = null, z = null):
        return 1
"#);
}

#[test]
pub fn nullargs_in_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defn foo (x y z) sys/nullargs static 1)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init():
        pass

    static func foo(x = null, y = null, z = null):
        return 1
"#);
}

#[test]
pub fn nullargs_in_class_test_4() {
  assert_eq!(parse_compile_decl("((defclass ClassName (Node) (defn foo (x y z) static sys/nullargs 1)))"),
             r#"extends Reference


class ClassName extends Node:

    func _init():
        pass

    static func foo(x = null, y = null, z = null):
        return 1
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
"#);
}

#[test]
pub fn simple_self_closure_class_test() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node) (defn test () (lambda () self))))"),
             r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

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
        return _LambdaBlock.new(self)
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

    func _fn_foo_1(x):
        return _fn_foo_1(_self_0)


class Foo extends Node:

    func _init():
        pass

    func test():
        var _locals = _Labels.new(self)
        return _locals._fn_foo_1(76)
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

    func _fn_foo_1(x):
        return _fn_foo_1(_self_0)


class Foo extends Node:

    func _init():
        pass

    func test():
        var _locals = _Labels_0.new(self)
        return _locals._fn_foo_1(76)
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


static func add_one(x):
    return x + 1


static func example(x):
    return x


class Foo extends Reference:

    func _init():
        __gdlisp_outer_class_0.example(3)

    var __gdlisp_outer_class_0 = load("res://TEST.gd")
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
"#);
}

#[test]
pub fn main_class_test_1() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node2D) main))"),
             r#"extends Node2D


func _init():
    pass
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
"#);
}

#[test]
pub fn main_class_test_3() {
  assert_eq!(parse_compile_decl("((defclass Foo (Node2D) main (defn foo () 1)) (defn run () Foo))"),
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


func test(x):
    return x.get_node("Target/Node")
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

    func __gdlisp_set_x(_unused):
        push_error("Cannot assign to nonexistent field \'x\'")

    var x setget __gdlisp_set_x, __gdlisp_get_x
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

    func __gdlisp_set_x(a):
        pass

    func __gdlisp_get_x():
        push_error("Cannot access nonexistent field \'x\'")

    var x setget __gdlisp_set_x, __gdlisp_get_x
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

    func __gdlisp_set_x(a):
        pass

    func __gdlisp_get_x():
        return 10

    var x setget __gdlisp_set_x, __gdlisp_get_x
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


func __gdlisp_set_x(a):
    pass


func __gdlisp_get_x():
    return 10


var x setget __gdlisp_set_x, __gdlisp_get_x
"#);
}

#[test]
pub fn class_setget_test_6() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main
                                      (defn (set x-y) (a))
                                      (defn (get x-y) () 10)))"#),
             r#"extends Node


func _init():
    pass


func __gdlisp_set_x_y(a):
    pass


func __gdlisp_get_x_y():
    return 10


var x_y setget __gdlisp_set_x_y, __gdlisp_get_x_y
"#);
}

#[test]
pub fn class_setget_test_7() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main
                                      (defn (set x-y) (a))))"#),
             r#"extends Node


func _init():
    pass


func __gdlisp_set_x_y(a):
    pass


func __gdlisp_get_x_y():
    push_error("Cannot access nonexistent field \'x_y\'")


var x_y setget __gdlisp_set_x_y, __gdlisp_get_x_y
"#);
}

#[test]
pub fn class_setget_test_8() {
  assert_eq!(parse_compile_decl(r#"((defclass ClassName (Node) main
                                      (defn (get x-y) () 10)))"#),
             r#"extends Node


func _init():
    pass


func __gdlisp_get_x_y():
    return 10


func __gdlisp_set_x_y(_unused):
    push_error("Cannot assign to nonexistent field \'x_y\'")


var x_y setget __gdlisp_set_x_y, __gdlisp_get_x_y
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

#[test]
pub fn builtin_patched_class_test() {
  // Make sure the patched classes (`PATCHED_CLASS_NAMES` in
  // `class_loader.rs`) are being loaded correctly.
  assert_eq!(parse_and_run("((print (instance? (File:new) File))
                             (print (= (typeof (File:new)) File)))"),
             "\nTrue\nTrue\n");
}

#[test]
pub fn builtin_singleton_class_test() {
  assert_eq!(parse_and_run("((print (typeof Engine):name) (print _Engine:name) (print (Engine:get_class)))"),
             "\n_Engine\n_Engine\n_Engine\n");
}
