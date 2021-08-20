
extern crate gdlisp;

use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

// TODO Object with non-nullary constructor should probably be an
// error, since it will always be lazy-constructed with no arguments.

#[test]
pub fn empty_object_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_with_member_var_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defvar x 100)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    var x = 100
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_with_constructor_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defvar x) (defn _init () (set self:x 0))))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        self.x = 0
    var x
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_with_member_fn_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defvar x 100) (defn foo () self:x)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    var x = 100
    func foo():
        return self.x
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_with_exports_test() {
  assert_eq!(
    parse_compile_decl_err("((defobject Foo (Reference) (defvar x 1 (export Int))))"),
    Err(PError::from(GDError::new(GDErrorF::ExportOnInnerClassVar("x".to_owned()), SourceOffset(28)))),
  );
}

#[test]
pub fn object_with_static_fn_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defn foo () static 10)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    static func foo():
        return 10
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_with_static_fn_self_test() {
  assert_eq!(
    parse_compile_decl_err("((defobject Foo (Reference) (defvar x 10) (defn foo () static self:x)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar("self".to_owned()), SourceOffset(62)))),
  );
}

#[test]
pub fn object_with_const_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defconst Bar 100)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    const Bar = 100
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_with_signal_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defsignal my-signal)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    signal my_signal
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn main_object_test() {
  assert_eq!(
    parse_compile_decl_err("((defobject Foo (Reference) main))"),
    Err(PError::from(GDError::new(GDErrorF::DottedListError, SourceOffset(28)))),
  );
}

#[test]
pub fn simple_self_closure_object_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Node) (defn test () (lambda () self))))"),
             r#"extends Reference
class _LambdaBlock_2 extends GDLisp.Function:
    var _self_1
    func _init(_self_1):
        self._self_1 = _self_1
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return _self_1
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
class _Foo_Singleton_0 extends Node:
    func _init():
        pass
    func test():
        return _LambdaBlock_2.new(self)
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn labels_self_closure_object_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Node) (defn test () (labels ((foo (x) (foo self))) (foo 76)))))"),
             r#"extends Reference
class _Labels_5 extends Reference:
    var _self_1
    func _init(_self_1):
        self._self_1 = _self_1
    func _fn_foo_3(x_4):
        return _fn_foo_3(_self_1)
class _Foo_Singleton_0 extends Node:
    func _init():
        pass
    func test():
        var _locals_2 = _Labels_5.new(self)
        return _locals_2._fn_foo_3(76)
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_outer_ref_test() {
  assert_eq!(parse_compile_decl("((defn outer ()) (defobject Foo (Reference) (defvar x 100) (defn foo () (outer) self:x)))"),
             r#"extends Reference
static func outer():
    return null
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    var x = 100
    func foo():
        __gdlisp_outer_class_1.outer()
        return self.x
    var __gdlisp_outer_class_1 = load("res://TEST.gd")
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn object_static_outer_ref_test() {
  assert_eq!(parse_compile_decl("((defn outer ()) (defobject Foo (Reference) (defvar x 100) (defn foo () static (outer) 1)))"),
             r#"extends Reference
static func outer():
    return null
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    var x = 100
    static func foo():
        load("res://TEST.gd").outer()
        return 1
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn get_node_on_self_object_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defn foo () $Target/Node)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    func foo():
        return self.get_node("Target/Node")
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn get_node_on_self_static_object_test() {
  assert_eq!(
    parse_compile_decl_err("((defobject Foo (Reference) (defn foo () static $Target/Node)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar("self".to_owned()), SourceOffset(48)))),
  );
}

#[test]
pub fn get_node_on_explicit_target_object_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference) (defn foo (x) x:$Target/Node)))"),
             r#"extends Reference
class _Foo_Singleton_0 extends Reference:
    func _init():
        pass
    func foo(x_1):
        return x_1.get_node("Target/Node")
static func Foo():
    var this_file = load("res://TEST.gd")
    if !this_file.has_meta("__gdlisp_Singleton_Foo"):
        var value = _Foo_Singleton_0.new()
        this_file.set_meta("__gdlisp_Singleton_Foo", value)
        return value
    return this_file.get_meta("__gdlisp_Singleton_Foo")
static func run():
    return null
"#);
}

#[test]
pub fn nonsense_modifier_object_test_1() {
  assert_eq!(
    parse_compile_decl_err(r#"((defobject Foo (Node) public public))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(30)))),
  );
}

#[test]
pub fn nonsense_modifier_object_test_2() {
  assert_eq!(
    parse_compile_decl_err(r#"((defobject Foo (Node) public private))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(30)))),
  );
}

#[test]
pub fn nonsense_modifier_object_test_3() {
  assert_eq!(
    parse_compile_decl_err(r#"((defobject Foo (Node) (defn example () static static)))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("static")), SourceOffset(47)))),
  );
}

#[test]
#[ignore]
pub fn simple_self_run_object_test() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference)
       (defvar x 1)
       (defn _init ())
       (defn double ()
         (* self:x 2)))
     (print Foo:x)
     (print (Foo:double))
     (set Foo:x 10)
     (print (Foo:double)))
  "#), "\n1\n2\n20\n");
}

#[test]
#[ignore]
pub fn self_with_closure_run_object_test() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference)
       (defvar x 1)
       (defn increment ()
         (lambda ()
           (set self:x (+ self:x 1)))))
     (let ((fn (Foo:increment)))
       (print (funcall fn))
       (print (funcall fn))
       (print (funcall fn))))
  "#), "\n2\n3\n4\n");
}

#[test]
#[ignore]
pub fn macro_in_object_test_1() {
  assert_eq!(parse_and_run(r#"
    ((defmacro declare-fn (name)
       `(defn ,name () 99))
     (defobject Foo (Reference)
       (declare-fn aa)
       (declare-fn bb))
     (print (Foo:aa))
     (print (Foo:bb)))
  "#), "\n99\n99\n");
}

#[test]
#[ignore]
pub fn macro_in_object_test_2() {
  assert_eq!(parse_and_run(r#"
    ((defmacro my-value ()
       630)
     (defobject Foo (Reference)
       (defn foo () (my-value)))
     (print (Foo:foo)))
  "#), "\n630\n");
}

#[test]
#[ignore]
pub fn macro_in_object_test_3() {
  assert_eq!(parse_and_run(r#"
    ((defmacro declare-fns ()
       `(progn (defn a () 67) (defn b () 68)))
     (defobject Foo (Reference)
       (declare-fns))
     (print (Foo:a))
     (print (Foo:b)))
  "#), "\n67\n68\n");
}

#[test]
#[ignore]
pub fn macro_uses_object_test() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference)
       (defvar x))
     (defmacro through-foo ()
       (set Foo:x 5)
       Foo:x)
     (print (through-foo)))"#),
             "\n5\n");
}

#[test]
#[ignore]
pub fn reference_to_const_in_object_test() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference)
       (defconst CONSTANT 100))
     (print Foo:CONSTANT))"#),
             "\n100\n");
}

#[test]
#[ignore]
pub fn reference_to_static_in_object_test() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference)
       (defn foo () static 98))
     (print (Foo:foo)))"#),
             "\n98\n");
}

#[test]
#[ignore]
pub fn reference_to_outer_in_object_test_1() {
  // TODO This one raises an interesting point. An outer Reference
  // class and an inner Reference singleton object will instantly be
  // in a cycle and never get freed (and Godot issues a warning at
  // exit time if this happens). Should we detect this? Should it be
  // allowed?
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defobject Foo (Reference)
       (defn foo () (outer)))
     (print (Foo:foo))
     (set (elt Foo "__gdlisp_outer_class_1") nil))"#); // Nasty hack to break the cyclic reference (have to use elt and a string or GDLisp will catch on to what I'm doing)
  assert_eq!(output, "\n100\n");
}

#[test]
#[ignore]
pub fn reference_to_outer_in_object_test_2() {
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defobject Foo (Reference)
       (defn foo () static (outer)))
     (print (Foo:foo)))"#);
  assert_eq!(output, "\n100\n");
}

#[test]
#[ignore]
pub fn initialization_of_object_test() {
  // Make sure that a singleton object is only initialized once, even
  // if we reference it a couple of times.
  let output = parse_and_run(r#"
    ((defobject Foo (Reference)
       (defn _init ()
         (print "Initializing"))
       (defn foo ()
         18))
     (print "Start")
     (print (Foo:foo))
     (print "Middle")
     (print (Foo:foo))
     (print "End"))"#);
  assert_eq!(output, "\nStart\nInitializing\n18\nMiddle\n18\nEnd\n");
}
