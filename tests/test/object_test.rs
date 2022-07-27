
extern crate gdlisp;

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
class _AnonymousClass extends Reference:
    func _init():
        pass
static func _lazy_0():
    var _this_file_1_0 = load("res://TEST.gd")
    var _cond_1 = null
    if _this_file_1_0.has_meta("__gdlisp_Lazy__G_3"):
        _cond_1 = _this_file_1_0.get_meta("__gdlisp_Lazy__G_3")
    else:
        if true:
            var _value_2_2 = _AnonymousClass.new()
            _this_file_1_0.set_meta("__gdlisp_Lazy__G_3", _value_2_2)
            _cond_1 = _value_2_2
        else:
            _cond_1 = null
    return _cond_1
static func Foo():
    return GDLisp.Cons.new(GDLisp.Cons.new(GDLisp.intern("access-slot"), GDLisp.Cons.new(GDLisp.Cons.new(GDLisp.intern("contextual-load"), GDLisp.Cons.new("res://TEST.gd", null)), GDLisp.Cons.new(GDLisp.intern("_lazy_0"), null))), null)
static func run():
    return null
"#);
}

#[test]
pub fn main_object_test() {
  assert_eq!(
    parse_compile_decl_err("((defobject Foo (Reference) main))"),
    Err(PError::from(GDError::new(GDErrorF::DottedListError, SourceOffset(1)))),
  );
}

#[test]
pub fn simple_self_run_object_test_1() {
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
pub fn simple_self_run_object_test_2() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo Reference
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
pub fn simple_self_run_object_test_3() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference) public
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
pub fn simple_self_run_object_test_4() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference) private
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
pub fn empty_object_run_test() {
  assert_eq!(parse_and_run(r#"
    ((defobject Foo (Reference)) Foo (print 1))
  "#), "\n1\n");
}

#[test]
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

/* /////
#[test]
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
*/

/* /////
#[test]
pub fn reference_to_outer_in_object_test_1() {
  // TODO This one raises an interesting point. An outer Reference
  // class and an inner Reference singleton object will instantly be
  // in a cycle and never get freed (and Godot issues a warning at
  // exit time if this happens). Should we detect this? Should it be
  // allowed? Can we hack it with weakref somehow?
  let output = parse_and_run(r#"
    ((defn outer () 100)
     (defobject Foo (Reference)
       (defn foo () (outer)))
     (print (Foo:foo))
     (set (elt Foo "__gdlisp_outer_class_1") nil))"#); // Nasty hack to break the cyclic reference (have to use elt and a string or GDLisp will catch on to what I'm doing)
  assert_eq!(output, "\n100\n");
}
*/

#[test]
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
