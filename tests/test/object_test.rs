// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

extern crate gdlisp;

use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn empty_object_test() {
  assert_eq!(parse_compile_decl("((defobject Foo (Reference)))"),
             r#"extends Reference


class _AnonymousClass extends Reference:

    func _init():
        pass


static func _lazy_0():
    var _this_file_1 = load("res://TEST.gd")
    var _cond = null
    if _this_file_1.has_meta("__gdlisp_Lazy__G_3"):
        _cond = _this_file_1.get_meta("__gdlisp_Lazy__G_3")
    else:
        if true:
            var _value_2 = _AnonymousClass.new()
            _this_file_1.set_meta("__gdlisp_Lazy__G_3", _value_2)
            _cond = _value_2
        else:
            _cond = null
    return _cond


static func __gdlisp_SymbolMacroFunction_Foo():
    return GDLisp.cons(GDLisp.cons(GDLisp.intern("access-slot"), GDLisp.cons(GDLisp.cons(GDLisp.intern("contextual-load"), GDLisp.cons("res://TEST.gd", null)), GDLisp.cons(GDLisp.intern("_lazy_0"), null))), null)
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
  // See Issue #85
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

#[test]
pub fn object_uses_gdlisp_functions_test() {
  let output = parse_and_run(r#"
    ((defobject MyObject (Reference)
       (defvar example-var [1 2 3 4])
       (defn sum ()
         (array/fold #'+ @example-var 0)))
     (print (MyObject:sum)))
  "#);
  assert_eq!(output, "\n10\n");
}
