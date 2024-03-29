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

use super::common::*;

#[test]
pub fn simple_lazy_test() {
  assert_eq!(parse_compile_decl("((deflazy x 100))"),
             r#"extends Reference


static func _lazy_0():
    var _this_file_1 = load("res://TEST.gd")
    var _cond = null
    if _this_file_1.has_meta("__gdlisp_Lazy__G_3"):
        _cond = _this_file_1.get_meta("__gdlisp_Lazy__G_3")
    else:
        if true:
            var _value_2 = 100
            _this_file_1.set_meta("__gdlisp_Lazy__G_3", _value_2)
            _cond = _value_2
        else:
            _cond = null
    return _cond


static func __gdlisp_SymbolMacroFunction_x():
    return GDLisp.cons(GDLisp.cons(GDLisp.intern("access-slot"), GDLisp.cons(GDLisp.cons(GDLisp.intern("contextual-load"), GDLisp.cons("res://TEST.gd", null)), GDLisp.cons(GDLisp.intern("_lazy_0"), null))), null)
"#);
}

#[test]
pub fn simple_private_lazy_test() {
  // The private modifier should parse and work correctly, but it
  // doesn't change the output compared to the simple_lazy_test case.
  assert_eq!(parse_compile_decl("((deflazy x 100 private))"),
             r#"extends Reference


static func _lazy_0():
    var _this_file_1 = load("res://TEST.gd")
    var _cond = null
    if _this_file_1.has_meta("__gdlisp_Lazy__G_3"):
        _cond = _this_file_1.get_meta("__gdlisp_Lazy__G_3")
    else:
        if true:
            var _value_2 = 100
            _this_file_1.set_meta("__gdlisp_Lazy__G_3", _value_2)
            _cond = _value_2
        else:
            _cond = null
    return _cond


static func __gdlisp_SymbolMacroFunction_x():
    return GDLisp.cons(GDLisp.cons(GDLisp.intern("access-slot"), GDLisp.cons(GDLisp.cons(GDLisp.intern("contextual-load"), GDLisp.cons("res://TEST.gd", null)), GDLisp.cons(GDLisp.intern("_lazy_0"), null))), null)
"#);
}

#[test]
pub fn simple_lazy_run_test_1() {
  assert_eq!(parse_and_run(r#"
    ((deflazy x 100)
     (print x))
  "#), "\n100\n");
}

#[test]
pub fn simple_lazy_run_test_2() {
  // The 3 should only print once
  assert_eq!(parse_and_run(r#"
    ((deflazy x (progn (print 3) 100))
     (print x)
     (print x))
  "#), "\n3\n100\n100\n");
}

// TODO Test lazy vals getting reinitialized after a file is unloaded and reloaded
