
extern crate gdlisp;

use super::common::*;

#[test]
pub fn simple_lazy_test() {
  assert_eq!(parse_compile_decl("((deflazy x 100))"),
             r#"extends Reference
static func _lazy_0():
    var _this_file_1_0 = load("res://TEST.gd")
    var _cond_1 = null
    if _this_file_1_0.has_meta("__gdlisp_Lazy__G_3"):
        _cond_1 = _this_file_1_0.get_meta("__gdlisp_Lazy__G_3")
    else:
        if true:
            var _value_2_2 = 100
            _this_file_1_0.set_meta("__gdlisp_Lazy__G_3", _value_2_2)
            _cond_1 = _value_2_2
        else:
            _cond_1 = null
    return _cond_1
static func x():
    return GDLisp.Cons.new(GDLisp.Cons.new(GDLisp.intern("access-slot"), GDLisp.Cons.new(GDLisp.Cons.new(GDLisp.intern("contextual-load"), GDLisp.Cons.new("res://TEST.gd", null)), GDLisp.Cons.new(GDLisp.intern("_lazy_0"), null))), null)
static func run():
    return null
"#);
}

#[test]
pub fn simple_private_lazy_test() {
  // The private modifier should parse and work correctly, but it
  // doesn't change the output compared to the simple_lazy_test case.
  assert_eq!(parse_compile_decl("((deflazy x 100 private))"),
             r#"extends Reference
static func _lazy_0():
    var _this_file_1_0 = load("res://TEST.gd")
    var _cond_1 = null
    if _this_file_1_0.has_meta("__gdlisp_Lazy__G_3"):
        _cond_1 = _this_file_1_0.get_meta("__gdlisp_Lazy__G_3")
    else:
        if true:
            var _value_2_2 = 100
            _this_file_1_0.set_meta("__gdlisp_Lazy__G_3", _value_2_2)
            _cond_1 = _value_2_2
        else:
            _cond_1 = null
    return _cond_1
static func x():
    return GDLisp.Cons.new(GDLisp.Cons.new(GDLisp.intern("access-slot"), GDLisp.Cons.new(GDLisp.Cons.new(GDLisp.intern("contextual-load"), GDLisp.Cons.new("res://TEST.gd", null)), GDLisp.Cons.new(GDLisp.intern("_lazy_0"), null))), null)
static func run():
    return null
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
