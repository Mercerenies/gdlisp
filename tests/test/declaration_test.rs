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

use gdlisp::ir::identifier::ClassNamespace;
use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::{parse_compile_decl, parse_compile_decl_err};

#[test]
pub fn empty_file_test() {
  // TODO Should not have init here (empty class exception, which I
  // don't think is needed anymore); also applies in macro_test::simple_minimalist_test
  assert_eq!(parse_compile_decl("()"), r#"extends Reference


func _init():
    pass
"#);
}

#[test]
pub fn simple_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) x))"),
             r#"extends Reference


static func foo(x):
    return x
"#);
}

#[test]
pub fn lambda_in_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () x) x))"), r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

    var x

    func _init(x):
        self.x = x
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        return x

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")


static func foo(x):
    return x
"#);
}

#[test]
pub fn closed_rw_in_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () (set x 1)) x))"), r#"extends Reference


class _LambdaBlock extends GDLisp.Function:

    var x

    func _init(x):
        self.x = x
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0

    func call_func():
        x.contents = 1
        return x.contents

    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")


static func foo(x):
    x = GDLisp.Cell.new(x)
    return x.contents
"#);
}

#[test]
pub fn mutually_recursive_test() {
  assert_eq!(parse_compile_decl("((defn foo () (bar)) (defn bar () (foo)))"), r#"extends Reference


static func foo():
    return bar()


static func bar():
    return foo()
"#);
}

#[test]
pub fn nonexistent_function_test() {
  assert_eq!(
    parse_compile_decl_err("((defn foo () (bar)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("bar")), SourceOffset(14)))),
  );
}

#[test]
pub fn progn_decl_test() {
  assert_eq!(parse_compile_decl("((progn (progn (defn foo () ()) (defn bar () ()))))"), r#"extends Reference


static func foo():
    return null


static func bar():
    return null
"#);
}

#[test]
pub fn declare_value_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare value x) (defn foo () x))"), r#"extends Reference


static func foo():
    return x
"#);
}

#[test]
pub fn declare_value_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal x) (defn foo () x))"), r#"extends Reference


static func foo():
    return x
"#);
}

#[test]
pub fn declare_value_test_3() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal x) (defconst y x))"), r#"extends Reference


const y = x
"#);
}

#[test]
pub fn declare_value_test_4() {
  assert_eq!(parse_compile_decl("((sys/declare constant x) (defconst y x))"), r#"extends Reference


const y = x
"#);
}

#[test]
pub fn declare_value_non_const_test() {
  assert_eq!(
    parse_compile_decl_err("((sys/declare value x) (defconst y x))"),
    Err(PError::from(GDError::new(GDErrorF::NotConstantEnough(String::from("y")), SourceOffset(35)))),
  );
}

#[test]
pub fn declare_function_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare function f ()) (defn foo () (f)))"), r#"extends Reference


static func foo():
    return f()
"#);
}

#[test]
pub fn declare_function_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare function f (a &opt b)) (defn foo () (f 1) (f 1 2)))"), r#"extends Reference


static func foo():
    f(1, null)
    return f(1, 2)
"#);
}

#[test]
pub fn declare_function_test_3() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction f (a &opt b)) (defn foo () (f 1) (f 1 2)))"), r#"extends Reference


static func foo():
    f(1, null)
    return f(1, 2)
"#);
}

#[test]
pub fn nonsense_modifier_function_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () public private 1))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(21)))),
  );
}

#[test]
pub fn declare_function_inner_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare function f ()) (defclass Foo (Reference) (defn _init () (f))))"),
             r#"extends Reference


class Foo extends Reference:

    func _init():
        __gdlisp_outer_class_0.f()

    var __gdlisp_outer_class_0 = load("res://TEST.gd")
"#);
}

#[test]
pub fn declare_function_inner_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction f ()) (defclass Foo (Reference) (defn _init () (f))))"),
             r#"extends Reference


class Foo extends Reference:

    func _init():
        f()
"#);
}

#[test]
pub fn duplicate_const_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defconst A 1) (defconst A 1))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(17)))),
  );
}

#[test]
pub fn duplicate_const_in_main_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defconst A 1) (defclass Foo () main (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateNameConflictInMainClass(ClassNamespace::Value, String::from("A")), SourceOffset(39)))),
  );
}

#[test]
pub fn duplicate_const_with_var_in_main_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defconst A 1) (defclass Foo () main (defvar A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateNameConflictInMainClass(ClassNamespace::Value, String::from("A")), SourceOffset(39)))),
  );
}

#[test]
pub fn duplicate_const_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defconst A 1) (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(34)))),
  );
}

#[test]
pub fn duplicate_var_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defvar A 1) (defvar A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(32)))),
  );
}

#[test]
pub fn duplicate_var_const_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defvar A 1) (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(32)))),
  );
}

#[test]
pub fn duplicate_fn_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defn foo ()) (defn foo ())))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(33)))),
  );
}

#[test]
pub fn duplicate_static_fn_in_class_test_1() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defn foo ()) (defn foo () static)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(33)))),
  );
}

#[test]
pub fn duplicate_static_fn_in_class_test_2() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defn foo () static) (defn foo () static)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(40)))),
  );
}

#[test]
pub fn duplicate_signal_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defsignal foo) (defsignal foo ())))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Signal, String::from("foo")), SourceOffset(35)))),
  );
}

#[test]
pub fn duplicate_fn_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () 1) (defn foo () 2))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(18)))),
  );
}

#[test]
pub fn duplicate_fn_main_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () 1) (defclass Foo (Reference) main (defn foo () 2)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateNameConflictInMainClass(ClassNamespace::Function, String::from("foo")), SourceOffset(49)))),
  );
}

#[test]
pub fn duplicate_fn_main_class_test_static() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () 1) (defclass Foo (Reference) main (defn foo () static 2)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateNameConflictInMainClass(ClassNamespace::Function, String::from("foo")), SourceOffset(49)))),
  );
}

#[test]
pub fn overlapping_namespaces_test() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defconst foo 1))"),
             r#"extends Reference


static func foo():
    return null


const foo = 1
"#);
}

#[test]
pub fn overlapping_namespaces_in_class_test() {
  assert_eq!(parse_compile_decl("((defclass Foo () (defsignal foo) (defn foo ()) (defconst foo 1)))"),
             r#"extends Reference


class Foo extends Reference:

    func _init():
        pass

    signal foo

    func foo():
        return null

    const foo = 1
"#);
}
#[test]
pub fn duplicate_const_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () (new Reference (defconst A 1) (defconst A 1))))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(45)))),
  );
}

#[test]
pub fn duplicate_var_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () (new Reference (defvar A 1) (defvar A 1))))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(43)))),
  );
}

#[test]
pub fn duplicate_var_const_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () (new Reference (defvar A 1) (defconst A 1))))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(43)))),
  );
}

#[test]
pub fn duplicate_fn_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn xyz () (new Reference (defn foo ()) (defn foo ()))))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(44)))),
  );
}

#[test]
pub fn duplicate_signal_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn xyz () (new Reference (defsignal foo) (defsignal foo ()))))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Signal, String::from("foo")), SourceOffset(46)))),
  );
}

#[test]
pub fn declare_value_reserved_word_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal while) (defconst y while))"), r#"extends Reference


const y = _while
"#);
}

#[test]
pub fn declare_value_reserved_word_test_2() {
  // Invalid syntax in GDScript, but that's what you get for using a
  // `sys/` primitive irresponsibly ¯\_(ツ)_/¯
  assert_eq!(parse_compile_decl("((sys/declare superglobal (x while)) (defconst y x))"), r#"extends Reference


const y = while
"#);
}

#[test]
pub fn declare_function_reserved_word_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction self ()) (defn foo () (self)))"), r#"extends Reference


static func foo():
    return _self()
"#);
}

#[test]
pub fn declare_function_reserved_word_test_2() {
  // Invalid syntax in GDScript, but that's what you get for using a
  // `sys/` primitive irresponsibly ¯\_(ツ)_/¯
  assert_eq!(parse_compile_decl("((sys/declare superfunction (x self) ()) (defn foo () (x)))"), r#"extends Reference


static func foo():
    return self()
"#);
}

#[test]
pub fn declare_value_custom_name_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal (x pizza)) (defconst y x))"), r#"extends Reference


const y = pizza
"#);
}

#[test]
pub fn declare_value_custom_name_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal (x pepperoni-pizza)) (defconst y x))"),
             r#"extends Reference


const y = pepperoni_pizza
"#);
}

#[test]
pub fn declare_function_custom_name_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction (x pizza) ()) (defn foo () (x)))"), r#"extends Reference


static func foo():
    return pizza()
"#);
}

#[test]
pub fn declare_function_custom_name_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction (x pepperoni-pizza) ()) (defn foo () (x)))"), r#"extends Reference


static func foo():
    return pepperoni_pizza()
"#);
}

#[test]
pub fn bad_sys_declare_test() {
  assert_eq!(
    parse_compile_decl_err("((sys/declare not-a-valid-declaration-type foobar))"),
    Err(PError::from(GDError::new(GDErrorF::BadSysDeclare(String::from("not-a-valid-declaration-type")), SourceOffset(1)))),
  );
}
