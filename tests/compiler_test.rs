
extern crate gdlisp;

use gdlisp::compile::Compiler;
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::compile::symbol_table::{LocalVar, SymbolTable};
use gdlisp::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use gdlisp::compile::symbol_table::call_magic::DefaultCall;
use gdlisp::parser;
use gdlisp::ir;
use gdlisp::gdscript::library;

// TODO For the love of all that is good, split this into several files.

fn bind_helper_symbols(table: &mut SymbolTable) {
  // Binds a few helper names to the symbol table for the sake of
  // debugging.
  table.set_fn(String::from("foo"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("foo")), Box::new(DefaultCall));
  table.set_fn(String::from("foo1"), FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, String::from("foo1")), Box::new(DefaultCall));
  table.set_fn(String::from("foo2"), FnCall::unqualified(FnSpecs::new(2, 0, false), FnScope::Global, String::from("foo2")), Box::new(DefaultCall));
  table.set_fn(String::from("bar"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("bar")), Box::new(DefaultCall));
  table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
}

// TODO Currently, this panics if it fails. This is okay-ish, since
// it's only being used for tests. But once we unify all of our errors
// (so we can represent parse errors and compile errors with one
// common type), we should return a Result<...> here.
fn parse_compile_and_output(input: &str) -> String {
  parse_compile_and_output_h(input).0
}

fn parse_compile_and_output_h(input: &str) -> (String, String) {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
  let mut table = SymbolTable::new();
  bind_helper_symbols(&mut table);
  library::bind_builtins(&mut table);

  let mut builder = StmtBuilder::new();
  let value = ir::compile_expr(&value).unwrap();
  let () = compiler.compile_stmt(&mut builder, &mut table, &mut stmt_wrapper::Return, &value).unwrap();
  let (stmts, helpers) = builder.build();
  let a = stmts.into_iter().map(|stmt| stmt.to_gd(0)).collect::<String>();
  let b = helpers.into_iter().map(|decl| decl.to_gd(0)).collect::<String>();
  (a, b)
}

#[test]
pub fn expr_tests() {
  assert_eq!(parse_compile_and_output("100"), "return 100\n");
  assert_eq!(parse_compile_and_output("(progn 100 200 300)"), "return 300\n");
  assert_eq!(parse_compile_and_output("()"), "return GDLisp.Nil\n");
}

#[test]
pub fn progn_tests() {
  assert_eq!(parse_compile_and_output("(progn (foo) (bar) (foo))"), "foo()\nbar()\nreturn foo()\n");
  assert_eq!(parse_compile_and_output("(progn)"), "return GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(progn (progn))"), "return GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(progn ())"), "return GDLisp.Nil\n");
}

#[test]
pub fn if_tests_expr() {
  assert_eq!(parse_compile_and_output("(if 1 2 3)"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = 2\nelse:\n    _if_0 = 3\nreturn _if_0\n");
  assert_eq!(parse_compile_and_output("(if 1 2)"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = 2\nelse:\n    _if_0 = GDLisp.Nil\nreturn _if_0\n");
  assert_eq!(parse_compile_and_output("(if 1 2 ())"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = 2\nelse:\n    _if_0 = GDLisp.Nil\nreturn _if_0\n");
  //assert_eq!(parse_compile_and_output("(if _if_0 2 ())"), "var _if_1 = GDLisp.Nil\nif _if_0:\n    _if_1 = 2\nelse:\n    _if_1 = GDLisp.Nil\nreturn _if_1\n"); // Variable scoping issues; can't do this one right now
  assert_eq!(parse_compile_and_output("(if 1 (foo) (bar))"), "var _if_0 = GDLisp.Nil\nif 1:\n    _if_0 = foo()\nelse:\n    _if_0 = bar()\nreturn _if_0\n");
}

#[test]
pub fn if_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (if 1 2 3) 1)"), "if 1:\n    pass\nelse:\n    pass\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(progn (if 1 (foo) (bar)) 1)"), "if 1:\n    foo()\nelse:\n    bar()\nreturn 1\n");
}

#[test]
pub fn cond_tests_expr() {
  assert_eq!(parse_compile_and_output("(cond ((bar) (foo)) (foobar (bar)))"), "var _cond_0 = GDLisp.Nil\nif bar():\n    _cond_0 = foo()\nelse:\n    if foobar:\n        _cond_0 = bar()\n    else:\n        _cond_0 = GDLisp.Nil\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((bar) (foo)) (foobar (bar))) 1)"), "if bar():\n    foo()\nelse:\n    if foobar:\n        bar()\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn cond_tests_abbr_expr() {
  assert_eq!(parse_compile_and_output("(cond ((foo) (foo)) ((bar)))"), "var _cond_0 = GDLisp.Nil\nif foo():\n    _cond_0 = foo()\nelse:\n    var _cond_1 = bar()\n    if _cond_1:\n        _cond_0 = _cond_1\n    else:\n        _cond_0 = GDLisp.Nil\nreturn _cond_0\n");
}

#[test]
pub fn cond_tests_abbr_stmt() {
  assert_eq!(parse_compile_and_output("(progn (cond ((foo) (foo)) ((bar))) 1)"), "if foo():\n    foo()\nelse:\n    var _cond_0 = bar()\n    if _cond_0:\n        pass\n    else:\n        pass\nreturn 1\n");
}

#[test]
pub fn while_tests() {
  assert_eq!(parse_compile_and_output("(while 1)"), "while 1:\n    pass\nreturn GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(while (foo) (foo1 0) (foo2 0 0))"), "while foo():\n    foo1(0)\n    foo2(0, 0)\nreturn GDLisp.Nil\n");
  assert_eq!(parse_compile_and_output("(foo1 (while (bar)))"), "while bar():\n    pass\nreturn foo1(GDLisp.Nil)\n");
}

#[test]
pub fn compound_while_tests() {
  // If expressions cannot be compiled into a single GDScript
  // expression, so this forces the while loop to use the "compound"
  // form.
  assert_eq!(parse_compile_and_output("(while (if 1 2 3) (foo))"), "while true:\n    var _if_0 = GDLisp.Nil\n    if 1:\n        _if_0 = 2\n    else:\n        _if_0 = 3\n    if _if_0:\n        break\n    foo()\nreturn GDLisp.Nil\n")
}

#[test]
pub fn let_tests() {
  assert_eq!(parse_compile_and_output("(let () 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(let (a) 1)"), "var a_0 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a)) 1)"), "var a_0 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (foo1 a))"), "var a_0 = 1\nreturn foo1(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a 1) (b 2)) (foo2 a b))"), "var a_0 = 1\nvar b_1 = 2\nreturn foo2(a_0, b_1)\n");
  assert_eq!(parse_compile_and_output("(let ((a (foo) (bar))) (foo1 a))"), "foo()\nvar a_0 = bar()\nreturn foo1(a_0)\n");
  assert_eq!(parse_compile_and_output("(let ((a) b) 1)"), "var a_0 = GDLisp.Nil\nvar b_1 = GDLisp.Nil\nreturn 1\n");
  assert_eq!(parse_compile_and_output("(let (a (b)) 1)"), "var a_0 = GDLisp.Nil\nvar b_1 = GDLisp.Nil\nreturn 1\n");
}

#[test]
pub fn var_shadowing() {
  assert_eq!(parse_compile_and_output("(let ((a)) (let ((a a)) a))"), "var a_0 = GDLisp.Nil\nvar a_1 = a_0\nreturn a_1\n");
}

#[test]
pub fn inline_if_in_let_test() {
  assert_eq!(parse_compile_and_output("(let ((a (if (foo) (bar) (foo)))) a)"), "var _if_0 = GDLisp.Nil\nif foo():\n    _if_0 = bar()\nelse:\n    _if_0 = foo()\nvar a_1 = _if_0\nreturn a_1\n");
}

#[test]
pub fn basic_lambda_test() {

  let result0 = parse_compile_and_output_h("(lambda ())");
  assert_eq!(result0.0, "return _LambdaBlock_0.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func():\n        return GDLisp.Nil\n    func call_funcv(args):\n        if args is GDLisp.NilClass:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

  let result1 = parse_compile_and_output_h("(lambda (a) a)");
  assert_eq!(result1.0, "return _LambdaBlock_1.new()\n");
  assert_eq!(result1.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

  let result2 = parse_compile_and_output_h("(progn (lambda (a) a) 1)");
  assert_eq!(result2.0, "return 1\n");
  assert_eq!(result2.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () a))");
  assert_eq!(result0.0, "var a_0 = GDLisp.Nil\nreturn _LambdaBlock_1.new(a_0)\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    var a_0\n    func _init(a_0):\n        self.a_0 = a_0\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func():\n        return a_0\n    func call_funcv(args):\n        if args is GDLisp.NilClass:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn non_closure_lambda_test() {

  let result0 = parse_compile_and_output_h("(let (a) (lambda () (let (a) a)))");
  assert_eq!(result0.0, "var a_0 = GDLisp.Nil\nreturn _LambdaBlock_2.new()\n");
  assert_eq!(result0.1, "class _LambdaBlock_2 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 0\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func():\n        var a_1 = GDLisp.Nil\n        return a_1\n    func call_funcv(args):\n        if args is GDLisp.NilClass:\n            return call_func()\n        else:\n            push_error(\"Too many arguments\")\n");

}

#[test]
pub fn basic_funcall_test() {
  assert_eq!(parse_compile_and_output("(funcall 1)"), "return GDLisp.funcall(1, GDLisp.Nil)\n");
  assert_eq!(parse_compile_and_output("(progn (funcall 1) 2)"), "GDLisp.funcall(1, GDLisp.Nil)\nreturn 2\n");
  assert_eq!(parse_compile_and_output("(funcall 1 2 3)"), "return GDLisp.funcall(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, GDLisp.Nil)))\n");
}

#[test]
pub fn funcall_lambda_test() {

  let result0 = parse_compile_and_output_h("(let ((f (lambda (a) a))) (funcall f 100))");
  assert_eq!(result0.0, "var f_2 = _LambdaBlock_1.new()\nreturn GDLisp.funcall(f_2, GDLisp.Cons.new(100, GDLisp.Nil))\n");
  assert_eq!(result0.1, "class _LambdaBlock_1 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(a_0):\n        return a_0\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");
}

#[test]
pub fn simple_builtin_test() {
  assert_eq!(parse_compile_and_output("(cons 1 2)"), "return GDLisp.Cons.new(1, 2)\n");
  assert_eq!(parse_compile_and_output("(cons 1 (cons 2 3))"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, 3))\n");
}

#[test]
pub fn function_ref_test() {
  let result0 = parse_compile_and_output_h("(function foo1)");
  assert_eq!(result0.0, "return _FunctionRefBlock_0.new()\n");
  assert_eq!(result0.1, "class _FunctionRefBlock_0 extends GDLisp.Function:\n    func _init():\n        self.__gdlisp_required = 1\n        self.__gdlisp_optional = 0\n        self.__gdlisp_rest = false\n    func call_func(arg0):\n        return foo1(arg0)\n    func call_funcv(args):\n        var required_0 = null\n        if args is GDLisp.NilClass:\n            push_error(\"Not enough arguments\")\n        else:\n            required_0 = args.car\n            args = args.cdr\n        if args is GDLisp.NilClass:\n            return call_func(required_0)\n        else:\n            push_error(\"Too many arguments\")\n");
}

#[test]
#[should_panic]
pub fn nonexistent_assignment_test() {
  parse_compile_and_output("(setq nonexistent-var 0)");
}

#[test]
pub fn assignment_test() {

  // No cell; only accessed directly
  let result0 = parse_compile_and_output("(let ((x 1)) (setq x 2))");
  assert_eq!(result0, "var x_0 = 1\nx_0 = 2\nreturn x_0\n");

  // No cell; only accessed directly
  let result1 = parse_compile_and_output("(let ((x 1)) (setq x 2) 3)");
  assert_eq!(result1, "var x_0 = 1\nx_0 = 2\nreturn 3\n");

  // Cell; accessed inside lambda
  let result2 = parse_compile_and_output("(let ((x 1)) (lambda () (setq x 2)))");
  assert_eq!(result2, "var x_0 = GDLisp.Cell.new(1)\nreturn _LambdaBlock_1.new(x_0)\n");

  // Cell; accessed both inside and outside lambda
  let result3 = parse_compile_and_output("(let ((x 1)) (lambda () (setq x 2)) (setq x 3))");
  assert_eq!(result3, "var x_0 = GDLisp.Cell.new(1)\nx_0.contents = 3\nreturn x_0.contents\n");

  // No cell; read-only
  let result4 = parse_compile_and_output("(let ((x 1)) (lambda () x) x)");
  assert_eq!(result4, "var x_0 = 1\nreturn x_0\n");

  // Cell; closure and access separately
  let result5 = parse_compile_and_output("(let ((x 1)) (lambda () (setq x 1)) x)");
  assert_eq!(result5, "var x_0 = GDLisp.Cell.new(1)\nreturn x_0.contents\n");

}

#[test]
pub fn addition_compile_test() {
  assert_eq!(parse_compile_and_output("(+)"), "return 0\n");
  assert_eq!(parse_compile_and_output("(+ 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(+ 1 2)"), "return 1 + 2\n");
  assert_eq!(parse_compile_and_output("(+ 1 2 3)"), "return 1 + 2 + 3\n");
}

#[test]
pub fn multiplication_compile_test() {
  assert_eq!(parse_compile_and_output("(*)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(* 2)"), "return 2\n");
  assert_eq!(parse_compile_and_output("(* 2 3)"), "return 2 * 3\n");
  assert_eq!(parse_compile_and_output("(* 2 3 4)"), "return 2 * 3 * 4\n");
}

#[test]
pub fn subtraction_compile_test() {
  assert_eq!(parse_compile_and_output("(- 2)"), "return -2\n");
  assert_eq!(parse_compile_and_output("(- 2 3)"), "return 2 - 3\n");
  assert_eq!(parse_compile_and_output("(- 2 3 4)"), "return 2 - 3 - 4\n");
}

#[test]
pub fn division_compile_test() {
  assert_eq!(parse_compile_and_output("(/ 2)"), "return 1 / float(2)\n");
  assert_eq!(parse_compile_and_output("(/ 2 3)"), "return float(2) / float(3)\n");
  assert_eq!(parse_compile_and_output("(/ 2 3 4)"), "return float(2) / float(3) / float(4)\n");
  assert_eq!(parse_compile_and_output("(/ 2.0 3 4.0)"), "return 2e0 / float(3) / 4e0\n");
}

#[test]
pub fn int_division_compile_test() {
  assert_eq!(parse_compile_and_output("(div 2)"), "return 1 / 2\n");
  assert_eq!(parse_compile_and_output("(div 2 3)"), "return 2 / 3\n");
  assert_eq!(parse_compile_and_output("(div 2 3 4)"), "return 2 / 3 / 4\n");
  assert_eq!(parse_compile_and_output("(div foobar 3 4)"), "return int(foobar) / 3 / 4\n");
}

#[test]
pub fn eq_compile_test() {
  assert_eq!(parse_compile_and_output("(= 1)"), "return true\n");
  assert_eq!(parse_compile_and_output("(= 1 2)"), "return 1 == 2\n");
  assert_eq!(parse_compile_and_output("(= 1 2 3)"), "return 1 == 2 && 2 == 3\n");
}

#[test]
pub fn eq_compile_test_stateful() {
  assert_eq!(parse_compile_and_output("(= (foo))"), "foo()\nreturn true\n");
  assert_eq!(parse_compile_and_output("(= (foo) (foo))"), "return foo() == foo()\n");
  assert_eq!(parse_compile_and_output("(= (foo) (foo) (foo))"), "var _cmp_0 = foo()\nvar _cmp_1 = foo()\nvar _cmp_2 = foo()\nreturn _cmp_0 == _cmp_1 && _cmp_1 == _cmp_2\n");
}

#[test]
pub fn cmp_compile_test() {
  assert_eq!(parse_compile_and_output("(< 1 2)"), "return 1 < 2\n");
  assert_eq!(parse_compile_and_output("(> 1 2 3)"), "return 1 > 2 && 2 > 3\n");
  assert_eq!(parse_compile_and_output("(<= 1 2)"), "return 1 <= 2\n");
  assert_eq!(parse_compile_and_output("(>= 1 2 3)"), "return 1 >= 2 && 2 >= 3\n");
}

#[test]
pub fn cmp_compile_test_stateful() {
  assert_eq!(parse_compile_and_output("(< (foo))"), "foo()\nreturn true\n");
  assert_eq!(parse_compile_and_output("(<= (foo) (foo))"), "return foo() <= foo()\n");
  assert_eq!(parse_compile_and_output("(> (foo) (foo) (foo))"), "var _cmp_0 = foo()\nvar _cmp_1 = foo()\nvar _cmp_2 = foo()\nreturn _cmp_0 > _cmp_1 && _cmp_1 > _cmp_2\n");
  assert_eq!(parse_compile_and_output("(>= (foo) (foo))"), "return foo() >= foo()\n");
}

#[test]
pub fn ne_compile_test() {
  assert_eq!(parse_compile_and_output("(/= 1)"), "return true\n");
  assert_eq!(parse_compile_and_output("(/= (foo))"), "foo()\nreturn true\n");
  assert_eq!(parse_compile_and_output("(/= 1 2)"), "return 1 != 2\n");
  assert_eq!(parse_compile_and_output("(/= 1 2 3)"), "return GDLisp.ne(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, GDLisp.Nil)))\n");
}

#[test]
pub fn simple_length_test() {
  assert_eq!(parse_compile_and_output("(length ())"), "return GDLisp.length(GDLisp.Nil)\n");
}

#[test]
pub fn list_test() {
  assert_eq!(parse_compile_and_output("(list 1 2 3)"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, GDLisp.Nil)))\n");
}

#[test]
pub fn semiglobal_flet_test() {

  let result0 = parse_compile_and_output_h("(flet ((f (x) (+ x 1))) (f 10))");
  assert_eq!(result0.0, "return _flet_0(10)\n");
  assert_eq!(result0.1, "static func _flet_0(x_1):\n    return x_1 + 1\n");

}

#[test]
pub fn semiglobal_flet_test_indirect() {

  let result0 = parse_compile_and_output_h("(flet ((f (x) (+ x 1))) (funcall (function f) 10))");
  assert_eq!(result0.0, "return GDLisp.funcall(_FunctionRefBlock_2.new(), GDLisp.Cons.new(10, GDLisp.Nil))\n");
  assert_eq!(result0.1, r#"static func _flet_0(x_1):
    return x_1 + 1
class _FunctionRefBlock_2 extends GDLisp.Function:
    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func(arg0):
        return _flet_0(arg0)
    func call_funcv(args):
        var required_0 = null
        if args is GDLisp.NilClass:
            push_error("Not enough arguments")
        else:
            required_0 = args.car
            args = args.cdr
        if args is GDLisp.NilClass:
            return call_func(required_0)
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn local_flet_test() {

  let result0 = parse_compile_and_output_h(r#"
    (let ((x 1))
      (flet ((f () (+ x 1)))
        (f)))
  "#);
  assert_eq!(result0.0, "var x_0 = 1\nvar _flet_2 = _LambdaBlock_1.new(x_0)\nreturn _flet_2.call_func()\n");
  assert_eq!(result0.1, r#"class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func():
        return x_0 + 1
    func call_funcv(args):
        if args is GDLisp.NilClass:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn local_flet_test_indirect() {

  let result0 = parse_compile_and_output_h(r#"
    (let ((x 1))
      (flet ((f () (+ x 1)))
        (funcall (function f))))
  "#);
  assert_eq!(result0.0, "var x_0 = 1\nvar _flet_2 = _LambdaBlock_1.new(x_0)\nreturn GDLisp.funcall(_flet_2, GDLisp.Nil)\n");
  assert_eq!(result0.1, r#"class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func():
        return x_0 + 1
    func call_funcv(args):
        if args is GDLisp.NilClass:
            return call_func()
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn local_flet_closure_test() {
  let result0 = parse_compile_and_output_h(r#"
    (let ((x 1))
      (flet ((f () x))
        (let ((g (lambda () (f))))
          (funcall g))))
  "#);
  assert_eq!(result0.0, "var x_0 = 1\nvar _flet_2 = _LambdaBlock_1.new(x_0)\nvar g_4 = _LambdaBlock_3.new(_flet_2)\nreturn GDLisp.funcall(g_4, GDLisp.Nil)\n");
}

#[test]
pub fn semiglobal_labels_test() {

  let result0 = parse_compile_and_output_h("(labels ((f (x) (+ x 1))) (f 10))");
  assert_eq!(result0.0, "return _flet_0(10)\n");
  assert_eq!(result0.1, "static func _flet_0(x_1):\n    return x_1 + 1\n");

}

#[test]
pub fn semiglobal_labels_test_indirect() {

  let result0 = parse_compile_and_output_h("(labels ((f (x) (+ x 1))) (funcall (function f) 10))");
  assert_eq!(result0.0, "return GDLisp.funcall(_FunctionRefBlock_2.new(), GDLisp.Cons.new(10, GDLisp.Nil))\n");
  assert_eq!(result0.1, r#"static func _flet_0(x_1):
    return x_1 + 1
class _FunctionRefBlock_2 extends GDLisp.Function:
    func _init():
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func(arg0):
        return _flet_0(arg0)
    func call_funcv(args):
        var required_0 = null
        if args is GDLisp.NilClass:
            push_error("Not enough arguments")
        else:
            required_0 = args.car
            args = args.cdr
        if args is GDLisp.NilClass:
            return call_func(required_0)
        else:
            push_error("Too many arguments")
"#);

}

#[test]
pub fn recursive_single_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x))) (f 1))");
  assert_eq!(result0.0, "var _locals_1 = _Labels_0.new()\nreturn _locals_1._fn_f_2(1)\n");
  assert_eq!(result0.1, r#"class _Labels_0 extends Reference:
    func _init():
        pass
    func _fn_f_2(x_3):
        return _fn_f_2(x_3)
"#);
}

#[test]
pub fn recursive_double_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (g x)) (g (x) (f x))) (g (f 1)))");
  assert_eq!(result0.0, "var _locals_1 = _Labels_0.new()\nreturn _locals_1._fn_g_3(_locals_1._fn_f_2(1))\n");
  assert_eq!(result0.1, r#"class _Labels_0 extends Reference:
    func _init():
        pass
    func _fn_f_2(x_4):
        return _fn_g_3(x_4)
    func _fn_g_3(x_5):
        return _fn_f_2(x_5)
"#);
}

#[test]
pub fn recursive_single_with_extra_beginning_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f (g x))) (g (x) 10)) (f 1))");
  assert_eq!(result0.0, "var _locals_3 = _Labels_2.new()\nreturn _locals_3._fn_f_4(1)\n");
  assert_eq!(result0.1, r#"static func _flet_0(x_1):
    return 10
class _Labels_2 extends Reference:
    func _init():
        pass
    func _fn_f_4(x_5):
        return _fn_f_4(_flet_0(x_5))
"#);
}

#[test]
pub fn recursive_single_with_extra_end_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x)) (g (x) (f x))) (g 1))");
  assert_eq!(result0.0, "var _locals_1 = _Labels_0.new()\nvar _flet_6 = _LambdaBlock_5.new(_locals_1)\nreturn _flet_6.call_func(1)\n");
  assert_eq!(result0.1, r#"class _Labels_0 extends Reference:
    func _init():
        pass
    func _fn_f_2(x_3):
        return _fn_f_2(x_3)
class _LambdaBlock_5 extends GDLisp.Function:
    var _locals_1
    func _init(_locals_1):
        self._locals_1 = _locals_1
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func(x_4):
        return _locals_1._fn_f_2(x_4)
    func call_funcv(args):
        var required_0 = null
        if args is GDLisp.NilClass:
            push_error("Not enough arguments")
        else:
            required_0 = args.car
            args = args.cdr
        if args is GDLisp.NilClass:
            return call_func(required_0)
        else:
            push_error("Too many arguments")
"#);
}

#[test]
pub fn recursive_single_indirect_labels_test() {
  let result0 = parse_compile_and_output_h("(labels ((f (x) (f x))) (funcall (function f) 1))");
  assert_eq!(result0.0, "var _locals_1 = _Labels_0.new()\nreturn GDLisp.funcall(_FunctionRefBlock_4.new(_locals_1), GDLisp.Cons.new(1, GDLisp.Nil))\n");
  assert_eq!(result0.1, r#"class _Labels_0 extends Reference:
    func _init():
        pass
    func _fn_f_2(x_3):
        return _fn_f_2(x_3)
class _FunctionRefBlock_4 extends GDLisp.Function:
    var _locals_1
    func _init(_locals_1):
        self._locals_1 = _locals_1
        self.__gdlisp_required = 1
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func(arg0):
        return _locals_1._fn_f_2(arg0)
    func call_funcv(args):
        var required_0 = null
        if args is GDLisp.NilClass:
            push_error("Not enough arguments")
        else:
            required_0 = args.car
            args = args.cdr
        if args is GDLisp.NilClass:
            return call_func(required_0)
        else:
            push_error("Too many arguments")
"#);
}
