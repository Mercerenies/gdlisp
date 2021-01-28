
extern crate gdlisp;

use gdlisp::ir;
use gdlisp::compile::Compiler;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::compile::body::builder::CodeBuilder;
use gdlisp::gdscript::decl;
use gdlisp::gdscript::library;
use gdlisp::parser;

// TODO Make this return a Result<...> as well.
fn parse_compile_and_output(input: &str) -> String {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Reference".to_owned()));
  let decls = ir::compile_toplevel(&value).unwrap();
  compiler.compile_decls(&mut builder, &mut table, &decls).unwrap();
  let class = builder.build();

  class.to_gd()

}

#[test]
pub fn empty_class_test() {
  assert_eq!(parse_compile_and_output("()"), "extends Reference\nstatic func run():\n    return GDLisp.Nil\n");
}

#[test]
pub fn simple_function_declaration_test() {
  assert_eq!(parse_compile_and_output("((defn foo (x) x))"),
             "extends Reference\nstatic func foo(x_0):\n    return x_0\nstatic func run():\n    return GDLisp.Nil\n");
}

#[test]
pub fn lambda_in_function_declaration_test() {
  assert_eq!(parse_compile_and_output("((defn foo (x) (lambda () x) x))"), r#"extends Reference
class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func():
        return x_0
    func call_funcv(args):
        if args is GDLisp.NilClass:
            return call_func()
        else:
            push_error("Too many arguments")
static func foo(x_0):
    return x_0
static func run():
    return GDLisp.Nil
"#);
}

#[test]
pub fn closed_rw_in_function_declaration_test() {
  assert_eq!(parse_compile_and_output("((defn foo (x) (lambda () (setq x 1)) x))"), r#"extends Reference
class _LambdaBlock_1 extends GDLisp.Function:
    var x_0
    func _init(x_0):
        self.x_0 = x_0
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = false
    func call_func():
        x_0.contents = 1
        return x_0.contents
    func call_funcv(args):
        if args is GDLisp.NilClass:
            return call_func()
        else:
            push_error("Too many arguments")
static func foo(x_0):
    x_0 = GDLisp.Cell.new(x_0)
    return x_0.contents
static func run():
    return GDLisp.Nil
"#);
}

#[test]
pub fn mutually_recursive_test() {
  assert_eq!(parse_compile_and_output("((defn foo () (bar)) (defn bar () (foo)))"), r#"extends Reference
static func foo():
    return bar()
static func bar():
    return foo()
static func run():
    return GDLisp.Nil
"#);
}

#[test]
#[should_panic]
pub fn nonexistent_function_test() {
  parse_compile_and_output("((defn foo () (bar)))");
}
