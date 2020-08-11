
extern crate gdlisp;

use gdlisp::compile::Compiler;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::StmtBuilder;
use gdlisp::gdscript::{expr, stmt};
use gdlisp::parser;

fn as_return(expr: expr::Expr) -> stmt::Stmt {
  stmt::Stmt::ReturnStmt(expr)
}

// TODO Currently, this panics if it fails. This is okay-ish, since
// it's only being used for tests. But once we unify all of our errors
// (so we can represent parse errors and compile errors with one
// common type), we should return a Result<...> here.
fn parse_compile_and_output(input: &str) -> String {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));

  let mut builder = StmtBuilder::new();
  let () = compiler.compile_statement(&mut builder, as_return, &value).unwrap();
  // TODO Print helpers here, too
  let (stmts, _) = builder.build();
  stmts.into_iter().map(|stmt| stmt.to_gd(0)).collect::<String>()
}

#[test]
pub fn expr_tests() {
  assert_eq!(parse_compile_and_output("100"), "return 100\n");
  assert_eq!(parse_compile_and_output("(progn 100 200 300)"), "100\n200\nreturn 300\n");
}
