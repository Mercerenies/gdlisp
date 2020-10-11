
extern crate gdlisp;

use gdlisp::compile::Compiler;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::CodeBuilder;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use gdlisp::runner;
use gdlisp::runner::into_gd_file::IntoGDFile;
use gdlisp::parser;
use gdlisp::ir;
use gdlisp::gdscript::library;
use gdlisp::gdscript::decl;

use tempfile::{Builder, NamedTempFile};

use std::path::Path;
use std::io::{self, Write};

pub const BEGIN_GDLISP_TESTS: &'static str = "__BEGIN_GDLISP_TESTS__";

fn template_contents<P : AsRef<Path>>(filename: P) -> String {
  format!(r#"
extends SceneTree

func _init():
    var file = load("{}")
    print("{}")
    file.run()
"#, filename.as_ref().display(), BEGIN_GDLISP_TESTS)
}

pub fn write_to_file<T>(data: &T) -> io::Result<NamedTempFile>
where T : IntoGDFile + ?Sized {
  let mut tmp = Builder::new()
    .prefix("__gdlisp_test")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile()?;
  data.write_to_gd(&mut tmp)?;
  tmp.flush()?;
  Ok(tmp)
}

pub fn run_temporary<T>(data: &T) -> io::Result<String>
where T : IntoGDFile + ?Sized {
  let temp_file = write_to_file(data)?;
  let runner_text = template_contents(temp_file.path());
  runner::run_with_temporary(&runner_text)
}

fn bind_helper_symbols(table: &mut SymbolTable) {
  // TODO This is just a single-argument shim which calls print. It
  // will be obsolete once we have an actual print function in the
  // language.
  table.set_fn(String::from("print"), FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, String::from("print")));
}

// TODO As in compiler_test.rs, it would be ideal if this would return
// some type of unified error (parse, compile, and IO can all err
// here) rather than panic.
pub fn parse_and_run(input: &str) -> String {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
  let mut table = SymbolTable::new();
  bind_helper_symbols(&mut table);
  library::bind_builtins(&mut table);

  let decls = ir::compile_toplevel(&value).unwrap();
  let mut builder = CodeBuilder::new(decl::ClassExtends::Named(String::from("Reference")));
  compiler.compile_decls(&mut builder, &mut table, &decls).unwrap();
  run_temporary(&builder.build()).unwrap()
}
