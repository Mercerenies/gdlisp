
#![allow(dead_code)]

extern crate gdlisp;

pub mod import;

use gdlisp::compile::Compiler;
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::{CodeBuilder, StmtBuilder};
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::compile::symbol_table::local_var::LocalVar;
use gdlisp::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use gdlisp::compile::symbol_table::call_magic::DefaultCall;
use gdlisp::compile::preload_resolver::DefaultPreloadResolver;
use gdlisp::runner;
use gdlisp::runner::into_gd_file::IntoGDFile;
use gdlisp::runner::path::{RPathBuf, PathSrc};
use gdlisp::parser;
use gdlisp::ir;
use gdlisp::ir::incremental::IncCompiler;
use gdlisp::gdscript::library;
use gdlisp::gdscript::decl;
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::config::ProjectConfig;

use tempfile::{Builder, TempDir};

use std::path::PathBuf;
use std::fs::{File, copy};
use std::io::{self, Write};
use std::str::FromStr;

pub const BEGIN_GDLISP_TESTS: &'static str = "__BEGIN_GDLISP_TESTS__";

/*
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
*/

pub fn dummy_config() -> ProjectConfig {
  ProjectConfig { root_directory: PathBuf::from_str(".").unwrap(), optimizations: false } // Infallible
}

pub fn dummy_pipeline() -> Pipeline {
  let mut pipeline = Pipeline::new(dummy_config());
  let path = RPathBuf::new(PathSrc::Res, PathBuf::from("TEST.lisp")).unwrap();
  pipeline.set_currently_loading_file(path);
  pipeline
}

fn bind_helper_symbols(table: &mut SymbolTable) {
  // TODO This is just a single-argument shim which calls print. It
  // will be obsolete once we have an actual print function in the
  // language.
  table.set_fn(String::from("print"), FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Global, String::from("print")), Box::new(DefaultCall));
}

pub fn dump_files<T>(dir: &mut TempDir, data: &T) -> io::Result<()>
where T : IntoGDFile + ?Sized {

  // The target file itself
  let mut target_file = File::create(dir.path().join("TEST.gd"))?;
  data.write_to_gd(&mut target_file)?;

  // The runner shim
  let mut temp_file = File::create(dir.path().join("main.tscn"))?;
  write!(temp_file, r#"
[gd_scene load_steps=2 format=2]

[ext_resource path="res://main.gd" type="Script" id=1]

[node name="main" type="Node"]
script = ExtResource( 1 )
"#)?;
  let mut temp_scr_file = File::create(dir.path().join("main.gd"))?;
  write!(temp_scr_file, r#"
extends Node

func _ready():
    var file = load("res://TEST.gd")
    print("{}")
    file.run()
    get_tree().quit()

"#, BEGIN_GDLISP_TESTS)?;

  // The GDLisp.gd file
  copy("GDLisp.gd", dir.path().join("GDLisp.gd"))?;

  // Project file
  let mut project_file = File::create(dir.path().join("project.godot"))?;
  write!(project_file, r#"
config_version=4

[application]

run/main_scene="res://main.tscn"

[autoload]

GDLisp="*res://GDLisp.gd"

"#)?;

  Ok(())

}

// TODO As in compiler_test.rs, it would be ideal if this would return
// some type of unified error (parse, compile, and IO can all err
// here) rather than panic.
pub fn parse_and_run(input: &str) -> String {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver));
  let mut table = SymbolTable::new();
  bind_helper_symbols(&mut table);
  library::bind_builtins(&mut table, false);

  let mut pipeline = dummy_pipeline();

  let (decls, _macros) = ir::compile_toplevel(&mut pipeline, &value).unwrap();
  let mut builder = CodeBuilder::new(decl::ClassExtends::named(String::from("Reference")));
  compiler.compile_toplevel(&mut pipeline, &mut builder, &mut table, &decls).unwrap();

  let mut temp_dir = Builder::new().prefix("__gdlisp_test").rand_bytes(5).tempdir().unwrap();
  let code_output = builder.build();
  // println!("{}", code_output.to_gd());
  dump_files(&mut temp_dir, &code_output).unwrap();
  let result = runner::run_project(temp_dir).unwrap();

  match result.find(BEGIN_GDLISP_TESTS) {
    None => result,
    Some(idx) => result[idx + BEGIN_GDLISP_TESTS.bytes().count()..].to_owned(),
  }

}

fn bind_helper_symbols_comp(table: &mut SymbolTable) {
  // Binds a few helper names to the symbol table for the sake of
  // debugging.
  table.set_fn(String::from("foo"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("foo")), Box::new(DefaultCall));
  table.set_fn(String::from("foo1"), FnCall::file_constant(FnSpecs::new(1, 0, None), FnScope::Global, String::from("foo1")), Box::new(DefaultCall));
  table.set_fn(String::from("foo2"), FnCall::file_constant(FnSpecs::new(2, 0, None), FnScope::Global, String::from("foo2")), Box::new(DefaultCall));
  table.set_fn(String::from("bar"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("bar")), Box::new(DefaultCall));
  table.set_fn(String::from("bar"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("bar")), Box::new(DefaultCall));
  table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
  table.set_var(String::from("glob"), LocalVar::file_constant(String::from("glob")));
}

// TODO Currently, this panics if it fails. This is okay-ish, since
// it's only being used for tests. But once we unify all of our errors
// (so we can represent parse errors and compile errors with one
// common type), we should return a Result<...> here.
pub fn parse_compile_and_output(input: &str) -> String {
  parse_compile_and_output_h(input).0
}

pub fn parse_compile_and_output_h(input: &str) -> (String, String) {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver));
  let mut table = SymbolTable::new();
  bind_helper_symbols_comp(&mut table);
  library::bind_builtins(&mut table, false);

  let mut pipeline = dummy_pipeline();

  let mut builder = StmtBuilder::new();
  let value = {
    let mut icompiler = IncCompiler::new(value.all_symbols());
    icompiler.bind_builtin_macros(&mut pipeline);
    icompiler.compile_expr(&mut pipeline, &value)
  }.unwrap();
  let () = compiler.compile_stmt(&mut pipeline, &mut builder, &mut table, &mut stmt_wrapper::Return, &value).unwrap();
  let (stmts, helpers) = builder.build();
  let a = stmts.into_iter().map(|stmt| stmt.to_gd(0)).collect::<String>();
  let b = helpers.into_iter().map(|decl| decl.to_gd(0)).collect::<String>();
  (a, b)
}


// TODO Make this return a Result<...> as well.
pub fn parse_compile_decl(input: &str) -> String {
  let parser = parser::ASTParser::new();
  let value = parser.parse(input).unwrap();
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver));
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table, false);

  let mut pipeline = dummy_pipeline();

  let mut builder = CodeBuilder::new(decl::ClassExtends::named("Reference".to_owned()));
  let (decls, _macros) = ir::compile_toplevel(&mut pipeline, &value).unwrap();
  compiler.compile_toplevel(&mut pipeline, &mut builder, &mut table, &decls).unwrap();
  let class = builder.build();

  class.to_gd()

}
