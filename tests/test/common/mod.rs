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

#![allow(dead_code)]

extern crate gdlisp;

pub mod import;

use gdlisp::compile::Compiler;
use gdlisp::compile::stmt_wrapper;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::body::builder::{CodeBuilder, StmtBuilder};
use gdlisp::compile::body::class_scope::OutsideOfClass;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::compile::symbol_table::local_var::LocalVar;
use gdlisp::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use gdlisp::compile::symbol_table::call_magic::CallMagic;
use gdlisp::compile::preload_resolver::DefaultPreloadResolver;
use gdlisp::runner::godot::GodotCommand;
use gdlisp::runner::into_gd_file::IntoGDFile;
use gdlisp::runner::path::{RPathBuf, PathSrc};
use gdlisp::runner::version::VersionInfo;
use gdlisp::AST_PARSER;
use gdlisp::ir;
use gdlisp::ir::incremental::IncCompiler;
use gdlisp::ir::main_function::{StaticMainFunctionHandler, DisallowMainFunctionHandler};
use gdlisp::gdscript::library;
use gdlisp::gdscript::decl;
use gdlisp::gdscript::class_extends::ClassExtends;
use gdlisp::gdscript::spacing::SpacedDeclPrinter;
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::error::{PError, IOError};
use gdlisp::pipeline::config::ProjectConfig;
use gdlisp::pipeline::source::SourceOffset;

use tempfile::{Builder, TempDir};

use std::process::{Output, Stdio};
use std::path::PathBuf;
use std::fs::{File, copy};
use std::io::{self, Write};
use std::str::FromStr;

pub const TEST_FUNCTION_NAME: &'static str = "run_test";

pub const BEGIN_GDLISP_TESTS: &'static str = "__BEGIN_GDLISP_TESTS__";

/// A type containing string output from both `stdout` and `stderr`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringOutput {
  pub stdout: String,
  pub stderr: String,
}

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

fn main_function_handler() -> StaticMainFunctionHandler {
  StaticMainFunctionHandler::new(TEST_FUNCTION_NAME.to_owned())
}

pub fn dummy_config() -> ProjectConfig {
  ProjectConfig {
    root_directory: PathBuf::from_str(".").unwrap(), // Infallible
    optimizations: false,
    godot_version: VersionInfo::default(),
  }
}

pub fn dummy_pipeline() -> Pipeline {
  let mut pipeline = Pipeline::new(dummy_config());
  let path = RPathBuf::new(PathSrc::Res, PathBuf::from("TEST.lisp")).unwrap();
  pipeline.set_currently_loading_file(path);
  pipeline
}

fn bind_helper_symbols(_table: &mut SymbolTable) {
  // Does nothing right now. May remove this later.
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
    file.run_test()
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

fn parse_and_run_err_impl(input: &str, runner: &mut GodotCommand) -> Result<Output, PError> {
  let value = AST_PARSER.parse(input)?;
  let used_names = value.all_symbols();
  let mut table = SymbolTable::new();
  bind_helper_symbols(&mut table);
  library::bind_builtins(&mut table, false);

  let mut pipeline = dummy_pipeline();

  let (decls, _macros) = ir::compile_and_check(&mut pipeline, &value, &main_function_handler())?;
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver), decls.minimalist_flag);
  let mut builder = CodeBuilder::new(ClassExtends::SimpleIdentifier(String::from("Reference")));
  compiler.frame(&mut pipeline, &mut builder, &mut table, &mut OutsideOfClass).compile_toplevel(&decls)?;

  let mut temp_dir = Builder::new().prefix("__gdlisp_test").rand_bytes(5).tempdir().map_err(|err| IOError::new(err, SourceOffset(0)))?;
  let code_output = builder.build();
  // println!("{}", code_output.to_gd());
  dump_files(&mut temp_dir, &code_output).map_err(|err| IOError::new(err, SourceOffset(0)))?;

  runner.project_dir(temp_dir.path()).output()
    .map_err(|err| PError::from(IOError::new(err, SourceOffset(0))))
}

fn strip_cr(input: &str) -> String {
  // Windows outputs \r\n and Linux outputs \n. We strip \r so our
  // tests can look for \n in every case.
  input.replace("\r", "")
}

pub fn parse_and_run_err(input: &str) -> Result<String, PError> {
  let mut runner = GodotCommand::base();
  let Output { stdout, .. } = parse_and_run_err_impl(input, &mut runner)?;
  let result = String::from_utf8_lossy(&stdout);
  match result.find(BEGIN_GDLISP_TESTS) {
    None => Ok(strip_cr(&result)),
    Some(idx) => Ok(strip_cr(&result[idx + BEGIN_GDLISP_TESTS.bytes().count()..])),
  }
}

pub fn parse_and_run(input: &str) -> String {
  parse_and_run_err(input).unwrap()
}

pub fn parse_and_run_with_stderr_err(input: &str) -> Result<StringOutput, PError> {
  let mut runner = GodotCommand::base();
  runner.stderr(Stdio::piped());
  let Output { stdout, stderr, .. } = parse_and_run_err_impl(input, &mut runner)?;

  let stdout = String::from_utf8_lossy(&stdout);
  let stderr = String::from_utf8_lossy(&stderr);

  let stdout = match stdout.find(BEGIN_GDLISP_TESTS) {
    None => strip_cr(&stdout),
    Some(idx) => strip_cr(&stdout[idx + BEGIN_GDLISP_TESTS.bytes().count()..]),
  };
  let stderr = strip_cr(&stderr);

  Ok(StringOutput { stdout, stderr })
}

pub fn parse_and_run_with_stderr(input: &str) -> StringOutput {
  parse_and_run_with_stderr_err(input).unwrap()
}

fn bind_helper_symbols_comp(table: &mut SymbolTable) {
  // Binds a few helper names to the symbol table for the sake of
  // debugging.
  table.set_fn(String::from("foo"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("foo")), CallMagic::DefaultCall);
  table.set_fn(String::from("foo1"), FnCall::file_constant(FnSpecs::new(1, 0, None), FnScope::Global, String::from("foo1")), CallMagic::DefaultCall);
  table.set_fn(String::from("foo2"), FnCall::file_constant(FnSpecs::new(2, 0, None), FnScope::Global, String::from("foo2")), CallMagic::DefaultCall);
  table.set_fn(String::from("bar"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("bar")), CallMagic::DefaultCall);
  table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
  table.set_var(String::from("glob"), LocalVar::file_constant(String::from("glob")));
}

pub fn parse_compile_and_output_err(input: &str) -> Result<String, PError> {
  parse_compile_and_output_err_h(input).map(|x| x.0)
}

pub fn parse_compile_and_output_err_h(input: &str) -> Result<(String, String), PError> {
  let value = AST_PARSER.parse(input)?;
  let used_names = value.all_symbols();
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver), false);
  let mut table = SymbolTable::new();
  bind_helper_symbols_comp(&mut table);
  library::bind_builtins(&mut table, false);

  let mut pipeline = dummy_pipeline();

  let mut builder = StmtBuilder::new();
  let value = {
    let mut icompiler = IncCompiler::new(value.all_symbols());
    icompiler.bind_builtin_macros(&mut pipeline);
    let expr = icompiler.compile_expr(&mut pipeline, &value)?;
    ir::loops::check_expr(&expr)?;
    expr
  };
  {
    let mut class_scope = OutsideOfClass;
    let mut frame = compiler.frame(&mut pipeline, &mut builder, &mut table, &mut class_scope);
    let () = frame.compile_stmt(&mut stmt_wrapper::Return, &value)?;
  }
  let (stmts, helpers) = builder.build();
  let a = stmts.into_iter().map(|stmt| stmt.to_gd(0)).collect::<String>();
  let b = output_decls(helpers);
  Ok((a, b))
}

fn output_decls(decls: Vec<decl::Decl>) -> String {
  let printer = SpacedDeclPrinter::new();
  let mut string = String::new();
  printer.write_gd(&mut string, decls.iter()).unwrap();
  string
}

pub fn parse_compile_and_output(input: &str) -> String {
  parse_compile_and_output_err(input).unwrap()
}

pub fn parse_compile_and_output_h(input: &str) -> (String, String) {
  parse_compile_and_output_err_h(input).unwrap()
}

pub fn parse_compile_decl_err(input: &str) -> Result<String, PError> {
  let value = AST_PARSER.parse(input)?;
  let used_names = value.all_symbols();
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table, false);

  let mut pipeline = dummy_pipeline();

  let mut builder = CodeBuilder::new(ClassExtends::SimpleIdentifier("Reference".to_owned()));
  let (decls, _macros) = ir::compile_and_check(&mut pipeline, &value, &DisallowMainFunctionHandler)?;
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver), decls.minimalist_flag);
  compiler.frame(&mut pipeline, &mut builder, &mut table, &mut OutsideOfClass).compile_toplevel(&decls)?;
  let class = builder.build();

  Ok(class.to_gd())

}

pub fn parse_compile_decl(input: &str) -> String {
  parse_compile_decl_err(input).unwrap()
}
