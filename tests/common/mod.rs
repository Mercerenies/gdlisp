
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

use tempfile::{Builder, TempDir};

use std::fs::{File, copy};
use std::io::{self, Write};

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

fn bind_helper_symbols(table: &mut SymbolTable) {
  // TODO This is just a single-argument shim which calls print. It
  // will be obsolete once we have an actual print function in the
  // language.
  table.set_fn(String::from("print"), FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, String::from("print")));
}

pub fn dump_files<T>(dir: &mut TempDir, data: &T) -> io::Result<()>
where T : IntoGDFile + ?Sized {

  // The target file itself
  let mut target_file = File::create(dir.path().join("target.gd"))?;
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
    var file = load("res://target.gd")
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
  let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
  let mut table = SymbolTable::new();
  bind_helper_symbols(&mut table);
  library::bind_builtins(&mut table);

  let decls = ir::compile_toplevel(&value).unwrap();
  let mut builder = CodeBuilder::new(decl::ClassExtends::Named(String::from("Reference")));
  compiler.compile_decls(&mut builder, &mut table, &decls).unwrap();

  let mut temp_dir = Builder::new().prefix("__gdlisp_test").rand_bytes(5).tempdir().unwrap();
  let code_output = builder.build();
  //println!("{}", code_output.to_gd());
  dump_files(&mut temp_dir, &code_output).unwrap();
  let result = runner::run_project(temp_dir).unwrap();

  match result.find(BEGIN_GDLISP_TESTS) {
    None => result,
    Some(idx) => result[idx + BEGIN_GDLISP_TESTS.bytes().count()..].to_owned(),
  }

}
