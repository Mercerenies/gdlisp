
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::pipeline::error::{PError, IOError};
use crate::pipeline::config::ProjectConfig;
use crate::runner::path::RPathBuf;
use crate::sxp::ast::AST;
use crate::ir;
use crate::ir::incremental::IncCompiler;
use crate::ir::macros::MacroData;
use crate::ir::identifier::{Namespace, Id};
use crate::ir::main_function::StaticMainFunctionHandler;
use crate::ir::arglist::ordinary::ArgList;
use crate::compile::Compiler;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::body::class_scope::OutsideOfClass;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::symbol_table::call_magic::CallMagic;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::preload_resolver::DefaultPreloadResolver;
use crate::compile::error::{GDError, GDErrorF};
use crate::gdscript::library;
use crate::gdscript::class_extends::ClassExtends;
use crate::gdscript::metadata::REPL_FUNCTION_NAME;
use crate::optimize::gdscript::run_standard_passes;
use crate::SOME_AST_PARSER;

use tempfile::Builder;

use std::convert::TryFrom;
use std::io::Write;
use std::path::Path;
use std::collections::HashMap;

/// A `Repl` instance maintains the state of a read-eval-print loop.
/// Specifically, it maintains a running Godot instance
/// (lazily-initialized, as always), as well as a table of all known
/// symbols from *previous* REPL commands.
///
/// Commands are sent to a `Repl` via [`Repl::run_code`] or
/// [`Repl::parse_and_run_code`].
pub struct Repl {
  pipeline: Pipeline,
  full_symbol_table: SymbolTable,
  full_macro_table: HashMap<Id, MacroData>,
}

impl Repl {

  const REPL_FILENAME: &'static str = "/tmp/REPL.lisp";

  pub fn new(config: ProjectConfig) -> Repl {
    Repl::with_pipeline(Pipeline::new(config))
  }

  pub fn with_pipeline(pipeline: Pipeline) -> Repl {
    Repl {
      pipeline,
      full_symbol_table: SymbolTable::new(),
      full_macro_table: HashMap::new(),
    }
  }

  /// Normally, the macro server and all of the subsystems are lazy
  /// and only load when requested. However, this creates the awkward
  /// effect where the REPL loads fast but then the first command (and
  /// only the first command) takes a long time to run. This function
  /// can be called to force the subsystems to load immediately.
  pub fn force_load(&mut self) {
    self.parse_and_run_code("()").expect("Internal error in force_load");
  }

  /// Runs the code given by the AST as GDLisp source. The result is
  /// returned. In case of success, the result shall be the string
  /// representation of the value produced by the expression. On
  /// failure, the result shall be the error message.
  ///
  /// The top-level AST shall be a list, or a GDLisp error will be
  /// returned. The list can consist of a mix of expressions and
  /// declarations freely. The declarations will be put into scope
  /// first (in the order they are declared), and then (and *only*
  /// then) the expressions will be evaluated in order. If there are
  /// no expressions and the declarations are successfully compiled,
  /// then a string representation of the nil list will be returned as
  /// a default value.
  ///
  /// `run_code` exhibits a strong exception guarantee. If this
  /// function returns an error, then the [`Repl`] object it was
  /// called on has not been modified. On the other hand, if this
  /// function returns an `Ok` value, then the `Repl` object's
  /// internal symbol table will have any new declarations from the
  /// given code added to it.
  ///
  /// For a version that parses the code and *then* runs it, see
  /// [`parse_and_run_code`](Repl::parse_and_run_code).
  pub fn run_code(&mut self, code: &AST) -> Result<String, PError> {
    self.pipeline.set_currently_loading_file(RPathBuf::try_from(String::from(Repl::REPL_FILENAME)).unwrap());

    let mut compiler = Compiler::new(FreshNameGenerator::new(code.all_symbols()), Box::new(DefaultPreloadResolver), false);

    let mut icompiler = IncCompiler::with_ambient_symbols(code.all_symbols(), self.full_symbol_table.clone()); // TODO Don't like this clone here, symbol tables are big and cloning is expensive.
    icompiler.bind_macros_from(self.full_macro_table.iter().map(|(id, data)| (id.clone(), data.clone())));
    let (ir, macros) = icompiler.compile_toplevel(&mut self.pipeline, code, &Repl::main_function_handler())?;
    ir::check_ir(&ir)?;

    if ir.minimalist_flag {
      return Err(PError::from(GDError::new(GDErrorF::MinimalistAtRepl, SourceOffset(0))));
    }

    let mut table = SymbolTable::new();
    library::bind_builtins(&mut table, false);
    self.bind_existing_names(&mut table);

    let mut builder = CodeBuilder::new(ClassExtends::SimpleIdentifier("Node".to_owned()));
    compiler.frame(&mut self.pipeline, &mut builder, &mut table, &mut OutsideOfClass).compile_toplevel(&ir)?;
    let mut result = builder.build();
    if self.pipeline.config().optimizations {
      run_standard_passes(&mut result)?;
    }

    let mut tmpfile = Builder::new()
      .prefix("__gdlisp_replfile")
      .suffix(".gd")
      .rand_bytes(5)
      .tempfile()
      .map_err(|err| IOError::new(err, SourceOffset(0)))?;
    let tmpfile_name = tmpfile.path().to_owned();

    write!(tmpfile, "{}", result.to_gd()).map_err(|err| IOError::new(err, SourceOffset(0)))?;
    tmpfile.flush().map_err(|err| IOError::new(err, SourceOffset(0)))?;

    let server = self.pipeline.get_server_mut();
    let macro_id =
      server.stand_up_macro(String::from(REPL_FUNCTION_NAME), ArgList::empty(), tmpfile)
      .map_err(|err| IOError::new(err, SourceOffset(0)))?;

    let final_result = server.run_server_file_str(macro_id, vec!(), vec!(), SourceOffset(0))?;

    // If everything happened correctly, then save the symbols we got
    // from this line.
    self.remember_symbols(&tmpfile_name, &mut table, &ir::export::get_export_list(&ir.decls));
    self.full_macro_table.extend(macros.into_iter());

    Ok(final_result)
  }

  /// Parse the code as one or more GDLisp S-expressions and then run
  /// the result in the REPL.
  ///
  /// See [`Repl::run_code`] for more details on the effects running
  /// code has on the [`Repl`] object itself.
  pub fn parse_and_run_code(&mut self, code: &str) -> Result<String, PError> {
    let ast = SOME_AST_PARSER.parse(code)?;
    self.run_code(&ast)
  }

  fn remember_symbols(&mut self, filename: &Path, table: &mut SymbolTable, exports: &[Id]) {
    let direct_load_import = VarName::DirectLoad(filename.to_string_lossy().into_owned());

    for name in exports {
      match name.namespace {
        Namespace::Value => {
          let mut var = table.get_var(&name.name).unwrap().to_owned();
          var.name = var.name.into_imported_var(direct_load_import.clone());
          self.full_symbol_table.set_var(name.name.to_owned(), var);
        }
        Namespace::Function => {
          let mut func = table.get_fn(&name.name).unwrap().0.to_owned();
          func.object = func.object.into_imported_var(direct_load_import.clone());
          self.full_symbol_table.set_fn(name.name.to_owned(), func, CallMagic::DefaultCall);
        }
      }
    }
  }

  fn bind_existing_names(&self, table: &mut SymbolTable) {
    table.assign_from(&self.full_symbol_table);
  }

  fn main_function_handler() -> StaticMainFunctionHandler {
    StaticMainFunctionHandler::new(String::from(REPL_FUNCTION_NAME))
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::runner::version::VersionInfo;
  use std::path::PathBuf;

  fn dummy_config() -> ProjectConfig {
    ProjectConfig {
      root_directory: PathBuf::from("."),
      optimizations: false,
      godot_version: VersionInfo::default(),
    }
  }

  #[test]
  fn basic_repl_test() {
    let mut repl = Repl::new(dummy_config());
    assert_eq!(repl.parse_and_run_code("(+ 1 1)"), Ok(String::from("2")));
  }

  #[test]
  fn decl_then_expr_in_one_repl_command_test() {
    let mut repl = Repl::new(dummy_config());
    assert_eq!(repl.parse_and_run_code("(defn foo (x) (+ x 1)) (foo 100)"), Ok(String::from("101")));
  }

  #[test]
  fn decl_then_expr_in_two_repl_commands_test() {
    let mut repl = Repl::new(dummy_config());
    assert_eq!(repl.parse_and_run_code("(defn foo (x) (+ x 1))"), Ok(String::from("()")));
    assert_eq!(repl.parse_and_run_code("(foo 100)"), Ok(String::from("101")));
  }

  #[test]
  fn failed_decl_then_expr_repl_test() {
    let mut repl = Repl::new(dummy_config());
    assert_eq!(repl.parse_and_run_code("(defn foo (x) (+ x 1)) nonexistent-variable"),
               Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("nonexistent-variable")), SourceOffset(23)))));
    assert_eq!(repl.parse_and_run_code("(foo 100)"),
               Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("foo")), SourceOffset(0)))));

  }

  #[test]
  fn failed_repl_continues_test() {
    let mut repl = Repl::new(dummy_config());
    assert_eq!(repl.parse_and_run_code("(defn foo (x) (+ x 1))"), Ok(String::from("()")));
    assert_eq!(repl.parse_and_run_code("nonexistent-variable"),
               Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("nonexistent-variable")), SourceOffset(0)))));
    assert_eq!(repl.parse_and_run_code("(foo 100)"), Ok(String::from("101")));

  }

  #[test]
  fn minimalist_at_repl_test() {
    let mut repl = Repl::new(dummy_config());
    assert_eq!(repl.parse_and_run_code("(sys/nostdlib)"),
               Err(PError::from(GDError::new(GDErrorF::MinimalistAtRepl, SourceOffset(0)))));
  }

}
