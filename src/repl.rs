
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::pipeline::error::{PError, IOError};
use crate::pipeline::config::ProjectConfig;
use crate::runner::path::RPathBuf;
use crate::sxp::ast::AST;
use crate::ir;
use crate::ir::main_function::StaticMainFunctionHandler;
use crate::ir::arglist::ordinary::ArgList;
use crate::compile::Compiler;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::body::class_scope::OutsideOfClass;
use crate::compile::symbol_table::SymbolTable;
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

pub struct Repl {
  pipeline: Pipeline,
}

impl Repl {

  const REPL_FILENAME: &'static str = "/tmp/REPL.lisp";

  pub fn new(config: ProjectConfig) -> Repl {
    Repl::with_pipeline(Pipeline::new(config))
  }

  pub fn with_pipeline(pipeline: Pipeline) -> Repl {
    Repl { pipeline }
  }

  pub fn run_code(&mut self, code: &AST) -> Result<String, PError> {
    self.pipeline.set_currently_loading_file(RPathBuf::try_from(String::from(Repl::REPL_FILENAME)).unwrap());

    let mut compiler = Compiler::new(FreshNameGenerator::new(code.all_symbols()), Box::new(DefaultPreloadResolver));

    let (ir, _macros) = ir::compile_and_check(&mut self.pipeline, code, &Repl::main_function_handler())?;

    if ir.minimalist_flag {
      return Err(PError::from(GDError::new(GDErrorF::MinimalistAtRepl, SourceOffset(0))));
    }

    let mut table = SymbolTable::new();
    library::bind_builtins(&mut table, false);

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

    write!(tmpfile, "{}", result.to_gd()).map_err(|err| IOError::new(err, SourceOffset(0)))?;
    tmpfile.flush().map_err(|err| IOError::new(err, SourceOffset(0)))?;

    let server = self.pipeline.get_server_mut();
    let macro_id =
      server.stand_up_macro(String::from(REPL_FUNCTION_NAME), ArgList::empty(), tmpfile)
      .map_err(|err| IOError::new(err, SourceOffset(0)))?;

    server.run_server_file_str(macro_id, vec!(), vec!(), SourceOffset(0))
  }

  pub fn parse_and_run_code(&mut self, code: &str) -> Result<String, PError> {
    let ast = SOME_AST_PARSER.parse(code)?;
    self.run_code(&ast)
  }

  fn main_function_handler() -> StaticMainFunctionHandler {
    StaticMainFunctionHandler::new(String::from(REPL_FUNCTION_NAME))
  }

}
