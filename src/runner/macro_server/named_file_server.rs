
use crate::sxp::ast::AST;
use crate::runner::macro_server::lazy::LazyServer;
use crate::compile::names;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic::compile_default_call;
use crate::gdscript::expr::{Expr as GDExpr};
use crate::ir::arglist::ArgList;
use crate::pipeline::error::{Error as PError};
use crate::parser;
use super::command::ServerCommand;

use tempfile::NamedTempFile;

use std::collections::HashMap;
use std::path::Path;
use std::io;

pub struct NamedFileServer {
  server: LazyServer,
  macro_files: HashMap<String, (MacroCall, NamedTempFile)>,
}

#[derive(Clone, Debug)]
pub struct MacroCall {
  pub index: u32, // Index in the lookup table on the GDScript side
  pub original_name: String, // Probably not needed, but we have it so we may as well keep track of it.
  pub name: String,
}

#[allow(clippy::new_without_default)]
impl NamedFileServer {

  pub fn new() -> NamedFileServer {
    NamedFileServer {
      server: LazyServer::new(),
      macro_files: HashMap::new(),
    }
  }

  fn load_file_on_server(&mut self, path: &Path) -> io::Result<u32> {
    let server = self.server.get_mut()?;
    let cmd = ServerCommand::Load((*path.to_string_lossy()).to_owned());
    let result = server.issue_command(&cmd)?;
    result.parse().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
  }

  pub fn stand_up_file(&mut self, name: String, file: NamedTempFile) -> io::Result<()> {
    let idx = self.load_file_on_server(file.path())?;
    let gdname = names::lisp_to_gd(&name);
    let call = MacroCall { index: idx, original_name: name.to_owned(), name: gdname };
    self.macro_files.insert(name, (call, file));
    Ok(())
  }

  pub fn get_file(&self, name: &str) -> Option<&MacroCall> {
    self.macro_files.get(name).map(|x| &x.0)
  }

  pub fn run_server_file(&mut self, call: &MacroCall, parms: ArgList, args: Vec<GDExpr>)
                         -> Result<AST, PError> {
    let specs = FnSpecs::from(parms);
    let call_object =
      GDExpr::Subscript(
        Box::new(GDExpr::Attribute(Box::new(GDExpr::var("MAIN")), String::from("loaded_files"))),
        Box::new(GDExpr::from(call.index as i32)),
      );
    let call = FnCall {
      scope: FnScope::Global,
      object: Some(Box::new(call_object)),
      function: call.name.to_owned(),
      specs: specs,
    };
    let server = self.server.get_mut()?;
    let eval_str = compile_default_call(call, args)?.to_gd();
    let result = server.issue_command(&ServerCommand::Eval(eval_str))?;
    let parser = parser::ASTParser::new();
    let parsed = parser.parse(&result)?;
    Ok(parsed)
  }

}
