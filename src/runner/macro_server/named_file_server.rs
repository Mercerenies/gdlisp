
use crate::sxp::ast::AST;
use crate::runner::macro_server::lazy::LazyServer;
use crate::compile::names;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic::compile_default_call;
use crate::gdscript::expr::{Expr as GDExpr};
use crate::ir::arglist::ArgList;
use crate::pipeline::error::{Error as PError};
use crate::parser;
use super::command::ServerCommand;
use super::response;

use tempfile::NamedTempFile;

use std::collections::HashMap;
use std::path::Path;
use std::io;
use std::convert::TryFrom;

pub struct NamedFileServer {
  server: LazyServer,
  macro_files: HashMap<MacroID, (MacroCall, NamedTempFile)>,
  next_id: MacroID,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Default)]
#[repr(transparent)]
pub struct MacroID(pub u32);

#[derive(Clone, Debug)]
pub struct MacroCall {
  pub index: u32, // Index in the lookup table on the GDScript side
  pub original_name: String, // Probably not needed, but we have it so we may as well keep track of it.
  pub name: String,
}

// TODO Make this return GDError like it probably should.
fn response_to_string(response: response::ServerResponse) -> io::Result<String> {
  String::try_from(response).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

#[allow(clippy::new_without_default)]
impl NamedFileServer {

  pub fn new() -> NamedFileServer {
    NamedFileServer {
      server: LazyServer::new(),
      macro_files: HashMap::new(),
      next_id: MacroID::smallest_unreserved(),
    }
  }

  fn load_file_on_server(&mut self, path: &Path) -> io::Result<u32> {
    let server = self.server.get_mut()?;
    let cmd = ServerCommand::Load((*path.to_string_lossy()).to_owned());
    let result: String = response_to_string(server.issue_command(&cmd)?)?;
    result.parse().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
  }

  pub fn stand_up_file(&mut self, name: String, file: NamedTempFile) -> io::Result<MacroID> {
    //let mut buf = String::new();
    //std::io::stdin().read_line(&mut buf).expect("Failed");
    let idx = self.load_file_on_server(file.path())?;
    let gdname = names::lisp_to_gd(&name);
    let call = MacroCall { index: idx, original_name: name, name: gdname };
    let id = self.next_id;
    self.next_id = self.next_id.next();
    self.macro_files.insert(id, (call, file));
    Ok(id)
  }

  pub fn get_file(&self, id: MacroID) -> Option<&MacroCall> {
    self.macro_files.get(&id).map(|x| &x.0)
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
    let result = response_to_string(server.issue_command(&ServerCommand::Eval(eval_str))?)?;
    let parser = parser::ASTParser::new();
    let parsed = parser.parse(&result)?;
    Ok(parsed)
  }

  pub fn set_global_name_generator<'a>(&mut self, gen: &FreshNameGenerator<'a>) -> io::Result<()> {
    let server = self.server.get_mut()?;
    let json = gen.to_json();
    let exec_str = format!("GDLisp.global_name_generator = GDLisp.FreshNameGenerator.from_json({})",
                           json.to_string());
    let cmd = ServerCommand::Exec(exec_str);
    let _result = response_to_string(server.issue_command(&cmd)?)?;
    Ok(())
  }

  pub fn reset_global_name_generator(&mut self) -> io::Result<()> {
    let server = self.server.get_mut()?;
    let exec_str = String::from(r#"GDLisp.global_name_generator = GDLisp.FreshNameGenerator.new([], "")"#);
    let cmd = ServerCommand::Exec(exec_str);
    let _result = response_to_string(server.issue_command(&cmd)?)?;
    Ok(())
  }

}

impl MacroID {

  pub const RESERVED: u32 = 1024;

  pub fn smallest_unreserved() -> MacroID {
    MacroID(MacroID::RESERVED)
  }

  pub fn is_reserved(self) -> bool {
    self.0 < MacroID::RESERVED
  }

  pub fn next(self) -> MacroID {
    MacroID(self.0 + 1)
  }

}
