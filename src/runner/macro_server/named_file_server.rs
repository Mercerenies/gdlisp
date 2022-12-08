
//! A server which manages macro resolution and can run Godot code.
//!
//! A [`NamedFileServer`] is a more powerful form of
//! [`MacroServer`](super::MacroServer). Whereas the latter manages
//! the primitive operations of sending commands to and from a Godot
//! process, a `NamedFileServer` manages the higher-level operations
//! of declaring and calling macros.

use crate::sxp::ast::AST;
use crate::runner::macro_server::lazy::LazyServer;
use crate::compile::names;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs, FnName};
use crate::compile::symbol_table::call_magic::compile_default_call;
use crate::compile::stmt_wrapper::{self, StmtWrapper};
use crate::gdscript::expr::{Expr as GDExpr};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::library;
use crate::ir::arglist::ordinary::ArgList;
use crate::pipeline::error::{PError, IOError};
use crate::pipeline::source::SourceOffset;
use crate::AST_PARSER;
use super::command::ServerCommand;
use super::response;

use tempfile::NamedTempFile;
use serde::{Serialize, Deserialize};

use std::collections::HashMap;
use std::path::Path;
use std::io;
use std::convert::TryFrom;

/// A `NamedFileServer` maintains a [`LazyServer`], as well as a
/// registry of the macros which have been uploaded to the server.
///
/// Macros are normally uploaded to the file server via
/// [`stand_up_macro`](NamedFileServer::stand_up_macro), which also
/// adds the macro to the server's registry for later access.
/// Alternatively, files which do *not* contain macros can be uploaded
/// with [`stand_up_file`](NamedFileServer::stand_up_file). The latter
/// method loads a file onto the macro server without adding it to the
/// registry. Hence, the file is available for other macros to use,
/// provided they know its name, but it is unavailable for direct
/// calling, since it is not a macro. Finally,
/// [`add_reserved_macro`](NamedFileServer::add_reserved_macro) is
/// used to add data to the registry without standing up any files.
/// This is used to add standard library macros (which are side-loaded
/// using another mechanism before `NamedFileServer` is even
/// constructed) to the registry.
///
/// Macros are indexed by [`MacroID`], a wrapper struct around `u32`.
pub struct NamedFileServer {
  server: LazyServer,
  macro_files: HashMap<MacroID, MacroCall>,
  next_id: MacroID,
  next_reserved_id: MacroID,
}

/// A simple identifier type which is used as the key type in
/// [`NamedFileServer`].
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Default, Serialize, Deserialize)]
#[repr(transparent)]
pub struct MacroID(u32);

#[derive(Debug)]
struct MacroCall {
  index: u32, // Index in the lookup table on the GDScript side.
  #[allow(dead_code)]
  original_name: String, // Probably not needed, but we have it so we may as well keep track of it.
  name: String,
  parms: ArgList,
  #[allow(dead_code)]
  file: Option<NamedTempFile>, // Macros can optionally retain a file resource, which will be deleted when the macro is discarded from scope.
}

const RESERVED_MACRO_INDEX: u32 = u32::MAX;

// TODO Make this return GDError like it probably should.
fn response_to_string(response: response::ServerResponse) -> io::Result<String> {
  String::try_from(response).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

#[allow(clippy::new_without_default)]
impl NamedFileServer {

  /// Constructs a new `NamedFileServer`. The server does not
  /// initially spawn off a Godot process and will only do so once
  /// required to (similar to [`LazyServer`]).
  pub fn new() -> NamedFileServer {
    NamedFileServer {
      server: LazyServer::new(),
      macro_files: HashMap::new(),
      next_id: MacroID::smallest_unreserved(),
      next_reserved_id: MacroID::smallest_stdlib(),
    }
  }

  /// Adds a reserved standard library macro to this file server's
  /// known macros list.
  ///
  /// A reserved macro is a special kind of macro that does not have
  /// its own file. Instead, a reserved macro is a macro that is
  /// side-loaded onto the macro server via some other means (usually,
  /// by being present in the standard library file `GDLisp.lisp`,
  /// which gets preloaded into the macro server at process start
  /// time). This function does *not* instruct the Godot process to
  /// load any new files or to do anything at all. Instead, this
  /// function simply makes the `NamedFileServer` struct aware that
  /// there is a macro with the given name and argument list in the
  /// standard library file. It is the caller's responsibility to
  /// ensure that the information is actually correct.
  pub fn add_reserved_macro(&mut self, name: String, parms: ArgList) -> MacroID {
    let id = self.next_reserved_id;
    self.next_reserved_id = self.next_reserved_id.next();
    self.macro_files.insert(id, MacroCall {
      index: RESERVED_MACRO_INDEX,
      original_name: name.clone(),
      name,
      parms,
      file: None,
    });
    id
  }

  fn load_file_on_server(&mut self, path: &Path) -> io::Result<u32> {
    let server = self.server.get_mut()?;
    let cmd = ServerCommand::Load((*path.to_string_lossy()).to_owned());
    let result: String = response_to_string(server.issue_command(&cmd)?)?;
    result.parse().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
  }

  /// Stand up a file on the `NamedFileServer`.
  ///
  /// This should be used instead of
  /// [`stand_up_macro`](NamedFileServer::stand_up_macro) in cases
  /// where the resulting file will never be called directly.
  /// Specifically, this should be used to supply the macro server
  /// with resources that are not directly macros but which may be
  /// needed indirectly by other macros in the future. The file will
  /// simply be loaded into the server using [`ServerCommand::Load`]
  /// blindly, and it is the caller's responsibility to keep track of
  /// the filename and provide some way to access the loaded file.
  pub fn stand_up_file(&mut self, file: NamedTempFile) -> io::Result<()> {
    self.load_file_on_server(file.path())?;
    Ok(())
  }

  /// Stand up a file as a macro file on the `NamedFileServer`.
  ///
  /// This is the more powerful form of `stand_up_file`. In addition
  /// to physically loading the file via [`ServerCommand::Load`], this
  /// method also stores the macro, with the given name and argument
  /// list, in the `NamedFileServer`'s macro registry, which can later
  /// be accessed via
  /// [`run_server_file`](NamedFileServer::run_server_file).
  ///
  /// This method returns a [`MacroID`] which can be used later to
  /// call the macro.
  pub fn stand_up_macro(&mut self, name: String, parms: ArgList, file: NamedTempFile) -> io::Result<MacroID> {
    let idx = self.load_file_on_server(file.path())?;
    let gdname = names::lisp_to_gd(&name);
    let call = MacroCall { index: idx, original_name: name, name: gdname, parms, file: Some(file) };
    let id = self.next_id;
    self.next_id = self.next_id.next();
    self.macro_files.insert(id, call);
    Ok(id)
  }

  fn get_file(&self, id: MacroID) -> Option<&MacroCall> {
    self.macro_files.get(&id)
  }

  /// Run a macro with the given arguments. `id` should be a
  /// [`MacroID`] returned from a prior call to
  /// [`stand_up_macro`](NamedFileServer::stand_up_macro) or similar,
  /// and `args` should be a list of Godot expressions to provide as
  /// the arguments. In case of Godot errors, including but not
  /// limited to parsing errors, IO communication errors, or semantic
  /// errors in macro evaluation, an `Err` value is returned.
  ///
  /// `prelude` is a vector of statements which will be run before the
  /// actual macro call and can be used to set up the environment or
  /// provide local variables to the macro arguments.
  ///
  /// # Panics
  ///
  /// This method will panic if given an invalid `id` value. The `id`
  /// must be the macro ID from a prior invocation of `stand_up_macro`
  /// or [`add_reserved_macro`](NamedFileServer::add_reserved_macro)
  /// on the same `NamedFileServer` instance.
  pub fn run_server_file(&mut self, id: MacroID, prelude: Vec<Stmt>, args: Vec<GDExpr>, pos: SourceOffset)
                         -> Result<AST, PError> {
    let result = self.run_server_file_str(id, prelude, args, pos)?;
    let parsed = AST_PARSER.parse(&result)?;
    //println!("{}", parsed);
    Ok(parsed)
  }

  pub fn run_server_file_str(&mut self, id: MacroID, prelude: Vec<Stmt>, args: Vec<GDExpr>, pos: SourceOffset)
                             -> Result<String, PError> {
    let call = self.get_file(id).expect("Invalid MacroID in run_server_file");
    let specs = FnSpecs::from(call.parms.clone());
    let call_object =
      if id.is_reserved() {
        let gdlisp_root = library::gdlisp_root_var_name();
        FnName::OnLocalVar(Box::new(gdlisp_root))
      } else {

        // A non-reserved macro should never end up at this position.
        // (This would get caught by the below try_from, but we can
        // supply a better error message in this specific case)
        if call.index == RESERVED_MACRO_INDEX {
          panic!("Non-reserved macro at reserved index {}", call.index);
        }

        let index_error_message = format!("Macro reference indices exceeded i32 range, got {}", call.index);
        let call_index = i32::try_from(call.index).expect(&index_error_message);

        let target = VarName::SubscriptedConstant(
          Box::new(VarName::ImportedConstant(
            Box::new(VarName::FileConstant(String::from("MAIN"))),
            String::from("loaded_files"),
          )),
          call_index,
        );
        FnName::OnLocalVar(Box::new(target))
      };
    let call = FnCall {
      scope: FnScope::Global,
      object: call_object,
      function: call.name.to_owned(),
      specs: specs,
      is_macro: true,
    };
    self.do_macro_call(prelude, call, args, pos)
  }

  fn do_macro_call(&mut self,
                   prelude: Vec<Stmt>,
                   call: FnCall,
                   args: Vec<GDExpr>,
                   pos: SourceOffset) -> Result<String, PError> {
    let server = self.server.get_mut().map_err(|err| IOError::new(err, pos))?;
    let expr = compile_default_call(call, args, pos)?;
    let mut stmts = prelude;
    stmts.push(stmt_wrapper::Return.wrap_expr(expr));
    let mut exec_str = String::new();
    Stmt::write_gd_stmts(stmts.iter(), &mut exec_str, 4).expect("Could not write to string in do_macro_call");
    let result = server.issue_command(&ServerCommand::Exec(exec_str)).map_err(|err| IOError::new(err, pos))?;
    let result = response_to_string(result).map_err(|err| IOError::new(err, pos))?;
    Ok(result)
  }

  /// Issues a command to the server setting its global name generator
  /// to `gen`.
  pub fn set_global_name_generator(&mut self, gen: &FreshNameGenerator) -> io::Result<()> {
    let server = self.server.get_mut()?;
    let json = gen.to_json();
    let exec_str = format!("    GDLisp.__gdlisp_Global_name_generator = GDLisp.FreshNameGenerator.from_json({})", json);
    let cmd = ServerCommand::Exec(exec_str);
    let _result = response_to_string(server.issue_command(&cmd)?)?;
    Ok(())
  }

  /// Issues a command to the server setting the global name generator
  /// to a newly-constructed name generator.
  pub fn reset_global_name_generator(&mut self) -> io::Result<()> {
    let server = self.server.get_mut()?;
    let exec_str = String::from(r#"    GDLisp.__gdlisp_Global_name_generator = GDLisp.FreshNameGenerator.new([], "")"#);
    let cmd = ServerCommand::Exec(exec_str);
    let _result = response_to_string(server.issue_command(&cmd)?)?;
    Ok(())
  }

  /// Returns true if the Godot subprocess is running and is healthy.
  /// If the child process has started, then this delegates to
  /// [`MacroServer::is_process_healthy`](super::MacroServer::is_process_healthy).
  /// If not, then this returns false, as the process is not running.
  pub fn is_process_healthy(&mut self) -> bool {
    match self.server.get_mut_if_initialized() {
      None => false,
      Some(server) => server.is_process_healthy(),
    }
  }

}

impl MacroID {

  // Current partitioning scheme:
  //
  // 0-63: Unused (future use)
  // 64-511: Built-in (stdlib) macros
  // 512-1023: Unused (future use)
  // 1024-max: User-defined macros

  const RESERVED: u32 = 1024;

  fn smallest_stdlib() -> MacroID {
    MacroID(64)
  }

  fn smallest_unreserved() -> MacroID {
    MacroID(MacroID::RESERVED)
  }

  fn is_reserved(self) -> bool {
    self.0 < MacroID::RESERVED
  }

  fn next(self) -> MacroID {
    MacroID(self.0 + 1)
  }

}
