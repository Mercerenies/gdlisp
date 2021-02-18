
// Incremental compilation (supplies backbone for macro resolution)

use super::symbol_table::SymbolTable;
use super::{MAIN_BODY_NAME, resolve_call_name};
use super::arglist::ArgList;
use super::literal::Literal;
use super::expr::Expr;
use super::special_form;
use super::depends::Dependencies;
use super::decl::{self, Decl};
use super::macros;
use crate::sxp::dotted::{DottedExpr, TryFromDottedExprError};
use crate::sxp::ast::AST;
use crate::sxp::reify::Reify;
use crate::compile::error::Error;
use crate::compile::names;
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic::compile_default_call;
use crate::gdscript::library;
use crate::gdscript::expr::{Expr as GDExpr};
use crate::runner::macro_server::lazy::LazyServer;
use crate::runner::macro_server::command::ServerCommand;
use crate::parser;

use tempfile::NamedTempFile;

use std::io;
use std::path::Path;
use std::convert::{TryFrom, TryInto};
use std::borrow::Borrow;
use std::collections::HashMap;

pub struct IncCompiler {
  symbols: SymbolTable,
  server: LazyServer,
  #[allow(dead_code)] // Need to keep these so the files stay alive until compilation is done
  temporary_files: Vec<NamedTempFile>,
  macro_files: HashMap<String, MacroCall>
}

#[derive(Clone, Debug)]
pub struct MacroCall {
  pub index: u32,
  pub name: String,
}

// TODO Move server, temporary_files, macro_files into something that provides a
// nice Rust-side interface to the server.

#[allow(clippy::new_without_default)]
impl IncCompiler {

  pub fn new() -> IncCompiler {
    IncCompiler {
      symbols: SymbolTable::new(),
      server: LazyServer::new(),
      temporary_files: vec!(),
      macro_files: HashMap::new(),
    }
  }

  fn resolve_macro_call(&mut self, call: &MacroCall, head: &str, tail: &[&AST]) -> Result<AST, Error> {
    match self.symbols.get(head) {
      Some(Decl::MacroDecl(mdecl)) => {
        let specs = FnSpecs::from(mdecl.args.clone());
        let call_object =
          GDExpr::Subscript(
            Box::new(GDExpr::Attribute(Box::new(GDExpr::var("MAIN")), String::from("loaded_files"))),
            Box::new(GDExpr::from(call.index as i32))
          );
        let call = FnCall {
          scope: FnScope::Global,
          object: Some(Box::new(call_object)),
          function: call.name.to_owned(),
          specs: specs,
        };
        let args: Vec<_> = tail.iter().map(|x| x.reify()).collect();
        let server = self.server.get_mut().expect("IO Error on server"); // TODO Fix Expect
        let eval_str = compile_default_call(call, args)?.to_gd();
        let result = server.issue_command(&ServerCommand::Eval(eval_str)).expect("IO Error on server"); // TODO Fix Expect
        let parser = parser::ASTParser::new();
        Ok(parser.parse(&result).expect("Malformed input returned from macro server")) // TODO Fix Expect
      }
      _ => {
        Err(Error::NoSuchFn(head.to_owned()))
      }
    }
  }

  fn try_resolve_macro_call(&mut self, ast: &AST) -> Result<Option<AST>, Error> {
    let vec: Vec<&AST> = match DottedExpr::new(ast).try_into() {
      Err(TryFromDottedExprError {}) => return Ok(None),
      Ok(v) => v,
    };
    if vec.is_empty() {
      Ok(None) // Nil is not a macro call.
    } else {
      let head = resolve_call_name(vec[0])?;
      let tail = &vec[1..];
      if let Some(call) = self.macro_files.get(head) {
        let call = call.clone(); // Can't borrow self mutably below, so let's get rid of the immutable borrow above.
        self.resolve_macro_call(&call, head, tail).map(Some)
      } else {
        Ok(None)
      }
    }
  }

  pub fn resolve_simple_call(&mut self, head: &str, tail: &[&AST]) -> Result<Expr, Error> {
    if let Some(sf) = special_form::dispatch_form(head, tail)? {
      Ok(sf)
    } else if let Some(call) = self.macro_files.get(head) {
      let call = call.clone(); // Can't borrow self mutably below, so let's get rid of the immutable borrow above.
      let result = self.resolve_macro_call(&call, head, tail)?;
      self.compile_expr(&result)
    } else {
      let args = tail.iter().map(|x| self.compile_expr(x)).collect::<Result<Vec<_>, _>>()?;
      Ok(Expr::Call(head.to_owned(), args))
    }
  }

  pub fn compile_expr(&mut self, expr: &AST) -> Result<Expr, Error> {
    match expr {
      AST::Nil | AST::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        if vec.is_empty() {
          Ok(Expr::Literal(Literal::Nil))
        } else {
          let head = resolve_call_name(vec[0])?;
          let tail = &vec[1..];
          self.resolve_simple_call(head, tail)
        }
      }
      AST::Array(vec) => {
        let vec = vec.iter().map(|e| self.compile_expr(e)).collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::Array(vec))
      }
      AST::Int(n) => {
        Ok(Expr::Literal(Literal::Int(*n)))
      }
      AST::Bool(b) => {
        Ok(Expr::Literal(Literal::Bool(*b)))
      }
      AST::Float(f) => {
        Ok(Expr::Literal(Literal::Float(*f)))
      }
      AST::String(s) => {
        Ok(Expr::Literal(Literal::String(s.to_owned())))
      }
      AST::Symbol(s) => {
        Ok(Expr::LocalVar(s.to_string()))
      }
    }
  }

  pub fn compile_decl(&mut self, decl: &AST)
                      -> Result<Decl, Error> {
    let vec: Vec<&AST> = DottedExpr::new(decl).try_into()?;
    if vec.is_empty() {
      return Err(Error::UnknownDecl(decl.clone()));
    }
    match vec[0] {
      AST::Symbol(s) => {
        match s.borrow() {
          "defn" => {
            if vec.len() < 3 {
              return Err(Error::InvalidDecl(decl.clone()));
            }
            let name = match vec[1] {
            AST::Symbol(s) => s,
              _ => return Err(Error::InvalidDecl(decl.clone())),
            };
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args)?;
            let body = vec[3..].iter().map(|expr| self.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
            Ok(Decl::FnDecl(decl::FnDecl {
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            }))
          }
          "defmacro" => {
            if vec.len() < 3 {
              return Err(Error::InvalidDecl(decl.clone()));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s,
              _ => return Err(Error::InvalidDecl(decl.clone())),
            };
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args)?;
            let body = vec[3..].iter().map(|expr| self.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
            Ok(Decl::MacroDecl(decl::MacroDecl {
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            }))
          }
          _ => {
            Err(Error::UnknownDecl(decl.clone()))
          }
        }
      }
      _ => {
        Err(Error::InvalidDecl(decl.clone()))
      }
    }
  }

  fn compile_decl_or_expr(&mut self, main: &mut Vec<Expr>, curr: &AST)
                          -> Result<(), Error> {
    let mut candidate: Option<AST>; // Just need somewhere to store the intermediate.
    let mut curr = curr; // Change lifetime :)
    while let Some(ast) = self.try_resolve_macro_call(curr)? {
      candidate = Some(ast);
      curr = &candidate.as_ref().unwrap();
    }
    // Check if we're looking at a top-level progn.
    if let Ok(vec) = Vec::try_from(DottedExpr::new(curr)) {
      if !vec.is_empty() && matches!(vec[0], AST::Symbol(progn) if progn == "progn") {
        for inner in &vec[1..] {
          self.compile_decl_or_expr(main, inner)?;
        }
        return Ok(());
      }
    }
    // TODO The intention of catching DottedListError here is to
    // catch the initial dotted list check. If we encounter
    // DottedListError somewhere else in the computation, it's
    // possible it's an error we need to propagate. Consider this.
    match self.compile_decl(curr) {
      Err(Error::UnknownDecl(_)) | Err(Error::DottedListError) => main.push(self.compile_expr(curr)?),
      Err(e) => return Err(e),
      Ok(d) => {
        let is_macro = d.is_macro();
        let name = d.name().to_owned();
        self.symbols.set(name.clone(), d);
        if is_macro {
          // TODO Handle error correctly
          self.bind_macro(&name)?;
        }
      }
    };
    Ok(())
  }

  pub fn compile_toplevel(mut self, body: &AST)
                          -> Result<Vec<Decl>, Error> {
    let body: Vec<_> = DottedExpr::new(body).try_into()?;
    let mut main: Vec<Expr> = Vec::new();
    for curr in body {
      self.compile_decl_or_expr(&mut main, curr)?;
    }
    let main_decl = Decl::FnDecl(decl::FnDecl {
      name: MAIN_BODY_NAME.to_owned(),
      args: ArgList::empty(),
      body: Expr::Progn(main),
    });
    self.symbols.set(MAIN_BODY_NAME.to_owned(), main_decl);
    self.server.shutdown().expect("IO Error"); // TODO Should we suppress this error? It is only a shutdown
    Ok(self.symbols.into())
  }

  pub fn bind_macro(&mut self, name: &str) -> Result<(), Error> {
    // Now we need to find the dependencies and spawn up the
    // server for the macro itself.
    let mut deps = Dependencies::identify(&self.symbols, &name);
    deps.purge_unknowns(library::all_builtin_names().into_iter());
    // Aside from built-in functions, it must be the case that
    // all referenced functions are already defined.
    let names = deps.try_into_knowns()?;
    let tmpfile = macros::create_macro_file(&self.symbols, names)?;
    let idx = self.load_file_on_server(tmpfile.path()).expect("IO Error"); // TODO Get rid of .expect(...)
    self.temporary_files.push(tmpfile);
    self.macro_files.insert(name.to_owned(), MacroCall { index: idx, name: names::lisp_to_gd(name) });
    Ok(())
  }

  fn load_file_on_server(&mut self, path: &Path) -> io::Result<u32> {
    // TODO Can we get these two .expect() calls to return something in io::Result?
    let server = self.server.get_mut()?;
    let cmd = ServerCommand::Load((*path.to_string_lossy()).to_owned());
    let result = server.issue_command(&cmd)?;
    Ok(result.parse().expect("Invalid response from server on load file"))
  }

}

impl Default for IncCompiler {

  fn default() -> IncCompiler {
    IncCompiler::new()
  }

}

impl From<IncCompiler> for Vec<Decl> {

  fn from(compiler: IncCompiler) -> Vec<Decl> {
    compiler.symbols.into()
  }

}
