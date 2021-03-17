
// Incremental compilation (supplies backbone for macro resolution)

use super::symbol_table::SymbolTable;
use super::MAIN_BODY_NAME;
use super::call_name::CallName;
use super::arglist::{ArgList, SimpleArgList};
use super::literal::Literal;
use super::import::{ImportDecl, ImportName};
use super::expr::Expr;
use super::special_form;
use super::depends::Dependencies;
use super::decl::{self, Decl};
use super::macros::{self, MacroData};
use super::constant::MaybeConstant;
use super::identifier::{Id, IdLike, Namespace};
use crate::sxp::dotted::{DottedExpr, TryFromDottedExprError};
use crate::sxp::ast::{self, AST};
use crate::sxp::reify::Reify;
use crate::compile::error::Error;
use crate::gdscript::library;
use crate::runner::macro_server::named_file_server::MacroCall;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;

use std::convert::{TryFrom, TryInto};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};

pub struct IncCompiler {
  symbols: SymbolTable,
  macros: HashMap<String, MacroData>,
  imports: Vec<ImportDecl>,
}

#[allow(clippy::new_without_default)]
impl IncCompiler {

  pub fn new() -> IncCompiler {
    IncCompiler {
      symbols: SymbolTable::new(),
      macros: HashMap::new(),
      imports: vec!(),
    }
  }

  fn resolve_macro_call(&mut self, pipeline: &mut Pipeline, call: &MacroCall, head: &str, tail: &[&AST])
                        -> Result<AST, PError> {
    match self.macros.get(head) {
      Some(MacroData { args: parms, .. }) => {
        let args: Vec<_> = tail.iter().map(|x| x.reify()).collect();
        let ast = pipeline.get_server_mut().run_server_file(call, parms.clone(), args)?;
        Ok(ast)
      }
      _ => {
        Err(PError::from(Error::NoSuchFn(head.to_owned())))
      }
    }
  }

  fn get_macro_file<'a, 'b, 'c>(&'a self, pipeline: &'b Pipeline, head: &'c str) -> Option<&'b MacroCall> {
    self.macros.get(head).map(|x| x.id).and_then(|id| pipeline.get_server().get_file(id))
  }

  fn try_resolve_macro_call(&mut self, pipeline: &mut Pipeline, ast: &AST) -> Result<Option<AST>, PError> {
    let vec: Vec<&AST> = match DottedExpr::new(ast).try_into() {
      Err(TryFromDottedExprError {}) => return Ok(None),
      Ok(v) => v,
    };
    if !vec.is_empty() {
      let head = self.resolve_call_name(pipeline, vec[0])?;
      if let CallName::SimpleName(head) = head {
        let tail = &vec[1..];
        if let Some(call) = self.get_macro_file(pipeline, &head) {
          let call = call.clone(); // Can't borrow self mutably below, so let's get rid of the immutable borrow above.
          return self.resolve_macro_call(pipeline, &call, &head, tail).map(Some);
        }
      }
    }
    Ok(None)
  }

  fn resolve_call_name(&mut self, pipeline: &mut Pipeline, ast: &AST) -> Result<CallName, PError> {
    if let Some((lhs, name)) = CallName::try_resolve_method_name(ast) {
      let lhs = self.compile_expr(pipeline, lhs)?;
      Ok(CallName::MethodName(Box::new(lhs), name.to_owned()))
    } else {
      match ast {
        AST::Symbol(s) => Ok(CallName::SimpleName(s.clone())),
        _ => Err(PError::from(Error::CannotCall(ast.clone()))),
      }
    }
  }

  pub fn resolve_simple_call(&mut self, pipeline: &mut Pipeline, head: &str, tail: &[&AST])
                             -> Result<Expr, PError> {
    if let Some(sf) = special_form::dispatch_form(self, pipeline, head, tail)? {
      Ok(sf)
    } else if let Some(call) = self.get_macro_file(pipeline, head) {
      let call = call.clone(); // Can't borrow self mutably below, so let's get rid of the immutable borrow above.
      let result = self.resolve_macro_call(pipeline, &call, head, tail)?;
      self.compile_expr(pipeline, &result)
    } else {
      let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
      Ok(Expr::Call(head.to_owned(), args))
    }
  }

  pub fn compile_expr(&mut self, pipeline: &mut Pipeline, expr: &AST) -> Result<Expr, PError> {
    match expr {
      AST::Nil | AST::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        if vec.is_empty() {
          Ok(Expr::Literal(Literal::Nil))
        } else {
          let head = self.resolve_call_name(pipeline, vec[0])?;
          let tail = &vec[1..];
          match head {
            CallName::SimpleName(head) => {
              self.resolve_simple_call(pipeline, &head, tail)
            }
            CallName::MethodName(target, head) => {
              let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
              Ok(Expr::MethodCall(target, head, args))
            }
          }
        }
      }
      AST::Array(vec) => {
        let vec = vec.iter().map(|e| self.compile_expr(pipeline, e)).collect::<Result<Vec<_>, _>>()?;
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
      AST::Vector2(x, y) => {
        let x = self.compile_expr(pipeline, &*x)?;
        let y = self.compile_expr(pipeline, &*y)?;
        Ok(Expr::Vector2(Box::new(x), Box::new(y)))
      }
      AST::Vector3(x, y, z) => {
        let x = self.compile_expr(pipeline, &*x)?;
        let y = self.compile_expr(pipeline, &*y)?;
        let z = self.compile_expr(pipeline, &*z)?;
        Ok(Expr::Vector3(Box::new(x), Box::new(y), Box::new(z)))
      }
    }
  }

  // TODO Separate into a piece that checks whether we're a valid
  // decl, so we don't get weird "no such function defclass" errors on
  // Error::InvalidDecl.
  pub fn compile_decl(&mut self, pipeline: &mut Pipeline, decl: &AST)
                      -> Result<Decl, PError> {
    let vec: Vec<&AST> = DottedExpr::new(decl).try_into()?;
    if vec.is_empty() {
      return Err(PError::from(Error::UnknownDecl(decl.clone())));
    }
    match vec[0] {
      AST::Symbol(s) => {
        match s.borrow() {
          "defn" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s,
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args)?;
            let body = vec[3..].iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            Ok(Decl::FnDecl(decl::FnDecl {
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            }))
          }
          "defmacro" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s,
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args)?;
            let body = vec[3..].iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            Ok(Decl::MacroDecl(decl::MacroDecl {
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            }))
          }
          "defconst" => {
            if vec.len() != 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let value = self.compile_expr(pipeline, vec[2])?;
            value.validate_const_expr(&name)?;
            Ok(Decl::ConstDecl(decl::ConstDecl { name, value }))
          }
          "defclass" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let superclass = match vec[2] {
              AST::Cons(car, cdr) =>
                match (&**car, &**cdr) {
                  (AST::Symbol(superclass_name), AST::Nil) => superclass_name.to_owned(),
                  _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
                },
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let mut class = decl::ClassDecl::new(name, superclass);
            let mut decl_body = &vec[3..];
            if vec.len() > 3 && vec[3] == &AST::Symbol(String::from("main")) {
              class.main_class = true;
              decl_body = &vec[4..];
            }
            for decl in decl_body {
              self.compile_class_inner_decl(pipeline, &mut class, decl)?;
            }
            Ok(Decl::ClassDecl(class))
          }
          _ => {
            Err(PError::from(Error::UnknownDecl(decl.clone())))
          }
        }
      }
      _ => {
        Err(PError::from(Error::InvalidDecl(decl.clone())))
      }
    }
  }

  fn compile_class_inner_decl(&mut self,
                              pipeline: &mut Pipeline,
                              acc: &mut decl::ClassDecl,
                              curr: &AST)
                              -> Result<(), PError> {
    // TODO Error if we declare constructor twice

    // Deal with macros
    let mut candidate: Option<AST>;
    let mut curr = curr;
    while let Some(ast) = self.try_resolve_macro_call(pipeline, curr)? {
      candidate = Some(ast);
      curr = &candidate.as_ref().unwrap();
    }

    let vec = Vec::try_from(DottedExpr::new(curr)).map_err(Error::from)?;
    if vec.is_empty() {
      return Err(PError::from(Error::InvalidDecl(curr.clone())));
    }
    if let AST::Symbol(s) = vec[0] {
      match s.as_str() {
        "progn" => {
          // Top-level magic progn
          for d in &vec[1..] {
            self.compile_class_inner_decl(pipeline, acc, d)?;
          }
          Ok(())
        }
        "defvar" => {
          if vec.len() < 2 || vec.len() > 4 {
            return Err(PError::from(Error::InvalidDecl(curr.clone())));
          }
          if let AST::Symbol(vname) = vec[1] {
            let name = vname.to_owned();

            // Parse value and export
            let mut value = None;
            let mut export = None;
            let mut idx = 2;
            // Value
            if let Some(v) = vec.get(idx) {
              if !(matches!(v, AST::Cons(car, _) if **car == AST::Symbol(String::from("export")))) {
                let e = self.compile_expr(pipeline, v)?;
                e.validate_const_expr(&name)?;
                value = Some(e);
                idx += 1;
              }
            };
            // Export
            if let Some(v) = vec.get(idx) {
              if let DottedExpr { elements, terminal: AST::Nil } = DottedExpr::new(v) {
                if elements.get(0) == Some(&&AST::Symbol(String::from("export"))) {
                  let args = elements[1..].iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<_, _>>()?;
                  export = Some(decl::Export { args });
                  idx += 1;
                }
              }
            }
            // (Extra)
            if idx < vec.len() {
              return Err(PError::from(Error::InvalidDecl(curr.clone())));
            }

            // Exports are only allowed on the main class
            if export.is_some() && !acc.main_class {
              return Err(PError::from(Error::ExportOnInnerClassVar(vname.clone())));
            }

            let decl = decl::ClassVarDecl { export, name, value };
            acc.decls.push(decl::ClassInnerDecl::ClassVarDecl(decl));
            Ok(())
          } else {
            Err(PError::from(Error::InvalidDecl(curr.clone())))
          }
        }
        "defn" => {
          // TODO Unify some of this with the equivalent code from compile_decl?
          if vec.len() < 3 {
            return Err(PError::from(Error::InvalidDecl(curr.clone())));
          }
          if let AST::Symbol(fname) = vec[1] {
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = SimpleArgList::parse(args)?;
            let body = vec[3..].iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            if fname == "_init" {
              // Constructor
              acc.constructor = decl::ConstructorDecl { args, body: Expr::Progn(body) };
            } else {
              let decl = decl::ClassFnDecl { name: fname.to_owned(), args, body: Expr::Progn(body) };
              acc.decls.push(decl::ClassInnerDecl::ClassFnDecl(decl));
            }
            Ok(())
          } else {
            Err(PError::from(Error::InvalidDecl(curr.clone())))
          }
        }
        "defsignal" => {
          if vec.len() < 2 || vec.len() > 3 {
            return Err(PError::from(Error::InvalidDecl(curr.clone())));
          }
          let name = match vec[1] {
            AST::Symbol(s) => s.to_owned(),
            _ => return Err(PError::from(Error::InvalidDecl(curr.clone()))),
          };
          let nil = AST::Nil;
          let args = vec.get(2).map_or(&nil, |x| *x);
          let args: Vec<_> = DottedExpr::new(args).try_into()?;
          let args = SimpleArgList::parse(args)?;
          acc.decls.push(decl::ClassInnerDecl::ClassSignalDecl(decl::ClassSignalDecl { name, args }));
          Ok(())
        }
        _ => {
          Err(PError::from(Error::InvalidDecl(curr.clone())))
        }
      }
    } else {
      Err(PError::from(Error::InvalidDecl(curr.clone())))
    }
  }

  pub fn compile_import(&mut self, curr: &AST) -> Result<Option<ImportDecl>, Error> {
    if let Ok(vec) = Vec::try_from(DottedExpr::new(curr)) {
      if !vec.is_empty() && *vec[0] == ast::symbol("use") {
        return ImportDecl::parse(&vec[1..]).map_err(Error::from).map(Some);
      }
    }
    Ok(None)
  }

  fn import_macros_from(&mut self, unit: &TranslationUnit, import: &ImportDecl) {
    for imp in import.names(&unit.exports) {
      let ImportName { namespace: namespace, in_name: import_name, out_name: export_name } = imp;
      if namespace == Namespace::Function { // Macros are always in the function namespace
        if let Some(data) = unit.macros.get(&export_name) {
          self.macros.insert(import_name, data.to_imported());
        }
      }
    }
  }

  fn compile_decl_or_expr(&mut self, pipeline: &mut Pipeline, main: &mut Vec<Expr>, curr: &AST)
                          -> Result<(), PError> {
    let mut candidate: Option<AST>; // Just need somewhere to store the intermediate.
    let mut curr = curr; // Change lifetime :)
    while let Some(ast) = self.try_resolve_macro_call(pipeline, curr)? {
      candidate = Some(ast);
      curr = &candidate.as_ref().unwrap();
    }
    // Check if we're looking at a top-level progn.
    if let Ok(vec) = Vec::try_from(DottedExpr::new(curr)) {
      if !vec.is_empty() && matches!(vec[0], AST::Symbol(progn) if progn == "progn") {
        for inner in &vec[1..] {
          self.compile_decl_or_expr(pipeline, main, inner)?;
        }
        return Ok(());
      }
    }
    let imp = self.compile_import(curr)?;
    if let Some(imp) = imp {
      let file = pipeline.load_file(imp.filename.path())?;
      self.import_macros_from(&file, &imp);
      self.imports.push(imp);
    } else {
      // TODO The intention of catching DottedListError here is to
      // catch the initial dotted list check. If we encounter
      // DottedListError somewhere else in the computation, it's
      // possible it's an error we need to propagate. Consider this.
      match self.compile_decl(pipeline, curr) {
        Err(PError::GDError(Error::UnknownDecl(_))) | Err(PError::GDError(Error::DottedListError)) => main.push(self.compile_expr(pipeline, curr)?),
        Err(e) => return Err(e),
        Ok(d) => {
          let name = d.name().to_owned();
          self.symbols.set(d.to_id(), d.clone());
          if let Decl::MacroDecl(mdecl) = d {
            self.bind_macro(pipeline, &name, mdecl)?;
          }
        }
      };
    }
    Ok(())
  }

  pub fn compile_toplevel(mut self, pipeline: &mut Pipeline, body: &AST)
                          -> Result<(decl::TopLevel, HashMap<String, MacroData>), PError> {
    let body: Result<Vec<_>, TryFromDottedExprError> = DottedExpr::new(body).try_into();
    let body: Vec<_> = body?; // *sigh* Sometimes the type checker just doesn't get it ...
    let mut main: Vec<Expr> = Vec::new();
    for curr in body {
      self.compile_decl_or_expr(pipeline, &mut main, curr)?;
    }
    let main_decl = Decl::FnDecl(decl::FnDecl {
      name: MAIN_BODY_NAME.to_owned(),
      args: ArgList::empty(),
      body: Expr::Progn(main),
    });
    self.symbols.set(Id::new(Namespace::Function, MAIN_BODY_NAME.to_owned()), main_decl);
    Ok(self.into())
  }

  pub fn bind_macro(&mut self, pipeline: &mut Pipeline, name: &str, decl: decl::MacroDecl) -> Result<(), PError> {

    let translation_names = self.imports.iter().map(|import| {
      let unit = pipeline.load_file(&import.filename.path())?;
      Ok(import.names(&unit.exports))
    }).collect::<Result<Vec<_>, PError>>()?;
    let imported_names: HashSet<_> =
      translation_names.into_iter().flatten().map(ImportName::into_imported_id).collect();

    // Now we need to find the dependencies and spawn up the
    // server for the macro itself.
    let mut deps = Dependencies::identify(&self.symbols, &imported_names, &*Id::build(Namespace::Function, name));
    deps.purge_unknowns(library::all_builtin_names().iter().map(|x| x as &dyn IdLike));

    // Aside from built-in functions, it must be the case that
    // all referenced functions are already defined.
    let names = deps.try_into_knowns().map_err(Error::from)?;
    let tmpfile = macros::create_macro_file(pipeline, self.imports.clone(), &self.symbols, names)?;
    let m_id = pipeline.get_server_mut().stand_up_file(name.to_owned(), tmpfile)?;
    self.macros.insert(name.to_owned(), MacroData { id: m_id, args: decl.args, imported: false });

    Ok(())
  }

}

impl Default for IncCompiler {

  fn default() -> IncCompiler {
    IncCompiler::new()
  }

}

impl From<IncCompiler> for (decl::TopLevel, HashMap<String, MacroData>) {

  fn from(compiler: IncCompiler) -> (decl::TopLevel, HashMap<String, MacroData>) {
    let toplevel = decl::TopLevel {
      imports: compiler.imports,
      decls: compiler.symbols.into(),
    };
    let macros: HashMap<_, _> = compiler.macros.into_iter().filter(|(_, x)| !x.imported).collect();
    (toplevel, macros)
  }

}
