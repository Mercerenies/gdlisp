
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
use super::identifier::{Id, IdLike, Namespace};
use super::modifier::{self, ParseRule};
use super::export::Visibility;
use crate::sxp::dotted::{DottedExpr, TryFromDottedExprError};
use crate::sxp::ast::AST;
use crate::sxp::reify::Reify;
use crate::compile::error::Error;
use crate::compile::resource_type::ResourceType;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::gdscript::library;
use crate::gdscript::library::macros::MacroState;
use crate::gdscript::decl::Static;
use crate::runner::macro_server::named_file_server::MacroCall;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;

use std::convert::{TryFrom, TryInto};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};

// TODO If a macro (from GDLisp) needs to use a GDScript resource,
// what do we even do? I can't trace dependencies on the GDScript
// side.

pub struct IncCompiler {
  names: FreshNameGenerator<'static>,
  symbols: SymbolTable,
  macros: HashMap<String, MacroData>,
  imports: Vec<ImportDecl>,
  minimalist: bool, // A minimalist compiler doesn't use stdlib and doesn't do macro expansion
}

#[allow(clippy::new_without_default)]
impl IncCompiler {

  pub fn new(names: Vec<&str>) -> IncCompiler {
    let names = FreshNameGenerator::new(names).to_owned_names();
    IncCompiler {
      names: names,
      symbols: SymbolTable::new(),
      macros: HashMap::new(),
      imports: vec!(),
      minimalist: false,
    }
  }

  fn resolve_macro_call(&mut self, pipeline: &mut Pipeline, head: &str, tail: &[&AST])
                        -> Result<AST, PError> {
    match self.macros.get(head) {
      Some(MacroData { id, args: parms, .. }) => {
        // If we're in a minimalist file, then macro expansion is automatically an error
        if self.minimalist {
          return Err(PError::from(Error::MacroInMinimalistError(head.to_owned())));
        }

        if id.is_reserved() {
          // Reserved for built-in macros; it runs in Rust
          let func = library::macros::get_builtin_macro(*id).ok_or_else(|| PError::from(Error::NoSuchFn(head.to_owned())))?;
          let state = MacroState { generator: &mut self.names };
          let ast = func(state, tail)?;
          Ok(ast)
        } else {
          // User-defined macro; runs in Godot
          let call = self.get_macro_file(pipeline, &head).expect("Could not find macro file").clone();
          let args: Vec<_> = tail.iter().map(|x| x.reify()).collect();

          let server = pipeline.get_server_mut();
          server.set_global_name_generator(&self.names)?;
          let ast = server.run_server_file(&call, parms.clone(), args);
          server.reset_global_name_generator()?;

          // Check the error on `ast' here, not above, because we want
          // reset_global_name_generator to run if and only if
          // set_global_name_generator runs.
          Ok(ast?)
        }
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
        if self.macros.get(&head).is_some() {
          return self.resolve_macro_call(pipeline, &head, tail).map(Some);
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
    } else if self.macros.get(head).is_some() {
      let result = self.resolve_macro_call(pipeline, head, tail)?;
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
      AST::Dictionary(vec) => {
        let vec = vec.iter().map(|(k, v)| Ok((self.compile_expr(pipeline, k)?, self.compile_expr(pipeline, v)?))).collect::<Result<Vec<_>, PError>>()?;
        Ok(Expr::Dictionary(vec))
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
            let (mods, body) = modifier::function::parser().parse(&vec[3..])?;
            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            let mut decl = decl::FnDecl {
              visibility: Visibility::FUNCTION,
              call_magic: None,
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            };
            for m in mods {
              m.apply(&mut decl);
            }
            Ok(Decl::FnDecl(decl))
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
            let (mods, body) = modifier::macros::parser().parse(&vec[3..])?;
            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            let mut decl = decl::MacroDecl {
              visibility: Visibility::MACRO,
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            };
            for m in mods {
              m.apply(&mut decl);
            }
            Ok(Decl::MacroDecl(decl))
          }
          "defconst" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let value = self.compile_expr(pipeline, vec[2])?;
            let (mods, body) = modifier::constant::parser().parse(&vec[3..])?;
            if !body.is_empty() {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let mut decl = decl::ConstDecl { visibility: Visibility::CONST, name, value };
            for m in mods {
              m.apply(&mut decl);
            }
            Ok(Decl::ConstDecl(decl))
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
            let (mods, decl_body) = modifier::class::parser().parse(&vec[3..])?;
            let mut class = decl::ClassDecl::new(name, superclass);
            for m in mods {
              m.apply(&mut class);
            }
            for decl in decl_body {
              self.compile_class_inner_decl(pipeline, &mut class, decl)?;
            }
            Ok(Decl::ClassDecl(class))
          }
          "defenum" => {
            if vec.len() < 2 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let (mods, body) = modifier::enums::parser().parse(&vec[2..])?;
            let clauses = body.iter().map(|clause| {
              let clause = match clause {
                AST::Symbol(_) => AST::list(vec!((*clause).clone())),
                _ => (*clause).clone(),
              };
              let clause = Vec::try_from(DottedExpr::new(&clause))?;
              let (name, value) = match &clause[..] {
                [name] => (name, None),
                [name, value] => (name, Some(value)),
                _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
              };
              let name = match name {
                AST::Symbol(s) => s.to_owned(),
                _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
              };
              let value = value.map(|v| self.compile_expr(pipeline, v)).transpose()?;
              Ok((name, value))
            }).collect::<Result<_, _>>()?;
            let mut enum_decl = decl::EnumDecl { visibility: Visibility::ENUM, name, clauses };
            for m in mods {
              m.apply(&mut enum_decl);
            }
            Ok(Decl::EnumDecl(enum_decl))
          }
          "sys/declare" => {
            // (sys/declare value name)
            // (sys/declare function name (args...))
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            match vec[1] {
              AST::Symbol(value) if value == "value" || value == "superglobal" => {
                if vec.len() > 3 {
                  return Err(PError::from(Error::InvalidDecl(decl.clone())));
                }
                let name = match vec[2] {
                  AST::Symbol(s) => s.to_owned(),
                  _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
                };
                let declare_type =
                  if value == "superglobal" {
                    decl::DeclareType::Superglobal
                  } else {
                    decl::DeclareType::Value
                  };
                let declare = decl::DeclareDecl {
                  visibility: Visibility::DECLARE,
                  declare_type,
                  name
                };
                Ok(Decl::DeclareDecl(declare))
              }
              AST::Symbol(function) if function == "function" => {
                if vec.len() != 4 {
                  return Err(PError::from(Error::InvalidDecl(decl.clone())));
                }
                let name = match vec[2] {
                  AST::Symbol(s) => s.to_owned(),
                  _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
                };
                let args: Vec<_> = DottedExpr::new(vec[3]).try_into()?;
                let args = ArgList::parse(args)?;
                let declare = decl::DeclareDecl {
                  visibility: Visibility::DECLARE,
                  declare_type: decl::DeclareType::Function(args),
                  name
                };
                Ok(Decl::DeclareDecl(declare))
              }
              _ => {
                Err(PError::from(Error::InvalidDecl(decl.clone())))
              }
            }
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

  // TODO Do we need to take two tables here (static and non-static) like we do in compile?
  pub fn compile_class_inner_decl(&mut self,
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
        "defconst" => {
          // TODO Combine this with the other defconst in a helper function
          if vec.len() != 3 {
            return Err(PError::from(Error::InvalidDecl(curr.clone())));
          }
          let name = match vec[1] {
            AST::Symbol(s) => s.to_owned(),
            _ => return Err(PError::from(Error::InvalidDecl(curr.clone()))),
          };
          let value = self.compile_expr(pipeline, vec[2])?;
          acc.decls.push(decl::ClassInnerDecl::ClassConstDecl(decl::ConstDecl { visibility: Visibility::CONST, name, value }));
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

            // Determine if static
            let (mods, body) = modifier::instance_method::parser().parse(&vec[3..])?;

            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            if fname == "_init" {
              // Constructor
              acc.constructor = decl::ConstructorDecl { args, body: Expr::Progn(body) };
              for m in mods {
                m.apply_to_constructor(&mut acc.constructor)?;
              }
            } else {
              let mut decl = decl::ClassFnDecl {
                is_static: Static::NonStatic,
                name: fname.to_owned(),
                args,
                body: Expr::Progn(body),
              };
              for m in mods {
                m.apply(&mut decl);
              }
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
      if !vec.is_empty() && *vec[0] == AST::symbol("use") {
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
      let res_type = ResourceType::from(&imp);
      if res_type.can_have_macros() {
        let file = pipeline.load_file(imp.filename.path())?;
        self.import_macros_from(&file, &imp);
      }
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

    // File-level modifiers
    let (mods, body) = modifier::file::parser().parse(&body)?;
    for m in mods {
      m.apply(&mut self);
    }

    // Compile
    let mut main: Vec<Expr> = Vec::new();
    for curr in body {
      self.compile_decl_or_expr(pipeline, &mut main, curr)?;
    }
    let main_decl = Decl::FnDecl(decl::FnDecl {
      visibility: Visibility::FUNCTION,
      call_magic: None,
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
    let tmpfile = macros::create_macro_file(pipeline, self.imports.clone(), &self.symbols, names, self.minimalist)?;
    let m_id = pipeline.get_server_mut().stand_up_file(name.to_owned(), tmpfile)?;
    self.macros.insert(name.to_owned(), MacroData { id: m_id, args: decl.args, imported: false });

    Ok(())
  }

  pub fn locally_save_macro<B>(&mut self, name: &str, func: impl FnOnce(&mut Self) -> B) -> B {
    let saved_value = self.macros.remove(name);
    let result = func(self);
    if let Some(saved_value) = saved_value {
      self.macros.insert(name.to_string(), saved_value);
    }
    result
  }

  pub fn has_macro(&self, name: &str) -> bool {
    self.macros.contains_key(name)
  }

  pub fn unbind_macro(&mut self, name: &str) {
    self.macros.remove(name);
  }

  pub fn bind_builtin_macros(&mut self) {
    library::bind_builtin_macros(&mut self.macros);
  }

  pub fn symbol_table(&mut self) -> &mut SymbolTable {
    &mut self.symbols
  }

  pub fn mark_as_minimalist(&mut self) {
    self.minimalist = true;
  }

}

impl From<IncCompiler> for (decl::TopLevel, HashMap<String, MacroData>) {

  fn from(compiler: IncCompiler) -> (decl::TopLevel, HashMap<String, MacroData>) {
    let toplevel = decl::TopLevel {
      imports: compiler.imports,
      decls: compiler.symbols.into(),
      minimalist_flag: compiler.minimalist,
    };
    let macros: HashMap<_, _> = compiler.macros.into_iter().filter(|(_, x)| !x.imported).collect();
    (toplevel, macros)
  }

}
