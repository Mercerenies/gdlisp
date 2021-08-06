
// Incremental compilation (supplies backbone for macro resolution)

use super::declaration_table::DeclarationTable;
use super::MAIN_BODY_NAME;
use super::call_name::CallName;
use super::arglist::{ArgList, SimpleArgList};
use super::literal::Literal;
use super::import::{ImportDecl, ImportName};
use super::expr::{Expr, ExprF};
use super::special_form;
use super::depends::Dependencies;
use super::decl::{self, Decl, DeclF};
use super::macros::{self, MacroData};
use super::identifier::{Id, IdLike, Namespace};
use super::modifier::{self, ParseRule};
use super::export::Visibility;
use crate::sxp::dotted::{DottedExpr, TryFromDottedExprError};
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::reify::Reify;
use crate::compile::error::Error;
use crate::compile::resource_type::ResourceType;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::gdscript::library;
use crate::gdscript::decl::Static;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;
use crate::pipeline::source::SourceOffset;

use std::convert::{TryFrom, TryInto};
use std::borrow::{Cow, Borrow};
use std::collections::{HashMap, HashSet};

// TODO If a macro (from GDLisp) needs to use a GDScript resource,
// what do we even do? I can't trace dependencies on the GDScript
// side.

pub struct IncCompiler {
  names: FreshNameGenerator<'static>,
  table: DeclarationTable,
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
      table: DeclarationTable::new(),
      macros: HashMap::new(),
      imports: vec!(),
      minimalist: false,
    }
  }

  fn resolve_macro_call(&mut self, pipeline: &mut Pipeline, head: &str, tail: &[&AST], pos: SourceOffset)
                        -> Result<AST, PError> {
    match self.macros.get(head) {
      Some(MacroData { id, .. }) => {
        // If we're in a minimalist file, then macro expansion is automatically an error
        if self.minimalist {
          return Err(PError::from(Error::MacroInMinimalistError(head.to_owned())));
        }

        let args: Vec<_> = tail.iter().map(|x| x.reify()).collect();
        let server = pipeline.get_server_mut();
        server.set_global_name_generator(&self.names)?;

        let ast = server.run_server_file(*id, args, pos);

        server.reset_global_name_generator()?;

        ast
      }
      _ => {
        Err(PError::from(Error::NoSuchFn(head.to_owned())))
      }
    }
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
          return self.resolve_macro_call(pipeline, &head, tail, ast.pos).map(Some);
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
      match &ast.value {
        ASTF::Symbol(s) => Ok(CallName::SimpleName(s.clone())),
        _ => Err(PError::from(Error::CannotCall(ast.clone()))),
      }
    }
  }

  pub fn resolve_simple_call(&mut self, pipeline: &mut Pipeline, head: &str, tail: &[&AST], pos: SourceOffset)
                             -> Result<Expr, PError> {
    if let Some(sf) = special_form::dispatch_form(self, pipeline, head, tail, pos)? {
      Ok(sf)
    } else if self.macros.get(head).is_some() {
      let result = self.resolve_macro_call(pipeline, head, tail, pos)?;
      self.compile_expr(pipeline, &result)
    } else {
      let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
      Ok(Expr::new(ExprF::Call(head.to_owned(), args), pos))
    }
  }

  pub fn compile_expr(&mut self, pipeline: &mut Pipeline, expr: &AST) -> Result<Expr, PError> {
    match &expr.value {
      ASTF::Nil | ASTF::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        if vec.is_empty() {
          Ok(Expr::literal(Literal::Nil, expr.pos))
        } else {
          let head = self.resolve_call_name(pipeline, vec[0])?;
          let tail = &vec[1..];
          match head {
            CallName::SimpleName(head) => {
              self.resolve_simple_call(pipeline, &head, tail, expr.pos)
            }
            CallName::MethodName(target, head) => {
              let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
              Ok(Expr::new(ExprF::MethodCall(target, head, args), expr.pos))
            }
          }
        }
      }
      ASTF::Array(vec) => {
        let vec = vec.iter().map(|e| self.compile_expr(pipeline, e)).collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::new(ExprF::Array(vec), expr.pos))
      }
      ASTF::Dictionary(vec) => {
        let vec = vec.iter().map(|(k, v)| Ok((self.compile_expr(pipeline, k)?, self.compile_expr(pipeline, v)?))).collect::<Result<Vec<_>, PError>>()?;
        Ok(Expr::new(ExprF::Dictionary(vec), expr.pos))
      }
      ASTF::Int(n) => {
        Ok(Expr::new(ExprF::Literal(Literal::Int(*n)), expr.pos))
      }
      ASTF::Bool(b) => {
        Ok(Expr::new(ExprF::Literal(Literal::Bool(*b)), expr.pos))
      }
      ASTF::Float(f) => {
        Ok(Expr::new(ExprF::Literal(Literal::Float(*f)), expr.pos))
      }
      ASTF::String(s) => {
        Ok(Expr::new(ExprF::Literal(Literal::String(s.to_owned())), expr.pos))
      }
      ASTF::Symbol(s) => {
        Ok(Expr::new(ExprF::LocalVar(s.to_string()), expr.pos))
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
    match &vec[0].value {
      ASTF::Symbol(s) => {
        match s.borrow() {
          "defn" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s,
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
              body: Expr::progn(body, vec[0].pos),
            };
            for m in mods {
              m.apply(&mut decl);
            }
            Ok(Decl::new(DeclF::FnDecl(decl), vec[0].pos))
          }
          "defmacro" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s,
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
              body: Expr::progn(body, vec[0].pos),
            };
            for m in mods {
              m.apply(&mut decl);
            }
            Ok(Decl::new(DeclF::MacroDecl(decl), vec[0].pos))
          }
          "defconst" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s.to_owned(),
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
            Ok(Decl::new(DeclF::ConstDecl(decl), vec[0].pos))
          }
          "defclass" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let superclass = match &vec[2].value {
              ASTF::Cons(car, cdr) =>
                match (&car.value, &cdr.value) {
                  (ASTF::Symbol(superclass_name), ASTF::Nil) => superclass_name.to_owned(),
                  _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
                },
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let (mods, decl_body) = modifier::class::parser().parse(&vec[3..])?;
            let mut class = decl::ClassDecl::new(name, superclass, vec[0].pos);
            for m in mods {
              m.apply(&mut class);
            }
            for decl in decl_body {
              self.compile_class_inner_decl(pipeline, &mut class, decl)?;
            }
            Ok(Decl::new(DeclF::ClassDecl(class), vec[0].pos))
          }
          "defenum" => {
            if vec.len() < 2 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
            };
            let (mods, body) = modifier::enums::parser().parse(&vec[2..])?;
            let clauses = body.iter().map(|clause| {
              let clause = match &clause.value {
                ASTF::Symbol(_) => AST::dotted_list(vec!((*clause).clone()), AST::new(ASTF::Nil, clause.pos)),
                _ => (*clause).clone(),
              };
              let clause = Vec::try_from(DottedExpr::new(&clause))?;
              let (name, value) = match &clause[..] {
                [name] => (name, None),
                [name, value] => (name, Some(value)),
                _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
              };
              let name = match &name.value {
                ASTF::Symbol(s) => s.to_owned(),
                _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
              };
              let value = value.map(|v| self.compile_expr(pipeline, v)).transpose()?;
              Ok((name, value))
            }).collect::<Result<_, _>>()?;
            let mut enum_decl = decl::EnumDecl { visibility: Visibility::ENUM, name, clauses };
            for m in mods {
              m.apply(&mut enum_decl);
            }
            Ok(Decl::new(DeclF::EnumDecl(enum_decl), vec[0].pos))
          }
          "sys/declare" => {
            // (sys/declare value name)
            // (sys/declare function name (args...))
            if vec.len() < 3 {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            let (mut declare, body) = match &vec[1].value {
              ASTF::Symbol(value) if value == "value" || value == "superglobal" => {
                let name = match &vec[2].value {
                  ASTF::Symbol(s) => s.to_owned(),
                  _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
                };
                let declare_type =
                  if value == "superglobal" {
                    decl::DeclareType::Superglobal
                  } else {
                    decl::DeclareType::Value
                  };
                let decl = decl::DeclareDecl {
                  visibility: Visibility::DECLARE,
                  declare_type,
                  name
                };
                (decl, &vec[3..])
              }
              ASTF::Symbol(function) if function == "function" || function == "superfunction" => {
                let name = match &vec[2].value {
                  ASTF::Symbol(s) => s.to_owned(),
                  _ => return Err(PError::from(Error::InvalidDecl(decl.clone()))),
                };
                let args: Vec<_> = DottedExpr::new(vec[3]).try_into()?;
                let args = ArgList::parse(args)?;
                let declare_type =
                  if function == "superfunction" {
                    decl::DeclareType::SuperglobalFn(args)
                  } else {
                    decl::DeclareType::Function(args)
                  };
                let decl = decl::DeclareDecl {
                  visibility: Visibility::DECLARE,
                  declare_type,
                  name
                };
                (decl, &vec[4..])
              }
              _ => {
                return Err(PError::from(Error::InvalidDecl(decl.clone())))
              }
            };
            let (mods, body) = modifier::declare::parser().parse(&body)?;
            if !body.is_empty() {
              return Err(PError::from(Error::InvalidDecl(decl.clone())));
            }
            for m in mods {
              m.apply(&mut declare);
            }
            Ok(Decl::new(DeclF::DeclareDecl(declare), vec[0].pos))
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
    if let ASTF::Symbol(s) = &vec[0].value {
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
          let name = match &vec[1].value {
            ASTF::Symbol(s) => s.to_owned(),
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
          if let ASTF::Symbol(vname) = &vec[1].value {
            let name = vname.to_owned();

            // Parse value and export
            let mut value = None;
            let mut export = None;
            let mut idx = 2;
            // Value
            if let Some(v) = vec.get(idx) {
              if !(matches!(&v.value, ASTF::Cons(car, _) if car.value == ASTF::Symbol(String::from("export")))) {
                let e = self.compile_expr(pipeline, v)?;
                value = Some(e);
                idx += 1;
              }
            };
            // Export
            if let Some(v) = vec.get(idx) {
              if let DottedExpr { elements, terminal: AST { value: ASTF::Nil, pos: _ } } = DottedExpr::new(v) {
                if elements.get(0).map(|x| &x.value) == Some(&ASTF::Symbol(String::from("export"))) {
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
          if let ASTF::Symbol(fname) = &vec[1].value {
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = SimpleArgList::parse(args)?;

            // Determine if static
            let (mods, body) = modifier::instance_method::parser().parse(&vec[3..])?;

            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            if fname == "_init" {
              // Constructor
              acc.constructor = decl::ConstructorDecl { args, body: Expr::progn(body, vec[0].pos) };
              for m in mods {
                m.apply_to_constructor(&mut acc.constructor)?;
              }
            } else {
              let mut decl = decl::ClassFnDecl {
                is_static: Static::NonStatic,
                name: fname.to_owned(),
                args,
                body: Expr::progn(body, vec[0].pos),
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
          let name = match &vec[1].value {
            ASTF::Symbol(s) => s.to_owned(),
            _ => return Err(PError::from(Error::InvalidDecl(curr.clone()))),
          };
          let nil = AST::new(ASTF::Nil, vec[0].pos);
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
      if !vec.is_empty() && vec[0].value == ASTF::symbol("use") {
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
      if !vec.is_empty() && matches!(&vec[0].value, ASTF::Symbol(progn) if progn == "progn") {
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
          self.table.add(d.clone());
          if let DeclF::MacroDecl(mdecl) = d.value {
            self.bind_macro(pipeline, mdecl, d.pos, false)?;
          }
        }
      };
    }
    Ok(())
  }

  pub fn compile_toplevel(mut self, pipeline: &mut Pipeline, body: &AST)
                          -> Result<(decl::TopLevel, HashMap<String, MacroData>), PError> {
    let pos = body.pos;
    let body: Result<Vec<_>, TryFromDottedExprError> = DottedExpr::new(body).try_into();
    let body: Vec<_> = body?; // *sigh* Sometimes the type checker just doesn't get it ...

    // File-level modifiers
    let (mods, body) = modifier::file::parser().parse(&body)?;
    for m in mods {
      m.apply(&mut self);
    }

    self.bind_builtin_macros(pipeline); // No-op if minimalist is true.

    // Compile
    let mut main: Vec<Expr> = Vec::new();
    for curr in body {
      self.compile_decl_or_expr(pipeline, &mut main, curr)?;
    }
    let main_decl = DeclF::FnDecl(decl::FnDecl {
      visibility: Visibility::FUNCTION,
      call_magic: None,
      name: MAIN_BODY_NAME.to_owned(),
      args: ArgList::empty(),
      body: Expr::progn(main, pos),
    });
    // main_decl is synthesized from the file itself, so
    // SourceOffset(0) isn't just a cop-out here; it's the actual
    // right answer.
    let main_decl = Decl::new(main_decl, SourceOffset(0));
    self.table.add(main_decl);

    Ok(self.into())
  }

  pub fn bind_macro(&mut self, pipeline: &mut Pipeline, mut decl: decl::MacroDecl, pos: SourceOffset, generate_name: bool) -> Result<(), PError> {
    let orig_name = decl.name.to_owned();

    let tmp_name = if generate_name {
      self.names.generate_with("_macro")
    } else {
      orig_name.to_owned()
    };

    // bind_macro is a no-op in a minimalist compile
    if self.minimalist {
      return Ok(());
    }

    let translation_names = self.imports.iter().map(|import| {
      let unit = pipeline.load_file(&import.filename.path())?;
      Ok(import.names(&unit.exports))
    }).collect::<Result<Vec<_>, PError>>()?;
    let imported_names: HashSet<_> =
      translation_names.into_iter().flatten().map(ImportName::into_imported_id).collect();

    // If we're generating a name, then we need to modify the symbol
    // table to reflect that name.
    decl.name = tmp_name.to_owned();
    let table = if generate_name {
      let mut table = self.table.clone();
      table.add(Decl::new(DeclF::MacroDecl(decl.to_owned()), pos));
      Cow::Owned(table)
    } else {
      Cow::Borrowed(&self.table)
    };

    // Now we need to find the dependencies and spawn up the
    // server for the macro itself.
    let mut deps = Dependencies::identify(table.borrow(), &imported_names, &*Id::build(Namespace::Function, &tmp_name));
    deps.purge_unknowns(library::all_builtin_names(self.minimalist).iter().map(|x| x as &dyn IdLike));

    // Aside from built-in functions, it must be the case that
    // all referenced functions are already defined.
    let names = deps.try_into_knowns().map_err(Error::from)?;
    let tmpfile = macros::create_macro_file(pipeline, self.imports.clone(), table.borrow(), names, self.minimalist)?;
    let m_id = pipeline.get_server_mut().stand_up_macro(tmp_name, decl.args, tmpfile)?;
    self.macros.insert(orig_name, MacroData { id: m_id, imported: false });

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

  pub fn bind_builtin_macros(&mut self, pipeline: &mut Pipeline) {
    if !self.minimalist {
      library::bind_builtin_macros(&mut self.macros, pipeline);
    }
  }

  pub fn declaration_table(&mut self) -> &mut DeclarationTable {
    &mut self.table
  }

  pub fn mark_as_minimalist(&mut self) {
    self.minimalist = true;
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator<'static> {
    &mut self.names
  }

}

impl From<IncCompiler> for (decl::TopLevel, HashMap<String, MacroData>) {

  fn from(compiler: IncCompiler) -> (decl::TopLevel, HashMap<String, MacroData>) {
    let toplevel = decl::TopLevel {
      imports: compiler.imports,
      decls: compiler.table.into(),
      minimalist_flag: compiler.minimalist,
    };
    let macros: HashMap<_, _> = compiler.macros.into_iter().filter(|(_, x)| !x.imported).collect();
    (toplevel, macros)
  }

}
