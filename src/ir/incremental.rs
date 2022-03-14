
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
use crate::sxp::reify::pretty::reify_pretty_expr;
use crate::compile::error::{Error, ErrorF};
use crate::compile::resource_type::ResourceType;
use crate::compile::names;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::frame::MAX_QUOTE_REIFY_DEPTH;
use crate::compile::body::class_initializer::InitTime;
use crate::gdscript::library;
use crate::gdscript::decl::Static;
use crate::pipeline::error::{Error as PError, IOError};
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;
use crate::pipeline::source::SourceOffset;

use std::convert::{TryFrom, TryInto};
use std::borrow::{Cow, Borrow};
use std::hash::Hash;
use std::collections::{HashMap, HashSet};

// TODO If a macro (from GDLisp) needs to use a GDScript resource,
// what do we even do? I can't trace dependencies on the GDScript
// side.

pub struct IncCompiler {
  names: FreshNameGenerator,
  table: DeclarationTable,
  macros: HashMap<Id, MacroData>,
  imports: Vec<ImportDecl>,
  minimalist: bool, // A minimalist compiler doesn't use stdlib and doesn't do macro expansion
}

#[allow(clippy::new_without_default)]
impl IncCompiler {

  /// Here, we list the heads for all valid declaration types. Note
  /// that `progn` is specifically not included here; `progn` is an
  /// expression-level special form which is also a deeply magical
  /// construct treated in a special way by the compiler during
  /// parsing. It is *not* a declaration, even though it can look like
  /// one syntactically.
  pub const DECL_HEADS: [&'static str; 7] = [
    "defn", "defmacro", "defconst", "defclass", "defenum", "sys/declare",
    "define-symbol-macro",
  ];

  pub fn new(names: Vec<&str>) -> IncCompiler {
    let names = FreshNameGenerator::new(names);
    IncCompiler {
      names: names,
      table: DeclarationTable::new(),
      macros: HashMap::new(),
      imports: vec!(),
      minimalist: false,
    }
  }

  fn resolve_macro_call<K: IdLike>(&mut self, pipeline: &mut Pipeline, head: &K, tail: &[&AST], pos: SourceOffset)
                           -> Result<AST, PError>
  where Id : Borrow<K>,
        K : Hash + Eq + ToOwned<Owned=Id> + ?Sized {
    match self.macros.get(head) {
      Some(MacroData { id, .. }) => {
        // If we're in a minimalist file, then macro expansion is automatically an error
        if self.minimalist {
          let head = head.to_owned().name;
          return Err(PError::from(Error::new(ErrorF::MacroInMinimalistError(head), pos)));
        }

        // Local name generator that will be shared among the
        // arguments but not used outside of this function.
        let mut local_gen = FreshNameGenerator::new(vec!());

        let mut prelude = vec!();
        let mut args = vec!();
        for arg in tail {
          let (stmts, expr) = reify_pretty_expr(arg, MAX_QUOTE_REIFY_DEPTH, &mut local_gen);
          prelude.extend(stmts.into_iter());
          args.push(expr);
        }
        let server = pipeline.get_server_mut();
        server.set_global_name_generator(&self.names).map_err(|err| IOError::new(err, pos))?;

        let ast = server.run_server_file(*id, prelude, args, pos);

        server.reset_global_name_generator().map_err(|err| IOError::new(err, pos))?;

        // Set the source position for the entire macro expansion to
        // be the source of the macro call (TODO A long-term solution
        // for this is to make SourceOffset unbelievably complex, so
        // we can do cool errors that say "error occured at line 6,
        // which is line 8 of macro expansion for blah blah blah, but
        // that's very involved)
        let mut ast = ast?;
        ast.each_source_mut(|_| pos);

        Ok(ast)
      }
      _ => {
        let head = head.to_owned();
        match head.namespace {
          Namespace::Function => Err(PError::from(Error::new(ErrorF::NoSuchFn(head.name), pos))),
          Namespace::Value => Err(PError::from(Error::new(ErrorF::NoSuchVar(head.name), pos))),
        }
      }
    }
  }

  fn try_resolve_macro_call(&mut self, pipeline: &mut Pipeline, ast: &AST) -> Result<Option<AST>, PError> {
    let vec: Vec<&AST> = match DottedExpr::new(ast).try_into() {
      Err(TryFromDottedExprError { pos: _ }) => return Ok(None),
      Ok(v) => v,
    };
    if !vec.is_empty() {
      let head = CallName::resolve_call_name(self, pipeline, vec[0])?;
      if let CallName::SimpleName(head) = head {
        let tail = &vec[1..];
        if self.macros.get(&*Id::build(Namespace::Function, &head)).is_some() {
          return self.resolve_macro_call(pipeline, &*Id::build(Namespace::Function, &head), tail, ast.pos).map(Some);
        }
      }
    }
    Ok(None)
  }

  fn try_resolve_symbol_macro_call(&mut self, pipeline: &mut Pipeline, head: &str, pos: SourceOffset) -> Result<Option<AST>, PError> {
    if self.macros.get(&*Id::build(Namespace::Value, &head)).is_some() {
      return self.resolve_macro_call(pipeline, &*Id::build(Namespace::Value, &head), &[], pos).map(Some);
    }
    Ok(None)
  }

  pub fn resolve_simple_call(&mut self, pipeline: &mut Pipeline, head: &str, tail: &[&AST], pos: SourceOffset)
                             -> Result<Expr, PError> {
    if let Some(sf) = special_form::dispatch_form(self, pipeline, head, tail, pos)? {
      Ok(sf)
    } else if self.macros.get(&*Id::build(Namespace::Function, head)).is_some() {
      let result = self.resolve_macro_call(pipeline, &*Id::build(Namespace::Function, head), tail, pos)?;
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
          let head = CallName::resolve_call_name(self, pipeline, vec[0])?;
          let tail = &vec[1..];
          // TODO Can this be part of CallName?
          match head {
            CallName::SimpleName(head) => {
              self.resolve_simple_call(pipeline, &head, tail, expr.pos)
            }
            CallName::MethodName(target, head) => {
              let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
              Ok(Expr::new(ExprF::MethodCall(target, head, args), expr.pos))
            }
            CallName::AtomicName(head) => {
              let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
              Ok(Expr::new(ExprF::AtomicCall(head, args), expr.pos))
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
        // Symbol macro resolution
        let macro_result = self.try_resolve_symbol_macro_call(pipeline, &s, expr.pos)?;
        if let Some(macro_result) = macro_result {
          self.compile_expr(pipeline, &macro_result)
        } else {
          Ok(Expr::new(ExprF::LocalVar(s.to_string()), expr.pos))
        }
      }
    }
  }

  /// A simple check to see whether a given AST should be parsed as a
  /// declaration or not.
  ///
  /// There are many contexts (including the very top-level of a file)
  /// where either a declaration or an expression is acceptable. This
  /// function is used to determine whether the AST should be parsed
  /// as a declaration or an expression. Note that a `true` result for
  /// `is_decl` is *not* a guarantee that parsing as a declaration
  /// will be error-free; it is merely an indication that the
  /// declaration parse should be attempted.
  ///
  /// If `decl` is not a proper list (as per the definition in
  /// [`DottedExpr`](crate::sxp::dotted::DottedExpr)), then `is_decl`
  /// returns false. Otherwise, the first term of the list is checked
  /// against several known symbol values
  /// ([`IncCompiler::DECL_HEADS`]) to determine if the AST represents
  /// a declaration.
  pub fn is_decl(decl: &AST) -> bool {
    let vec: Result<Vec<&AST>, _> = DottedExpr::new(decl).try_into();
    if let Ok(vec) = vec {
      if let Some(AST { value: ASTF::Symbol(head), pos: _ }) = vec.get(0) {
        return IncCompiler::DECL_HEADS.contains(&head.borrow());
      }
    }
    false
  }

  /// Given the body of a [`decl::FnDecl`] or a possible
  /// [`decl::ConstructorDecl`], check to see if the first expression
  /// present is a "super" call.
  ///
  /// If a "super" call is present, its arguments, as well as a slice
  /// of the rest of the body, is returned. Otherwise, the whole slice
  /// is returned untouched.
  pub fn detect_super<'a, 'b>(body: &'a [&'b AST]) -> (Option<Vec<&'b AST>>, &'a [&'b AST]) {
    if !body.is_empty() {
      let first: Result<Vec<_>, _> = DottedExpr::new(&body[0]).try_into();
      if let Ok(mut first) = first {
        if !first.is_empty() && first[0].value == ASTF::Symbol(String::from("super")) {
          first.remove(0);
          let super_args = first;
          return (Some(super_args), &body[1..]);
        }
      }
    }
    (None, body)
  }

  pub fn compile_decl(&mut self, pipeline: &mut Pipeline, decl: &AST)
                      -> Result<Decl, PError> {
    let vec: Vec<&AST> = DottedExpr::new(decl).try_into()?;
    if vec.is_empty() {
      return Err(PError::from(Error::new(ErrorF::UnknownDecl(decl.clone()), decl.pos)));
    }
    match &vec[0].value {
      ASTF::Symbol(s) => {
        match s.borrow() {
          "defn" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s,
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
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
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s,
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
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
          "define-symbol-macro" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s,
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
            };
            let value = self.compile_expr(pipeline, vec[2])?;
            let (mods, body) = modifier::macros::parser().parse(&vec[3..])?;
            if !body.is_empty() {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let mut decl = decl::SymbolMacroDecl {
              visibility: Visibility::SYMBOL_MACRO,
              name: name.to_owned(),
              body: value,
            };
            for m in mods {
              m.apply_to_symbol_macro(&mut decl);
            }
            Ok(Decl::new(DeclF::SymbolMacroDecl(decl), vec[0].pos))
          }
          "defconst" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
            };
            let value = self.compile_expr(pipeline, vec[2])?;
            let (mods, body) = modifier::constant::parser().parse(&vec[3..])?;
            if !body.is_empty() {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let mut decl = decl::ConstDecl { visibility: Visibility::CONST, name, value };
            for m in mods {
              m.apply(&mut decl);
            }
            Ok(Decl::new(DeclF::ConstDecl(decl), vec[0].pos))
          }
          "defclass" => {
            if vec.len() < 3 {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
            };
            let superclass = match &vec[2].value {
              ASTF::Nil =>
                library::REFERENCE_NAME.to_owned(),
              ASTF::Cons(car, cdr) =>
                match (&car.value, &cdr.value) {
                  (ASTF::Symbol(superclass_name), ASTF::Nil) => superclass_name.to_owned(),
                  _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
                },
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
            };
            let (mods, decl_body) = modifier::class::parser().parse(&vec[3..])?;
            let mut class = decl::ClassDecl::new(name, superclass);
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
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let name = match &vec[1].value {
              ASTF::Symbol(s) => s.to_owned(),
              _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
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
                _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
              };
              let name = match &name.value {
                ASTF::Symbol(s) => s.to_owned(),
                _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
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
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            let (mut declare, body) = match &vec[1].value {
              ASTF::Symbol(value) if value == "value" || value == "superglobal" => {
                let name = match &vec[2].value {
                  ASTF::Symbol(s) => s.to_owned(),
                  _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
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
                if vec.len() < 4 {
                  return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
                }
                let name = match &vec[2].value {
                  ASTF::Symbol(s) => s.to_owned(),
                  _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos))),
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
                return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)))
              }
            };
            let (mods, body) = modifier::declare::parser().parse(&body)?;
            if !body.is_empty() {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)));
            }
            for m in mods {
              m.apply(&mut declare);
            }
            Ok(Decl::new(DeclF::DeclareDecl(declare), vec[0].pos))
          }
          _ => {
            Err(PError::from(Error::new(ErrorF::UnknownDecl(decl.clone()), decl.pos)))
          }
        }
      }
      _ => {
        Err(PError::from(Error::new(ErrorF::InvalidDecl(decl.clone()), decl.pos)))
      }
    }
  }

  // TODO Do we need to take two tables here (static and non-static) like we do in compile?
  pub fn compile_class_inner_decl(&mut self,
                                  pipeline: &mut Pipeline,
                                  acc: &mut decl::ClassDecl,
                                  curr: &AST)
                                  -> Result<(), PError> {

    // Deal with macros
    let mut candidate: Option<AST>;
    let mut curr = curr;
    while let Some(ast) = self.try_resolve_macro_call(pipeline, curr)? {
      candidate = Some(ast);
      curr = &candidate.as_ref().unwrap();
    }

    let vec = Vec::try_from(DottedExpr::new(curr)).map_err(|x| Error::from_value(x, curr.pos))?;
    if vec.is_empty() {
      return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)));
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
            return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)));
          }
          let name = match &vec[1].value {
            ASTF::Symbol(s) => s.to_owned(),
            _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos))),
          };
          let value = self.compile_expr(pipeline, vec[2])?;
          acc.decls.push(decl::ClassInnerDecl::new(decl::ClassInnerDeclF::ClassConstDecl(decl::ConstDecl { visibility: Visibility::CONST, name, value }), vec[0].pos));
          Ok(())
        }
        "defvar" => {
          if vec.len() < 2 {
            return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)));
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
            // Modifiers
            let (mods, body) = modifier::var::parser().parse(&vec[idx..])?;
            // (Extra)
            if !body.is_empty() {
              return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)));
            }

            // Exports are only allowed on the main class
            if export.is_some() && !acc.main_class {
              return Err(PError::from(Error::new(ErrorF::ExportOnInnerClassVar(vname.clone()), curr.pos)));
            }

            let decl = {
              let mut decl = decl::ClassVarDecl { export, name, value, init_time: InitTime::default() };
              for m in mods {
                m.apply(&mut decl);
              }
              decl
            };
            acc.decls.push(decl::ClassInnerDecl::new(decl::ClassInnerDeclF::ClassVarDecl(decl), vec[0].pos));
            Ok(())
          } else {
            Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)))
          }
        }
        "defn" => {
          // TODO Unify some of this with the equivalent code from compile_decl?
          if vec.len() < 3 {
            return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)));
          }
          if let ASTF::Symbol(fname) = &vec[1].value {
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = SimpleArgList::parse(args, vec[2].pos)?;

            // Determine if static
            let (mods, body) = modifier::instance_method::parser().parse(&vec[3..])?;

            // Check for super call (only valid in constructor, but we
            // run the check unconditionally)
            let (super_call, body) = IncCompiler::detect_super(body);
            let super_call = super_call.map(|super_call| {
              super_call.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()
            }).transpose()?;

            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            if fname == library::CONSTRUCTOR_NAME {
              // Constructor

              // There can only be one constructor defined in the class
              if acc.constructor.is_some() {
                return Err(PError::from(Error::new(ErrorF::DuplicateConstructor, vec[0].pos)));
              }

              let super_call = super_call.unwrap_or_default();
              let mut constructor = decl::ConstructorDecl { args, super_call, body: Expr::progn(body, vec[0].pos) };
              for m in mods {
                m.apply_to_constructor(&mut constructor)?;
              }
              acc.constructor = Some(constructor);

            } else {
              // Ordinary functions cannot have super

              if super_call.is_some() {
                return Err(PError::from(Error::new(ErrorF::NoSuchFn(String::from("super")), vec[0].pos)));
              }
              let mut decl = decl::ClassFnDecl {
                is_static: Static::NonStatic,
                name: fname.to_owned(),
                args,
                body: Expr::progn(body, vec[0].pos),
              };
              for m in mods {
                m.apply(&mut decl);
              }
              acc.decls.push(decl::ClassInnerDecl::new(decl::ClassInnerDeclF::ClassFnDecl(decl), vec[0].pos));

            }
            Ok(())
          } else {
            Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)))
          }
        }
        "defsignal" => {
          if vec.len() < 2 || vec.len() > 3 {
            return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)));
          }
          let name = match &vec[1].value {
            ASTF::Symbol(s) => s.to_owned(),
            _ => return Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos))),
          };
          let nil = AST::new(ASTF::Nil, vec[0].pos);
          let args = vec.get(2).map_or(&nil, |x| *x);
          let args_pos = args.pos;
          let args: Vec<_> = DottedExpr::new(args).try_into()?;
          let args = SimpleArgList::parse(args, args_pos)?;
          acc.decls.push(decl::ClassInnerDecl::new(decl::ClassInnerDeclF::ClassSignalDecl(decl::ClassSignalDecl { name, args }), vec[0].pos));
          Ok(())
        }
        _ => {
          Err(PError::from(Error::new(ErrorF::InvalidDecl(curr.clone()), curr.pos)))
        }
      }
    } else {
      Err(PError::from(Error::new(ErrorF::UnknownDecl(curr.clone()), curr.pos)))
    }
  }

  pub fn compile_import(&mut self, curr: &AST) -> Result<Option<ImportDecl>, Error> {
    if let Ok(vec) = Vec::try_from(DottedExpr::new(curr)) {
      if !vec.is_empty() && vec[0].value == ASTF::symbol("use") {
        return ImportDecl::parse(&vec[1..]).map_err(|x| Error::from_value(x, curr.pos)).map(Some);
      }
    }
    Ok(None)
  }

  fn import_macros_from(&mut self, unit: &TranslationUnit, import: &ImportDecl) {
    for imp in import.names(&unit.exports) {
      let ImportName { namespace: namespace, in_name: import_name, out_name: export_name } = imp;
      if let Some(data) = unit.macros.get(&*Id::build(namespace, &export_name)) {
        self.macros.insert(Id::new(namespace, import_name), data.to_imported());
      };
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
    if let Some(imp) = self.compile_import(curr)? { // TODO Consider doing the is_decl thing with imports so we have a nice, pure function called is_import to call here instead?
      let res_type = ResourceType::from(&imp);
      if res_type.can_have_macros() {
        let file = pipeline.load_file(imp.filename.path())?;
        self.import_macros_from(&file, &imp);
      }
      self.imports.push(imp);
    } else if IncCompiler::is_decl(curr) {
      let d = self.compile_decl(pipeline, curr)?;
      self.table.add(d.clone());
      if let DeclF::MacroDecl(mdecl) = d.value {
        self.bind_macro(pipeline, mdecl, d.pos, false, Namespace::Function)?;
      } else if let DeclF::SymbolMacroDecl(mdecl) = d.value {
        let mdecl = decl::MacroDecl::from(mdecl);
        self.bind_macro(pipeline, mdecl, d.pos, true, Namespace::Value)?;
      }
    } else {
      main.push(self.compile_expr(pipeline, curr)?);
    }
    Ok(())
  }

  pub fn compile_toplevel(mut self, pipeline: &mut Pipeline, body: &AST)
                          -> Result<(decl::TopLevel, HashMap<Id, MacroData>), PError> {
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

  pub fn bind_macro(&mut self, pipeline: &mut Pipeline, mut decl: decl::MacroDecl, pos: SourceOffset, generate_name: bool, namespace: Namespace) -> Result<(), PError> {
    let orig_name = decl.name.to_owned();

    let tmp_name = if generate_name {
      self.names.generate_with("_macro")
    } else {
      orig_name.to_owned()
    };

    // bind_macro is a no-op in a minimalist compile
    if self.minimalist {
      let id = pipeline.get_server_mut().add_reserved_macro(names::lisp_to_gd(&orig_name), decl.args);
      self.macros.insert(Id::new(namespace, orig_name), MacroData { id, imported: false });
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
      match namespace {
        Namespace::Function => {
          table.add(Decl::new(DeclF::MacroDecl(decl.to_owned()), pos))
        }
        Namespace::Value => {
          let symdecl = decl::SymbolMacroDecl { visibility: decl.visibility, name: decl.name.to_owned(), body: decl.body.clone() };
          table.add(Decl::new(DeclF::SymbolMacroDecl(symdecl), pos))
        }
      }
      Cow::Owned(table)
    } else {
      Cow::Borrowed(&self.table)
    };

    // Now we need to find the dependencies and spawn up the
    // server for the macro itself.
    let mut deps = Dependencies::identify(table.borrow(), &imported_names, &*Id::build(namespace, &tmp_name));
    deps.purge_unknowns(library::all_builtin_names(self.minimalist).iter().map(|x| x as &dyn IdLike));

    // Aside from built-in functions, it must be the case that
    // all referenced functions are already defined.
    let names = deps.try_into_knowns().map_err(|x| Error::from_value(x, pos))?;
    let tmpfile = macros::create_macro_file(pipeline, self.imports.clone(), table.borrow(), names, pos, self.minimalist)?;
    let m_id = pipeline.get_server_mut().stand_up_macro(tmp_name, decl.args, tmpfile).map_err(|err| IOError::new(err, pos))?;
    self.macros.insert(Id::new(namespace, orig_name), MacroData { id: m_id, imported: false });

    Ok(())
  }

  pub fn locally_save_macro<B, K>(&mut self, name: &K, func: impl FnOnce(&mut Self) -> B) -> B
  where Id : Borrow<K>,
        K : Hash + Eq + ToOwned<Owned=Id> + ?Sized {
    let saved_value = self.macros.remove(name);
    let result = func(self);
    if let Some(saved_value) = saved_value {
      self.macros.insert(name.to_owned(), saved_value);
    }
    result
  }

  pub fn has_macro<K>(&self, name: &K) -> bool
  where Id : Borrow<K>,
    K : Hash + Eq + ToOwned<Owned=Id> + ?Sized {
    self.macros.contains_key(name)
  }

  pub fn unbind_macro<K>(&mut self, name: &K)
  where Id : Borrow<K>,
        K : Hash + Eq + ToOwned<Owned=Id> + ?Sized {
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

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator {
    &mut self.names
  }

}

impl From<IncCompiler> for (decl::TopLevel, HashMap<Id, MacroData>) {

  fn from(compiler: IncCompiler) -> (decl::TopLevel, HashMap<Id, MacroData>) {
    let toplevel = decl::TopLevel {
      imports: compiler.imports,
      decls: compiler.table.into(),
      minimalist_flag: compiler.minimalist,
    };
    let macros: HashMap<_, _> = compiler.macros.into_iter().filter(|(_, x)| !x.imported).collect();
    (toplevel, macros)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser;

  fn parse_ast(input: &str) -> AST {
    let parser = parser::ASTParser::new();
    parser.parse(input).unwrap()
  }

  // TODO More tests

  #[test]
  fn is_decl_test() {
    assert!(IncCompiler::is_decl(&parse_ast("(defn foo ())")));
    assert!(IncCompiler::is_decl(&parse_ast("(defclass Example (Reference) (defn bar (x)))")));
    assert!(IncCompiler::is_decl(&parse_ast("(defmacro foo ())")));
    assert!(IncCompiler::is_decl(&parse_ast("(defconst MY_CONST 3)")));
    assert!(IncCompiler::is_decl(&parse_ast("(defenum MyEnum A B C)")));
    assert!(IncCompiler::is_decl(&parse_ast("(define-symbol-macro my-macro 3)")));
    assert!(IncCompiler::is_decl(&parse_ast("(sys/declare value xyz)")));
  }

  #[test]
  fn is_not_decl_test() {
    assert!(!IncCompiler::is_decl(&parse_ast("100")));
    assert!(!IncCompiler::is_decl(&parse_ast("((defn foo ()))")));
    assert!(!IncCompiler::is_decl(&parse_ast("abc")));
    assert!(!IncCompiler::is_decl(&parse_ast("(progn 1 2 3)")));
    assert!(!IncCompiler::is_decl(&parse_ast("(progn (defconst MY_CONST 3))")));
    assert!(!IncCompiler::is_decl(&parse_ast("#t")));
  }

}
