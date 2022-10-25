
// Incremental compilation (supplies backbone for macro resolution)

use super::declaration_table::DeclarationTable;
use super::main_function::MainFunctionHandler;
use super::call_name::CallName;
use super::classification;
use super::arglist;
use super::arglist::error::ArgListParseError;
use super::arglist::ordinary::ArgList;
use super::arglist::constructor::ConstructorArgList;
use super::arglist::simple::SimpleArgList;
use super::import::{ImportDecl, ImportName};
use super::expr::{Expr, ExprF};
use super::special_form;
use super::depends::Dependencies;
use super::decl::{self, Decl, DeclF, InstanceFunctionName};
use super::macros::{self, MacroData};
use super::identifier::{Id, IdLike, Namespace};
use super::modifier::{self, ParseRule};
use super::export::Visibility;
use super::bootstrapping::{compile_bootstrapping_decl, compile_bootstrapping_class_inner_decl};
use crate::util::one::One;
use crate::sxp::dotted::{DottedExpr, TryFromDottedExprError};
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::literal::{Literal as ASTLiteral};
use crate::sxp::reify::pretty::reify_pretty_expr;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::error::{GDError, GDErrorF};
use crate::compile::resource_type::ResourceType;
use crate::compile::args::{self, Expecting, ExpectedShape};
use crate::compile::names;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::names::generator::NameGenerator;
use crate::compile::frame::MAX_QUOTE_REIFY_DEPTH;
use crate::compile::body::class_initializer::InitTime;
use crate::gdscript::library;
use crate::gdscript::metadata;
use crate::gdscript::decl::Static;
use crate::pipeline::error::{PError, IOError};
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;
use crate::pipeline::source::SourceOffset;
use crate::runner::macro_server::named_file_server::MacroID;

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
  ambient_symbols: SymbolTable,
  minimalist: bool, // A minimalist compiler doesn't use stdlib and doesn't do macro expansion
}

#[allow(clippy::new_without_default)]
impl IncCompiler {

  pub fn new(names: Vec<&str>) -> IncCompiler {
    IncCompiler::with_ambient_symbols(names, SymbolTable::new())
  }

  pub fn with_ambient_symbols(names: Vec<&str>, ambient_symbols: SymbolTable) -> IncCompiler {
    let names = FreshNameGenerator::new(names);
    IncCompiler {
      names: names,
      table: DeclarationTable::new(),
      macros: HashMap::new(),
      imports: vec!(),
      ambient_symbols: ambient_symbols,
      minimalist: false,
    }
  }

  fn resolve_macro_call<K: IdLike>(&mut self,
                                   pipeline: &mut Pipeline,
                                   head: &K,
                                   tail: &[&AST],
                                   macro_id: MacroID,
                                   pos: SourceOffset)
                                   -> Result<AST, PError>
  where Id : Borrow<K>,
        K : Hash + Eq + ToOwned<Owned=Id> + ?Sized {
    // If we're in a minimalist file, then macro expansion is automatically an error
    if self.minimalist {
      let head = head.to_owned().name;
      return Err(PError::from(GDError::new(GDErrorF::MacroInMinimalistError(head), pos)));
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

    let ast = server.run_server_file(macro_id, prelude, args, pos);

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

  fn try_resolve_macro_call(&mut self, pipeline: &mut Pipeline, ast: &AST) -> Result<Option<AST>, PError> {
    let vec: Vec<&AST> = match DottedExpr::new(ast).try_into() {
      Err(TryFromDottedExprError { pos: _ }) => return Ok(None),
      Ok(v) => v,
    };
    if !vec.is_empty() {
      let head = CallName::resolve_call_name(self, pipeline, vec[0])?;
      if let CallName::SimpleName(head) = head {
        let tail = &vec[1..];
        if let Some(MacroData { id, imported: _ }) = self.macros.get(&*Id::build(Namespace::Function, &head)) {
          let id = *id;
          return self.resolve_macro_call(pipeline, &*Id::build(Namespace::Function, &head), tail, id, ast.pos).map(Some);
        }
      }
    }
    Ok(None)
  }

  fn try_resolve_symbol_macro_call(&mut self, pipeline: &mut Pipeline, head: &str, pos: SourceOffset) -> Result<Option<AST>, PError> {
    if let Some(MacroData { id, imported: _ }) = self.macros.get(&*Id::build(Namespace::Value, head)) {
      let id = *id;
      return self.resolve_macro_call(pipeline, &*Id::build(Namespace::Value, head), &[], id, pos).map(Some);
    }
    Ok(None)
  }

  pub fn resolve_simple_call(&mut self, pipeline: &mut Pipeline, head: &str, tail: &[&AST], pos: SourceOffset)
                             -> Result<Expr, PError> {
    if let Some(sf) = special_form::dispatch_form(self, pipeline, head, tail, pos)? {
      Ok(sf)
    } else if let Some(MacroData { id, imported: _ }) = self.macros.get(&*Id::build(Namespace::Function, head)) {
      let id = *id;
      let result = self.resolve_macro_call(pipeline, &*Id::build(Namespace::Function, head), tail, id, pos)?;
      self.compile_expr(pipeline, &result)
    } else {
      let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
      Ok(Expr::new(ExprF::Call(head.to_owned(), args), pos))
    }
  }

  pub fn compile_expr(&mut self, pipeline: &mut Pipeline, expr: &AST) -> Result<Expr, PError> {
    match &expr.value {
      ASTF::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        assert!(!vec.is_empty(), "Internal error in compile_expr, expecting non-empty list, got {:?}", &expr);
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
          CallName::SuperName(head) => {
            let args = tail.iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
            Ok(Expr::new(ExprF::SuperCall(head, args), expr.pos))
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
      ASTF::Atom(lit) => {
        // We handle symbols specially, but for any other literal, we
        // simply delegate to [`Expr::from_ast_literal`].
        if let ASTLiteral::Symbol(s) = lit {
          // Symbol macro resolution
          let macro_result = self.try_resolve_symbol_macro_call(pipeline, s, expr.pos)?;
          if let Some(macro_result) = macro_result {
            self.compile_expr(pipeline, &macro_result)
          } else {
            Ok(Expr::new(ExprF::LocalVar(s.to_string()), expr.pos))
          }
        } else {
          Ok(Expr::from_ast_literal(lit, expr.pos))
        }
      }
    }
  }

  pub fn compile_decl(&mut self,
                      pipeline: &mut Pipeline,
                      acc: &mut impl Extend<Decl>,
                      decl: &AST)
                      -> Result<(), PError> {
    let vec: Vec<&AST> = DottedExpr::new(decl).try_into()?;
    if vec.is_empty() {
      return Err(PError::from(GDError::new(GDErrorF::UnknownDecl(decl.clone()), decl.pos)));
    }
    match &vec[0].value {
      ASTF::Atom(ASTLiteral::Symbol(s)) => {
        match s.borrow() {
          "sys/min-godot-version" => {
            // TODO In principle, this should be a macro that the user
            // can use directly (`ifc`), but that requires us to be
            // able to use macros in stdlib (#103), which is not
            // currently possible. So it's a sys/ directive for now.
            Expecting::at_least(1).validate("sys/min-godot-version", decl.pos, &vec[1..])?;
            let min_version = ExpectedShape::extract_i32("sys/min-godot-version", vec[1].clone())?;
            let actual_version = pipeline.config().godot_version.version;
            if actual_version.into_i32() >= min_version {
              for decl in &vec[2..] {
                self.compile_decl(pipeline, acc, decl)?;
              }
            }
            Ok(())
          }
          "defn" => {
            Expecting::at_least(2).validate("defn", decl.pos, &vec[1..])?;
            let name = ExpectedShape::extract_symbol("defn", vec[1].clone())?;
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args, decl.pos)?;
            let (mods, body) = modifier::function::parser().parse(&vec[3..])?;
            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            let mut decl = decl::FnDecl {
              visibility: Visibility::FUNCTION,
              call_magic: None,
              name: name,
              args: args,
              body: Expr::progn(body, vec[0].pos),
            };
            for m in mods {
              m.apply(&mut decl);
            }
            acc.extend(One(Decl::new(DeclF::FnDecl(decl), vec[0].pos)));
            Ok(())
          }
          "defmacro" => {
            Expecting::at_least(2).validate("defmacro", decl.pos, &vec[1..])?;
            let name = ExpectedShape::extract_symbol("defmacro", vec[1].clone())?;
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args, decl.pos)?;
            let (mods, body) = modifier::macros::parser().parse(&vec[3..])?;
            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            let mut decl = decl::MacroDecl {
              visibility: Visibility::MACRO,
              name: name,
              args: args,
              body: Expr::progn(body, vec[0].pos),
            };
            for m in mods {
              m.apply(&mut decl);
            }
            acc.extend(One(Decl::new(DeclF::MacroDecl(decl), vec[0].pos)));
            Ok(())
          }
          "define-symbol-macro" => {
            Expecting::at_least(2).validate("define-symbol-macro", decl.pos, &vec[1..])?;
            let name = ExpectedShape::extract_symbol("define-symbol-macro", vec[1].clone())?;
            let value = self.compile_expr(pipeline, vec[2])?;
            let (mods, body) = modifier::macros::parser().parse(&vec[3..])?;
            ExpectedShape::validate_end_of_list("define-symbol-macro", body, decl.pos)?;
            let mut decl = decl::SymbolMacroDecl {
              visibility: Visibility::SYMBOL_MACRO,
              name: name,
              body: value,
            };
            for m in mods {
              m.apply_to_symbol_macro(&mut decl);
            }
            acc.extend(One(Decl::new(DeclF::SymbolMacroDecl(decl), vec[0].pos)));
            Ok(())
          }
          "defconst" => {
            Expecting::at_least(2).validate("defconst", decl.pos, &vec[1..])?;
            let name = ExpectedShape::extract_symbol("defconst", vec[1].clone())?;
            let value = self.compile_expr(pipeline, vec[2])?;
            let (mods, body) = modifier::constant::parser().parse(&vec[3..])?;
            ExpectedShape::validate_end_of_list("defconst", body, decl.pos)?;
            let mut decl = decl::ConstDecl { visibility: Visibility::CONST, name, value };
            for m in mods {
              m.apply(&mut decl);
            }
            acc.extend(One(Decl::new(DeclF::ConstDecl(decl), vec[0].pos)));
            Ok(())
          }
          "defclass" => {
            Expecting::at_least(2).validate("defclass", decl.pos, &vec[1..])?;
            let name = ExpectedShape::extract_symbol("defclass", vec[1].clone())?;
            let superclass = match &vec[2].value {
              ASTF::Atom(ASTLiteral::Nil) =>
                library::REFERENCE_NAME.to_owned(),
              ASTF::Cons(car, cdr) =>
                match (&car.value, &cdr.value) {
                  (ASTF::Atom(ASTLiteral::Symbol(superclass_name)), ASTF::Atom(ASTLiteral::Nil)) => superclass_name.to_owned(),
                  (_, ASTF::Atom(ASTLiteral::Nil)) => return Err(PError::from(GDError::new(GDErrorF::BadExtendsClause, vec[2].pos))),
                  _ => return Err(PError::from(GDError::new(GDErrorF::BadExtendsClause, vec[2].pos))),
                },
              _ => return Err(PError::from(GDError::new(GDErrorF::BadExtendsClause, decl.pos))),
            };
            let (mods, decl_body) = modifier::class::parser().parse(&vec[3..])?;
            let mut class = decl::ClassDecl::new(name, superclass);
            for m in mods {
              m.apply(&mut class);
            }
            for decl in decl_body {
              self.compile_class_inner_decl(pipeline, &mut class, decl)?;
            }
            acc.extend(One(Decl::new(DeclF::ClassDecl(class), vec[0].pos)));
            Ok(())
          }
          "defenum" => {
            Expecting::at_least(1).validate("defenum", decl.pos, &vec[1..])?;
            let name = ExpectedShape::extract_symbol("defenum", vec[1].clone())?;
            let (mods, body) = modifier::enums::parser().parse(&vec[2..])?;
            let clauses = body.iter().map(|clause| {
              let clause = match &clause.value {
                ASTF::Atom(ASTLiteral::Symbol(_)) => AST::dotted_list(vec!((*clause).clone()), AST::nil(clause.pos)),
                _ => (*clause).clone(),
              };
              let clause = Vec::try_from(DottedExpr::new(&clause))?;
              let (name, value) = match &clause[..] {
                [name] => (name, None),
                [name, value] => (name, Some(value)),
                _ => return Err(PError::from(GDError::new(GDErrorF::BadEnumClause, decl.pos))),
              };
              let name = ExpectedShape::extract_symbol("defenum", (*name).to_owned())?;
              let value = value.map(|v| self.compile_expr(pipeline, v)).transpose()?;
              Ok((name, value))
            }).collect::<Result<_, _>>()?;
            let mut enum_decl = decl::EnumDecl { visibility: Visibility::ENUM, name, clauses };
            for m in mods {
              m.apply(&mut enum_decl);
            }
            acc.extend(One(Decl::new(DeclF::EnumDecl(enum_decl), vec[0].pos)));
            Ok(())
          }
          "sys/declare" => {
            // (sys/declare value name)
            // (sys/declare function name (args...))
            Expecting::at_least(2).validate("sys/declare", decl.pos, &vec[1..])?;
            let value_type = ExpectedShape::extract_symbol("sys/declare", vec[1].clone())?;
            let (mut declare, body) = match &*value_type {
              "value" | "constant" | "superglobal" => {
                let (name, target_name) = IncCompiler::get_declare_decl_name(vec[2])?;
                let declare_type =
                  match &*value_type {
                    "value" => decl::DeclareType::Value,
                    "constant" => decl::DeclareType::Constant,
                    "superglobal" => decl::DeclareType::Superglobal,
                    _ => unreachable!(),
                  };
                let decl = decl::DeclareDecl {
                  visibility: Visibility::DECLARE,
                  declare_type,
                  name,
                  target_name,
                };
                (decl, &vec[3..])
              }
              "function" | "superfunction" => {
                Expecting::at_least(3).validate("sys/declare", decl.pos, &vec[1..])?;
                let (name, target_name) = IncCompiler::get_declare_decl_name(vec[2])?;
                let args: Vec<_> = DottedExpr::new(vec[3]).try_into()?;
                let args = ArgList::parse(args, decl.pos)?;
                let declare_type =
                  if value_type == "superfunction" {
                    decl::DeclareType::SuperglobalFn(args)
                  } else {
                    decl::DeclareType::Function(args)
                  };
                let decl = decl::DeclareDecl {
                  visibility: Visibility::DECLARE,
                  declare_type,
                  name,
                  target_name,
                };
                (decl, &vec[4..])
              }
              value_type => {
                return Err(PError::from(GDError::new(GDErrorF::BadSysDeclare(value_type.to_owned()), decl.pos)))
              }
            };
            let (mods, body) = modifier::declare::parser().parse(body)?;
            ExpectedShape::validate_end_of_list("sys/declare", body, decl.pos)?;
            for m in mods {
              m.apply(&mut declare);
            }
            acc.extend(One(Decl::new(DeclF::DeclareDecl(declare), vec[0].pos)));
            Ok(())
          }
          "sys/bootstrap" => {
            // (sys/bootstrap directive)
            Expecting::exactly(1).validate("sys/bootstrap", decl.pos, &vec[1..])?;
            let directive = ExpectedShape::extract_symbol("sys/bootstrap", vec[1].clone())?;
            compile_bootstrapping_decl(self, pipeline, acc, &directive, decl.pos)?;
            Ok(())
          }
          _ => {
            Err(PError::from(GDError::new(GDErrorF::UnknownDecl(decl.clone()), decl.pos)))
          }
        }
      }
      _ => {
        Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("(declaration)"), vec[0].clone(), ExpectedShape::Symbol), decl.pos)))
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
      curr = candidate.as_ref().unwrap();
    }

    let vec = Vec::try_from(DottedExpr::new(curr)).map_err(|x| GDError::from_value(x, curr.pos))?;
    if vec.is_empty() {
      return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("(declaration)"), curr.clone(), ExpectedShape::NonemptyList), curr.pos)));
    }
    if let Some(s) = vec[0].as_symbol_ref() {
      match s {
        "progn" => {
          // Top-level magic progn
          for d in &vec[1..] {
            self.compile_class_inner_decl(pipeline, acc, d)?;
          }
          Ok(())
        }
        "defconst" => {
          // TODO Combine this with the other defconst in a helper function
          Expecting::exactly(2).validate("defconst", curr.pos, &vec[1..])?;
          let name = ExpectedShape::extract_symbol("defconst", vec[1].clone())?;
          let value = self.compile_expr(pipeline, vec[2])?;
          acc.decls.push(decl::ClassInnerDecl::new(decl::ClassInnerDeclF::ClassConstDecl(decl::ConstDecl { visibility: Visibility::CONST, name, value }), vec[0].pos));
          Ok(())
        }
        "defvar" => {
          Expecting::at_least(1).validate("defvar", curr.pos, &vec[1..])?;
          let name = ExpectedShape::extract_symbol("defvar", vec[1].clone())?;

          // Parse value and export
          let mut value = None;
          let mut export = None;
          let mut idx = 2;
          // Value
          if let Some(v) = vec.get(idx) {
            if !(matches!(&v.value, ASTF::Cons(car, _) if car.value == ASTF::symbol("export"))) {
              let e = self.compile_expr(pipeline, v)?;
              value = Some(e);
              idx += 1;
            }
          };
          // Export
          if let Some(v) = vec.get(idx) {
            if let Ok(elements) = Vec::<&AST>::try_from(DottedExpr::new(v)) {
              if elements.get(0).map(|x| &x.value) == Some(&ASTF::symbol("export")) {
                let args = elements[1..].iter().map(|x| self.compile_expr(pipeline, x)).collect::<Result<_, _>>()?;
                export = Some(decl::Export { args });
                idx += 1;
              }
            }
          }
          // Modifiers
          let (mods, body) = modifier::var::parser().parse(&vec[idx..])?;
          // (Extra)
          ExpectedShape::validate_end_of_list("defvar", body, curr.pos)?;

          // Exports are only allowed on the main class
          if export.is_some() && !acc.main_class {
            return Err(PError::from(GDError::new(GDErrorF::ExportOnInnerClassVar(name), curr.pos)));
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
        }
        "defn" => {
          // TODO Unify some of this with the equivalent code from compile_decl?
          Expecting::at_least(2).validate("defn", curr.pos, &vec[1..])?;
          if let Some(fname) = InstanceFunctionName::parse(vec[1]) {
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = arglist::parser::parse(args)?;

            // Determine if static
            let (mods, body) = modifier::instance_method::parser().parse(&vec[3..])?;

            // Check for super call (only valid in constructor, but we
            // run the check unconditionally)
            let (super_call, body) = classification::detect_super(body);
            let super_call = match super_call {
              None => None,
              Some(super_call) => {
                let call =
                  super_call.arguments.iter()
                    .map(|expr| self.compile_expr(pipeline, expr))
                    .collect::<Result<Vec<_>, _>>()?;
                Some(decl::SuperCall { call, pos: super_call.pos })
              }
            };

            let body = body.iter().map(|expr| self.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
            if fname.is_constructor_function() {
              // Constructor

              // There can only be one constructor defined in the class
              if acc.constructor.is_some() {
                return Err(PError::from(GDError::new(GDErrorF::DuplicateConstructor, vec[0].pos)));
              }

              let args = ConstructorArgList::try_from(args).map_err(|err| {
                ArgListParseError::new(err, vec[0].pos)
              })?;

              let super_call = super_call.unwrap_or_else(|| decl::SuperCall::empty(vec[0].pos));
              let mut constructor = decl::ConstructorDecl { args, super_call, body: Expr::progn(body, vec[0].pos) };
              for m in mods {
                m.apply_to_constructor(&mut constructor)?;
              }
              acc.constructor = Some(constructor);

            } else {
              // Ordinary functions cannot have init super calls (The
              // `(super:foo)` syntax is handled separately).

              let args = SimpleArgList::try_from(args).map_err(|err| {
                ArgListParseError::new(err, vec[0].pos)
              })?;

              if let Some(super_call) = &super_call {
                return Err(PError::from(GDError::new(GDErrorF::BadSuperCall(String::from("(init)")), super_call.pos)));
              }
              let mut decl = decl::ClassFnDecl {
                is_static: Static::NonStatic,
                name: fname,
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
            Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("defn"), vec[1].clone(), ExpectedShape::InstanceFnName), vec[1].pos)))
          }
        }
        "defsignal" => {
          Expecting::between(1, 2).validate("defsignal", curr.pos, &vec[1..])?;
          let name = ExpectedShape::extract_symbol("defsignal", vec[1].clone())?;
          let nil = AST::nil(vec[0].pos);
          let args = vec.get(2).map_or(&nil, |x| *x);
          let args_pos = args.pos;
          let args: Vec<_> = DottedExpr::new(args).try_into()?;
          let args = SimpleArgList::parse(args, args_pos)?;
          acc.decls.push(decl::ClassInnerDecl::new(decl::ClassInnerDeclF::ClassSignalDecl(decl::ClassSignalDecl { name, args }), vec[0].pos));
          Ok(())
        }
        "sys/bootstrap" => {
          // (sys/bootstrap directive)
          Expecting::exactly(1).validate("sys/bootstrap", curr.pos, &vec[1..])?;
          let directive = ExpectedShape::extract_symbol("sys/bootstrap", vec[1].clone())?;
          compile_bootstrapping_class_inner_decl(self, pipeline, acc, &directive, curr.pos)?;
          Ok(())
        }
        _ => {
          Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("(declaration)"), vec[0].clone(), ExpectedShape::Symbol), curr.pos)))
        }
      }
    } else {
      Err(PError::from(GDError::new(GDErrorF::UnknownDecl(curr.clone()), curr.pos)))
    }
  }

  pub fn compile_import(&mut self, curr: &AST) -> Result<Option<ImportDecl>, GDError> {
    if let Ok(vec) = Vec::try_from(DottedExpr::new(curr)) {
      if !vec.is_empty() && vec[0].value == ASTF::symbol("use") {
        return ImportDecl::parse(&vec[1..]).map_err(|x| GDError::from_value(x, curr.pos)).map(Some);
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
      curr = candidate.as_ref().unwrap();
    }
    // Check if we're looking at a top-level progn.
    if let Ok(vec) = Vec::try_from(DottedExpr::new(curr)) {
      if !vec.is_empty() && vec[0].value == ASTF::symbol("progn") {
        for inner in &vec[1..] {
          self.compile_decl_or_expr(pipeline, main, inner)?;
        }
        return Ok(());
      }
    }
    if let Some(imp) = self.compile_import(curr)? { // TODO Consider doing the is_decl thing with imports so we have a nice, pure function called is_import to call here instead?
      let res_type = ResourceType::from(&imp);
      if res_type.can_have_macros() {
        let file = pipeline.load_file(imp.filename.path(), curr.pos)?;
        self.import_macros_from(file, &imp);
      }
      self.imports.push(imp);
    } else if classification::is_decl(curr) {
      let mut new_decls: Vec<Decl> = Vec::new();
      // Most declaration-level constructs produce exactly one
      // declaration, but there are a handful (mainly bootstrapping
      // primitives) that can produce several.
      self.compile_decl(pipeline, &mut new_decls, curr)?;
      for d in new_decls {
        self.table.add_unless_exists(d.clone())?;
        if let DeclF::MacroDecl(mdecl) = d.value {
          self.bind_macro(pipeline, mdecl, d.pos, false, Namespace::Function)?;
        } else if let DeclF::SymbolMacroDecl(mdecl) = d.value {
          let mdecl = decl::MacroDecl::from(mdecl);
          self.bind_macro(pipeline, mdecl, d.pos, true, Namespace::Value)?;
        }
      }
    } else {
      main.push(self.compile_expr(pipeline, curr)?);
    }
    Ok(())
  }

  pub fn compile_toplevel(mut self, pipeline: &mut Pipeline, body: &AST, main_function_handler: &impl MainFunctionHandler)
                          -> Result<(decl::TopLevel, HashMap<Id, MacroData>), PError> {
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
    main_function_handler.handle_main(&mut self, &main)?;

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
      let unit = pipeline.load_file(&import.filename.path(), pos)?;
      Ok(import.names(&unit.exports))
    }).collect::<Result<Vec<_>, PError>>()?;
    let mut imported_names: HashSet<_> =
      translation_names.into_iter().flatten().map(ImportName::into_imported_id).collect();

    // Now add any ambient symbols (most commonly, names defined in
    // prior lines in a REPL) to the known imports.
    for (name, _) in self.ambient_symbols.vars() {
      imported_names.insert(Id::new(Namespace::Value, name.to_owned()));
    }
    for (name, _, _) in self.ambient_symbols.fns() {
      imported_names.insert(Id::new(Namespace::Function, name.to_owned()));
    }

    // If we're generating a name, then we need to modify the symbol
    // table to reflect that name.
    decl.name = tmp_name.to_owned();
    let table = if generate_name {
      let mut table = self.table.clone();
      match namespace {
        Namespace::Function => {
          table.add_unless_exists(Decl::new(DeclF::MacroDecl(decl.to_owned()), pos))?;
        }
        Namespace::Value => {
          let symdecl = decl::SymbolMacroDecl { visibility: decl.visibility, name: decl.name.to_owned(), body: decl.body.clone() };
          table.add_unless_exists(Decl::new(DeclF::SymbolMacroDecl(symdecl), pos))?;
        }
      }
      Cow::Owned(table)
    } else {
      Cow::Borrowed(&self.table)
    };

    // Now we need to find the dependencies and spawn up the
    // server for the macro itself.
    let mut deps = Dependencies::identify(table.borrow(), &imported_names, &*Id::build(namespace, &tmp_name), pos);
    deps.purge_unknowns(library::all_builtin_names(self.minimalist).iter().map(|x| x as &dyn IdLike<NS=Namespace>));

    let runtime_name =
      if namespace == Namespace::Value {
        metadata::symbol_macro(&tmp_name)
      } else {
        tmp_name
      };

    // Aside from built-in functions, it must be the case that
    // all referenced functions are already defined.
    let names = deps.try_into_knowns()?;
    let tmpfile = macros::create_macro_file(pipeline, self.imports.clone(), table.borrow(), names, &self.ambient_symbols, pos, self.minimalist)?;
    let m_id = pipeline.get_server_mut().stand_up_macro(runtime_name, decl.args, tmpfile).map_err(|err| IOError::new(err, pos))?;
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

  pub fn bind_macros_from(&mut self, existing_macros: impl IntoIterator<Item=(Id, MacroData)>) {
    self.macros.extend(existing_macros.into_iter())
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

  /// The name of the declaration in a `sys/declare` form can take one
  /// of two forms. It can either be a single symbol literal or a list
  /// of two symbol literals. In the former case, the symbol is
  /// considered to be both the GDLisp name and the target GDScript
  /// name, with the latter being escaped using `lisp_to_gd`. In the
  /// latter case, the first symbol is the GDLisp name, and the second
  /// is the target GDScript name, escaped with `lisp_to_gd_bare`
  /// (note that we use the "bare" variant in this case, to allow
  /// explicit overriding of GDScript identifiers in this case).
  ///
  /// This function returns the GDLisp name and, if present, the
  /// GDScript target name. No escaping is done by this function.
  fn get_declare_decl_name(form: &AST) -> Result<(String, Option<String>), GDError> {
    fn inner(form: &AST) -> Option<(String, Option<String>)> {
      if let Some(name) = form.as_symbol_ref() {
        // Single symbol; target GDScript name should be determined
        // automatically.
        Some((name.to_owned(), None))
      } else {
        let list: Vec<&AST> = DottedExpr::new(form).try_into().ok()?;
        Expecting::exactly(2).validate("sys/declare", form.pos, &list).ok()?;
        let (name, target_name) = args::two(list);
        if let (Some(name), Some(target_name)) = (name.as_symbol_ref(), target_name.as_symbol_ref()) {
          Some((name.to_owned(), Some(target_name.to_owned())))
        } else {
          None
        }
      }
    }
    // Replace any errors with the expected shape, since that will be
    // more helpful in this case than a vague "unexpected dotted list"
    // or something.
    inner(form).ok_or_else(|| {
      GDError::new(
        GDErrorF::InvalidArg(String::from("sys/declare"), form.to_owned(), ExpectedShape::SymbolOrPairOfSymbols),
        form.pos,
      )
    })
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
  use crate::pipeline::Pipeline;
  use crate::pipeline::config::ProjectConfig;
  use crate::pipeline::resolver::PanickingNameResolver;
  use crate::runner::version::VersionInfo;

  use std::path::PathBuf;

  // TODO More tests

  fn dummy_config() -> ProjectConfig {
    ProjectConfig {
      root_directory: PathBuf::from(r"."),
      optimizations: false,
      godot_version: VersionInfo::default(),
    }
  }

  fn dummy_pipeline() -> Pipeline {
    Pipeline::with_resolver(dummy_config(), Box::new(PanickingNameResolver))
  }

  #[test]
  fn bad_call_test() {

    let mut icompiler = IncCompiler::new(vec!());
    icompiler.mark_as_minimalist();

    let mut pipeline = dummy_pipeline();

    let ast = AST::list(
      vec!(
        AST::from_value(5, SourceOffset(500)),
        AST::from_value(6, SourceOffset(600)),
      ),
      SourceOffset(0),
    );

    assert_eq!(
      icompiler.compile_expr(&mut pipeline, &ast),
      Err(PError::from(GDError::new(GDErrorF::CannotCall(AST::from_value(5, SourceOffset(500))), SourceOffset(500)))),
    );

  }

}
