
//! Factory functions for building and working with some common
//! declaration types.

use super::import::ImportTable;
use super::frame::CompilerFrame;
use super::stateful::NeedsResult;
use super::names;
use super::names::generator::NameGenerator;
use super::names::registered::RegisteredNameGenerator;
use super::body::builder::{StmtBuilder, CodeBuilder, HasDecls};
use super::body::class_initializer::ClassBuilder;
use super::body::class_scope::DirectClassScope;
use super::stmt_wrapper::{self, StmtWrapper};
use super::symbol_table::{HasSymbolTable, ClassTablePair};
use super::symbol_table::local_var::LocalVar;
use super::symbol_table::function_call::OuterStaticRef;
use super::error::GDError;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl::{self, Decl, DeclF};
use crate::gdscript::class_extends::ClassExtends;
use crate::gdscript::library;
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use crate::pipeline::source::SourceOffset;
use crate::ir;
use crate::ir::access_type::AccessType;
use crate::ir::arglist::simple::SimpleArgList;

type IRLiteral = ir::literal::Literal;
type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ordinary::ArgList;

/// This is the structure returned by [`declare_function_with_init`].
struct DeclaredFnWithInit {
  function: decl::FnDecl,
  inits: Vec<Expr>,
}

/// Appends (to the builder) a variable declaration statement.
///
/// The variable will have a generated name produced by `gen`. The
/// generated name is returned.
pub fn declare_var(gen: &mut impl NameGenerator,
                   builder: &mut StmtBuilder,
                   prefix: &str,
                   value: Option<Expr>,
                   pos: SourceOffset)
                   -> String {
  let var_name = gen.generate_with(prefix);
  let value = value.unwrap_or_else(|| Expr::null(pos));
  builder.append(Stmt::var_decl(var_name.clone(), value, pos));
  var_name
}

/// As far as the IR is concerned, main classes and other classes are
/// roughly equivalent in functionality, except that the former can
/// have variables with custom export declarations. When we compile
/// classes to GDScript, non-main classes compile to inner classes on
/// the script (i.e. to a
/// [`ClassDecl`](crate::gdscript::decl::ClassDecl)), while main
/// classes flatten all of their declarations (and "extends" clauses)
/// into the main content of the file itself. This function implements
/// the latter behavior.
pub fn flatten_class_into_main(import_table: &ImportTable, builder: &mut CodeBuilder, class: decl::ClassDecl) {
  let decl::ClassDecl { name: _, mut extends, body } = class;
  transform_extends_path_for_main(import_table, &mut extends);
  builder.extends(extends);
  for decl in body {
    builder.add_decl(decl);
  }
}

fn transform_extends_path_for_main(import_table: &ImportTable, extends: &mut ClassExtends) {
  match extends {
    ClassExtends::Qualified(lhs, _) => {
      transform_extends_path_for_main(import_table, lhs);
    }
    ClassExtends::SimpleIdentifier(name) => {
      // If the name is an import name, convert it *back* to its path
      // name, since we can't access import names at this point in the
      // code.
      if let Some(path) = import_table.get(name) {
        *extends = ClassExtends::StringLit(path.to_owned());
      }
    }
    ClassExtends::StringLit(_) => {
      // Already a path, so do nothing.
    }
  }
}

pub fn declare_function(frame: &mut CompilerFrame<impl HasDecls>,
                        gd_name: String,
                        args: IRArgList,
                        body: &IRExpr,
                        result_destination: &(impl StmtWrapper + ?Sized))
                        -> Result<decl::FnDecl, GDError> {
  let result = declare_function_with_init(frame, gd_name, args, &[], body, result_destination)?;
  assert!(result.inits.is_empty(), "declare_function got nonempty initializers: {:?}", result.inits);
  Ok(result.function)
}

fn declare_function_with_init(frame: &mut CompilerFrame<impl HasDecls>,
                              gd_name: String,
                              args: IRArgList,
                              inits: &[IRExpr],
                              body: &IRExpr,
                              result_destination: &(impl StmtWrapper + ?Sized))
                              -> Result<DeclaredFnWithInit, GDError> {
  let local_vars = body.get_locals();
  let (arglist, gd_args) = args.into_gd_arglist(&mut RegisteredNameGenerator::new_local_var(frame.table));
  let mut stmt_builder = StmtBuilder::new();
  for arg in &gd_args {
    if local_vars.get(&arg.lisp_name).unwrap_or(&AccessType::None).requires_cell() {
      // Special behavior to wrap the argument in a cell.
      library::cell::wrap_var_in_cell(&mut stmt_builder, &arg.gd_name, body.pos)
    }
  }
  frame.with_local_vars(&mut gd_args.into_iter().map(|x| (x.lisp_name.to_owned(), LocalVar::local(x.gd_name, *local_vars.get(&x.lisp_name).unwrap_or(&AccessType::None)))), |frame| {
    frame.with_builder(&mut stmt_builder, |frame| {
      frame.compile_stmt(result_destination, body)
    })?;
    let function = decl::FnDecl {
      name: gd_name,
      args: arglist,
      body: stmt_builder.build_into(frame.builder),
    };
    let inits = inits.iter().map::<Result<Expr, GDError>, _>(|expr| {
      let mut inner_builder = StmtBuilder::new();
      let cexpr = frame.with_builder(&mut inner_builder, |frame| {
        frame.compile_expr(expr, NeedsResult::Yes).map(|x| x.expr)
      })?;
      let (stmts, decls) = inner_builder.build();
      if stmts.is_empty() {
        // If we never used the stmts, then it's a simple enough
        // expression to compile directly.
        frame.builder.add_decls(decls);
        Ok(cexpr)
      } else {
        // We used accessory statements, so we need an IIFE to get
        // to a place where we can compile statements.
        let mut new_inner_builder = StmtBuilder::new();
        let cexpr = frame.with_builder(&mut new_inner_builder, |frame| {
          frame.compile_expr(&expr.clone().self_evaluating_lambda(), NeedsResult::Yes).map(|x| x.expr)
        })?;
        let (stmts, decls) = new_inner_builder.build();
        assert!(stmts.is_empty(), "Non-empty statements in self-evaluating lambda at declare_function_with_init: got {:?}", stmts);
        frame.builder.add_decls(decls);
        Ok(cexpr)
      }
    }).collect::<Result<Vec<_>, _>>()?;
    Ok(DeclaredFnWithInit { function, inits })
  })
}

pub fn declare_class(frame: &mut CompilerFrame<impl HasDecls>,
                     gd_name: String,
                     extends: ClassExtends,
                     main_class: bool,
                     constructor: &ir::decl::ConstructorDecl,
                     decls: &[ir::decl::ClassInnerDecl],
                     pos: SourceOffset)
                     -> Result<decl::ClassDecl, GDError> {

  let mut class_scope = DirectClassScope::new();

  let self_var = LocalVar::self_var();

  let mut body = vec!();
  let mut outer_ref_name = String::new();
  let needs_outer_ref =
    constructor.needs_outer_class_ref(frame.table) || decls.iter().any(|x| x.needs_outer_class_ref(frame.table));
  if needs_outer_ref && !main_class {
    outer_ref_name = frame.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  let mut class_init_builder = ClassBuilder::new();
  let mut instance_table = frame.table.clone();
  let mut static_table = frame.table.clone();
  instance_table.with_local_var::<Result<(), GDError>, _>(String::from("self"), self_var, |instance_table| {

    // Modify all of the names in the instance / static table. We run
    // this even if needs_outer_ref is false, because we might still
    // need the static_table updates, and the instance_table updates
    // will be harmlessly ignored in that case.
    if !main_class {
      for (_, call, _) in instance_table.fns_mut() {
        call.object.update_for_inner_scope(&OuterStaticRef::InnerInstanceVar(&outer_ref_name), frame.preload_resolver(), frame.pipeline);
      }
      for (_, call, _) in static_table.fns_mut() {
        call.object.update_for_inner_scope(&OuterStaticRef::InnerStatic, frame.preload_resolver(), frame.pipeline);
      }
    }

    let (constructor, constructor_helpers) = declare_constructor(&mut CompilerFrame::new(frame.compiler, frame.pipeline, frame.builder, instance_table, &mut class_scope), constructor)?;
    body.push(Decl::new(DeclF::InitFnDecl(constructor), pos));
    for helper in constructor_helpers {
      body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, helper), pos));
    }

    for d in decls {

      // Do the actual declaration.
      let tables = ClassTablePair { instance_table, static_table: &mut static_table };
      body.push(frame.compiler.compile_class_inner_decl(frame.pipeline, &mut class_init_builder, tables, &mut class_scope, d)?);

      // Sync up the instance and static tables' synthetic variables.
      // (TODO (HACK) Can we make the tables sync up properly, rather
      // than just blindly throwing information at each other at every
      // turn?)
      instance_table.dump_synthetics_to(&mut static_table);
      static_table.dump_synthetics_to(instance_table);

    }

    // The instance and static tables should have the same synthetics,
    // so dump one into the parent.
    instance_table.dump_synthetics_to(frame.table);

    Ok(())
  })?;

  let mut decl = decl::ClassDecl {
    name: gd_name,
    extends: extends,
    body: body,
  };

  if needs_outer_ref && !main_class {
    inner_class::add_outer_class_ref_named(&mut decl, frame.preload_resolver(), frame.pipeline, outer_ref_name, pos);
  }

  class_init_builder.declare_proxies_from_scope(class_scope);

  class_init_builder.fill_out_synthetic_fields(&mut decl, pos);

  let class_init = class_init_builder.build_into(frame.builder);
  class_init.apply(&mut decl, pos)?;

  Ok(decl)
}

pub fn declare_constructor(frame: &mut CompilerFrame<impl HasDecls>,
                           constructor: &ir::decl::ConstructorDecl)
                           -> Result<(decl::InitFnDecl, Vec<decl::FnDecl>), GDError> {
  let pos = constructor.body.pos;

  // First, we save which original arguments which are instance
  // fields.
  let has_any_instance_field_args = constructor.args.has_any_instance_fields();
  let instance_fields_to_init: Vec<(String, bool)> =
    constructor.args.iter_vars()
    .map(|(name, is_instance_field)| (name.to_owned(), is_instance_field))
    .collect();

  let simple_arglist = SimpleArgList { args: constructor.args.args.iter().map(|(name, is_instance_field)| {
    if *is_instance_field {
      frame.compiler.name_generator().generate_with(&names::lisp_to_gd(name))
    } else {
      name.to_owned()
    }
  }).collect() };

  let constructor_with_init = declare_function_with_init(frame,
                                                         String::from(library::CONSTRUCTOR_NAME),
                                                         IRArgList::from(simple_arglist),
                                                         &constructor.super_call.call,
                                                         &constructor.body,
                                                         &stmt_wrapper::Vacuous)?;

  let DeclaredFnWithInit { function: constructor, inits } = constructor_with_init;
  let decl::FnDecl { name: _, args, mut body } = constructor;

  // Handle the instance field initialization
  if has_any_instance_field_args {
    let mut new_body: Vec<Stmt> = Vec::new();
    let all_args: Vec<&str> = args.all_args_iter().collect();

    // It should be the case that the argument list we put in at the
    // beginning should have the same length as the one we got out at
    // the end. I'm asserting that here, because if I change the
    // calling convention somewhere down the line and this assumption
    // breaks, I want to know.
    assert!(all_args.len() == instance_fields_to_init.len(), "Calling convention not compatible with factory.rs assumptions");

    for (arg_name, (field_name, is_instance_field)) in all_args.into_iter().zip(instance_fields_to_init.into_iter()) {
      if is_instance_field {
        let field_name = names::lisp_to_gd(&field_name);
        new_body.push(make_instance_var_initializer(&field_name, arg_name, pos));
      }
    }

    new_body.append(&mut body);
    body = new_body;
  }

  let constructor = decl::InitFnDecl { args, super_call: inits, body };

  Ok((constructor, vec!()))
}

/// Returns a statement which assigns the value of the given local
/// variable to an instance variable with the given name.
fn make_instance_var_initializer(instance_var_name: &str, local_var_name: &str, pos: SourceOffset) -> Stmt {
  Stmt::simple_assign(
    Expr::self_var(pos).attribute(instance_var_name, pos),
    Expr::var(local_var_name, pos),
    pos,
  )
}

/// Compiles a GDLisp literal to a GDScript expression, using `pos` as
/// the source offset of the resulting expression. Literals are simple
/// expressions and, as such, this transformation cannot fail.
pub fn compile_literal(literal: &IRLiteral, pos: SourceOffset) -> Expr {
  match literal {
    IRLiteral::Nil => Expr::null(pos),
    IRLiteral::Int(n) => Expr::from_value(*n, pos),
    IRLiteral::Float(f) => Expr::from_value(*f, pos),
    IRLiteral::Bool(b) => Expr::from_value(*b, pos),
    IRLiteral::String(s) => Expr::from_value(s.to_owned(), pos),
    IRLiteral::Symbol(s) => Expr::call(Some(library::gdlisp_root(pos)), "intern", vec!(Expr::from_value(s.to_owned(), pos)), pos),
  }
}
