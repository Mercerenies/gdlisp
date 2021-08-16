
//! Factory functions for building and working with some common
//! declaration types.

use super::Compiler;
use super::names::fresh::FreshNameGenerator;
use super::body::builder::{StmtBuilder, CodeBuilder, HasDecls};
use super::stmt_wrapper::{self, StmtWrapper};
use super::symbol_table::{HasSymbolTable, SymbolTable, ClassTablePair};
use super::symbol_table::local_var::LocalVar;
use super::error::Error;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl::{self, Decl, DeclF, ClassExtends};
use crate::gdscript::library;
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::ir;
use crate::ir::access_type::AccessType;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

/// Appends (to the builder) a variable declaration statement.
///
/// The variable will have a generated name produced by `gen`. The
/// generated name is returned.
pub fn declare_var<'a>(gen: &mut FreshNameGenerator<'a>,
                       builder: &mut StmtBuilder,
                       prefix: &str,
                       value: Option<Expr>,
                       pos: SourceOffset)
                       -> String {
  let var_name = gen.generate_with(prefix);
  let value = value.unwrap_or(Expr::null(pos));
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
pub fn flatten_class_into_main(builder: &mut CodeBuilder, class: decl::ClassDecl) {
  let decl::ClassDecl { name: _, extends, body } = class;
  builder.extends(extends);
  for decl in body {
    builder.add_decl(decl);
  }
}

pub fn declare_function<'a>(compiler: &mut Compiler<'a>,
                            pipeline: &mut Pipeline,
                            builder: &mut impl HasDecls,
                            table: &mut SymbolTable,
                            gd_name: String,
                            args: IRArgList,
                            body: &IRExpr,
                            result_destination: &impl StmtWrapper)
                            -> Result<decl::FnDecl, Error> {
    let local_vars = body.get_locals();
    let (arglist, gd_args) = args.into_gd_arglist(compiler.name_generator());
    let mut stmt_builder = StmtBuilder::new();
    for arg in &gd_args {
      if local_vars.get(&arg.0).unwrap_or(&AccessType::None).requires_cell() {
        // Special behavior to wrap the argument in a cell.
        wrap_var_in_cell(&mut stmt_builder, &arg.1, body.pos)
      }
    }
    table.with_local_vars(&mut gd_args.into_iter().map(|x| (x.0.to_owned(), LocalVar::local(x.1, *local_vars.get(&x.0).unwrap_or(&AccessType::None)))), |table| {
      compiler.compile_stmt(pipeline, &mut stmt_builder, table, result_destination, body)
    })?;
    Ok(decl::FnDecl {
      name: gd_name,
      args: arglist,
      body: stmt_builder.build_into(builder),
    })
}

fn wrap_var_in_cell(stmt_builder: &mut StmtBuilder, arg: &str, pos: SourceOffset) {
  let var = Expr::var(arg, pos);
  stmt_builder.append(Stmt::simple_assign(var.clone(), library::construct_cell(var), pos));
}

pub fn declare_class<'a>(compiler: &mut Compiler<'a>,
                         pipeline: &mut Pipeline,
                         builder: &mut impl HasDecls,
                         table: &mut SymbolTable,
                         gd_name: String,
                         extends: ClassExtends,
                         main_class: bool,
                         constructor: &ir::decl::ConstructorDecl,
                         decls: &[ir::decl::ClassInnerDecl],
                         pos: SourceOffset)
                         -> Result<decl::ClassDecl, Error> {

  let self_var = LocalVar::self_var();

  let mut body = vec!();
  let mut outer_ref_name = String::new();
  let needs_outer_ref =
    constructor.needs_outer_class_ref(table) || decls.iter().any(|x| x.needs_outer_class_ref(table));
  if needs_outer_ref && !main_class {
    outer_ref_name = compiler.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  let mut instance_table = table.clone();
  let mut static_table = table.clone();
  instance_table.with_local_var::<Result<(), Error>, _>(String::from("self"), self_var, |instance_table| {

    // Modify all of the names in the instance / static table. We run
    // this even if needs_outer_ref is false, because we might still
    // need the static_table updates, and the instance_table updates
    // will be harmlessly ignored in that case.
    if !main_class {
      for (_, call, _) in instance_table.fns_mut() {
        call.object.update_for_inner_scope(false, compiler.preload_resolver(), pipeline, &outer_ref_name);
      }
      for (_, call, _) in static_table.fns_mut() {
        call.object.update_for_inner_scope(true, compiler.preload_resolver(), pipeline, &outer_ref_name);
      }
    }

    body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, declare_constructor(compiler, pipeline, builder, instance_table, constructor)?), pos));

    for d in decls {
      let tables = ClassTablePair { instance_table, static_table: &mut static_table };
      body.push(compiler.compile_class_inner_decl(pipeline, builder, tables, d)?);
    }

    Ok(())
  })?;

  let mut decl = decl::ClassDecl {
    name: gd_name,
    extends: extends,
    body: body,
  };
  if needs_outer_ref && !main_class {
    inner_class::add_outer_class_ref_named(&mut decl, compiler.preload_resolver(), pipeline, outer_ref_name, pos);
  }
  Ok(decl)
}

pub fn declare_constructor<'a>(compiler: &mut Compiler<'a>,
                               pipeline: &mut Pipeline,
                               builder: &mut impl HasDecls,
                               table: &mut SymbolTable,
                               constructor: &ir::decl::ConstructorDecl)
                               -> Result<decl::FnDecl, Error> {
  declare_function(compiler,
                   pipeline,
                   builder,
                   table,
                   String::from(library::CONSTRUCTOR_NAME),
                   IRArgList::from(constructor.args.clone()),
                   &constructor.body,
                   &stmt_wrapper::Vacuous)
}
