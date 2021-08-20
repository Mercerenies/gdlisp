
use crate::ir;
use crate::ir::expr::LambdaClass;
use crate::compile::error::{Error, ErrorF};
use crate::compile::factory;
use crate::compile::CompilerFrame;
use crate::compile::Compiler;
use crate::compile::stateful::{StExpr, NeedsResult};
use crate::compile::body::builder::{StmtBuilder, HasDecls};
use crate::compile::symbol_table::{SymbolTable, ClassTablePair};
use crate::compile::symbol_table::local_var::LocalVar;
use crate::gdscript::decl::{self, Decl, DeclF};
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use crate::pipeline::source::SourceOffset;
use super::lambda;
use super::closure::ClosureData;

pub fn compile_lambda_class(frame: &mut CompilerFrame<StmtBuilder>,
                            class: &LambdaClass,
                            pos: SourceOffset)
                            -> Result<StExpr, Error> {
  // In the perfect world, we would do all of our operations *on*
  // frame rather than destructuring that variable here. But, the way
  // this function is currently written, we need access to both
  // frame.table and lambda_table throughout most of the computation.
  // Perhaps there's a way to refactor it, but it won't be easy.
  let CompilerFrame { compiler, pipeline, table, builder } = frame;
  let LambdaClass { extends, args: constructor_args, constructor, decls } = class;

  // Validate the extends declaration (must be a global variable)
  let extends = Compiler::resolve_extends(table, &extends, pos)?;

  // New GD name
  let gd_class_name = compiler.name_generator().generate_with("_AnonymousClass");

  let closure = {
    let mut closure = ClosureData::from(class);
    // No need to close around global variables, as they're available
    // everywhere.
    closure.purge_globals(table);
    closure
  };

  // Generate an outer class ref if we need access to the scope from
  // within the lambda class.
  let mut outer_ref_name = String::new();
  let needs_outer_ref = closure.closure_fns.needs_outer_class_ref(table);
  if needs_outer_ref {
    outer_ref_name = compiler.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  let mut lambda_table = SymbolTable::new();

  // Bind self into the lambda table.
  lambda_table.set_var(String::from("self"), LocalVar::self_var());

  // Bind all of the closure variables, closure functions, and global
  // variables inside.
  lambda::locally_bind_vars(compiler, table, &mut lambda_table, closure.closure_vars.names(), pos)?;
  lambda::locally_bind_fns(compiler, *pipeline, table, &mut lambda_table, closure.closure_fns.names(), pos, false, &outer_ref_name)?;
  lambda::copy_global_vars(table, &mut lambda_table);

  // Convert the closures to GDScript names.
  let gd_closure_vars = closure.to_gd_closure_vars(&lambda_table);
  let gd_src_closure_vars = closure.to_gd_closure_vars(table);

  // Build the constructor for the lambda class.
  let constructor = compile_lambda_class_constructor(&mut compiler.frame(pipeline, *builder, &mut lambda_table), &constructor, &gd_closure_vars, pos)?;

  // Build the class body for the lambda class.
  let mut class_body = vec!();
  class_body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, constructor), pos));
  for name in gd_closure_vars.iter() {
    class_body.push(Decl::new(DeclF::VarDecl(None, name.clone(), None), pos));
  }
  for d in decls {

    if d.is_static() {
      // Static methods / constants are not allowed on lambda classes
      return Err(Error::new(ErrorF::StaticOnLambdaClass(d.name().to_owned()), d.pos));
    }

    // Nothing static is allowed in lambda classes (static methods or
    // constants). The ClassTablePair simply gets a dummy static
    // symbol table that will never be used, since we just checked in
    // the above code that the declaration is non-static.
    let mut dummy_table = SymbolTable::new();
    let tables = ClassTablePair { instance_table: &mut lambda_table, static_table: &mut dummy_table };
    class_body.push(compiler.compile_class_inner_decl(pipeline, *builder, tables, d)?);

  }
  let mut class = decl::ClassDecl {
    name: gd_class_name.clone(),
    extends: extends,
    body: class_body,
  };

  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), *pipeline, outer_ref_name, pos);
  }

  builder.add_helper(Decl::new(DeclF::ClassDecl(class), pos));

  let constructor_args = constructor_args.iter().map(|expr| compiler.frame(pipeline, *builder, table).compile_expr(expr, NeedsResult::Yes)).collect::<Result<Vec<_>, _>>()?;
  let expr = lambda::make_constructor_call(gd_class_name, gd_src_closure_vars, constructor_args, pos);
  Ok(expr)
}

fn compile_lambda_class_constructor(frame: &mut CompilerFrame<impl HasDecls>,
                                    constructor: &ir::decl::ConstructorDecl,
                                    gd_closure_vars: &[String],
                                    pos: SourceOffset)
                                    -> Result<decl::FnDecl, Error> {
  let mut constructor = factory::declare_constructor(frame, constructor)?;
  let original_args = constructor.args.args;
  constructor.args.args = gd_closure_vars.to_vec();
  constructor.args.args.extend(original_args);
  for name in gd_closure_vars.iter().rev() {
    constructor.body.insert(0, super::assign_to_compiler(name.to_string(), name.to_string(), pos));
  }
  Ok(constructor)
}
