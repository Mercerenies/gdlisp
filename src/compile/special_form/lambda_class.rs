
use crate::ir;
use crate::ir::expr::LambdaClass;
use crate::compile::error::{Error, ErrorF};
use crate::compile::factory;
use crate::compile::CompilerFrame;
use crate::compile::Compiler;
use crate::compile::stateful::{StExpr, NeedsResult};
use crate::compile::body::builder::{StmtBuilder, HasDecls};
use crate::compile::body::class_initializer::ClassBuilder;
use crate::compile::body::class_scope::DirectClassScope;
use crate::compile::symbol_table::{SymbolTable, ClassTablePair};
use crate::compile::symbol_table::local_var::LocalVar;
use crate::compile::symbol_table::function_call::OuterStaticRef;
use crate::compile::names::registered::RegisteredNameGenerator;
use crate::compile::names::generator::NameGenerator;
use crate::gdscript::decl::{self, Decl, DeclF, VarDecl};
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
  let CompilerFrame { compiler, pipeline, table, builder, class_scope: original_class_scope } = frame;
  let LambdaClass { extends, args: constructor_args, constructor, decls } = class;

  // Note: We construct a new class scope here, since we're now inside
  // of a newly-declared class. We use the original_class_scope to
  // compile the constructor arguments, since super calls to the scope
  // enclosing the class declaration are still valid in that syntactic
  // position.
  let mut class_scope = DirectClassScope::new();

  // Validate the extends declaration (must be a global variable)
  let extends = Compiler::resolve_extends(table, &extends, pos)?;

  // New GD name
  let gd_class_name = RegisteredNameGenerator::new_global_var(table).generate_with("_AnonymousClass");

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

  let mut lambda_table = SymbolTable::with_synthetics_from(table);

  // Bind self into the lambda table.
  lambda_table.set_var(String::from("self"), LocalVar::self_var());

  // Bind all of the closure variables, closure functions, and global
  // variables inside.
  let forbidden_names = get_all_instance_scoped_vars(&decls);
  lambda::locally_bind_vars(compiler, table, &mut lambda_table, closure.closure_vars.names(), &forbidden_names, pos)?;
  lambda::locally_bind_fns(compiler, *pipeline, table, &mut lambda_table, closure.closure_fns.names(), pos, &OuterStaticRef::InnerInstanceVar(&outer_ref_name))?;
  lambda::copy_global_vars(table, &mut lambda_table);

  // Convert the closures to GDScript names.
  let gd_closure_vars = closure.to_gd_closure_vars(&lambda_table);
  let gd_src_closure_vars = closure.to_gd_closure_vars(table);

  // Build the constructor for the lambda class.
  let default_constructor: ir::decl::ConstructorDecl;
  let constructor = match constructor {
    None => {
      default_constructor = ir::decl::ConstructorDecl::empty(pos);
      &default_constructor
    }
    Some(c) => {
      &c
    }
  };
  let (constructor, constructor_helpers) = compile_lambda_class_constructor(&mut compiler.frame(pipeline, *builder, &mut lambda_table, &mut class_scope), &constructor, &gd_closure_vars, pos)?;

  // Build the class body for the lambda class.
  let mut class_init_builder = ClassBuilder::new();
  //#[allow(clippy::vec_init_then_push)] // For style consistency
  let class_body = {
    let mut class_body = vec!();
    class_body.push(Decl::new(DeclF::InitFnDecl(constructor), pos));
    for helper in constructor_helpers {
      class_body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, helper), pos));
    }
    for name in gd_closure_vars.iter() {
      class_body.push(Decl::new(DeclF::VarDecl(VarDecl::simple(name.clone())), pos));
    }
    for d in decls {

      if d.is_static() {
        // Static methods / constants are not allowed on lambda classes
        return Err(Error::new(ErrorF::StaticOnLambdaClass(d.name().into_owned()), d.pos));
      }

      // Nothing static is allowed in lambda classes (static methods
      // or constants). The ClassTablePair simply gets a dummy static
      // symbol table that will never be used, since we just checked
      // in the above code that the declaration is non-static.
      let mut dummy_table = SymbolTable::new();
      let tables = ClassTablePair { instance_table: &mut lambda_table, static_table: &mut dummy_table };
      class_body.push(compiler.compile_class_inner_decl(pipeline, &mut class_init_builder, tables, &mut class_scope, d)?);

    }
    class_body
  };

  let mut class = decl::ClassDecl {
    name: gd_class_name.clone(),
    extends: extends,
    body: class_body,
  };

  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), *pipeline, outer_ref_name, pos);
  }

  class_init_builder.declare_proxies_from_scope(class_scope);

  let class_init = class_init_builder.build_into(*builder);
  class_init.apply(&mut class, pos)?;

  builder.add_helper(Decl::new(DeclF::ClassDecl(class), pos));

  let constructor_args = constructor_args.iter().map(|expr| compiler.frame(pipeline, *builder, table, *original_class_scope).compile_expr(expr, NeedsResult::Yes)).collect::<Result<Vec<_>, _>>()?;
  let expr = lambda::make_constructor_call(gd_class_name, gd_src_closure_vars, constructor_args, pos);
  Ok(expr)
}

fn get_all_instance_scoped_vars(decls: &[ir::decl::ClassInnerDecl]) -> Vec<&str> {
  // TODO This is NOT a complete solution. See Issue #82
  // (https://github.com/Mercerenies/gdlisp/issues/82) for the
  // problems with this implementation.
  let mut result: Vec<&str> = vec![];
  for decl in decls {
    match &decl.value {
      ir::decl::ClassInnerDeclF::ClassVarDecl(var) => {
        result.push(&var.name);
      }
      ir::decl::ClassInnerDeclF::ClassConstDecl(cdecl) => {
        result.push(&cdecl.name);
      }
      ir::decl::ClassInnerDeclF::ClassSignalDecl(_) => {}
      ir::decl::ClassInnerDeclF::ClassFnDecl(_) => {}
    }
  }
  result
}

fn compile_lambda_class_constructor(frame: &mut CompilerFrame<impl HasDecls>,
                                    constructor: &ir::decl::ConstructorDecl,
                                    gd_closure_vars: &[String],
                                    pos: SourceOffset)
                                    -> Result<(decl::InitFnDecl, Vec<decl::FnDecl>), Error> {
  let (mut constructor, constructor_helpers) = factory::declare_constructor(frame, constructor)?;
  let original_args = constructor.args.args;
  constructor.args.args = gd_closure_vars.to_vec();
  constructor.args.args.extend(original_args);
  for name in gd_closure_vars.iter().rev() {
    constructor.body.insert(0, super::assign_to_compiler(name.to_string(), name.to_string(), pos));
  }
  Ok((constructor, constructor_helpers))
}
