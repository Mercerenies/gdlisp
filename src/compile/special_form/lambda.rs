
use crate::util::unzip_err;
use crate::ir;
use crate::ir::expr::{Locals, LocalFnClause};
use crate::ir::access_type::AccessType;
use crate::ir::identifier::Namespace;
use crate::compile::{Compiler, StExpr};
use crate::compile::frame::CompilerFrame;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::{LocalVar, VarScope, VarName};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope, FnName, OuterStaticRef};
use crate::compile::symbol_table::call_magic::CallMagic;
use crate::compile::stmt_wrapper;
use crate::compile::error::{Error, ErrorF};
use crate::compile::stateful::SideEffects;
use crate::compile::names::{self, NameTrans};
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::names::contextual::ContextualNameGenerator;
use crate::compile::names::generator::NameGenerator;
use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::decl::{self, Decl, DeclF, VarDecl};
use crate::gdscript::arglist::ArgList;
use crate::gdscript::library;
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use crate::pipeline::Pipeline;
use crate::pipeline::can_load::CanLoad;
use crate::pipeline::source::SourceOffset;
use super::lambda_vararg::generate_lambda_class;
use super::closure::{ClosureData, Function, LabelsComponent};

use std::borrow::Borrow;
use std::cmp::max;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

pub fn compile_labels_scc(frame: &mut CompilerFrame<StmtBuilder>,
                          clauses: &[&LocalFnClause],
                          pos: SourceOffset)
                          -> Result<Vec<(String, FnCall)>, Error> {
  // In the perfect world, we would do all of our operations *on*
  // frame rather than destructuring that variable here. But, the way
  // this function is currently written, we need access to both
  // frame.table and lambda_table throughout most of the computation.
  // Perhaps there's a way to refactor it, but it won't be easy.
  let CompilerFrame { compiler, pipeline, table, builder, class_scope } = frame;

  let mut class_scope = class_scope.closure_mut();

  let closure = {
    let mut closure = ClosureData::from(LabelsComponent(clauses));
    // No need to close around global variables, as they're available everywhere
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

  // Determine a name for the global class to represent the labels.
  let class_name = ContextualNameGenerator::new(table, Namespace::Value).generate_with("_Labels");
  table.add_synthetic_var(class_name.clone(), false);

  // Bind all of the closure variables, closure functions, and global
  // variables inside.
  let mut lambda_table = SymbolTable::with_synthetics_from(table);
  locally_bind_vars(compiler, table, &mut lambda_table, closure.closure_vars.names(), pos)?;
  locally_bind_fns(compiler, *pipeline, table, &mut lambda_table, closure.closure_fns.names(), pos, &OuterStaticRef::InnerInstanceVar(&outer_ref_name))?;
  copy_global_vars(table, &mut lambda_table);

  // Convert the closures to GDScript names.
  let gd_closure_vars = closure.to_gd_closure_vars(&lambda_table);
  let gd_src_closure_vars = closure.to_gd_closure_vars(table);

  let local_var_name = ContextualNameGenerator::new(&table, Namespace::Value).generate_with("_locals");

  // Bind the functions themselves
  let named_clauses = generate_names_for_scc_clauses(clauses.iter().copied(), compiler.name_generator());
  for (func_name, clause) in &named_clauses {
    let specs = FnSpecs::from(clause.args.to_owned());
    let fn_call = special_local_fn_call(local_var_name.clone(), func_name.clone(), specs);
    lambda_table.set_fn(clause.name.to_owned(), fn_call, CallMagic::DefaultCall);
  }

  let (bound_calls, functions) = unzip_err::<Error, Vec<_>, Vec<_>, _, _, _>(named_clauses.iter().map(|(func_name, clause)| {
    let mut lambda_table = lambda_table.clone(); // New table for this particular function
    let mut lambda_builder = StmtBuilder::new();
    let (arglist, gd_args) = clause.args.clone().into_gd_arglist(&mut compiler.name_generator());
    // Bind the function arguments
    for NameTrans { lisp_name: arg, gd_name: gd_arg } in &gd_args {
      let access_type = *closure.all_vars.get(&arg).unwrap_or(&AccessType::None);
      lambda_table.set_var(arg.to_owned(), LocalVar::local(gd_arg.to_owned(), access_type));
      wrap_in_cell_if_needed(arg, gd_arg, &closure.all_vars, &mut lambda_builder, pos);
    }
    compiler.frame(pipeline, &mut lambda_builder, &mut lambda_table, &mut *class_scope).compile_stmt(&stmt_wrapper::Return, &clause.body)?;
    let lambda_body = lambda_builder.build_into(*builder);
    let func_name = func_name.to_owned();
    let func = decl::FnDecl {
      name: func_name.clone(),
      args: arglist,
      body: lambda_body,
    };
    let call = FnCall {
      scope: FnScope::SpecialLocal(local_var_name.clone()),
      object: FnName::on_local_var(VarName::local(&local_var_name)),
      function: func_name,
      specs: FnSpecs::from(clause.args.to_owned()),
      is_macro: false,
    };
    Ok(((clause.name.to_owned(), call), func))
  }))?;

  let mut constructor_body = Vec::new();
  for var in &gd_closure_vars {
    constructor_body.push(super::assign_to_compiler(var.to_string(), var.to_string(), pos));
  }
  let constructor = decl::FnDecl {
    name: String::from(library::CONSTRUCTOR_NAME),
    args: ArgList::required(gd_closure_vars.iter().map(|x| x.to_owned()).collect()),
    body: constructor_body,
  };
  let mut class_body = vec!();
  for var in &gd_closure_vars {
    class_body.push(Decl::new(DeclF::VarDecl(VarDecl::simple(var.clone())), pos));
  }
  class_body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, constructor), pos));
  for func in functions {
    class_body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, func), pos));
  }
  let mut class = decl::ClassDecl {
    name: class_name.clone(),
    extends: decl::ClassExtends::named(String::from("Reference")),
    body: class_body,
  };

  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), *pipeline, outer_ref_name, pos);
  }

  builder.add_helper(Decl::new(DeclF::ClassDecl(class), pos));
  let constructor_args: Vec<_> = gd_src_closure_vars.into_iter().map(|x| Expr::new(ExprF::Var(x), pos)).collect();
  let expr = Expr::call(Some(Expr::new(ExprF::Var(class_name), pos)), "new", constructor_args, pos);
  builder.append(Stmt::new(StmtF::VarDecl(local_var_name.clone(), expr), pos));
  table.add_synthetic_var(local_var_name, true);

  Ok(bound_calls)
}

/// Generate an appropriate name for an SCC function generated from a
/// GDLisp function with the given name.
fn generate_scc_name(original_name: &str, gen: &mut FreshNameGenerator) -> String {
  let name_prefix = format!("_fn_{}", names::lisp_to_gd(original_name));
  gen.generate_with(&name_prefix)
}

fn generate_names_for_scc_clauses<'a>(clauses: impl Iterator<Item=&'a LocalFnClause>, gen: &mut FreshNameGenerator)
                                      -> Vec<(String, &'a LocalFnClause)> {
  clauses.map(|clause| {
    let func_name = generate_scc_name(&clause.name, gen);
    (func_name, clause)
  }).collect()
}

/// Compiles an [`FnCall`] for a `labels`-style function with the
/// given name and specs, on the given labels object.
pub fn special_local_fn_call(labels_var: String, function: String, specs: FnSpecs) -> FnCall {
  FnCall {
    scope: FnScope::SpecialLocal(labels_var),
    object: FnName::OnLocalScope,
    function,
    specs,
    is_macro: false,
  }
}

pub fn locally_bind_vars<'a, I, U>(compiler: &mut Compiler,
                                   table: &SymbolTable,
                                   lambda_table: &mut SymbolTable,
                                   closure_vars: I,
                                   pos: SourceOffset)
                                   -> Result<(), Error>
where I : Iterator<Item=&'a U>,
      U : Borrow<str>,
      U : ?Sized,
      U : 'a {
  for var in closure_vars {
    // Ensure the variable actually exists
    match table.get_var(var.borrow()) {
      None => return Err(Error::new(ErrorF::NoSuchVar(var.borrow().to_owned()), pos)),
      Some(gdvar) => {
        let mut new_var = gdvar.to_owned();
        // Ad-hoc rule for closing around self (TODO Generalize?)
        if new_var == LocalVar::self_var() { // TODO Special case over in VarName?
          new_var = LocalVar::local(compiler.name_generator().generate_with("_self"), AccessType::ClosedRead);
        }
        lambda_table.set_var(var.borrow().to_owned(), new_var);
      }
    };
  }
  Ok(())
}

pub fn locally_bind_fns<'a, I, U, L>(compiler: &mut Compiler,
                                     pipeline: &L,
                                     table: &SymbolTable,
                                     lambda_table: &mut SymbolTable,
                                     closure_fns: I,
                                     pos: SourceOffset,
                                     outer_static_ref: &OuterStaticRef<'_>)
                                     -> Result<(), Error>
where I : Iterator<Item=&'a U>,
      U : Borrow<str>,
      U : ?Sized,
      U : 'a,
      L : CanLoad {
  for func in closure_fns {
    // Ensure the function actually exists
    match table.get_fn(func.borrow()) {
      None => { return Err(Error::new(ErrorF::NoSuchFn(func.borrow().to_owned()), pos)) } // TODO Better error pos
      Some((call, magic)) => {
        let mut call = call.clone();
        call.object.update_for_inner_scope(outer_static_ref, compiler.preload_resolver(), pipeline);
        lambda_table.set_fn(func.borrow().to_owned(), call, magic.clone());
      }
    };
  }
  Ok(())
}

/// Copies all of the variables from `src_table` to `dest_table` whose
/// [`VarScope`] is [`VarScope::GlobalVar`]. This function does not
/// modify the function namespace in either table.
pub fn copy_global_vars(src_table: &SymbolTable, dest_table: &mut SymbolTable) {
  for (name, var) in src_table.vars() {
    if var.scope == VarScope::GlobalVar {
      dest_table.set_var(name.to_owned(), var.clone());
    }
  }
}

/// Compiles a call to a lambda class constructor, where the lambda
/// class has name `class_name`. The closure variables are given by
/// `gd_src_closure_vars`, and the resulting expression will be given
/// source offset `pos`.
///
/// Any expression in `suffix_args` will be suffixed onto the end of
/// the closure variable constructor arguments verbatim and can be
/// used to pass custom arguments to the constructor.
///
/// Despite technically being a method call, lambda constructors are
/// never stateful, so `side_effects` on the result will always be
/// [`SideEffects::None`].
pub fn make_constructor_call(class_name: String,
                             gd_src_closure_vars: impl IntoIterator<Item=String>,
                             suffix_args: Vec<StExpr>,
                             pos: SourceOffset)
                             -> StExpr {
  let side_effects = suffix_args.iter().map(|x| x.side_effects).fold(SideEffects::None, max);

  let mut constructor_args: Vec<_> = gd_src_closure_vars.into_iter().map(|x| Expr::new(ExprF::Var(x), pos)).collect();
  constructor_args.extend(suffix_args.into_iter().map(|x| x.expr));

  let expr = Expr::call(Some(Expr::new(ExprF::Var(class_name), pos)), "new", constructor_args, pos);
  StExpr { expr, side_effects }
}

pub fn closure_fn_to_gd_var(call: &FnCall) -> Option<String> {
  call.scope.local_name().map(str::to_owned)
}

fn wrap_in_cell_if_needed(name: &str, gd_name: &str, all_vars: &Locals, lambda_builder: &mut StmtBuilder, pos: SourceOffset) {
  if all_vars.get(name).unwrap_or(&AccessType::None).requires_cell() {
    lambda_builder.append(Stmt::simple_assign(Expr::var(gd_name, pos),
                                              library::cell::construct_cell(Expr::var(gd_name, pos)),
                                              pos));
  }
}

pub fn compile_lambda_stmt(frame: &mut CompilerFrame<StmtBuilder>,
                           args: &IRArgList,
                           body: &IRExpr,
                           pos: SourceOffset)
                           -> Result<StExpr, Error> {
  // In the perfect world, we would do all of our operations *on*
  // frame rather than destructuring that variable here. But, the way
  // this function is currently written, we need access to both
  // frame.table and lambda_table throughout most of the computation.
  // Perhaps there's a way to refactor it, but it won't be easy.
  let CompilerFrame { compiler, pipeline, table, builder, class_scope } = frame;

  let mut class_scope = class_scope.closure_mut();

  let (arglist, gd_args) = args.clone().into_gd_arglist(&mut compiler.name_generator());

  let closure = {
    let mut closure = ClosureData::from(Function::new(args, body));
    // No need to close around global variables, as they're available
    // everywhere.
    closure.purge_globals(table);
    closure
  };

  let mut lambda_table = SymbolTable::with_synthetics_from(table);

  // Bind the arguments to the lambda in the new lambda table.
  for arg in &gd_args {
    let access_type = *closure.all_vars.get(&arg.lisp_name).unwrap_or(&AccessType::None);
    lambda_table.set_var(arg.lisp_name.to_owned(), LocalVar::local(arg.gd_name.to_owned(), access_type));
  }

  // Generate an outer class ref if we need access to the scope from
  // within the lambda.
  let mut outer_ref_name = String::new();
  let needs_outer_ref = closure.closure_fns.needs_outer_class_ref(table);
  if needs_outer_ref {
    outer_ref_name = compiler.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  // Bind all of the closure variables, closure functions, and global
  // variables inside.
  locally_bind_vars(compiler, table, &mut lambda_table, closure.closure_vars.names(), pos)?;
  locally_bind_fns(compiler, *pipeline, table, &mut lambda_table, closure.closure_fns.names(), pos, &OuterStaticRef::InnerInstanceVar(&outer_ref_name))?;
  copy_global_vars(table, &mut lambda_table);

  // Convert the closures to GDScript names.
  let gd_closure_vars = closure.to_gd_closure_vars(&lambda_table);
  let gd_src_closure_vars = closure.to_gd_closure_vars(table);

  let lambda_body = {
    let mut lambda_builder = StmtBuilder::new();

    // Wrap arguments in cells, as needed.
    for NameTrans { lisp_name: arg, gd_name: gd_arg } in &gd_args {
      wrap_in_cell_if_needed(arg, gd_arg, &closure.all_vars, &mut lambda_builder, pos);
    }

    // Compile the lambda body.
    compiler.frame(pipeline, &mut lambda_builder, &mut lambda_table, &mut *class_scope).compile_stmt(&stmt_wrapper::Return, body)?;
    lambda_builder.build_into(*builder)
  };

  // Generate the enclosing class.
  let class_name = compiler.name_generator().generate_with("_LambdaBlock");
  let mut class = generate_lambda_class(class_name.clone(), args.clone().into(), arglist, &gd_closure_vars, lambda_body, pos);

  // Add outer class reference.
  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), *pipeline, outer_ref_name, pos);
  }

  // Place the resulting values in the builder.
  builder.add_helper(Decl::new(DeclF::ClassDecl(class), pos));
  Ok(make_constructor_call(class_name, gd_src_closure_vars, vec!(), pos))
}

/// This function compiles a GDLisp function reference, as constructed
/// using the `(function ...)` special form. GDLisp function
/// references compile to instances of private helper classes, similar
/// to lambda expressions.
///
/// If the function has scope [`FnScope::Local`], then it is already a
/// local variable, and the name of that local variable will be
/// returned without constructing an unnecessary second helper class.
/// If the function has scope [`FnScope::SpecialLocal`], then the
/// resulting helper class will have a single constructor argument:
/// the special local function object. Otherwise, the constructor
/// function will have no constructor arguments.
pub fn compile_function_ref(compiler: &mut Compiler,
                            pipeline: &mut Pipeline,
                            builder: &mut StmtBuilder,
                            _table: &mut SymbolTable,
                            func: FnCall,
                            pos: SourceOffset)
                            -> Result<StExpr, Error> {
  if let FnScope::Local(name) = func.scope {
    // If the function is already bound to a local variable, we can
    // happily reuse that variable. This is most likely to come up if
    // we take a function ref of an flet function with a nontrivial
    // closure.
    Ok(StExpr { expr: Expr::new(ExprF::Var(name), pos), side_effects: SideEffects::None })
  } else {
    let arglist = simple_arg_names(func.specs.runtime_arity());

    // Normally, function references are either to local variables (in
    // which case, we have a reference already) or functions (in which
    // case, there is no closure so a simple top-level class will do).
    // However, if the referent is from a nontrivial SCC of a labels
    // block, then we have to paradoxically close around it, since it
    // *is* local but doesn't satisfy the funcref interface.
    let gd_src_closure_vars =
      if let FnScope::SpecialLocal(name) = func.scope {
        vec!(name)
      } else {
        vec!()
      };

    let object = func.object.clone().into_inner_scope(&OuterStaticRef::InnerStatic, compiler.preload_resolver(), pipeline).into_expr(pos);
    let body = Stmt::new(
      StmtF::ReturnStmt(
        Expr::call(object, &func.function, arglist.args.iter().map(|x| Expr::var(x, pos)).collect(), pos)
      ),
      pos,
    );

    // Generate the class and the constructor call.
    let class_name = compiler.name_generator().generate_with("_FunctionRefBlock");
    let class = generate_lambda_class(class_name.clone(), func.specs, arglist, &gd_src_closure_vars, vec!(body), pos);
    builder.add_helper(Decl::new(DeclF::ClassDecl(class), pos));
    Ok(make_constructor_call(class_name, gd_src_closure_vars, vec!(), pos))
  }
}

/// Simple helper function to generate basic function arguments with a
/// regular naming scheme. This function makes no effort to avoid
/// conflicts with other variables and should only be used in scopes
/// where such conflicts are impossible.
///
/// The names generated by this function begin with "arg" and are
/// followed by a numerical index from 0 up to (exclusive) `count`.
///
/// # Examples
///
/// ```
/// # use gdlisp::compile::special_form::lambda::simple_arg_names;
/// # use gdlisp::gdscript::arglist::ArgList;
/// assert_eq!(simple_arg_names(0), ArgList::required(vec!()));
/// assert_eq!(simple_arg_names(4), ArgList::required(vec!("arg0".to_string(), "arg1".to_string(), "arg2".to_string(), "arg3".to_string())));
/// ```
pub fn simple_arg_names(count: usize) -> ArgList {
  let arg_names = (0..count).map(|i| format!("arg{}", i)).collect();
  ArgList::required(arg_names)
}
