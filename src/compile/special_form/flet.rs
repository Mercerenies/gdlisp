
use crate::ir;
use crate::compile::Compiler;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{HasSymbolTable, SymbolTable};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope};
use crate::compile::error::Error;
use crate::compile::stateful::{StExpr, NeedsResult};
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::expr::Expr;
use super::lambda;

use std::convert::AsRef;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

pub fn compile_flet<'a>(compiler: &mut Compiler<'a>,
                        builder: &mut StmtBuilder,
                        table: &mut SymbolTable,
                        clauses: &[(String, IRArgList, IRExpr)],
                        body: &IRExpr,
                        needs_result: NeedsResult)
                        -> Result<StExpr, Error> {
  let local_fns = clauses.iter().map(|(name, args, fbody)| {
    if is_declaration_semiglobal(args, fbody, table) {
      // No closure vars and any closure fns (if there are any) are
      // free of closures, so we can compile to SemiGlobal.
      let gd_name = compiler.name_generator().generate_with("_flet");
      let func = compiler.declare_function(builder, table, gd_name.clone(), args.clone(), fbody)?;
      builder.add_helper(Decl::FnDecl(decl::Static::IsStatic, func));
      let specs = FnSpecs::from(args.to_owned());
      let call = FnCall {
        scope: FnScope::SemiGlobal,
        object: None,
        function: gd_name,
        specs
      };
      Ok((name.to_owned(), call))
    } else {
      // Have to make a full closure object.
      let StExpr(stmt, _) = lambda::compile_lambda_stmt(compiler, builder, table, args, fbody)?;
      let local_name = compiler.declare_var(builder, "_flet", Some(stmt));
      let specs = FnSpecs::from(args.to_owned());
      let call = FnCall {
        scope: FnScope::Local(local_name.clone()),
        object: Some(Box::new(Expr::Var(local_name))),
        function: "call_func".to_owned(),
        specs
      };
      Ok((name.to_owned(), call))
    }
  }).collect::<Result<Vec<_>, Error>>()?;
  table.with_local_fns(&mut local_fns.into_iter(), |table| {
    compiler.compile_expr(builder, table, body, needs_result)
  })
}

fn is_declaration_semiglobal(args: &IRArgList, body: &IRExpr, table: &SymbolTable) -> bool {
  let (closure_vars, closure_fns) = body.get_names();
  let arg_var_names: Vec<_> = args.iter_vars().collect();
  // All referenced functions should be Global or SemiGlobal and all
  // referenced local variables should be found in the argument list.
  let mut closure_names = closure_vars.names();
  closure_names.all(|x| arg_var_names.contains(&&x[..])) &&
    all_names_are_nonlocal(closure_fns.names(), table)
}

fn all_names_are_nonlocal<I, T>(mut names: I, table: &SymbolTable)
                                -> bool
  where I : Iterator<Item=T>,
        T : AsRef<str> {
  names.all(|name| {
    table.get_fn(name.as_ref()).map_or(false, |call| !call.scope.is_local())
  })
}
