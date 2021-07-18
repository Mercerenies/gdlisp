
use crate::ir;
use crate::compile::Compiler;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{HasSymbolTable, SymbolTable};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope, FnName};
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::error::Error;
use crate::compile::stateful::{StExpr, NeedsResult};
use crate::compile::stmt_wrapper;
use crate::gdscript::decl::{self, Decl};
use crate::graph::Graph;
use crate::graph::top_sort::top_sort;
use crate::graph::tarjan;
use crate::pipeline::Pipeline;
use super::lambda;

use std::convert::AsRef;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

pub fn compile_flet<'a>(compiler: &mut Compiler<'a>,
                        pipeline: &mut Pipeline,
                        builder: &mut StmtBuilder,
                        table: &mut SymbolTable,
                        clauses: &[(String, IRArgList, IRExpr)],
                        body: &IRExpr,
                        needs_result: NeedsResult)
                        -> Result<StExpr, Error> {
  let local_fns = clauses.iter().map(|(name, args, fbody)| {
    let call = compile_flet_call(compiler, pipeline, builder, table, args.to_owned(), fbody)?;
    Ok((name.to_owned(), call))
  }).collect::<Result<Vec<_>, Error>>()?;
  table.with_local_fns(&mut local_fns.into_iter(), |table| {
    compiler.compile_expr(pipeline, builder, table, body, needs_result)
  })
}

fn compile_flet_call<'a>(compiler: &mut Compiler<'a>,
                         pipeline: &mut Pipeline,
                         builder: &mut StmtBuilder,
                         table: &mut SymbolTable,
                         args: IRArgList,
                         body: &IRExpr)
                         -> Result<FnCall, Error> {
  if is_declaration_semiglobal(&args, body, table) {
    // No closure vars and any closure fns (if there are any) are
    // free of closures, so we can compile to SemiGlobal.
    let gd_name = compiler.name_generator().generate_with("_flet");
    let func = compiler.declare_function(pipeline, builder, table, gd_name.clone(), args.clone(), body, &stmt_wrapper::Return)?;
    builder.add_helper(Decl::FnDecl(decl::Static::IsStatic, func));
    let specs = FnSpecs::from(args);
    Ok(FnCall {
      scope: FnScope::SemiGlobal,
      object: FnName::FileConstant,
      function: gd_name,
      specs,
      is_macro: false,
    })
  } else {
    // Have to make a full closure object.
    let StExpr(stmt, _) = lambda::compile_lambda_stmt(compiler, pipeline, builder, table, &args, body)?;
    let local_name = compiler.declare_var(builder, "_flet", Some(stmt));
    let specs = FnSpecs::from(args);
    Ok(FnCall {
      scope: FnScope::Local(local_name.clone()),
      object: FnName::on_local_var(VarName::Local(local_name)),
      function: "call_func".to_owned(),
      specs,
      is_macro: false,
    })
  }
}

pub fn compile_labels<'a>(compiler: &mut Compiler<'a>,
                          pipeline: &mut Pipeline,
                          builder: &mut StmtBuilder,
                          table: &mut SymbolTable,
                          clauses: &[(String, IRArgList, IRExpr)],
                          body: &IRExpr,
                          needs_result: NeedsResult)
                          -> Result<StExpr, Error> {
  // TODO This is rife with string cloning, because of the sloppy way
  // Graph is implemented. Once we fix Graph, we can eliminate some
  // clones here.
  let mut dependencies = Graph::from_nodes(clauses.iter().map(|(name, _, _)| name.clone()));
  for (name, _, fbody) in clauses {
    for ref_name in fbody.get_functions().into_names() {
      if dependencies.has_node(&ref_name) {
        dependencies.add_edge_no_dup(name.clone(), ref_name);
      }
    }
  }
  let sccs = tarjan::find_scc(&dependencies);
  let collated_graph = tarjan::build_scc_graph(&dependencies, &sccs);
  let collated_graph = collated_graph.transpose(); // We need the arrows pointing in load order, not dependency order
  let ordering: Vec<_> = top_sort(&collated_graph)
    .expect("SCC detection failed (cycle in resulting graph)")
    .into_iter().copied().collect();
  compile_labels_rec(compiler, pipeline, builder, table, body, needs_result, clauses, &dependencies, &sccs, &collated_graph, &ordering[..], 0)
}

// TODO Really...? A twelve argument recursive function? Really...? Do better.
fn compile_labels_rec<'a, 'b>(compiler: &mut Compiler<'a>,
                              pipeline: &mut Pipeline,
                              builder: &mut StmtBuilder,
                              table: &mut SymbolTable,
                              body: &IRExpr,
                              needs_result: NeedsResult,
                              clauses: &[(String, IRArgList, IRExpr)],
                              full_graph: &Graph<String>,
                              sccs: &tarjan::SCCSummary<'b, String>,
                              graph: &Graph<usize>,
                              ordering: &[usize],
                              ordering_idx: usize)
                              -> Result<StExpr, Error> {
  if ordering_idx < ordering.len() {
    let current_scc_idx = ordering[ordering_idx];
    let tarjan::SCC(current_scc) = sccs.get_scc_by_id(current_scc_idx).expect("SCC detection failed (invalid ID)");
    if current_scc.is_empty() {
      // That's weird. But whatever. No action needed.
      compile_labels_rec(compiler, pipeline, builder, table, body, needs_result, clauses, full_graph, sccs, graph, ordering, ordering_idx + 1)
    } else {
      let name = current_scc.iter().next().expect("Internal error in SCC detection (no first element?)");
      if current_scc.len() == 1 && !full_graph.has_edge(name, name) {
        // Simple FLet-like case.
        let name = current_scc.iter().next().expect("Internal error in SCC detection (no first element?)");
        let (_, args, expr) = clauses.iter().find(|(n, _, _)| &n == name).expect("Internal error in SCC detection (no function found?)");
        let call = compile_flet_call(compiler, pipeline, builder, table, args.to_owned(), expr)?;
        table.with_local_fn((*name).to_owned(), call, |table| {
          compile_labels_rec(compiler, pipeline, builder, table, body, needs_result, clauses, full_graph, sccs, graph, ordering, ordering_idx + 1)
        })
      } else {
        // Complicated mutual recursion case.
        let mut relevant_clauses = Vec::new();
        for name in current_scc {
          let clause = clauses.iter().find(|(n, _, _)| &n == name).expect("Internal error in SCC detection (no function found?)");
          relevant_clauses.push(clause);
        }
        // Go ahead and sort them just so we guarantee a consistent order for testing purposes.
        relevant_clauses.sort_by_key(|(name, _, _)| name);
        let calls = lambda::compile_labels_scc(compiler, pipeline, builder, table, &relevant_clauses[..])?;
        table.with_local_fns(&mut calls.into_iter(), |table| {
          compile_labels_rec(compiler, pipeline, builder, table, body, needs_result, clauses, full_graph, sccs, graph, ordering, ordering_idx + 1)
        })
      }
    }
  } else {
    compiler.compile_expr(pipeline, builder, table, body, needs_result)
  }
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
    table.get_fn(name.as_ref()).map_or(false, |(call, _)| !call.scope.is_local())
  })
}
