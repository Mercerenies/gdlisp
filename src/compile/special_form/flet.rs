
use crate::ir;
use crate::ir::expr::LocalFnClause;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{HasSymbolTable, SymbolTable};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope, FnName};
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::error::Error;
use crate::compile::stateful::{StExpr, NeedsResult};
use crate::compile::stmt_wrapper;
use crate::compile::factory;
use crate::compile::frame::CompilerFrame;
use crate::gdscript::decl::{self, Decl, DeclF};
use crate::graph::Graph;
use crate::graph::top_sort::top_sort;
use crate::graph::tarjan;
use crate::pipeline::source::SourceOffset;
use super::lambda;

use std::convert::AsRef;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

pub fn compile_flet(frame: &mut CompilerFrame<StmtBuilder>,
                    clauses: &[LocalFnClause],
                    body: &IRExpr,
                    needs_result: NeedsResult,
                    pos: SourceOffset)
                    -> Result<StExpr, Error> {
  let local_fns = clauses.iter().map(|clause| {
    let call = compile_flet_call(frame, clause.args.to_owned(), &clause.body, pos)?;
    Ok((clause.name.to_owned(), call))
  }).collect::<Result<Vec<_>, Error>>()?;
  frame.with_local_fns(&mut local_fns.into_iter(), |frame| {
    frame.compile_expr(body, needs_result)
  })
}

fn compile_flet_call(frame: &mut CompilerFrame<StmtBuilder>,
                     args: IRArgList,
                     body: &IRExpr,
                     pos: SourceOffset)
                     -> Result<FnCall, Error> {
  if is_declaration_semiglobal(&args, body, frame.table) {
    // No closure vars and any closure fns (if there are any) are
    // free of closures, so we can compile to SemiGlobal.
    let gd_name = frame.name_generator().generate_with("_flet");
    let func = factory::declare_function(frame, gd_name.clone(), args.clone(), body, &stmt_wrapper::Return)?;
    frame.builder.add_helper(Decl::new(DeclF::FnDecl(decl::Static::IsStatic, func), pos));
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
    let stmt = lambda::compile_lambda_stmt(frame, &args, body, pos)?.expr;
    let local_name = factory::declare_var(frame.compiler.name_generator(), frame.builder, "_flet", Some(stmt), pos);
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

pub fn compile_labels(frame: &mut CompilerFrame<StmtBuilder>,
                      clauses: &[LocalFnClause],
                      body: &IRExpr,
                      needs_result: NeedsResult,
                      pos: SourceOffset)
                      -> Result<StExpr, Error> {
  // TODO This is rife with string cloning, because of the sloppy way
  // Graph is implemented. Once we fix Graph, we can eliminate some
  // clones here.
  let mut dependencies = Graph::from_nodes(clauses.iter().map(|clause| clause.name.clone()));
  for clause in clauses {
    for ref_name in clause.body.get_functions().into_names() {
      if dependencies.has_node(&ref_name) {
        dependencies.add_edge_no_dup(clause.name.clone(), ref_name);
      }
    }
  }
  let sccs = tarjan::find_scc(&dependencies);
  let collated_graph = tarjan::build_scc_graph(&dependencies, &sccs);
  let collated_graph = collated_graph.transpose(); // We need the arrows pointing in load order, not dependency order
  let ordering: Vec<_> = top_sort(&collated_graph)
    .expect("SCC detection failed (cycle in resulting graph)")
    .into_iter().copied().collect();
  compile_labels_rec(frame, body, needs_result, pos, clauses, &dependencies, &sccs, &collated_graph, &ordering[..], 0)
}

// TODO Really...? A TEN argument recursive function? Really...? Do better.
fn compile_labels_rec<'b>(frame: &mut CompilerFrame<StmtBuilder>,
                          body: &IRExpr,
                          needs_result: NeedsResult,
                          pos: SourceOffset,
                          clauses: &[LocalFnClause],
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
      compile_labels_rec(frame, body, needs_result, pos, clauses, full_graph, sccs, graph, ordering, ordering_idx + 1)
    } else {
      let name = current_scc.iter().next().expect("Internal error in SCC detection (no first element?)");
      if current_scc.len() == 1 && !full_graph.has_edge(name, name) {
        // Simple FLet-like case.
        let name = current_scc.iter().next().expect("Internal error in SCC detection (no first element?)");
        let clause = clauses.iter().find(|clause| clause.name == **name).expect("Internal error in SCC detection (no function found?)");
        let call = compile_flet_call(frame, clause.args.to_owned(), &clause.body, pos)?;
        frame.with_local_fn((*name).to_owned(), call, |frame| {
          compile_labels_rec(frame, body, needs_result, pos, clauses, full_graph, sccs, graph, ordering, ordering_idx + 1)
        })
      } else {
        // Complicated mutual recursion case.
        let mut relevant_clauses = Vec::new();
        for name in current_scc {
          let clause = clauses.iter().find(|clause| clause.name == **name).expect("Internal error in SCC detection (no function found?)");
          relevant_clauses.push(clause);
        }
        // Go ahead and sort them just so we guarantee a consistent order for testing purposes.
        relevant_clauses.sort_by_key(|clause| &clause.name);
        let calls = lambda::compile_labels_scc(frame, &relevant_clauses[..], pos)?;
        frame.with_local_fns(&mut calls.into_iter(), |frame| {
          compile_labels_rec(frame, body, needs_result, pos, clauses, full_graph, sccs, graph, ordering, ordering_idx + 1)
        })
      }
    }
  } else {
    frame.compile_expr(body, needs_result)
  }
}

/// A function declaration is eligible to be semiglobal if all of the
/// following are true.
///
/// * All functions referenced in the body of the function are
///   non-local (i.e. [`FnScope::is_local`] returns false on their
///   scope).
///
/// * All variables referenced in the body of the function are
///   arguments to the function.
///
/// Semiglobal functions do not need to have explicit closure objects
/// constructed for them and can instead be hoisted on the GDScript
/// side into top-level global functions.
pub fn is_declaration_semiglobal(args: &IRArgList, body: &IRExpr, table: &SymbolTable) -> bool {
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
