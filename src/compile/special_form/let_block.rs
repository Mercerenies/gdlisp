
use crate::compile::frame::CompilerFrame;
use crate::compile::stateful::{StExpr, NeedsResult};
use crate::compile::factory;
use crate::compile::names;
use crate::compile::names::registered::RegisteredNameGenerator;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::compile::symbol_table::HasSymbolTable;
use crate::compile::symbol_table::local_var::LocalVar;
use crate::gdscript::library;
use crate::ir;
use crate::ir::access_type::AccessType;
use crate::ir::expr::LocalVarClause;
use crate::pipeline::source::SourceOffset;

type IRExpr = ir::expr::Expr;

pub fn compile_let(frame: &mut CompilerFrame<StmtBuilder>,
                   clauses: &[LocalVarClause],
                   body: &IRExpr,
                   needs_result: NeedsResult,
                   _pos: SourceOffset)
                   -> Result<StExpr, Error> {
  let closure_vars = body.get_locals();
  let var_names = clauses.iter().map::<Result<(String, String), Error>, _>(|clause| {
    let LocalVarClause { name: ast_name, value: expr } = clause;
    let ast_name = ast_name.to_owned();
    let result_value = frame.compile_expr(expr, NeedsResult::Yes)?.expr;
    let result_value =
      if closure_vars.get(&ast_name).unwrap_or(&AccessType::None).requires_cell() {
        library::cell::construct_cell(result_value)
      } else {
        result_value
      };
    let gd_name = factory::declare_var(&mut RegisteredNameGenerator::new_local_var(frame.table), frame.builder, &names::lisp_to_gd(&ast_name), Some(result_value), clause.value.pos);
    Ok((ast_name, gd_name))
  }).collect::<Result<Vec<_>, _>>()?;
  frame.with_local_vars(&mut var_names.into_iter().map(|x| (x.0.clone(), LocalVar::local(x.1, *closure_vars.get(&x.0).unwrap_or(&AccessType::None)))), |frame| {
    frame.compile_expr(body, needs_result)
  })
}
