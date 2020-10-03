
/*
pub struct Let;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::compile::symbol_table::{SymbolTable, HasSymbolTable};

use std::convert::TryInto;

impl SpecialForm for Let {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 table: &mut impl SymbolTable,
                 tail: &[&AST],
                 needs_result: NeedsResult)
                 -> Result<StExpr, Error> {
    if tail.len() < 1 {
      return Err(Error::TooFewArgs(String::from("let"), tail.len()));
    }
    let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
    let var_names = vars.iter().map(|curr| {
      let var: Vec<_> = match DottedExpr::new(curr) {
        DottedExpr { elements, terminal: AST::Nil } if elements.len() > 0 => elements,
        DottedExpr { elements, terminal: tail@AST::Symbol(_) } if elements.len() == 0 => vec!(tail),
        _ => return Err(Error::InvalidArg(String::from("let"), (*curr).clone(), String::from("variable declaration")))
      };
      let result_value = compiler.compile_stmts(builder, table, &var[1..], NeedsResult::Yes)?;
      let ast_name = match var[0] {
        AST::Symbol(s) => Ok(s.clone()),
        _ => Err(Error::InvalidArg(String::from("let"), (*curr).clone(), String::from("variable declaration"))),
      }?;
      let gd_name = compiler.declare_var(builder, &ast_name, Some(result_value.0));
      Ok((ast_name, gd_name))
    }).collect::<Result<Vec<_>, _>>()?;
    table.with_local_vars(&mut var_names.into_iter(), |table| {
      compiler.compile_stmts(builder, table, &tail[1..], needs_result)
    })
  }

}
*/
