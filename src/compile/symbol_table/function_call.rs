
use crate::gdscript::expr::Expr;
use crate::compile::error::Error;
use crate::compile::body::builder::StmtBuilder;
use super::call_magic::{CallMagic, DefaultCall};
use super::SymbolTable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
  pub scope: FnScope,
  pub object: Option<Box<Expr>>,
  pub function: String,
  pub specs: FnSpecs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnScope { Local, Global }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnSpecs {
  pub required: u32,
  pub optional: u32,
  pub rest: bool,
}

impl FnCall {

  pub fn unqualified(specs: FnSpecs, scope: FnScope, function: String) -> FnCall {
    FnCall { specs, scope, object: None, function }
  }

  pub fn qualified(specs: FnSpecs, scope: FnScope, object: Expr, function: String) -> FnCall {
    FnCall { specs, scope, object: Some(Box::new(object)), function }
  }

  pub fn into_expr(self,
                   builder: &mut StmtBuilder,
                   table: &mut SymbolTable,
                   args: Vec<Expr>)
                   -> Result<Expr, Error> {
    self.into_expr_with_magic(&DefaultCall, builder, table, args)
  }

  pub fn into_expr_with_magic(self,
                              magic: &dyn CallMagic,
                              builder: &mut StmtBuilder,
                              table: &mut SymbolTable,
                              args: Vec<Expr>)
                              -> Result<Expr, Error> {
    magic.compile(self, builder, table, args)
  }

}

impl FnSpecs {

  pub fn new(required: u32, optional: u32, rest: bool) -> FnSpecs {
    FnSpecs { required, optional, rest }
  }

  pub fn runtime_arity(&self) -> u32 {
    self.required + self.optional + if self.rest { 1 } else { 0 }
  }

  pub fn min_arity(&self) -> u32 {
    self.required
  }

  pub fn max_arity(&self) -> u32 {
    // TODO Is u32.MAX correct here? If we put an upper limit on
    // function arity, use that instead.
    if self.rest { u32::MAX } else { self.required + self.optional }
  }

}

// TODO Tests
