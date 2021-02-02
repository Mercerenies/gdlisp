
use crate::gdscript::expr::Expr;
use crate::compile::Compiler;
use crate::compile::error::Error;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::stateful::StExpr;
use super::call_magic::{CallMagic, DefaultCall};
use super::SymbolTable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
  pub scope: FnScope,
  pub object: Option<Box<Expr>>,
  pub function: String,
  pub specs: FnSpecs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FnScope {
  // A global function is defined at the global scope and will be
  // compiled to a globally-scoped function.
  Global,
  // A semiglobal function is still local in scope from the GDLisp
  // perspective, but it doesn't require any variable closures, so it
  // will be promoted to a global function on the GDScript side, for
  // efficiency reasons.
  SemiGlobal,
  // A local function is a closure which exists as a local variable on
  // the GDScript side, very similar to a lambda but with different
  // name resolution rules. The string parameter is the name of the
  // (GDScript) local variable referring to the function.
  Local(String),
}

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

  pub fn into_expr<'a>(self,
                       compiler: &mut Compiler<'a>,
                       builder: &mut StmtBuilder,
                       table: &mut SymbolTable,
                       args: Vec<StExpr>)
                       -> Result<Expr, Error> {
    self.into_expr_with_magic(&DefaultCall, compiler, builder, table, args)
  }

  pub fn into_expr_with_magic<'a>(self,
                                  magic: &dyn CallMagic,
                                  compiler: &mut Compiler<'a>,
                                  builder: &mut StmtBuilder,
                                  table: &mut SymbolTable,
                                  args: Vec<StExpr>)
                                  -> Result<Expr, Error> {
    magic.compile(self, compiler, builder, table, args)
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

impl FnScope {

  pub fn is_local(&self) -> bool {
    matches!(self, FnScope::Local(_))
  }

}

// TODO Tests
