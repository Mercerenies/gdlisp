
use crate::gdscript::expr::Expr;
use crate::ir::arglist::VarArg;
use crate::compile::Compiler;
use crate::compile::error::Error;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::stateful::StExpr;
use super::call_magic::{CallMagic, DefaultCall};
use super::local_var::VarName;
use super::SymbolTable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
  pub scope: FnScope,
  pub object: Option<Box<Expr>>, ///// Replace this with FnName (not Option<...>; the FnName handles the option part)
  pub function: String,
  pub specs: FnSpecs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FnScope {
  // A superglobal function is available in all scopes, such as
  // built-in GDScript functions like abs() or min(). Superglobals
  // never need to be closed around or qualified, even if imported.
  Superglobal,
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
  // A special local function is like a local function in that it
  // needs a closure. But a special local function is potentially
  // constructed with several other functions like it, so it will
  // still need an explicit closure if referred to via a funcref. This
  // is the worst case scenario, as we can make no assumptions about
  // the scoping of this function.
  SpecialLocal(String),
}

// Like local_var::VarName, this will eventually translate into an
// expression (or possibly a lack thereof) and consists of all of the
// expressions which denote valid function "name" translations.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum FnName {
  // A static function local to the file and defined at the top-level.
  FileConstant,
  // A superglobal name, such as built-in GDScript functions.
  Superglobal,
  // A file-level function defined in another file and imported.
  ImportedConstant(Box<VarName>),
  // A file-level function defined in the current file but being referenced from an inner case (see Issue #30).
  FileConstantQualified(String)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnSpecs {
  pub required: u32,
  pub optional: u32,
  pub rest: Option<VarArg>,
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

  pub fn new(required: u32, optional: u32, rest: Option<VarArg>) -> FnSpecs {
    FnSpecs { required, optional, rest }
  }

  pub fn has_rest(&self) -> bool {
    self.rest.is_some()
  }

  pub fn runtime_arity(&self) -> u32 {
    self.required + self.optional + if self.has_rest() { 1 } else { 0 }
  }

  pub fn min_arity(&self) -> u32 {
    self.required
  }

  pub fn max_arity(&self) -> u32 {
    // TODO Is u32.MAX correct here? If we put an upper limit on
    // function arity, use that instead.
    if self.has_rest() { u32::MAX } else { self.required + self.optional }
  }

}

impl FnScope {

  pub fn is_local(&self) -> bool {
    self.local_name().is_some()
  }

  pub fn local_name(&self) -> Option<&str> {
    match self {
      FnScope::Local(name) | FnScope::SpecialLocal(name) => Some(name),
      FnScope::Superglobal | FnScope::Global | FnScope::SemiGlobal => None,
    }
  }

}

impl FnName {

  pub fn imported_constant(orig_name: VarName) -> FnName {
    FnName::ImportedConstant(Box::new(orig_name))
  }

  pub fn file_constant_qualified(filename: &str) -> FnName {
    FnName::FileConstantQualified(String::from(filename))
  }

}

impl From<VarName> for FnName {
  fn from(var_name: VarName) -> FnName {
    FnName::imported_constant(var_name)
  }
}

// Note: An Option here does NOT denote failure to convert. FnName can
// be converted to an Option<Expr>, in the sense that "there is no
// expression here" is a completely valid result of conversion and
// indicates a function call which is not subscripted on a name.
impl From<FnName> for Option<Expr> {

  fn from(fn_name: FnName) -> Option<Expr> {
    match fn_name {
      FnName::FileConstant => None,
      FnName::Superglobal => None,
      FnName::ImportedConstant(var_name) => Some(Expr::from(*var_name)),
      FnName::FileConstantQualified(s) => Some(Expr::from(VarName::CurrentFile(s))),
    }
  }

}
