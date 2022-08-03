
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::Expr;
use crate::gdscript::op;
use crate::pipeline::source::SourceOffset;

use std::iter;

pub struct LambdaVarargBuilder {
  args_variable: String,
  stmts: Vec<Stmt>,
  args: Vec<String>,
  pos: SourceOffset,
}

impl LambdaVarargBuilder {

  pub fn new(args_variable: String, pos: SourceOffset) -> LambdaVarargBuilder {
    LambdaVarargBuilder::with_existing_args(args_variable, iter::empty(), pos)
  }

  pub fn with_existing_args(args_variable: String, args: impl Iterator<Item=String>, pos: SourceOffset) -> LambdaVarargBuilder {
    LambdaVarargBuilder {
      args_variable: args_variable,
      stmts: Vec::new(),
      args: args.collect(),
      pos: pos,
    }
  }

  pub fn with_inner_builder(&mut self, block: impl FnOnce(&mut LambdaVarargBuilder)) -> Vec<Stmt> {
    let mut inner_builder = LambdaVarargBuilder::with_existing_args(
      self.args_variable.to_owned(),
      self.args.iter().cloned(),
      self.pos,
    );
    block(&mut inner_builder);
    inner_builder.build()
  }

  pub fn declare_argument_var(&mut self, name: String) {
    self.stmts.push(Stmt::var_decl(name.clone(), Expr::null(self.pos), self.pos));
    self.args.push(name);
  }

  pub fn push_error(&mut self, message: &str) {
    self.stmts.push(Stmt::expr(
      Expr::simple_call("push_error", vec!(Expr::str_lit(message, self.pos)), self.pos),
    ));
  }

  pub fn pop_argument(&mut self, variable_name: &str) {
    self.stmts.push(Stmt::simple_assign(
      Expr::var(variable_name, self.pos),
      Expr::var(&self.args_variable, self.pos).attribute("car", self.pos),
      self.pos,
    ));
    self.stmts.push(Stmt::simple_assign(
      Expr::var(&self.args_variable, self.pos),
      Expr::var(&self.args_variable, self.pos).attribute("cdr", self.pos),
      self.pos,
    ));
  }

  pub fn pop_rest_of_arguments(&mut self) {
    self.args.push(self.args_variable.to_owned());
  }

  pub fn if_args_is_empty<F1, F2>(&mut self, empty_block: F1, nonempty_block: F2)
  where F1 : FnOnce(&mut LambdaVarargBuilder),
        F2 : FnOnce(&mut LambdaVarargBuilder) {
    let empty_case: Vec<Stmt> = self.with_inner_builder(empty_block);
    let nonempty_case: Vec<Stmt> = self.with_inner_builder(nonempty_block);
    self.stmts.push(stmt::if_else(
      Expr::binary(Expr::var(&self.args_variable, self.pos), op::BinaryOp::Eq, Expr::null(self.pos), self.pos),
      empty_case,
      nonempty_case,
      self.pos,
    ));
  }

  pub fn build(self) -> Vec<Stmt> {
    self.stmts
  }

}
