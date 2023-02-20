// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::op;
use crate::pipeline::source::SourceOffset;

use std::iter;

/// A `LambdaVarargBuilder` builds up what will eventually be a
/// `Vec<Stmt>`. In the course of doing so, we assume that a variable
/// with the name given by `args_variable` (in the constructor) is in
/// scope, and we accumulate values from that variable into several
/// others, eventually culminating in a final function call which uses
/// the accumulated arguments.
#[derive(Debug, Clone)]
pub struct LambdaVarargBuilder {
  args_variable: String,
  stmts: Vec<Stmt>,
  args: Vec<Expr>,
  pos: SourceOffset,
}

impl LambdaVarargBuilder {

  /// Construct a new builder with the given arguments variable, an
  /// empty list of accumulated arguments, and the given source
  /// position. The source position is used for GDLisp compiler error
  /// messages.
  pub fn new(args_variable: String, pos: SourceOffset) -> LambdaVarargBuilder {
    LambdaVarargBuilder::with_existing_args(args_variable, iter::empty(), pos)
  }

  /// Construct a new builder with the given arguments variable, an
  /// inherited list of accumulated arguments, and the given source
  /// position. The source position is used for GDLisp compiler error
  /// messages.
  pub fn with_existing_args(args_variable: String, args: impl Iterator<Item=Expr>, pos: SourceOffset) -> LambdaVarargBuilder {
    LambdaVarargBuilder {
      args_variable: args_variable,
      stmts: Vec::new(),
      args: args.collect(),
      pos: pos,
    }
  }

  /// Runs an internal block with a builder inherited from `self`,
  /// returning the statements produced by the inner builder.
  ///
  /// The new builder will be created with the same arguments variable
  /// and `SourceOffset`, and it will be created with the current
  /// accumulated argument list of the outer builder. Changes to the
  /// inner builder's argument list will not propagate to the outer.
  /// The given `block` shall be called with the inner builder, and
  /// then `inner_builder.build()` will be returned.
  pub fn with_inner_builder(&self, block: impl FnOnce(&mut LambdaVarargBuilder)) -> Vec<Stmt> {
    let mut inner_builder = LambdaVarargBuilder::with_existing_args(
      self.args_variable.to_owned(),
      self.args.iter().cloned(),
      self.pos,
    );
    block(&mut inner_builder);
    inner_builder.build()
  }

  /// Declares a variable (whose initial value is [`Expr::null`]), and
  /// adds it to the accumulated arguments list.
  pub fn declare_argument_var(&mut self, name: String) {
    self.stmts.push(Stmt::var_decl(name.clone(), Expr::null(self.pos), self.pos));
    self.args.push(Expr::new(ExprF::Var(name), self.pos));
  }

  /// Pushes a call to the GDScript built-in function `push_error`,
  /// with the given error message.
  pub fn push_error(&mut self, message: &str) {
    self.stmts.push(Stmt::expr(
      Expr::simple_call("push_error", vec!(Expr::str_lit(message, self.pos)), self.pos),
    ));
  }

  /// This method generates code to take the first element (i.e. the
  /// `car`) of the arguments variable and assign it to the variable
  /// indicated by `variable_name`. Then the arguments variable is
  /// reassigned to its own `cdr`. Note that this will fail if the
  /// arguments variable contains `nil`, so it is often better to run
  /// this inside of a
  /// [`if_args_is_empty`](LambdaVarargBuilder::if_args_is_empty)
  /// block, to be sure.
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

  /// Assigns an arbitrary value to the given variable.
  pub fn assign_to_var(&mut self, variable_name: &str, value: Expr) {
    self.stmts.push(Stmt::simple_assign(Expr::var(variable_name, self.pos), value, self.pos));
  }

  /// Takes the arguments variable, indicating all of the arguments
  /// that have not been processed yet, and adds it, as a single
  /// scalar unit, to the accumulated arguments list.
  ///
  /// The `function` argument indicates a transformative function to
  /// be applied to the arguments variable before using it as an
  /// accumulated argument. If the transformative function is not
  /// necessary, then [`LambdaVarargBuilder::pop_rest_of_arguments`]
  /// can be used instead.
  pub fn pop_rest_of_arguments_with<F>(&mut self, function: F)
  where F : FnOnce(Expr) -> Expr {
    let args_var = Expr::new(ExprF::Var(self.args_variable.to_owned()), self.pos);
    self.args.push(function(args_var));
  }

  /// Takes the arguments variable, indicating all of the arguments
  /// that have not been processed yet, and adds it, as a single
  /// scalar unit, to the accumulated arguments list.
  ///
  /// Equivalent to `self.pop_rest_of_arguments_with(|x| x)`.
  pub fn pop_rest_of_arguments(&mut self) {
    self.pop_rest_of_arguments_with(|x| x);
  }

  /// Generates code to call the function given by `function_name`,
  /// passing all of the accumulated arguments in order. The code is
  /// generated as part of a `return` statement, so generally speaking
  /// no more code should be executed after this point.
  pub fn call_function_with_arguments(&mut self, function_name: &str) {
    let all_args = self.args.clone();
    self.stmts.push(Stmt::return_stmt(Expr::simple_call(function_name, all_args, self.pos), self.pos));
  }

  /// Generates code for an `if` statement, where the first branch is
  /// followed if the arguments variable has the value `nil` and the
  /// second is followed otherwise.
  ///
  /// The two branches are created by passing `empty_block` and
  /// `nonempty_block`, respectively, to
  /// [`LambdaVarargBuilder::with_inner_builder`].
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

  /// Consumes the builder and returns the sequence of statements
  /// generated.
  pub fn build(self) -> Vec<Stmt> {
    self.stmts
  }

}
