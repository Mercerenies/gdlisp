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

//! Handlers for the main function in a GDLisp source file.

use super::incremental::IncCompiler;
use super::arglist::ordinary::ArgList;
use super::expr::Expr;
use super::decl::{Decl, DeclF, FnDecl};
use super::export::Visibility;
use crate::pipeline::source::SourceOffset;
use crate::compile::error::{GDError, GDErrorF};

/// A [`MainFunctionHandler`] which errs out if there are any
/// top-level expressions in the file. This handler performs no action
/// if the expression slice is empty.
#[derive(Clone, Debug, Copy)]
pub struct DisallowMainFunctionHandler;

/// A [`MainFunctionHandler`] implementing the original behavior of
/// GDLisp, accumulating all of the top-level expressions into a
/// static function with the given name.
///
/// This handler is not used in GDLisp production but is used in the
/// testing scaffolding in order to be able to run expressions
/// alongside declarations. It may be added back into GDLisp proper in
/// the future.
#[derive(Clone, Debug)]
pub struct StaticMainFunctionHandler {
  pub function_name: String,
}

/// When the incremental compiler compiles a source file, it accepts
/// top-level expressions and declarations. The declarations are
/// handled by the incremental compiler, and the expressions are
/// passed on to the `MainFunctionHandler` to determine what to do
/// with them.
///
/// The current default behavior in GDLisp is to err out if there are
/// any expressions at the top-level, leaving the behavior available
/// in future implementations. This implementation is provided by
/// [`DisallowMainFunctionHandler`].
pub trait MainFunctionHandler {

  fn handle_main(&self, icompiler: &mut IncCompiler, main: &[Expr]) -> Result<(), GDError>;

}

impl MainFunctionHandler for DisallowMainFunctionHandler {

  fn handle_main(&self, _icompiler: &mut IncCompiler, main: &[Expr]) -> Result<(), GDError> {
    if main.is_empty() {
      Ok(())
    } else {
      Err(GDError::new(GDErrorF::ExprAtTopLevel(main[0].clone()), main[0].pos))
    }
  }

}

impl StaticMainFunctionHandler {

  pub fn new(function_name: String) -> Self {
    StaticMainFunctionHandler { function_name }
  }

}

impl MainFunctionHandler for StaticMainFunctionHandler {

  fn handle_main(&self, icompiler: &mut IncCompiler, main: &[Expr]) -> Result<(), GDError> {
    // Note: main_decl is synthesized from the file itself, so
    // SourceOffset(0) isn't just a cop-out here; it's the actual
    // right answer.
    let pos = SourceOffset(0);
    let main_decl = DeclF::FnDecl(FnDecl {
      visibility: Visibility::FUNCTION,
      call_magic: None,
      name: self.function_name.to_owned(),
      args: ArgList::empty(),
      body: Expr::progn(main.to_vec(), pos),
    });
    let main_decl = Decl::new(main_decl, pos);
    icompiler.declaration_table().add(main_decl);
    Ok(())
  }

}
