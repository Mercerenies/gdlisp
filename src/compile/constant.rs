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

//! Functionality for checking whether an expression is actually an
//! allowable constant expression.
//!
//! For the moment, what's allowable as a constant expression is
//! *extremely* conservative. Even things like if-statements where all
//! arguments are constant are disallowed, as we have no way to
//! compile that on the GDScript side. Long term, it would be nice to
//! support things like that which are constant "in spirit" and work
//! around GDScript limitations. But for now, we're being super
//! strict.

use super::error::{GDError, GDErrorF};
use super::symbol_table::SymbolTable;
use super::symbol_table::local_var::{LocalVar, ValueHint};
use crate::pipeline::source::SourceOffset;
use crate::ir::expr::{Expr as IRExpr, ExprF as IRExprF, BareName, CallTarget};
use crate::ir::decl::{ClassInnerDecl, ClassInnerDeclF, Decl, DeclF};
use crate::ir::literal::Literal;
use crate::ir::scope::decl::on_each_lambda_class;

use phf::phf_set;

pub const CONSTANT_GDSCRIPT_FUNCTIONS: phf::Set<&'static str> = phf_set! {
  "NodePath", "bool", "int", "float", "String", "str", "Rect2", "AABB", "RID", "Dictionary",
  "Array", "PoolColorArray", "PoolByteArray", "PoolIntArray", "PoolRealArray", "PoolStringArray",
  "PoolVector2Array", "PoolVector3Array", "Vector2", "Vector3", "Transform2D", "Plane",
  "Quat", "Basis", "Transform", "Color",
};

pub fn validate_all_constant_scopes(decls: &[Decl], table: &SymbolTable) -> Result<(), GDError> {

  // Check all top-level constant and enum declarations, and delegate for top-level classes
  for decl in decls {
    match &decl.value {
      DeclF::ConstDecl(const_decl) => {
        validate_const_expr(&const_decl.name, &const_decl.value, table)?;
      }
      DeclF::EnumDecl(enum_decl) => {
        for (_, rhs) in &enum_decl.clauses {
          if let Some(rhs) = rhs {
            validate_const_expr(&enum_decl.name, rhs, table)?;
          }
        }
      }
      DeclF::ClassDecl(class_decl) => {
        validate_constant_names_in_class(&class_decl.decls, table)?;
      }
      _ => {}
    }
  }

  // Check all lambda classes
  on_each_lambda_class(decls, |cls| {
    validate_constant_names_in_class(&cls.decls, table)
  })?;

  Ok(())
}

fn validate_constant_names_in_class(inner_decls: &[ClassInnerDecl], table: &SymbolTable) -> Result<(), GDError> {
  for decl in inner_decls {
    match &decl.value {
      ClassInnerDeclF::ClassConstDecl(const_decl) => {
        validate_const_expr(&const_decl.name, &const_decl.value, table)?;
      }
      ClassInnerDeclF::ClassVarDecl(var_decl) => {
        if let Some(export) = &var_decl.export {
          for arg in &export.args {
            // As a special exception, we allow any literal symbols
            // appearing here, since they can reference things like
            // `int` freely. (TODO Just generally make exports fit
            // better with the rest of GDLisp)
            if !(matches!(&arg.value, IRExprF::BareName(_))) {
              validate_const_expr(&var_decl.name, arg, table)?;
            }
          }
        }
      }
      _ => {}
    }
  }
  Ok(())
}

pub fn is_const_expr(expr: &IRExpr, table: &SymbolTable) -> bool {
  validate_const_expr("UNUSED NAME IN is_const_expr", expr, table).is_ok()
}

pub fn validate_const_expr(name: &str, expr: &IRExpr, table: &SymbolTable) -> Result<(), GDError> {
  match &expr.value {
    IRExprF::BareName(var) => {
      match var {
        BareName::Plain(var_name) => {
          validate_const_var_name(name, var_name, table, expr.pos)
        }
        BareName::Atomic(_) => {
          // AtomicName is explicitly opting out of GDLisp's safety
          // checks, so we'll let it through and just trust the
          // programmer.
          Ok(())
        }
      }
    }
    IRExprF::Literal(lit) => {
      if let Literal::Symbol(_) = lit {
        non_constant_error(name, expr.pos)
      } else {
        Ok(())
      }
    }
    IRExprF::Progn(body) => {
      match &body[..] {
        [] => {
          // Empty progn, compiles to null.
          Ok(())
        }
        [single_term] => {
          // Single-term progn, compiles to the inside.
          validate_const_expr(name, single_term, table)
        }
        _ => {
          // Multiple terms will require multiple statements, forbid
          // it.
          non_constant_error(name, expr.pos)
        }
      }
    }
    IRExprF::CondStmt(_) => {
      // Note: We always compile CondStmt to full-form multi-line "if"
      // statements first. Sometimes, the optimizer might simplify
      // them down to ternary expressions, but that's an optimization
      // detail. As far as we're concerned, it's a statement and is
      // non-const.
      //
      // If the architecture changes and we start compiling to ternary
      // directly, then this constraint will loosen.
      non_constant_error(name, expr.pos)
    }
    IRExprF::WhileStmt(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::ForStmt(_, _, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Call(object, function_name, args) => {
      validate_const_call(name, object, function_name, args, table, expr.pos)
    }
    IRExprF::Let(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::FunctionLet(_, _, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Lambda(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::FuncRef(_) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Assign(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Quote(_) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::FieldAccess(lhs, _name) => {
      if is_name_of_enum(lhs, table) {
        Ok(())
      } else {
        non_constant_error(name, expr.pos)
      }
    }
    IRExprF::LambdaClass(_) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Yield(_) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Assert(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Return(_) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Break => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Continue => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::SpecialRef(_) => {
      Ok(())
    }
    IRExprF::ContextualFilename(_) => {
      // This resolves completely at compile-time and, as far as
      // GDScript is concerned, is just a constant string.
      Ok(())
    }
    IRExprF::Split(_, _) => {
      // Split specifically requires a temporary variable in order to
      // work, which we can't do in a constant expression
      non_constant_error(name, expr.pos)
    }
    IRExprF::Preload(_) => {
      Ok(())
    }
  }
}

fn validate_const_var_name(name: &str, var_name: &str, table: &SymbolTable, pos: SourceOffset) -> Result<(), GDError> {
  let var = table.get_var(var_name).ok_or_else(|| non_constant_error::<()>(name, pos).unwrap_err())?;
  if var.is_valid_const_expr() {
    Ok(())
  } else {
    non_constant_error(name, pos)
  }

}

fn validate_const_call(name: &str, object: &CallTarget, function_name: &str, args: &[IRExpr], table: &SymbolTable, pos: SourceOffset) -> Result<(), GDError> {
  match object {
    CallTarget::Super | CallTarget::Object(_) => {
      // Always disallowed.
      return non_constant_error(name, pos);
    }
    CallTarget::Atomic => {
      // `Atomic` is explicitly opting out of GDLisp's safety checks,
      // so we'll let the name through and just trust the programmer.
      // We'll still check the arguments though.
    }
    CallTarget::Scoped => {
      // Check the name to make sure it's a constant enough function.
      validate_scoped_const_call(name, function_name, args.len(), table, pos)?;
    }
  }
  for arg in args {
    validate_const_expr(name, arg, table)?;
  }
  Ok(())
}

fn validate_scoped_const_call(name: &str, function_name: &str, arg_count: usize, table: &SymbolTable, pos: SourceOffset) -> Result<(), GDError> {
  let (function, magic) = table.get_fn(function_name).ok_or_else(|| non_constant_error::<()>(name, pos).unwrap_err())?;
  if magic.is_default() {
    // If the call magic passes through to the implementation, then we
    // need to look at the function and see if it's sufficiently const
    // for GDScript's tastes.
    if function.can_be_called_as_const() {
      Ok(())
    } else {
      non_constant_error(name, pos)
    }
  } else {
    // If there's call magic, use that to determine const-ness.
    if magic.can_be_called_as_const(arg_count) {
      Ok(())
    } else {
      non_constant_error(name, pos)
    }
  }
}

fn is_name_of_enum(lhs: &IRExpr, table: &SymbolTable) -> bool {
  if let Some(lhs) = lhs.as_plain_name() {
    if let Some(LocalVar { value_hint: Some(ValueHint::Enum(_)), .. }) = table.get_var(lhs) {
      // Note: We don't care if the name we're referencing on the enum
      // is correct or not here. If we're subscripting an enum, then
      // it's fine. If the name is bad, then the compiler will catch
      // it in the next phase and we'll throw a much more accurate
      // `NoSuchEnumValue` (rather than `NotConstantEnough`).
      return true;
    }
  }
  false
}

fn non_constant_error<T>(name: &str, pos: SourceOffset) -> Result<T, GDError> {
  Err(GDError::new(GDErrorF::NotConstantEnough(name.to_owned()), pos))
}
