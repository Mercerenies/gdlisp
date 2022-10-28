
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
use super::symbol_table::local_var::{LocalVar, ValueHint, ValueHintsTable};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::op;
use crate::pipeline::source::{Sourced, SourceOffset};
use crate::ir::expr::{Expr as IRExpr, ExprF as IRExprF};
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
            validate_const_expr(&enum_decl.name, &rhs, table)?;
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
            if !(matches!(&arg.value, IRExprF::LocalVar(_))) {
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

pub fn validate_const_expr(name: &str, expr: &IRExpr, table: &SymbolTable) -> Result<(), GDError> {
  match &expr.value {
    IRExprF::LocalVar(var_name) => {
      validate_const_var_name(name, var_name, table, expr.pos)
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
    IRExprF::Call(function_name, args) => {
      validate_const_call(name, function_name, args.len(), table, expr.pos)?;
      for arg in args {
        validate_const_expr(name, arg, table)?;
      }
      Ok(())
    }
    IRExprF::Let(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::FLet(_, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::Labels(_, _) => {
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
    IRExprF::Array(values) => {
      for value in values {
        validate_const_expr(name, value, table)?;
      }
      Ok(())
    }
    IRExprF::Dictionary(rows) => {
      for (k, v) in rows {
        validate_const_expr(name, k, table)?;
        validate_const_expr(name, v, table)?;
      }
      Ok(())
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
    IRExprF::MethodCall(_, _, _) => {
      non_constant_error(name, expr.pos)
    }
    IRExprF::SuperCall(_, _) => {
      non_constant_error(name, expr.pos)
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
    IRExprF::AtomicName(_) => {
      // AtomicName is explicitly opting out of GDLisp's safety
      // checks, so we'll let it through and just trust the
      // programmer.
      Ok(())
    }
    IRExprF::AtomicCall(_name, args) => {
      // AtomicCall is explicitly opting out of GDLisp's safety
      // checks, so we'll let the name through and just trust the
      // programmer. We'll still check the arguments though.
      for arg in args {
        validate_const_expr(name, arg, table)?;
      }
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

fn validate_const_call(name: &str, function_name: &str, arg_count: usize, table: &SymbolTable, pos: SourceOffset) -> Result<(), GDError> {
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
  if let IRExprF::LocalVar(lhs) = &lhs.value {
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

/// Trait representing data which can be checked for a const-ness
/// property.
pub trait MaybeConstant: Sourced {

  /// Returns whether or not the expression is an allowable constant
  /// value. `table` should contain information which can provide
  /// useful hints ([`ValueHint`]) about the sort of value a given
  /// name refers to. If there are no hints to be provided, then
  /// [`VacuousValueHintsTable`](crate::compile::symbol_table::local_var::VacuousValueHintsTable)
  /// can be provided as a null implementation of the trait.
  fn is_allowable_const(&self, table: &impl ValueHintsTable) -> bool;

  /// Check whether [`MaybeConstant::is_allowable_const`] is true. If
  /// not, return [`GDErrorF::NotConstantEnough`]. The `name` is used to
  /// produce a more convenient error message and is not used to
  /// determine whether or not the value is constant.
  fn validate_const_expr(&self, name: &str, table: &impl ValueHintsTable) -> Result<(), GDError> {
    if self.is_allowable_const(table) {
      Ok(())
    } else {
      Err(GDError::new(GDErrorF::NotConstantEnough(name.to_owned()), self.get_source()))
    }
  }

}

impl MaybeConstant for Expr {
  fn is_allowable_const(&self, table: &impl ValueHintsTable) -> bool {
    match &self.value {
      ExprF::Var(v) => {
        if let Some(value_hint) = table.get_value_hint(v) {
          // Again, playing it super-safe right now. Only values
          // explicitly declared superglobal or constant are allowed,
          // and I'll only use sys/declare to define superglobals when
          // I know it's safe. sys/declare is used to bypass the
          // compiler's safety mechanisms, so here is one such
          // backdoor we're putting in.
          *value_hint == ValueHint::Superglobal || *value_hint == ValueHint::GlobalConstant
        } else {
          false // TODO Better?
        }
      }
      ExprF::Literal(_) => {
        true
      }
      ExprF::Subscript(a, b) => {
        a.is_allowable_const(table) && b.is_allowable_const(table)
      }
      ExprF::Attribute(_, _) => {
        false // I know, but Godot seems to disallow this one on principle
      }
      ExprF::Call(obj, name, args) => {
        // If the function call is a built-in GDScript function that
        // Godot accepts as constant *and* all args are constant, then
        // accept it. Otherwise, no.
        if obj.is_none() && CONSTANT_GDSCRIPT_FUNCTIONS.contains(&**name) {
          return args.iter().all(|x| x.is_allowable_const(table));
        }
        false
      }
      ExprF::SuperCall(_, _) => {
        false
      }
      ExprF::Unary(_, a) => {
        a.is_allowable_const(table)
      }
      ExprF::Binary(a, op, b) => {
        // So casts don't seem to be considered const in GDScript...
        a.is_allowable_const(table) && b.is_allowable_const(table) && *op != op::BinaryOp::Cast
      }
      ExprF::TernaryIf(t) => {
        t.true_case.is_allowable_const(table) && t.cond.is_allowable_const(table) && t.false_case.is_allowable_const(table)
      }
      ExprF::ArrayLit(arr) => {
        arr.iter().all(|x| x.is_allowable_const(table))
      }
      ExprF::DictionaryLit(arr) => {
        arr.iter().all(|(k, v)| k.is_allowable_const(table) && v.is_allowable_const(table))
      }
    }
  }
}
