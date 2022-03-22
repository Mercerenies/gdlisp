
//! Function call magic, for deterministic inlining behavior.
//!
//! Function calls, by default, compile to function calls in GDScript,
//! naturally. However, there are many situations where the function
//! on the GDScript side is simply a trivial wrapper around some
//! operation that should be inlined. This is such a trivial inline
//! step that's so ubiquitously useful that we do it here as a rule.
//! If a built-in function is called directly (i.e. not through a
//! funcref) then it can trigger a special [`CallMagic`] which
//! effectively inlines it.
//!
//! For a good example, look at the `+` GDLisp function. In general,
//! it compiles to `GDLisp.plus`, which iterates over its arguments,
//! adds them, and returns the result. But, of course, `(+ a b)`
//! shouldn't require a for loop, so any time we call `+` with an
//! arity known at compile time (i.e. without invoking funcrefs or the
//! like), we can compile directly to the `+` operator in GDScript,
//! which is much more efficient.
//!
//! Note that this is *not* general-purpose inlining, which will be
//! implemented later as a general pass over the IR. This is for the
//! very specific case of certain GDScript functions written in
//! `GDLisp.lisp` which I know how to inline effectively by hand.

pub mod table;

use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::literal::Literal;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::op;
use crate::gdscript::library;
use crate::gdscript::expr_wrapper;
use crate::compile::Compiler;
use crate::compile::factory;
use crate::compile::error::Error;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::stateful::StExpr;
use crate::compile::stmt_wrapper::{self, StmtWrapper};
use crate::compile::args::{self, Expecting};
use crate::ir::arglist::VarArg;
use crate::util;
use crate::pipeline::source::SourceOffset;
use super::function_call::FnCall;
use super::SymbolTable;

use serde::{Serialize, Deserialize};

/// A `CallMagic` can meaningfully compile a given function call
/// expression `call` into some GDScript [`Expr`].
///
/// Note that, although we talk about call magic applying to certain
/// designated builtin calls, strictly speaking every function call
/// written in GDLisp invokes call magic. This trait is *always* used
/// to compile function calls. In most cases, for user-defined
/// functions or for builtins without special behavior, the
/// [`CallMagic::DefaultCall`] option is used, which performs the
/// basic function call compilation routine via
/// [`compile_default_call`].
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CallMagic {
  /// A default [`CallMagic`] for any functions without special
  /// behavior. The `CallMagic` implementation for `DefaultCall`
  /// simply delegates to [`compile_default_call`].
  DefaultCall,
  /// [Call magic](CallMagic) for the `-` builtin.
  MinusOperation,
  /// [Call magic](CallMagic) for the (fractional) division operator.
  DivOperation,
  /// [Call magic](CallMagic) for the integer division operator.
  IntDivOperation,
  /// [Call magic](CallMagic) for the mathematical modulo operator.
  ModOperation,
  /// [Call magic](CallMagic) for the `min` function.
  MinFunction,
  /// [Call magic](CallMagic) for the `max` function.
  MaxFunction,
  /// [`CallMagic`] for the inequality `/=` operator. `NEqOperation`
  /// only provides special behavior if two or fewer arguments are
  /// given. If more than two arguments are given, `NEqOperation` will
  /// fall back to `fallback`.
  NEqOperation(Box<CallMagic>),
  /// The [call magic](CallMagic) for the Boolean negation operation.
  BooleanNotOperation,
  /// [`CallMagic`] for the builtin `list` function. This call magic
  /// compiles calls to literal `cons` cell constructors.
  ListOperation,
  /// [`CallMagic`] for the builtin `vector` function, which compiles
  /// `vector` calls to literal `Vector2` or `Vector3` constructions
  /// in GDScript.
  VectorOperation,
  /// [`CallMagic`] for array subscript (via `elt`). This magic
  /// compiles directly to the `foo[bar]` subscript notation in
  /// GDScript.
  ArraySubscript,
  /// [`CallMagic`] for array subscript assignment (via `set-elt`). This
  /// magic compiles directly to the `foo[bar] = baz` subscript notation
  /// in GDScript.
  ArraySubscriptAssign,
  /// [Call magic] for the `member?` builtin function. This magic
  /// compiles to the GDScript `in` operator.
  ElementOf,
  /// [Call magic] for instance type checks in Godot.
  ///
  /// Note that the GDLisp built-in `instance?` does *not* compile to
  /// this magic, as that function is overloaded internally to work on
  /// primitive type constants as well as actual instances. This magic
  /// is used on the internal system function `sys/instance-direct?`.
  InstanceOf,
  /// [Call magic] for the `$` syntax used to invoke `get_node` in
  /// GDScript.
  ///
  /// This call magic compiles expressions of the form `(sys/get-node
  /// a b)` into GDScript `$x` syntax wherever it makes sense to do
  /// so, or `(a:get-node b)` in any other context.
  GetNodeSyntax,
  /// `CompileToBinOp` is a [`CallMagic`] which compiles function
  /// calls to sequences of binary operator application.
  ///
  /// If no arguments are provided, then this magic compiles to `zero`
  /// unconditionally. If one argument is provided, it is passed
  /// through untouched. If two or more arguments are provided, they
  /// are combined using `bin` in order, with associativity given by
  /// `assoc`.
  ///
  /// This call magic is used to implement the GDLisp builtins `+` and
  /// `*`.
  CompileToBinOp(Literal, op::BinaryOp, Assoc),
  /// `CompileToTransCmp` is a [`CallMagic`] which compiles function
  /// calls to transitive sequences of binary comparison applications.
  ///
  /// Conceptually, `CompileToTransCmp` can be thought of as
  /// translating a call like `(< a b c d)` into `a < b and b < c and
  /// c < d`. However, the translation is more subtle than that, as
  /// any of the names which are evaluated twice (i.e. all except the
  /// first and last) have to be checked for side effects. If the
  /// arguments might exhibit side effects, then they will need to be
  /// assigned to local variables to avoid evaluating the calls twice
  /// in the expression.
  ///
  /// If zero arguments are provided, then an error is returned. If
  /// one argument is provided, the result is vacuously true, as there
  /// are no comparisons to be performed.
  ///
  /// This call magic is used for most builtin comparison operators,
  /// such as `=` and `<`. It is notably *not* used for `/=`, which is
  /// handled by [`CallMagic::NEqOperation`] due to its unique (non-transitive)
  /// behavior.
  CompileToTransCmp(op::BinaryOp),
}

/// Associativity of an operator.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum Assoc {
  /// A left associative operator, as `(a + b) + c`.
  Left,
  /// A right associative operator, as `a + (b + c)`.
  Right,
}

// For most of these (but *not* all of them), we need Vec<Expr>, not
// Vec<StExpr>. The latter gives more information than we usually
// need. So this helper just strips off the excess.
fn strip_st(x: Vec<StExpr>) -> Vec<Expr> {
  x.into_iter().map(|x| x.expr).collect()
}

/// The "default" mechanism for compiling a function call, in the
/// absence of any nontrivial magic. This function compiles `call` and
/// its arguments `args` into a standard GDScript function call,
/// taking into consideration any optional-argument or rest-argument
/// padding which needs to be done to make the call correct on the
/// GDScript call. This is called by [`CallMagic::compile`] in the
/// [`CallMagic::DefaultCall`] case to perform its work.
pub fn compile_default_call(call: FnCall, mut args: Vec<Expr>, pos: SourceOffset) -> Result<Expr, Error> {
  let FnCall { scope: _, object, function, specs, is_macro: _ } = call;
  // First, check arity
  Expecting::from(specs).validate(&function, pos, &args)?;
  // Get the "rest" arguments
  let rest = if args.len() < (specs.required + specs.optional) as usize {
    vec!()
  } else {
    args.split_off((specs.required + specs.optional) as usize)
  };
  // Extend with nil
  while args.len() < (specs.required + specs.optional) as usize {
    args.push(Expr::null(pos));
  }
  // Bind the "rest" arguments
  match specs.rest {
    None => {
      // We already checked arity, so if this assertion fails it's a
      // bug in the compiler.
      assert!(rest.is_empty());
    }
    Some(VarArg::RestArg) => {
      args.push(library::construct_list(rest, pos));
    }
    Some(VarArg::ArrArg) => {
      args.push(Expr::new(ExprF::ArrayLit(rest), pos));
    }
  }
  // Then compile the call.
  let object: Option<Expr> = object.into_expr(pos);
  Ok(Expr::new(ExprF::Call(object.map(Box::new), function, args), pos))
}

impl CallMagic {
  // TODO Currently, this uses the GD name in error messages, which is
  // super wonky, especially for stdlib calls. Store the Lisp name and
  // use it for this.

  /// Given a [`FnCall`] instance `call` and argument list `args`,
  /// compile the call into a GDScript [`Expr`]. `compiler` provides a
  /// fresh name generator and compilation state, `builder` provides
  /// the enclosing block body as a [`StmtBuilder`], and `table`
  /// provides the enclosing scope information.
  pub fn compile(&self,
                 call: FnCall,
                 compiler: &mut Compiler,
                 builder: &mut StmtBuilder,
                 table: &mut SymbolTable,
                 mut args: Vec<StExpr>, // TODO Get this declared immutable here and mutable on inner scopes only
                 pos: SourceOffset) -> Result<Expr, Error> {
    match self {
      CallMagic::DefaultCall => {
        let args = strip_st(args);
        compile_default_call(call, args, pos)
      }
      CallMagic::CompileToBinOp(zero, op, assoc) => {
        let args = strip_st(args);
        if args.is_empty() {
          let expr = Expr::from_value(zero.clone(), pos);
          Ok(expr)
        } else {
          Ok(match assoc {
            Assoc::Left => {
              util::fold1(args.into_iter(), |x, y| Expr::new(ExprF::Binary(Box::new(x), *op, Box::new(y)), pos))
            }
            Assoc::Right => {
              util::fold1(args.into_iter().rev(), |x, y| Expr::new(ExprF::Binary(Box::new(y), *op, Box::new(x)), pos))
            }
          }.unwrap())
        }
      }
      CallMagic::CompileToTransCmp(op) => {
        Expecting::at_least(1).validate(&call.function, pos, &args)?;
        match args.len() {
          0 => {
            unreachable!()
          }
          1 => {
            // Dump to the builder as a simple statement if it's stateful.
            (&stmt_wrapper::Vacuous).wrap_to_builder(builder, args[0].clone());
            Ok(Expr::from_value(true, pos))
          }
          2 => {
            let a = args.remove(0).expr;
            let b = args.remove(0).expr;
            Ok(Expr::new(ExprF::Binary(Box::new(a), *op, Box::new(b)), pos))
          }
          _ => {
            // We need to use several of the arguments twice, so any
            // arguments (such as function calls) which are
            // potentially stateful need to be stored in temporaries.
            // Note that simply accessing variables, even variables
            // which may change, is fine, since we're doing it twice
            // in a row, and nothing happens in between.
            let args = args.into_iter().map(|x| {
              let StExpr { expr, side_effects } = x;
              if side_effects.modifies_state() {
                let var_name = factory::declare_var(compiler.name_generator(), builder, "_cmp", Some(expr), pos);
                Expr::new(ExprF::Var(var_name), pos)
              } else {
                expr
              }
            });
            let comparisons = util::each_pair(args).map(|(x, y)| {
              Expr::new(ExprF::Binary(Box::new(x), *op, Box::new(y)), pos)
            });
            Ok(
              util::fold1(comparisons, |x, y| Expr::new(ExprF::Binary(Box::new(x), op::BinaryOp::And, Box::new(y)), pos)).unwrap()
            )
          }
        }
      }
      CallMagic::MinusOperation => {
        let args = strip_st(args);
        Expecting::at_least(1).validate(&call.function, pos, &args)?;
        match args.len() {
          0 => {
            unreachable!()
          }
          1 => {
            let arg = args::one(args);
            Ok(arg.unary(op::UnaryOp::Negate, pos))
          }
          _ => {
            Ok(
              util::fold1(args.into_iter(), |x, y| x.binary(op::BinaryOp::Sub, y, pos)).unwrap()
            )
          }
        }
      }
      CallMagic::DivOperation => {
        let mut args = strip_st(args);
        Expecting::at_least(1).validate(&call.function, pos, &args)?;
        match args.len() {
          0 => {
            unreachable!()
          }
          1 => {
            let one = Expr::from_value(1, pos);
            let arg = args::one(args);
            Ok(one.binary(op::BinaryOp::Div, expr_wrapper::float(arg), pos))
          }
          _ => {
            let first = args.remove(0);
            let result = args.into_iter().fold(first, |x, y| {
              x.binary(op::BinaryOp::Div, expr_wrapper::float(y), pos)
            });
            Ok(result)
          }
        }
      }
      // TODO Integer division (both the regular function and the call
      // magic) still does floating-point division if the first
      // argument is a vector. I want to do integer division to each
      // component of the vector, which GDScript has no built-in way
      // to do.
      CallMagic::IntDivOperation => {
        let mut args = strip_st(args);
        Expecting::at_least(1).validate(&call.function, pos, &args)?;
        match args.len() {
          0 => {
            unreachable!()
          }
          1 => {
            let one = Expr::from_value(1, pos);
            let arg = args::one(args);
            Ok(one.binary(op::BinaryOp::Div, expr_wrapper::int(arg), pos))
          }
          _ => {
            let first = args.remove(0);
            let result = args.into_iter().fold(first, |x, y| {
              x.binary(op::BinaryOp::Div, expr_wrapper::int(y), pos)
            });
            Ok(result)
          }
        }
      }
      CallMagic::ModOperation => {
        let args = strip_st(args);
        Expecting::exactly(2).validate(&call.function, pos, &args)?;
        let (x, y) = args::two(args);
        Ok(Expr::new(ExprF::Binary(Box::new(x), op::BinaryOp::Mod, Box::new(y)), pos))
      }
      CallMagic::MinFunction => {
        let args = strip_st(args);
        if args.is_empty() {
          let expr = Expr::var("INF", pos);
          Ok(expr)
        } else {
          Ok(
            util::fold1(args.into_iter(), |x, y| Expr::simple_call("min", vec!(x, y), pos)).unwrap(),
          )
        }
      }
      CallMagic::MaxFunction => {
        let args = strip_st(args);
        if args.is_empty() {
          let expr = Expr::var("INF", pos).unary(op::UnaryOp::Negate, pos);
          Ok(expr)
        } else {
          Ok(
            util::fold1(args.into_iter(), |x, y| Expr::simple_call("max", vec!(x, y), pos)).unwrap(),
          )
        }
      }
      CallMagic::NEqOperation(fallback) => {
        // We only optimize for the 0, 1, and 2 argument cases. Any more
        // arguments than that and the resulting expression would just be
        // long and annoying, and it's simply easier to call the built-in
        // anyway.
        Expecting::at_least(1).validate(&call.function, pos, &args)?;
        match args.len() {
          0 => {
            unreachable!()
          }
          1 => {
            // Dump to the builder as a simple statement if it's stateful.
            (&stmt_wrapper::Vacuous).wrap_to_builder(builder, args[0].clone());
            Ok(Expr::from_value(true, pos))
          }
          2 => {
            let (lhs, rhs) = args::two(strip_st(args));
            Ok(lhs.binary(op::BinaryOp::NE, rhs, pos))
          }
          _ => {
            fallback.compile(call, compiler, builder, table, args, pos)
          }
        }
      }
      CallMagic::BooleanNotOperation => {
        let args = strip_st(args);
        Expecting::exactly(1).validate(&call.function, pos, &args)?;
        let arg = args::one(args);
        Ok(arg.unary(op::UnaryOp::Not, pos))
      }
      CallMagic::ListOperation => {
        let args = strip_st(args);
        Ok(library::construct_list(args, pos))
      }
      CallMagic::VectorOperation => {
        let args = strip_st(args);
        Expecting::between(2, 3).validate(&call.function, pos, &args)?;
        match args.len() {
          2 => {
            let (x, y) = args::two(args);
            Ok(Expr::call(None, "Vector2", vec!(x, y), pos))
          }
          3 => {
            let (x, y, z) = args::three(args);
            Ok(Expr::call(None, "Vector3", vec!(x, y, z), pos))
          }
          _ => {
            unreachable!()
          }
        }
      }
      CallMagic::ArraySubscript => {
        let args = strip_st(args);
        Expecting::exactly(2).validate(&call.function, pos, &args)?;
        let (arr, n) = args::two(args);
        Ok(arr.subscript(n, pos))
      }
      CallMagic::ArraySubscriptAssign => {
        let args = strip_st(args);
        Expecting::exactly(3).validate(&call.function, pos, &args)?;
        let (x, arr, n) = args::three(args);

        let assign_target = arr.subscript(n, pos);
        builder.append(Stmt::simple_assign(assign_target.clone(), x, pos));
        Ok(assign_target)
      }
      CallMagic::ElementOf => {
        let args = strip_st(args);
        Expecting::exactly(2).validate(&call.function, pos, &args)?;
        let (value, arr) = args::two(args);

        Ok(value.binary(op::BinaryOp::In, arr, pos))
      }
      CallMagic::InstanceOf => {
        let args = strip_st(args);
        Expecting::exactly(2).validate(&call.function, pos, &args)?;
        let (value, type_) = args::two(args);

        Ok(value.binary(op::BinaryOp::Is, type_, pos))
      }
      CallMagic::GetNodeSyntax => {
        let args = strip_st(args);
        Expecting::exactly(2).validate(&call.function, pos, &args)?;
        let (value, path) = args::two(args);

        if let ExprF::Literal(Literal::String(s)) = &path.value {
          if value.value == ExprF::Var(String::from("self")) {
            // We can use the $x syntax on the GDScript side
            //
            // TODO Make sure the string only has symbols allowed in a
            // GDScript node path literal.
            return Ok(Expr::from_value(Literal::NodeLiteral(s.to_owned()), pos));
          }
        }

        // Otherwise, just compile to self.get_node.
        Ok(
          Expr::call(
            Some(value),
            "get_node",
            vec!(path),
            pos,
          ),
        )

      }
    }
  }

}
