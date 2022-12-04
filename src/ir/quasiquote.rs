
use crate::sxp::ast::{AST, ASTF};
use crate::ir::incremental::IncCompiler;
use crate::compile::error::{GDError, GDErrorF};
use super::expr::Expr;
use crate::pipeline::error::PError;
use crate::pipeline::Pipeline;

// TODO Where do I pick up PError in this file? Can we do all of this in GDError?

/// A `QQSpliced` represents an expression within an `unquote` or
/// `unquote-spliced` block. Specifically, a `QQSpliced` contains an
/// expression that, when the quasiquote is evaluated, will have its
/// value interpolated in some form or another into the surrounding
/// expression.
#[derive(Clone, Debug, PartialEq, Eq)]
enum QQSpliced {
  /// An expression that will be interpreted as a single atomic value.
  /// The inside of an `unquote` block is parsed as this variant.
  Single(Expr),
  /// An expression that will be spliced into an array or list
  /// expression. The inside of an `unquote-spliced` block parses as
  /// this variant.
  ///
  /// It is an error if a `QQSpliced::Several` appears in a context
  /// other than a list or array, as it cannot be spliced. For
  /// example, it is an error if a `QQSpliced::Several` appears within
  /// a dictionary literal.
  Several(Expr),
}

/// A value within a quasiquoting expression. `UnquotedValue` always
/// wraps a single [`AST`] and keeps track of what sort of expression
/// it is, as far as the quasiquoting engine is concerned.
#[derive(Clone, Debug, PartialEq, Eq)]
enum UnquotedValue<'a> {
  /// A simple, literal value which is not anything special as far as
  /// the quasiquoting engine is concerned.
  SimpleValue(&'a AST),
  /// A nested `quasiquote` block. The [`AST`] is the *inside* of the
  /// `quasiquote`, excluding the block itself.
  Quasiquote(&'a AST),
  /// An `unquote` block. The [`AST`] is the *inside* of the
  /// `unquote`, excluding the block itself.
  Unquote(&'a AST),
  /// An `unquote-spliced` block. The [`AST`] is the *inside* of the
  /// `unquote-spliced`, excluding the block itself.
  UnquoteSpliced(&'a AST),
}

impl QQSpliced {

  /// Extracts the [`QQSpliced::Single`] value from `self`. If `self`
  /// is not a [`QQSpliced::Single`] then an error is returned instead.
  ///
  /// The [`AST`] argument should be the original S-expression from
  /// which the `QQSpliced` was produced. It will only be used in the
  /// case of an error in order to provide a more detailed diagnostic.
  fn into_single(self, ast: &AST) -> Result<Expr, GDError> {
    match self {
      QQSpliced::Single(e) => Ok(e),
      QQSpliced::Several(_) => Err(GDError::new(GDErrorF::BadUnquoteSpliced(ast.clone()), ast.pos)),
    }
  }

}

impl<'a> UnquotedValue<'a> {

  /// Convenience constructor, delegates to
  /// [`UnquotedValue::SimpleValue`].
  fn verbatim(arg: &'a AST) -> UnquotedValue<'a> {
    UnquotedValue::SimpleValue(arg)
  }

  /// Reads the top-level S-expressions and looks for any special
  /// quasiquote keywords. Specifically, if the S-expression is a
  /// proper list of two elements and the first element is one of the
  /// following symbols, then the function returns an appropriate
  /// value.
  ///
  /// * If the symbol is `quasiquote`, then an
  /// [`UnquotedValue::Quasiquote`] containing the second element of
  /// the list is returned.
  ///
  /// * If the symbol is `unquote`, then an [`UnquotedValue::Unquote`]
  /// containing the second element of the list is returned.
  ///
  /// * If the symbol is `unquote-spliced`, then an
  /// [`UnquotedValue::UnquoteSpliced`] containing the second element
  /// of the list is returned.
  ///
  /// If the symbol is anything else, or if the AST has any other
  /// shape, then this function returns an
  /// [`UnquotedValue::SimpleValue`] containing the entire AST.
  fn parse(arg: &'a AST) -> UnquotedValue<'a> {
    if let ASTF::Cons(car, cdr) = &arg.value {
      if let Some(name) = car.as_symbol_ref() {
        if let ASTF::Cons(cadr, cddr) = &cdr.value {
          if cddr.value == ASTF::NIL {
            if name == "quasiquote" {
              return UnquotedValue::Quasiquote(cadr);
            } else if name == "unquote" {
              return UnquotedValue::Unquote(cadr);
            } else if name == "unquote-spliced" {
              return UnquotedValue::UnquoteSpliced(cadr);
            }
          }
        }
      }
    }
    UnquotedValue::SimpleValue(arg)
  }

}

pub fn quasiquote(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  arg: &AST)
                  -> Result<Expr, PError> {
  quasiquote_with_depth(icompiler, pipeline, arg, u32::MAX)
}

pub fn quasiquote_with_depth(icompiler: &mut IncCompiler,
                             pipeline: &mut Pipeline,
                             arg: &AST,
                             max_depth: u32)
                  -> Result<Expr, PError> {
  let mut engine = QuasiquoteEngine::new(icompiler, pipeline, max_depth);
  engine.quasiquote_indexed(arg, 0, 0)
}

/// The quasiquoting engine is the internal class whose implementation
/// recursively implements quasiquote parsing on an [`AST`].
///
/// A quasiquote engine requires mutable access to an [`IncCompiler`]
/// and a [`Pipeline`], in order to call [`IncCompiler::compile_expr`]
/// on unquoted values. Additionally, a quasiquote engine keeps track
/// of how deep it is into a Godot expression. This is a workaround
/// for a Godot parsing issue that manifests when an expression is
/// nested too deeply.
struct QuasiquoteEngine<'a, 'b> {
  icompiler: &'a mut IncCompiler,
  pipeline: &'b mut Pipeline,
  max_depth: u32,
}

impl<'a, 'b> QuasiquoteEngine<'a, 'b> {

  /// Given an incremental compiler and a pipeline, produce a
  /// [`QuasiquoteEngine`].
  ///
  /// The `max_depth` argument indicates how deeply nested a generated
  /// expression can be before the engine will split it into multiple
  /// expressions with a temporary variable in between. For the
  /// rationale behind this argument (as well as a reasonable default
  /// value for it), see
  /// [`MAX_QUOTE_REIFY_DEPTH`](crate::compile::frame::MAX_QUOTE_REIFY_DEPTH).
  fn new(icompiler: &'a mut IncCompiler, pipeline: &'b mut Pipeline, max_depth: u32) -> Self {
    QuasiquoteEngine { icompiler, pipeline, max_depth }
  }

  // Note: nesting_depth is the number of nested quasiquotes we're in.
  // An unquote encountered when nesting_depth is positive simply
  // decreases that value rather than performing an actual unquote
  // operation. current_depth is how far down we are into our
  // structure and is used to determine when to insert ExprF::Split
  // calls to avoid Godot parsing issues.

  fn quasiquote_indexed(&mut self,
                        arg: &AST,
                        nesting_depth: u32,
                        current_depth: u32)
                        -> Result<Expr, PError> {
    let (needs_split_wrapper, current_depth) =
      if current_depth > self.max_depth {
        (true, 0)
      } else {
        (false, current_depth)
      };

    self.quasiquote_spliced(arg, nesting_depth, current_depth).and_then(|qq| {
      let value = qq.into_single(arg)?;
      if needs_split_wrapper {
        let pos = value.pos;
        Ok(value.named_split("_quasiquote", pos))
      } else {
        Ok(value)
      }
    })

  }

  fn quasiquote_spliced(&mut self,
                        arg: &AST,
                        nesting_depth: u32,
                        current_depth: u32)
                        -> Result<QQSpliced, PError> {
    let unquoted_value = UnquotedValue::parse(arg);

    // Deal with nesting issues
    let (unquoted_value, nesting_depth) = match unquoted_value {
      UnquotedValue::SimpleValue(_) => {
        (UnquotedValue::verbatim(arg), nesting_depth)
      }
      UnquotedValue::Quasiquote(_) => {
        (UnquotedValue::verbatim(arg), nesting_depth + 1)
      }
      UnquotedValue::Unquote(_) | UnquotedValue::UnquoteSpliced(_) => {
        if nesting_depth > 0 {
          // We're inside a nested quasiquote, so do NOT unquote the value.
          (UnquotedValue::verbatim(arg), nesting_depth - 1)
        } else {
          (unquoted_value, nesting_depth)
        }
      }
    };

    match unquoted_value {
      UnquotedValue::Unquote(arg) => {
        self.icompiler.compile_expr(self.pipeline, arg).map(QQSpliced::Single)
      }
      UnquotedValue::UnquoteSpliced(arg) => {
        self.icompiler.compile_expr(self.pipeline, arg).map(QQSpliced::Several)
      }
      UnquotedValue::Quasiquote(_) => {
        // The above nesting handler should always eliminate
        // UnquotedValue::Quasiquote and convert it into
        // UnquotedValue::SimpleValue, so this should never happen.
        panic!("Internal error in quasiquote_spliced (impossible UnquotedValue::Quasiquote branch was reached)")
      }
      UnquotedValue::SimpleValue(arg) => {
        let body = match &arg.value {
          ASTF::Atom(lit) => {
            Expr::from_ast_literal(lit, arg.pos)
          }
          ASTF::Cons(car, cdr) => {
            let car = self.quasiquote_spliced(car, nesting_depth, current_depth + 1)?;
            let cdr = self.quasiquote_indexed(cdr, nesting_depth, current_depth + 1)?;
            match car {
              QQSpliced::Single(car) => {
                Expr::call(String::from("cons"), vec!(car, cdr), arg.pos)
              }
              QQSpliced::Several(car) => {
                let converted_car = Expr::call(String::from("sys/qq-smart-list"), vec!(car), arg.pos);
                Expr::call(String::from("append"), vec!(converted_car, cdr), arg.pos)
              }
            }
          }
        };
        Ok(QQSpliced::Single(body))
      }
    }
  }

}
