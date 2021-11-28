
use crate::sxp::ast::{AST, ASTF};
use crate::ir::incremental::IncCompiler;
use crate::compile::error::{Error as GDError, ErrorF as GDErrorF};
use super::expr::{Expr, ExprF};
use super::literal::Literal;
use crate::pipeline::error::Error;
use crate::pipeline::Pipeline;

#[derive(Clone, Debug, PartialEq, Eq)]
enum QQSpliced {
  Single(Expr),
  Several(Expr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnquotedValue<'a> {
  SimpleValue(&'a AST),
  Quasiquote(&'a AST),
  Unquote(&'a AST),
  UnquoteSpliced(&'a AST),
}

impl QQSpliced {
  fn into_single(self, ast: &AST) -> Result<Expr, GDError> {
    match self {
      QQSpliced::Single(e) => Ok(e),
      QQSpliced::Several(_) => Err(GDError::new(GDErrorF::BadUnquoteSpliced(ast.clone()), ast.pos)),
    }
  }
}

impl<'a> UnquotedValue<'a> {
  fn verbatim(arg: &'a AST) -> UnquotedValue<'a> {
    UnquotedValue::SimpleValue(arg)
  }
}

impl<'a> From<&'a AST> for UnquotedValue<'a> {
  fn from(arg: &'a AST) -> UnquotedValue<'a> {
    if let ASTF::Cons(car, cdr) = &arg.value {
      if let ASTF::Symbol(name) = &car.value {
        if let ASTF::Cons(cadr, cddr) = &cdr.value {
          if cddr.value == ASTF::Nil {
            if name == "quasiquote" {
              return UnquotedValue::Quasiquote(&*cadr);
            } else if name == "unquote" {
              return UnquotedValue::Unquote(&*cadr);
            } else if name == "unquote-spliced" {
              return UnquotedValue::UnquoteSpliced(&*cadr);
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
                  -> Result<Expr, Error> {
  quasiquote_with_depth(icompiler, pipeline, arg, u32::MAX)
}

pub fn quasiquote_with_depth(icompiler: &mut IncCompiler,
                             pipeline: &mut Pipeline,
                             arg: &AST,
                             max_depth: u32)
                  -> Result<Expr, Error> {
  let mut engine = QuasiquoteEngine::new(icompiler, pipeline, max_depth);
  engine.quasiquote_indexed(arg, 0, 0)
}

struct QuasiquoteEngine<'a, 'b> {
  icompiler: &'a mut IncCompiler,
  pipeline: &'b mut Pipeline,
  max_depth: u32,
}

impl<'a, 'b> QuasiquoteEngine<'a, 'b> {

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
                        -> Result<Expr, Error> {
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
                        -> Result<QQSpliced, Error> {
    let unquoted_value = UnquotedValue::from(arg);

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
          ASTF::Nil => {
            Expr::new(ExprF::Literal(Literal::Nil), arg.pos)
          }
          ASTF::Int(n) => {
            Expr::new(ExprF::Literal(Literal::Int(*n)), arg.pos)
          }
          ASTF::Bool(b) => {
            Expr::new(ExprF::Literal(Literal::Bool(*b)), arg.pos)
          }
          ASTF::Float(f) => {
            Expr::new(ExprF::Literal(Literal::Float(*f)), arg.pos)
          }
          ASTF::String(s) => {
            Expr::new(ExprF::Literal(Literal::String(s.to_owned())), arg.pos)
          }
          ASTF::Symbol(s) => {
            Expr::new(ExprF::Literal(Literal::Symbol(s.to_owned())), arg.pos)
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
          ASTF::Array(v) => {
            let v1 = v.iter().map(|x| self.quasiquote_spliced(x, nesting_depth, current_depth + 1)).collect::<Result<Vec<_>, _>>()?;

            let mut acc: Vec<Expr> = vec!();
            let mut current_vec: Vec<Expr> = vec!();
            for value in v1 {
              match value {
                QQSpliced::Single(x) => {
                  current_vec.push(x)
                }
                QQSpliced::Several(x) => {
                  let x = Expr::call(String::from("sys/qq-smart-array"), vec!(x), arg.pos);
                  if !current_vec.is_empty() {
                    acc.push(Expr::new(ExprF::Array(current_vec), arg.pos));
                    current_vec = vec!();
                  }
                  acc.push(x);
                }
              }
            }
            if !current_vec.is_empty() {
              acc.push(Expr::new(ExprF::Array(current_vec), arg.pos))
            }
            Expr::call(String::from("+"), acc, arg.pos)
          }
          ASTF::Dictionary(v) => {
            // TODO Does unquote-spliced make sense in this context?
            let v1 = v.iter().map(|(k, v)| Ok((self.quasiquote_indexed(k, nesting_depth, current_depth + 1)?, self.quasiquote_indexed(v, nesting_depth, current_depth + 1)?))).collect::<Result<Vec<_>, Error>>()?;
            Expr::new(ExprF::Dictionary(v1), arg.pos)
          }
        };
        Ok(QQSpliced::Single(body))
      }
    }
  }

}
