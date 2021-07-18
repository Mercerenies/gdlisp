
//! GDScript expressions.
//!
//! This module defines a [datatype](Expr) for representing
//! expressions in the GDScript language, as well as [`Expr::to_gd`]
//! for converting to GDScript syntax.
//!
//! Note: For names (such as strings), we expect that they've already
//! been sanitized for GDScript output. That should've happened
//! earlier in the compilation process. This precondition is not
//! checked anywhere in this module.

use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};
use crate::gdscript::literal::Literal;

pub const PRECEDENCE_LOWEST: i32 = -99;
pub const PRECEDENCE_SUBSCRIPT: i32 = 21;
pub const PRECEDENCE_ATTRIBUTE: i32 = 20;
pub const PRECEDENCE_CALL: i32 = 19;

/// The type of GDScript expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  Var(String),
  Literal(Literal),
  /// Subscript access, i.e. `foo[bar]`.
  Subscript(Box<Expr>, Box<Expr>),
  /// Attribute access, i.e. `foo.bar`.
  Attribute(Box<Expr>, String),
  /// A function call, possibly qualified by a value name. If the
  /// first argument is `None`, then this is akin to a call of the
  /// form `bar(...)`. If the first argument is `Some(foo)`, then this
  /// is akin to `foo.bar(...)`.
  Call(Option<Box<Expr>>, String, Vec<Expr>),
  /// A super call, i.e. `.bar(...)`.
  SuperCall(String, Vec<Expr>),
  Unary(UnaryOp, Box<Expr>),
  Binary(Box<Expr>, BinaryOp, Box<Expr>),
  /// A use of the ternary-if operator `foo if bar else baz`.
  TernaryIf(TernaryIf),
  ArrayLit(Vec<Expr>),
  DictionaryLit(Vec<(Expr, Expr)>),
}

/// The type used by [`Expr::TernaryIf`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TernaryIf {
  pub true_case: Box<Expr>,
  pub cond: Box<Expr>,
  pub false_case: Box<Expr>,
}

fn maybe_parens(cond: bool, inner: String) -> String {
  if cond {
    format!("({})", inner)
  } else {
    inner
  }
}

impl Expr {

  /// The literal expression `null`.
  pub fn null() -> Expr {
    Expr::Literal(Literal::Null)
  }

  /// A literal string.
  pub fn str_lit(a: &str) -> Expr {
    Expr::from(a.to_owned())
  }

  /// An [`Expr::Var`], referenced by name. The name will be cloned
  /// into the resulting value.
  pub fn var(a: &str) -> Expr {
    Expr::Var(a.to_owned())
  }

  /// A GDScript `yield` call.
  ///
  /// `yield` takes either zero or two arguments, so this function can
  /// produce either form of `yield` (by passing either `None` or
  /// `Some(a, b)`).
  pub fn yield_expr(args: Option<(Expr, Expr)>) -> Expr {
    let args = match args {
      None => vec!(),
      Some((x, y)) => vec!(x, y),
    };
    Expr::Call(None, String::from("yield"), args)
  }

  /// Convert to a GDScript string, assuming the ambient precedence is
  /// a specific value.
  ///
  /// Generally, callers will want to invoke [`Expr::to_gd`] and let the
  /// expression manage its own precedence.
  pub fn to_gd_prec(&self, prec: i32) -> String {
    match self {
      Expr::Var(s) => s.clone(),
      Expr::Literal(lit) => lit.to_gd(),
      Expr::Subscript(lhs, index) =>
        format!("{}[{}]", lhs.to_gd_prec(PRECEDENCE_SUBSCRIPT), index.to_gd_prec(PRECEDENCE_LOWEST)),
      Expr::Attribute(lhs, name) =>
        format!("{}.{}", lhs.to_gd_prec(PRECEDENCE_ATTRIBUTE), name),
      Expr::Call(class, name, args) => {
        let prefix = if let Some(class) = class {
          format!("{}.", class.to_gd_prec(PRECEDENCE_CALL))
        } else {
          String::from("")
        };
        let arglist = args.iter().map(|arg| arg.to_gd_prec(PRECEDENCE_LOWEST)).collect::<Vec<_>>().join(", ");
        format!("{}{}({})", prefix, name, arglist)
      },
      Expr::SuperCall(name, args) => {
        let arglist = args.iter().map(|arg| arg.to_gd_prec(PRECEDENCE_LOWEST)).collect::<Vec<_>>().join(", ");
        format!(".{}({})", name, arglist)
      },
      Expr::Unary(op, arg) => {
        let info = op.op_info();
        let arg = arg.to_gd_prec(info.precedence);
        let inner = if info.padding == op::Padding::NotRequired {
          format!("{}{}", info.name, arg)
        } else {
          format!("{} {}", info.name, arg)
        };
        maybe_parens(prec > info.precedence, inner)
      },
      Expr::Binary(lhs, op, rhs) => {
        // Implicit assumption that all operators are left-associative.
        let info = op.op_info();
        let lhs = lhs.to_gd_prec(info.precedence);
        let rhs = rhs.to_gd_prec(info.precedence + 1);
        let inner = if info.padding == op::Padding::NotRequired {
          format!("{}{}{}", lhs, info.name, rhs)
        } else {
          format!("{} {} {}", lhs, info.name, rhs)
        };
        maybe_parens(prec > info.precedence, inner)
      },
      Expr::TernaryIf(TernaryIf { true_case, cond, false_case }) => {
        // Ternary is right-associative.
        let info = op::TernaryOp.op_info();
        let lhs = true_case.to_gd_prec(info.precedence + 1);
        let cond = cond.to_gd_prec(PRECEDENCE_LOWEST);
        let rhs = false_case.to_gd_prec(info.precedence);
        let inner = format!("{} if {} else {}", lhs, cond, rhs);
        maybe_parens(prec > info.precedence, inner)
      },
      Expr::ArrayLit(vec) => {
        let mut first = true;
        let mut result = String::from("[");
        for x in vec {
          if !first {
            result.push_str(", ");
          }
          result.push_str(&x.to_gd_prec(PRECEDENCE_LOWEST));
          first = false;
        }
        result.push(']');
        result
      },
      Expr::DictionaryLit(vec) => {
        let mut first = true;
        let mut result = String::from("{");
        for (k, v) in vec {
          if !first {
            result.push_str(", ");
          }
          result.push_str(&format!("{}: {}", k.to_gd_prec(PRECEDENCE_LOWEST), v.to_gd_prec(PRECEDENCE_LOWEST)));
          first = false;
        }
        result.push('}');
        result
      },
    }
  }

  /// Convert a GDScript expression to a string. The result will
  /// contain valid GDScript syntax.
  pub fn to_gd(&self) -> String {
    self.to_gd_prec(PRECEDENCE_LOWEST)
  }

}

impl<T> From<T> for Expr
  where Literal : From<T> {
  fn from(x: T) -> Expr {
    Expr::Literal(Literal::from(x))
  }
}

impl From<TernaryIf> for Expr {
  fn from(x: TernaryIf) -> Expr {
    Expr::TernaryIf(x)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn basic_expr_types() {
    let var = Expr::Var(String::from("foobar"));
    let n = Expr::from(99);
    let name = String::from("attr");

    let arg1 = Expr::from(1);
    let arg2 = Expr::from(2);
    let lhs = Expr::Binary(Box::new(arg1), BinaryOp::Add, Box::new(arg2));

    let attr = Expr::Attribute(Box::new(var.clone()), name.clone());
    let subs = Expr::Subscript(Box::new(var.clone()), Box::new(n.clone()));

    assert_eq!(var.to_gd(), "foobar");
    assert_eq!(n.to_gd(), "99");
    assert_eq!(Expr::Subscript(Box::new(var.clone()), Box::new(n.clone())).to_gd(), "foobar[99]");
    assert_eq!(attr.to_gd(), "foobar.attr");
    assert_eq!(subs.to_gd(), "foobar[99]");
    assert_eq!(Expr::Attribute(Box::new(attr.clone()), name.clone()).to_gd(), "foobar.attr.attr");
    assert_eq!(Expr::Attribute(Box::new(subs.clone()), name.clone()).to_gd(), "foobar[99].attr");
    assert_eq!(Expr::Subscript(Box::new(attr.clone()), Box::new(n.clone())).to_gd(), "foobar.attr[99]");
    assert_eq!(Expr::Subscript(Box::new(subs.clone()), Box::new(n.clone())).to_gd(), "foobar[99][99]");

    assert_eq!(Expr::Subscript(Box::new(lhs.clone()), Box::new(n.clone())).to_gd(), "(1 + 2)[99]");
    assert_eq!(Expr::Attribute(Box::new(lhs.clone()), name.clone()).to_gd(), "(1 + 2).attr");

  }

  #[test]
  fn call_exprs() {
    let lhs = Expr::Var(String::from("lhs"));
    let name = String::from("func");
    let arg1 = Expr::from(1);
    let arg2 = Expr::from(2);
    let arg3 = Expr::from(3);

    let lhs_p = Expr::Binary(Box::new(arg1.clone()), BinaryOp::Add, Box::new(arg2.clone()));

    assert_eq!(Expr::Call(None, name.clone(), vec!()).to_gd(), "func()");
    assert_eq!(Expr::Call(None, name.clone(), vec!(arg1.clone())).to_gd(), "func(1)");
    assert_eq!(Expr::Call(None, name.clone(), vec!(arg1.clone(), arg2.clone())).to_gd(), "func(1, 2)");
    assert_eq!(Expr::Call(None, name.clone(), vec!(arg1.clone(), arg2.clone(), arg3.clone())).to_gd(), "func(1, 2, 3)");

    assert_eq!(Expr::Call(Some(Box::new(lhs.clone())), name.clone(), vec!()).to_gd(), "lhs.func()");
    assert_eq!(Expr::Call(Some(Box::new(lhs.clone())), name.clone(), vec!(arg1.clone())).to_gd(), "lhs.func(1)");
    assert_eq!(Expr::Call(Some(Box::new(lhs.clone())), name.clone(), vec!(arg1.clone(), arg2.clone())).to_gd(), "lhs.func(1, 2)");
    assert_eq!(Expr::Call(Some(Box::new(lhs.clone())), name.clone(), vec!(arg1.clone(), arg2.clone(), arg3.clone())).to_gd(), "lhs.func(1, 2, 3)");

    assert_eq!(Expr::Call(Some(Box::new(lhs_p.clone())), name.clone(), vec!()).to_gd(), "(1 + 2).func()");
    assert_eq!(Expr::Call(Some(Box::new(lhs_p.clone())), name.clone(), vec!(arg1.clone())).to_gd(), "(1 + 2).func(1)");
    assert_eq!(Expr::Call(Some(Box::new(lhs_p.clone())), name.clone(), vec!(arg1.clone(), arg2.clone())).to_gd(), "(1 + 2).func(1, 2)");
    assert_eq!(Expr::Call(Some(Box::new(lhs_p.clone())), name.clone(), vec!(arg1.clone(), arg2.clone(), arg3.clone())).to_gd(), "(1 + 2).func(1, 2, 3)");

    assert_eq!(Expr::SuperCall(name.clone(), vec!()).to_gd(), ".func()");

  }

  fn unary(op: UnaryOp, expr: &Expr) -> Expr {
    Expr::Unary(op, Box::new(expr.clone()))
  }

  fn binary(a: &Expr, op: BinaryOp, b: &Expr) -> Expr {
    Expr::Binary(Box::new(a.clone()), op, Box::new(b.clone()))
  }

  #[test]
  fn unary_ops() {
    let operand = Expr::from(3);
    assert_eq!(unary(UnaryOp::BitNot, &operand).to_gd(), "~3");
    assert_eq!(unary(UnaryOp::Negate, &operand).to_gd(), "-3");
    assert_eq!(unary(UnaryOp::Not, &operand).to_gd(), "!3");
  }

  #[test]
  fn binary_ops() {
    let a = Expr::Var(String::from("a"));
    let b = Expr::Var(String::from("b"));
    assert_eq!(binary(&a, BinaryOp::Times, &b).to_gd(), "a * b");
    assert_eq!(binary(&a, BinaryOp::Div, &b).to_gd(), "a / b");
    assert_eq!(binary(&a, BinaryOp::Mod, &b).to_gd(), "a % b");
    assert_eq!(binary(&a, BinaryOp::Add, &b).to_gd(), "a + b");
    assert_eq!(binary(&a, BinaryOp::Sub, &b).to_gd(), "a - b");
    assert_eq!(binary(&a, BinaryOp::LShift, &b).to_gd(), "a << b");
    assert_eq!(binary(&a, BinaryOp::RShift, &b).to_gd(), "a >> b");
    assert_eq!(binary(&a, BinaryOp::BitAnd, &b).to_gd(), "a & b");
    assert_eq!(binary(&a, BinaryOp::BitXor, &b).to_gd(), "a ^ b");
    assert_eq!(binary(&a, BinaryOp::BitOr, &b).to_gd(), "a | b");
    assert_eq!(binary(&a, BinaryOp::LT, &b).to_gd(), "a < b");
    assert_eq!(binary(&a, BinaryOp::GT, &b).to_gd(), "a > b");
    assert_eq!(binary(&a, BinaryOp::Eq, &b).to_gd(), "a == b");
    assert_eq!(binary(&a, BinaryOp::LE, &b).to_gd(), "a <= b");
    assert_eq!(binary(&a, BinaryOp::GE, &b).to_gd(), "a >= b");
    assert_eq!(binary(&a, BinaryOp::NE, &b).to_gd(), "a != b");
    assert_eq!(binary(&a, BinaryOp::Is, &b).to_gd(), "a is b");
    assert_eq!(binary(&a, BinaryOp::In, &b).to_gd(), "a in b");
    assert_eq!(binary(&a, BinaryOp::And, &b).to_gd(), "a && b");
    assert_eq!(binary(&a, BinaryOp::Or, &b).to_gd(), "a || b");
    assert_eq!(binary(&a, BinaryOp::Cast, &b).to_gd(), "a as b");
  }

  #[test]
  fn ternary_op() {
    let a = Box::new(Expr::from(1));
    let b = Box::new(Expr::from(2));
    let c = Box::new(Expr::from(3));
    assert_eq!(Expr::TernaryIf(TernaryIf { true_case: a, cond: b, false_case: c }).to_gd(), "1 if 2 else 3");
  }

  #[test]
  fn operator_precedence() {
    let a = Expr::Var(String::from("a"));
    let b = Expr::Var(String::from("b"));
    let c = Expr::Var(String::from("c"));

    let noparens = binary(&binary(&a, BinaryOp::Times, &b), BinaryOp::Add, &c);
    let needparens = binary(&binary(&a, BinaryOp::Add, &b), BinaryOp::Times, &c);
    assert_eq!(noparens.to_gd(), "a * b + c");
    assert_eq!(needparens.to_gd(), "(a + b) * c");

  }

  #[test]
  fn ternary_if_precedence_1() {
    let a = Box::new(Expr::from(1));
    let b = Box::new(Expr::from(2));
    let c = Box::new(Expr::from(3));
    let d = Box::new(Expr::from(4));
    let e = Box::new(Expr::from(5));

    let left_assoc = Expr::from(TernaryIf {
      true_case: Box::new(Expr::from(TernaryIf { true_case: a.clone(), cond: b.clone(), false_case: c.clone() })),
      cond: d.clone(),
      false_case: e.clone(),
    });
    let right_assoc = Expr::from(TernaryIf {
      true_case: a.clone(),
      cond: b.clone(),
      false_case: Box::new(Expr::from(TernaryIf { true_case: c.clone(), cond: d.clone(), false_case: e.clone() })),
    });
    assert_eq!(left_assoc.to_gd(), "(1 if 2 else 3) if 4 else 5");
    assert_eq!(right_assoc.to_gd(), "1 if 2 else 3 if 4 else 5");
  }

  #[test]
  fn ternary_if_precedence_2() {
    let a = Expr::Var(String::from("a"));
    let b = Expr::Var(String::from("b"));
    let c = Expr::Var(String::from("c"));
    let d = Expr::Var(String::from("d"));

    let case1 = Expr::from(TernaryIf {
      true_case: Box::new(binary(&a, BinaryOp::Or, &b)),
      cond: Box::new(c.clone()),
      false_case: Box::new(d.clone()),
    });
    assert_eq!(case1.to_gd(), "a || b if c else d");

    let case2 = Expr::from(TernaryIf {
      true_case: Box::new(a.clone()),
      cond: Box::new(binary(&b, BinaryOp::Or, &c)),
      false_case: Box::new(d.clone()),
    });
    assert_eq!(case2.to_gd(), "a if b || c else d");

    let case3 = Expr::from(TernaryIf {
      true_case: Box::new(a.clone()),
      cond: Box::new(b.clone()),
      false_case: Box::new(binary(&c, BinaryOp::Or, &d)),
    });
    assert_eq!(case3.to_gd(), "a if b else c || d");

    let case4 = Expr::from(TernaryIf {
      true_case: Box::new(binary(&a, BinaryOp::Cast, &b)),
      cond: Box::new(c.clone()),
      false_case: Box::new(d.clone()),
    });
    assert_eq!(case4.to_gd(), "(a as b) if c else d");

    let case5 = Expr::from(TernaryIf {
      true_case: Box::new(a.clone()),
      cond: Box::new(binary(&b, BinaryOp::Cast, &c)),
      false_case: Box::new(d.clone()),
    });
    assert_eq!(case5.to_gd(), "a if b as c else d");

    let case6 = Expr::from(TernaryIf {
      true_case: Box::new(a.clone()),
      cond: Box::new(b.clone()),
      false_case: Box::new(binary(&c, BinaryOp::Cast, &d)),
    });
    assert_eq!(case6.to_gd(), "a if b else (c as d)");

  }

  #[test]
  fn arrays() {
    assert_eq!(Expr::ArrayLit(vec!()).to_gd(), "[]");
    assert_eq!(Expr::ArrayLit(vec!(Expr::from(1))).to_gd(), "[1]");
    assert_eq!(Expr::ArrayLit(vec!(Expr::from(1), Expr::from(2))).to_gd(), "[1, 2]");
    assert_eq!(Expr::ArrayLit(vec!(Expr::from(1), Expr::from(2), Expr::from(3))).to_gd(), "[1, 2, 3]");
  }

  #[test]
  fn dictionaries() {
    assert_eq!(Expr::DictionaryLit(vec!()).to_gd(), "{}");
    assert_eq!(Expr::DictionaryLit(vec!((Expr::from(1), Expr::from(2)))).to_gd(), "{1: 2}");
    assert_eq!(Expr::DictionaryLit(vec!((Expr::from(1), Expr::from(2)), (Expr::from(3), Expr::from(4)))).to_gd(), "{1: 2, 3: 4}");
    assert_eq!(Expr::DictionaryLit(vec!((Expr::from(1), Expr::from(2)), (Expr::from(3), Expr::from(4)), (Expr::from(5), Expr::from(6)))).to_gd(), "{1: 2, 3: 4, 5: 6}");
  }

}
