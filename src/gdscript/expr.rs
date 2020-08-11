
use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};
use crate::gdscript::literal;

pub const PRECEDENCE_LOWEST: i32 = -99;
pub const PRECEDENCE_SUBSCRIPT: i32 = 21;
pub const PRECEDENCE_ATTRIBUTE: i32 = 20;
pub const PRECEDENCE_CALL: i32 = 19;

// Note: For names (such as strings), we expect that they've already
// been sanitized for GDScript output. That should've happened earlier
// in the compilation process.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  Var(String),
  Literal(literal::Literal),
  Subscript(Box<Expr>, Box<Expr>),
  Attribute(Box<Expr>, String),
  Call(Option<Box<Expr>>, String, Vec<Expr>),
  SuperCall(String, Vec<Expr>),
  Unary(UnaryOp, Box<Expr>),
  Binary(Box<Expr>, BinaryOp, Box<Expr>),
}

fn maybe_parens(cond: bool, inner: String) -> String {
  if cond {
    format!("({})", inner)
  } else {
    inner
  }
}

impl Expr {

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
    }
  }

  pub fn to_gd(&self) -> String {
    self.to_gd_prec(PRECEDENCE_LOWEST)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::literal::Literal;

  #[test]
  fn basic_expr_types() {
    let var = Expr::Var(String::from("foobar"));
    let n = Expr::Literal(Literal::Int(99));
    let name = String::from("attr");

    let arg1 = Expr::Literal(Literal::Int(1));
    let arg2 = Expr::Literal(Literal::Int(2));
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
    let arg1 = Expr::Literal(Literal::Int(1));
    let arg2 = Expr::Literal(Literal::Int(2));
    let arg3 = Expr::Literal(Literal::Int(3));

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
    let operand = Expr::Literal(Literal::Int(3));
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
  fn operator_precedence() {
    let a = Expr::Var(String::from("a"));
    let b = Expr::Var(String::from("b"));
    let c = Expr::Var(String::from("c"));

    let noparens = binary(&binary(&a, BinaryOp::Times, &b), BinaryOp::Add, &c);
    let needparens = binary(&binary(&a, BinaryOp::Add, &b), BinaryOp::Times, &c);
    assert_eq!(noparens.to_gd(), "a * b + c");
    assert_eq!(needparens.to_gd(), "(a + b) * c");

  }

}
