
//! Provides pretty-printing facilities for
//! [`AST`](crate::sxp::ast::AST) nodes.

use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::library;
use crate::compile::names::generator::NameGenerator;
use crate::compile::stmt_wrapper::StmtWrapper;
use crate::pipeline::source::{Sourced, SourceOffset};
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::literal::Literal;

/// Reifies the expression, as though through
/// [`Reify::reify`](super::Reify::reify). However, whereas
/// `Reify::reify` will generate a single expression, this function
/// will split the reification into several lines, using local
/// variables to break the code up as needed.
///
/// `value` is the value to be reified. `max_depth` is the maximum
/// depth of a single expression. After recursing deeper than this, a
/// local variable will be constructed in GDScript to store the
/// intermediate. A `max_depth` of zero will create a local variable
/// at every recursion step. `gen` is used to generate the names for
/// the reification helper variables.
///
/// This function returns a vector of intermediate variable
/// declaration statements, as well as the final expression. The
/// expression should be used in GDScript only after all of the
/// statements have been run. If you know how the expression will be
/// used, then [`reify_pretty`] may be more convenient.
pub fn reify_pretty_expr(value: &AST, max_depth: u32, gen: &mut impl NameGenerator) -> (Vec<Stmt>, Expr) {
  let mut printer = PrettyPrinter::new(gen, max_depth);
  let expr = printer.reify_pretty_rec(value, 0);
  (printer.build(), expr)
}

/// As [`reify_pretty_expr`], but the final expression is wrapped
/// using the statement wrapper `wrapper` and appended to the
/// statements vector.
pub fn reify_pretty<W>(value: &AST, max_depth: u32, gen: &mut impl NameGenerator, wrapper: &W) -> Vec<Stmt>
where W : StmtWrapper + ?Sized {
  let (mut stmts, expr) = reify_pretty_expr(value, max_depth, gen);
  stmts.push(wrapper.wrap_expr(expr));
  stmts
}

struct PrettyPrinter<'a, G: NameGenerator> {
  gen: &'a mut G,
  builder: Vec<Stmt>,
  max_depth: u32,
}

impl<'a, G: NameGenerator> PrettyPrinter<'a, G> {

  fn new(gen: &'a mut G, max_depth: u32) -> Self {
    PrettyPrinter { gen, max_depth, builder: Vec::new() }
  }

  fn reify_pretty_rec(&mut self, value: &AST, depth: u32) -> Expr {
    if depth > self.max_depth {
      // Make a local variable and store the result in it.
      let inner = self.reify_pretty_rec(value, 0);
      let name = self.gen.generate_with("_reify");
      self.builder.push(Stmt::var_decl(name.clone(), inner, value.get_source()));
      Expr::var(&name, value.get_source())
    } else {
      match &value.value {
        ASTF::Atom(lit) => {
          reify_literal(lit, value.pos)
        }
        ASTF::Cons(a, b) => {
          let a = self.reify_pretty_rec(a, depth + 1);
          let b = self.reify_pretty_rec(b, depth + 1);
          Expr::call(Some(library::gdlisp_root(value.pos)), "cons", vec!(a, b), value.pos)
        }
        ASTF::Array(v) => {
          let v = v.iter().map(|x| self.reify_pretty_rec(x, depth + 1)).collect();
          Expr::new(ExprF::ArrayLit(v), value.pos)
        }
        ASTF::Dictionary(v) => {
          let v = v.iter().map(|(k, v)| (self.reify_pretty_rec(k, depth + 1), self.reify_pretty_rec(v, depth + 1))).collect();
          Expr::new(ExprF::DictionaryLit(v), value.pos)
        }
      }
    }
  }

  fn build(self) -> Vec<Stmt> {
    self.builder
  }

}

fn reify_literal(value: &Literal, pos: SourceOffset) -> Expr {
  match value {
    Literal::Nil => {
      Expr::null(pos)
    }
    Literal::Int(n) => {
      Expr::from_value(*n, pos)
    }
    Literal::Bool(b) => {
      Expr::from_value(*b, pos)
    }
    Literal::Float(f) => {
      Expr::from_value(*f, pos)
    }
    Literal::String(s) => {
      Expr::from_value(s.to_owned(), pos)
    }
    Literal::Symbol(s) => {
      let s = Expr::from_value(s.to_owned(), pos);
      Expr::call(Some(library::gdlisp_root(pos)), "intern", vec!(s), pos)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;
  use crate::compile::names::fresh::FreshNameGenerator;

  fn cons(a: AST, b: AST) -> AST {
    AST::new(ASTF::cons(a, b), SourceOffset::default())
  }

  fn int(n: i32) -> AST {
    AST::new(ASTF::int(n), SourceOffset::default())
  }

  #[test]
  fn reify_pretty_test_1() {
    let (stmts, expr) = reify_pretty_expr(&cons(int(1), int(2)), u32::MAX, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert!(stmts.is_empty());
    assert_eq!(expr.to_gd(), "GDLisp.cons(1, 2)");
  }

  #[test]
  fn reify_pretty_test_2() {
    let (stmts, expr) = reify_pretty_expr(&cons(cons(cons(cons(int(1), int(2)), int(3)), int(4)), int(5)), u32::MAX, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert!(stmts.is_empty());
    assert_eq!(expr.to_gd(), "GDLisp.cons(GDLisp.cons(GDLisp.cons(GDLisp.cons(1, 2), 3), 4), 5)");
  }

  #[test]
  fn reify_pretty_test_3() {
    let ast = cons(cons(cons(cons(int(1), int(2)), int(3)), int(4)), cons(int(5), int(6)));
    let (stmts, expr) = reify_pretty_expr(&ast, u32::MAX, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert!(stmts.is_empty());
    assert_eq!(expr.to_gd(), "GDLisp.cons(GDLisp.cons(GDLisp.cons(GDLisp.cons(1, 2), 3), 4), GDLisp.cons(5, 6))");
  }

  #[test]
  fn reify_pretty_test_4() {
    let ast = cons(cons(cons(cons(int(1), int(2)), int(3)), int(4)), cons(int(5), int(6)));
    let (stmts, expr) = reify_pretty_expr(&ast, 3, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert_eq!(stmts, vec!("var _reify_0 = 1\n", "var _reify_1 = 2\n"));
    assert_eq!(expr.to_gd(), "GDLisp.cons(GDLisp.cons(GDLisp.cons(GDLisp.cons(_reify_0, _reify_1), 3), 4), GDLisp.cons(5, 6))");
  }

  #[test]
  fn reify_pretty_test_5() {
    let ast = cons(cons(cons(cons(int(1), int(2)), int(3)), int(4)), cons(int(5), int(6)));
    let (stmts, expr) = reify_pretty_expr(&ast, 2, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert_eq!(stmts, vec!("var _reify_0 = GDLisp.cons(1, 2)\n", "var _reify_1 = 3\n"));
    assert_eq!(expr.to_gd(), "GDLisp.cons(GDLisp.cons(GDLisp.cons(_reify_0, _reify_1), 4), GDLisp.cons(5, 6))");
  }

  #[test]
  fn reify_pretty_test_6() {
    let ast = cons(cons(cons(cons(int(1), int(2)), int(3)), int(4)), cons(int(5), int(6)));
    let (stmts, expr) = reify_pretty_expr(&ast, 1, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert_eq!(stmts, vec!(
      "var _reify_0 = 1\n",
      "var _reify_1 = 2\n",
      "var _reify_2 = GDLisp.cons(GDLisp.cons(_reify_0, _reify_1), 3)\n",
      "var _reify_3 = 4\n",
      "var _reify_4 = 5\n",
      "var _reify_5 = 6\n",
    ));
    assert_eq!(expr.to_gd(), "GDLisp.cons(GDLisp.cons(_reify_2, _reify_3), GDLisp.cons(_reify_4, _reify_5))");
  }

  #[test]
  fn reify_pretty_test_7() {
    let ast = cons(cons(cons(cons(int(1), int(2)), int(3)), int(4)), cons(int(5), int(6)));
    let (stmts, expr) = reify_pretty_expr(&ast, 0, &mut FreshNameGenerator::new(vec!()));
    let stmts: Vec<_> = stmts.into_iter().map(|s| s.to_gd(0)).collect();
    assert_eq!(stmts, vec!(
      "var _reify_0 = 1\n",
      "var _reify_1 = 2\n",
      "var _reify_2 = GDLisp.cons(_reify_0, _reify_1)\n",
      "var _reify_3 = 3\n",
      "var _reify_4 = GDLisp.cons(_reify_2, _reify_3)\n",
      "var _reify_5 = 4\n",
      "var _reify_6 = GDLisp.cons(_reify_4, _reify_5)\n",
      "var _reify_7 = 5\n",
      "var _reify_8 = 6\n",
      "var _reify_9 = GDLisp.cons(_reify_7, _reify_8)\n",
    ));
    assert_eq!(expr.to_gd(), "GDLisp.cons(_reify_6, _reify_9)");
  }

}
