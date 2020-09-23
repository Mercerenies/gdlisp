
use crate::gdscript::expr::Expr;
use crate::gdscript::library;

// A builtin is like a special form, but it's guaranteed to evaluate
// its arguments in applicative order like a normal function. As such,
// it doesn't need access to its argument list and is simply a
// translation from the name to a callable (usually something in
// GDLisp.gd).

// TODO One minor improvement would be to check argument count at
// compile-time.
pub fn translate_builtin(head: &str) -> Option<(Option<Box<Expr>>, String)> {
  match head {
    "cons" => Some((Some(Box::new(library::cons_class())), String::from("new"))),
    _ => None
  }
}
