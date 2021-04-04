
use crate::gdscript::expr::Expr;
use super::Pipeline;

// Trait for things that have a reasonable implementation of "load the
// current file" as an expression. The typical example is Pipeline,
// and this trait is mainly provided as a way for the type checker to
// say "I only need the load expression" as opposed to requiring the
// whole Pipeline in general.
pub trait CanLoad {
  fn load_expr(&self) -> Option<Expr>;
}

impl CanLoad for Pipeline {
  fn load_expr(&self) -> Option<Expr> {
    let mut filename = self.currently_loading_file()?.to_owned();
    filename.path_mut().set_extension("gd");
    Some(Expr::Call(None, String::from("load"), vec!(Expr::from(filename.to_string()))))
  }
}
