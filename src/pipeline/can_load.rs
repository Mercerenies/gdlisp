
use crate::gdscript::expr::Expr;
use crate::runner::path::RPathBuf;
use super::Pipeline;

// Trait for things that have a reasonable implementation of "load the
// current file" as an expression. The typical example is Pipeline,
// and this trait is mainly provided as a way for the type checker to
// say "I only need the load expression" as opposed to requiring the
// whole Pipeline in general.
pub trait CanLoad {
  fn current_filename(&self) -> Option<RPathBuf>;
  #[deprecated(note="Use current_filename and VarName::CurrentFile directly")]
  fn load_expr(&self) -> Option<Expr> {
    let filename = self.current_filename()?;
    Some(Expr::Call(None, String::from("load"), vec!(Expr::from(filename.to_string()))))
  }
}

impl CanLoad for Pipeline {
  fn current_filename(&self) -> Option<RPathBuf> {
    let mut filename = self.currently_loading_file()?.to_owned();
    filename.path_mut().set_extension("gd");
    Some(filename)
  }
}
