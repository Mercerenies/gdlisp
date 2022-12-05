
use crate::ir::expr::{Expr, ExprF, AssignTarget};
use crate::pipeline::source::SourceOffset;

// This enum represents the different ways a (set ...) assignment form
// can expand.
#[derive(Clone, Debug)]
pub enum AssignmentForm {
  Simple(AssignTarget),
  SetterCall(String, Vec<Expr>),
}

impl AssignmentForm {

  pub fn str_to_setter_prefix(name: &str) -> String {
    format!("set-{}", name)
  }

  pub fn into_expr(self, rhs: Expr, pos: SourceOffset) -> Expr {
    match self {
      AssignmentForm::Simple(target) => {
        Expr::new(ExprF::Assign(target, Box::new(rhs)), pos)
      }
      AssignmentForm::SetterCall(f, mut args) => {
        args.insert(0, rhs);
        Expr::call(f, args, pos)
      }
    }
  }

}
