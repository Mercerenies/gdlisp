
//! Exposes the [`SuperProxy`] type, for construction of instance
//! methods that delegate to a superclass method.

use crate::pipeline::source::SourceOffset;
use crate::gdscript::decl::FnDecl;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::names::generator::NameGenerator;
use crate::compile::special_form::lambda::simple_arg_names;

/// A supermethod proxy is a method on the current class that, when
/// invoked, simply delegates to a call of a given method on a
/// superclass.
#[derive(Default, Clone, Debug)]
pub struct SuperProxy {
  /// The name of the proxy method.
  pub name: String,
  /// The name of the superclass method to delegate to.
  pub super_name: String,
  /// The total number of arguments to be passed to the superclass
  /// method.
  pub args: usize,
  /// The position in the code where the construction of this proxy
  /// became necessary.
  pub pos: SourceOffset,
}

impl SuperProxy {

  /// The prefix used to generate names for superclass proxy methods.
  pub const PROXY_NAME: &'static str = "__gdlisp_super";

  /// Generates a superclass proxy method for the method with the
  /// given name and argument count. The provided name generator is
  /// used to come up with a unique name for the proxy method.
  pub fn generate(gen: &mut FreshNameGenerator, super_name: String, args: usize, pos: SourceOffset) -> SuperProxy {
    let name = gen.generate_with(SuperProxy::PROXY_NAME);
    SuperProxy { name, super_name, args, pos }
  }

}

impl From<SuperProxy> for FnDecl {

  fn from(proxy: SuperProxy) -> FnDecl {

    let proxy_params = simple_arg_names(proxy.args);
    let call_args: Vec<_> = proxy_params.all_args_iter().map(|name| Expr::var(name, proxy.pos)).collect();

    let body = vec!(
      Stmt::return_stmt(Expr::super_call(&proxy.super_name, call_args, proxy.pos), proxy.pos),
    );
    FnDecl {
      name: proxy.name,
      args: proxy_params,
      body: body,
    }
  }

}
