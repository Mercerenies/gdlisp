
//! Exposes the [`ClassScope`] trait and its various common
//! implementations.

// TODO Not a fan of all of the `dyn ClassScope` in this file. Can we
// remove at least *some* of the indirection?

use crate::compile::error::{Error as GDError, ErrorF as GDErrorF};
use crate::compile::symbol_table::local_var::LocalVar;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::gdscript::expr::Expr;
use crate::pipeline::source::SourceOffset;
use super::super_proxy::SuperProxy;

/// A class scope for situations where we're not inside a class. Any
/// attempt to call a superclass method in this scope will fail.
#[derive(Clone, Debug)]
pub struct OutsideOfClass;

/// A simple wrapper around a [`ClassScope`] which delegates to the
/// value it's borrowing.
pub struct ClassScopeMut<'a>(pub &'a mut dyn ClassScope);

/// A class scope for situations where we're *directly* inside of a
/// class, i.e. we're inside of a class and there's no closure
/// strictly between the current position and the class declaration in
/// the scope hierarchy.
#[derive(Clone, Debug, Default)]
pub struct DirectClassScope {
  super_proxies: Vec<SuperProxy>,
}

/// A class scope within a closure that is transitively contained
/// inside a class. This maintains a mutable reference to a
/// [`DirectClassScope`] for the purposes of accessing a common
/// supermethod proxy list with other closures of the same class.
pub struct ClosedClassScope<'a>(pub &'a mut DirectClassScope);

/// A `ClassScope` implementor keeps track of which class (if any)
/// we're currently compiling inside. This is used to track what
/// should be called when we invoke a `super` method.
pub trait ClassScope {

  /// Compile the necessary code to make a supermethod call, and then
  /// return an expression which will perform the call.
  fn super_call(&mut self,
                gen: &mut FreshNameGenerator,
                self_binding: &LocalVar,
                super_name: String,
                args: Vec<Expr>,
                pos: SourceOffset)
                -> Result<Expr, GDError>;

  /// Returns a new `ClassScope` which will handle supermethod calls
  /// inside of a closure within the current scope.
  fn closure_mut(&mut self) -> Box<dyn ClassScope + '_>;

}

impl DirectClassScope {

  /// A new, empty `DirectClassScope`. Equivalent to
  /// `DirectClassScope::default()`.
  pub fn new() -> DirectClassScope {
    DirectClassScope::default()
  }

  /// Returns a vector of superclass proxy methods, in an unspecified
  /// order.
  pub fn into_proxies(self) -> Vec<SuperProxy> {
    self.super_proxies
  }

  /// Adds a new supermethod proxy to the current scope.
  pub fn add_proxy(&mut self, proxy: SuperProxy) {
    self.super_proxies.push(proxy);
  }

}

impl ClassScope for OutsideOfClass {

  fn super_call(&mut self,
                _gen: &mut FreshNameGenerator,
                _self_binding: &LocalVar,
                super_name: String,
                _args: Vec<Expr>,
                pos: SourceOffset)
                -> Result<Expr, GDError> {
    // Always fail.
    Err(GDError::new(GDErrorF::BadSuperCall(super_name), pos))
  }

  fn closure_mut(&mut self) -> Box<dyn ClassScope + '_> {
    Box::new(ClassScopeMut(self))
  }

}

impl<'a> ClassScope for ClassScopeMut<'a> {

  fn super_call(&mut self,
                gen: &mut FreshNameGenerator,
                self_binding: &LocalVar,
                super_name: String,
                args: Vec<Expr>,
                pos: SourceOffset)
                -> Result<Expr, GDError> {
    // Delegate to inner
    self.0.super_call(gen, self_binding, super_name, args, pos)
  }

  fn closure_mut(&mut self) -> Box<dyn ClassScope + '_> {
    // *shrug* Just another delegator. Kind of silly but gets the job
    // done.
    Box::new(ClassScopeMut(self))
  }

}

impl ClassScope for DirectClassScope {

  fn super_call(&mut self,
                _gen: &mut FreshNameGenerator,
                _self_binding: &LocalVar,
                super_name: String,
                args: Vec<Expr>,
                pos: SourceOffset)
                -> Result<Expr, GDError> {
    Ok(Expr::super_call(&super_name, args, pos))
  }

  fn closure_mut(&mut self) -> Box<dyn ClassScope + '_> {
    // Create a closure scope with access to self.
    Box::new(ClosedClassScope(self))
  }

}

impl<'a> ClassScope for ClosedClassScope<'a> {

  fn super_call(&mut self,
                gen: &mut FreshNameGenerator,
                self_binding: &LocalVar,
                super_name: String,
                args: Vec<Expr>,
                pos: SourceOffset)
                -> Result<Expr, GDError> {
    let proxy = SuperProxy::generate(gen, super_name, args.len(), pos);
    let method_name = proxy.name.clone();

    self.0.add_proxy(proxy);

    Ok(Expr::call(Some(self_binding.expr(pos)), &method_name, args, pos))
  }

  fn closure_mut(&mut self) -> Box<dyn ClassScope + '_> {
    // Delegate to self; having two nested closures is no different
    // than having one closure for these purposes.
    Box::new(ClassScopeMut(self))
  }

}
