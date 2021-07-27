
//! Symbol tables store names and keep track of what variable or
//! function they reference.
//!
//! See the [`SymbolTable`] structure for more. For anything in the
//! variable namespace, we use [`local_var::LocalVar`] to keep track
//! of the value. For anything in the function namespace, we use
//! [`function_call::FnCall`] to keep track of its value, and we use
//! [`call_magic::CallMagic`] for special call semantics on functions.

pub mod function_call;
pub mod call_magic;
pub mod local_var;

use function_call::FnCall;
use call_magic::{CallMagic, DefaultCall};
use local_var::{LocalVar, ValueHint, ValueHintsTable};
use crate::util::debug_wrapper::DebugWrapper;

use std::collections::HashMap;
use std::borrow::Borrow;

/// GDLisp has two separate namespaces when it comes to name
/// resolution: the variable namespace and the function namespace.
/// Types, such as classes and primitive types, fall into the variable
/// namespace. `SymbolTable` encompasses a table of names available in
/// the current scope, in either namespace.
#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  locals: HashMap<String, LocalVar>,
  reverse_locals: HashMap<String, String>, // key: GDScript name, value: GDLisp name (to use in locals)
  functions: HashMap<String, (FnCall, DebugWrapper<Box<dyn CallMagic + 'static>>)>,
}

/// When we move into a class scope, we need to keep two symbol
/// tables: one for use in static contexts and one for use in instance
/// (non-static) contexts. This struct encapsulates that concept.
pub struct ClassTablePair<'a, 'b> {
  /// The table for use in static context.
  pub static_table: &'a mut SymbolTable,
  /// The table for use in instance (non-static) context.
  pub instance_table: &'b mut SymbolTable,
}

impl SymbolTable {

  /// A new, empty symbol table. Equivalent to
  /// `SymbolTable::default()`.
  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  /// Gets the variable with the given GDLisp name, or `None` if no such
  /// variable exists in the table.
  pub fn get_var(&self, name: &str) -> Option<&LocalVar> {
    self.locals.get(name)
  }

  /// Gets the variable with the given local GDScript name. Variables
  /// are indexed by both names, so this access is as efficient as
  /// [`SymbolTable::get_var`].
  pub fn get_var_by_gd_name(&self, gd_name: &str) -> Option<&LocalVar> {
    self.reverse_locals.get(gd_name).and_then(|name| self.locals.get(name))
  }

  /// Sets the variable with the given GDLisp name, returning the old
  /// value if one existed.
  pub fn set_var(&mut self, name: String, value: LocalVar) -> Option<LocalVar> {
    if let Some(gd_name) = value.simple_name() {
      self.reverse_locals.insert(gd_name.to_owned(), name.clone());
    }
    self.locals.insert(name, value)
  }

  /// Removes the variable with the given GDLisp name. If no such
  /// variable exists, then nothing is changed.
  pub fn del_var(&mut self, name: &str) {
    let value = self.locals.remove(name);
    if let Some(value) = value {
      if let Some(gd_name) = value.simple_name() {
        self.reverse_locals.remove(gd_name);
      }
    }
  }

  /// Gets the function call and call magic associated with the given
  /// GDLisp function name.
  pub fn get_fn(&self, name: &str) -> Option<(&FnCall, &(dyn CallMagic + 'static))> {
    self.functions.get(name).map(|(call, magic)| {
      (call, &*magic.0)
    })
  }

  /// Sets the function call and call magic associated with the given
  /// function name.
  pub fn set_fn(&mut self, name: String, value: FnCall, magic: Box<dyn CallMagic + 'static>) {
    self.functions.insert(name, (value, DebugWrapper(magic)));
  }

  /// Deletes any function call info and call magic associated with
  /// the given function name.
  pub fn del_fn(&mut self, name: &str) {
    self.functions.remove(name);
  }

  /// An iterator over the variable namespace of this table.
  pub fn vars(&self) -> impl Iterator<Item=(&str, &LocalVar)> {
    return self.locals.iter().map(|x| (x.0.borrow(), x.1));
  }

  /// An iterator over the function namespace of this table.
  pub fn fns(&self) -> impl Iterator<Item=(&str, &FnCall, &(dyn CallMagic + 'static))> {
    self.functions.iter().map(|(name, value)| {
      (name.borrow(), &value.0, &*value.1.0)
    })
  }

  /// An iterator over the function namespace of this table, providing
  /// mutable access to the function call information and the call
  /// magic.
  pub fn fns_mut(&mut self) -> impl Iterator<Item=(&str, &mut FnCall, &mut (dyn CallMagic + 'static))> {
    self.functions.iter_mut().map(|(name, value)| {
      (name.borrow(), &mut value.0, &mut *value.1.0)
    })
  }

  /// Iterates over both namespaces of `other` and sets the variable
  /// and function names in `self` to the values from `other`. If
  /// values already existed in `self` for some name, then they are
  /// overwritten.
  pub fn assign_from(&mut self, other: &SymbolTable) {
    for (name, value) in other.vars() {
      self.set_var(name.to_owned(), value.to_owned());
    }
    for (name, call, magic) in other.fns() {
      self.set_fn(name.to_owned(), call.to_owned(), dyn_clone::clone_box(magic));
    }
  }

}

/// Trait for objects which have a symbol table. Currently, there is
/// only one implementor of this trait: [`SymbolTable`] itself. This
/// is largely a holdover from an older version of the codebase, when
/// [`Compiler`](super::Compiler) directly stored a `SymbolTable` and
/// needed mutable access to it. Nowadays, it makes more sense to
/// simply access the table directly. This trait may get removed at
/// some point in the future and its methods merged into `impl
/// SymbolTable` directly.
pub trait HasSymbolTable {

  /// Borrows the symbol table from `self`.
  fn get_symbol_table(&self) -> &SymbolTable;

  /// Borrows the symbol table mutably from `self`.
  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable;

  /// Binds the local variable, calls `block` with `self` as argument,
  /// then binds the variable back to its previous value. This is the
  /// correct semantic behavior for introducing a local variable into
  /// some inner scope in GDScript.
  fn with_local_var<B, F>(&mut self,
                          name: String,
                          value: LocalVar,
                          block: F) -> B
  where F : FnOnce(&mut Self) -> B {
    let previous = self.get_symbol_table_mut().set_var(name.clone(), value);
    let result = block(self);
    if let Some(previous) = previous {
      self.get_symbol_table_mut().set_var(name, previous);
    } else {
      self.get_symbol_table_mut().del_var(&name);
    };
    result
  }

  /// Recursive convenience helper for calling
  /// [`HasSymbolTable::with_local_var`] on several variable names in
  /// sequence.
  fn with_local_vars<B>(&mut self,
                        vars: &mut dyn Iterator<Item=(String, LocalVar)>,
                        block: impl FnOnce(&mut Self) -> B) -> B {
    if let Some((name, value)) = vars.next() {
      self.with_local_var(name, value, |curr| {
        curr.with_local_vars(vars, block)
      })
    } else {
      block(self)
    }
  }

  /// Binds the local function, calls `block` with `self` as argument,
  /// then binds the function back to its previous value. This is the
  /// correct semantic behavior for introducing a local function into
  /// some inner scope in GDScript.
  fn with_local_fn<B>(&mut self,
                      name: String,
                      value: FnCall,
                      block: impl FnOnce(&mut Self) -> B) -> B {
    let previous = self.get_symbol_table_mut().get_fn(&name).map(|(p, m)| (p.clone(), dyn_clone::clone_box(m)));
    self.get_symbol_table_mut().set_fn(name.clone(), value, Box::new(DefaultCall));
    let result = block(self);
    if let Some((p, m)) = previous {
      self.get_symbol_table_mut().set_fn(name, p, m);
    } else {
      self.get_symbol_table_mut().del_fn(&name);
    };
    result
  }

  /// Recursive convenience helper for calling
  /// [`HasSymbolTable::with_local_fn`] on several function names in
  /// sequence.
  fn with_local_fns<B>(&mut self,
                       vars: &mut dyn Iterator<Item=(String, FnCall)>,
                       block: impl FnOnce(&mut Self) -> B) -> B {
    if let Some((name, value)) = vars.next() {
      self.with_local_fn(name, value, |curr| {
        curr.with_local_fns(vars, block)
      })
    } else {
      block(self)
    }
  }

}

impl HasSymbolTable for SymbolTable {

  fn get_symbol_table(&self) -> &SymbolTable {
    self
  }

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable {
    self
  }

}

// TODO This is a mess. Can we please store the value hints some way
// that doesn't require a reverse lookup on GDScript names, because
// that reverse lookup is just going to be awkward no matter what.
impl ValueHintsTable for SymbolTable {
  fn get_value_hint(&self, name: &str) -> Option<&ValueHint> {
    self.get_var_by_gd_name(name).and_then(|var| var.value_hint.as_ref())
  }
}

impl<'a> ClassTablePair<'a, 'a> {

  /// Returns either `self.static_table` or `self.instance_table`,
  /// depending on the type of scope we're in.
  pub fn into_table(self, is_static: bool) -> &'a mut SymbolTable {
    if is_static {
      self.static_table
    } else {
      self.instance_table
    }
  }

}

// TODO Test magic here as well.

#[cfg(test)]
mod tests {
  use super::*;
  use local_var::VarName;
  use function_call::{FnSpecs, FnScope};

  fn from_var_name(e: &VarName) -> &str {
    e.simple_name().expect("Unexpected nontrivial variable name")
  }

  #[test]
  fn test_vars() {
    let mut table = SymbolTable::new();
    assert_eq!(table.get_var("foo"), None);
    assert_eq!(table.set_var("foo".to_owned(), LocalVar::read("bar".to_owned())), None);
    assert_eq!(table.get_var("foo"), Some(&LocalVar::read("bar".to_owned())));
    assert_eq!(table.set_var("foo".to_owned(), LocalVar::read("baz".to_owned())),
               Some(LocalVar::read("bar".to_owned())));
    table.del_var("foo");
    assert_eq!(table.get_var("foo"), None);
  }

  #[test]
  fn test_iter_vars() {
    let mut table = SymbolTable::new();
    table.set_var("foo".to_owned(), LocalVar::read("bar".to_owned()));
    table.set_var("foo1".to_owned(), LocalVar::rw("baz".to_owned()));
    table.set_var("foo2".to_owned(), LocalVar::read("abcdef".to_owned()));
    let mut vec: Vec<_> = table.vars().map(|x| (x.0, from_var_name(&x.1.name))).collect();
    vec.sort_unstable();
    assert_eq!(vec, vec!(("foo", "bar"), ("foo1", "baz"), ("foo2", "abcdef")));
  }

  fn sample_fn() -> FnCall {
    FnCall::file_constant(FnSpecs::new(1, 0, None), FnScope::Global, "foobar".to_owned())
  }

  #[test]
  fn test_fns() {
    let mut table = SymbolTable::new();
    assert_eq!(table.get_fn("foo").map(|x| x.0), None);
    table.set_fn("foo".to_owned(), sample_fn(), Box::new(DefaultCall));
    assert_eq!(table.get_fn("foo").map(|x| x.0), Some(&sample_fn()));
    table.set_fn("foo".to_owned(), sample_fn(), Box::new(DefaultCall));
    table.del_fn("foo");
    assert_eq!(table.get_fn("foo").map(|x| x.0), None);
  }

  #[test]
  fn test_iter_fns() {
    let mut table = SymbolTable::new();
    table.set_fn("foo".to_owned(), sample_fn(), Box::new(DefaultCall));
    table.set_fn("foo1".to_owned(), sample_fn(), Box::new(DefaultCall));
    table.set_fn("foo2".to_owned(), sample_fn(), Box::new(DefaultCall));
    let mut vec: Vec<_> = table.fns().map(|(x, y, _)| (x, y)).collect();
    vec.sort_unstable_by(|a, b| a.0.cmp(b.0));
    let tmp = sample_fn();
    assert_eq!(vec, vec!(("foo", &tmp), ("foo1", &tmp), ("foo2", &tmp)));
  }

}
