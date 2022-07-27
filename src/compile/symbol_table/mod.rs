
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
use call_magic::{CallMagic};
use local_var::{LocalVar, ValueHint, ValueHintsTable};

use serde::{Serialize, Deserialize};

use std::collections::HashMap;
use std::collections::HashSet;
use std::borrow::Borrow;

/// GDLisp has two separate namespaces when it comes to name
/// resolution: the variable namespace and the function namespace.
/// Types, such as classes and primitive types, fall into the variable
/// namespace. `SymbolTable` encompasses a table of names available in
/// the current scope, in either namespace.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct SymbolTable {

  /// A mapping from GDLisp names to [`LocalVar`] instances, which can
  /// be local variables, file-level constants, GDScript constants, or
  /// several other types of values.
  locals: HashMap<String, LocalVar>,

  /// A mapping from GDScript names to GDLisp names. The values in
  /// this map shall always be valid keys in `locals`.
  reverse_locals: HashMap<String, String>,

  /// A mapping of GDScript variables that were
  /// synthetically-generated and do not have a GDLisp name
  /// corresponding to them. The Boolean indicates whether the
  /// variable is local to the current function or not. Locals will be
  /// cleared by [`SymbolTable::clear_synthetic_locals`].
  synthetic_locals: HashMap<String, bool>,

  /// A mapping from GDLisp names to [`FnCall`] and corresponding
  /// [`CallMagic`] instances.
  functions: HashMap<String, (FnCall, CallMagic)>,

  /// A mapping from GDScript names to GDLisp names. The values in
  /// this map shall always be valid keys in `functions`.
  reverse_functions: HashMap<String, String>,

  /// A set of GDScript functions that were synthetically-generated
  /// and do not have a GDLisp name corresponding to them.
  synthetic_functions: HashSet<String>,
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
    self.reverse_locals.get(gd_name).and_then(|name| self.get_var(name))
  }

  /// Gets the function with the given local GDScript name. Functions
  /// are indexed by both names, so this access is as efficient as
  /// [`SymbolTable::get_fn`].
  pub fn get_fn_by_gd_name(&self, gd_name: &str) -> Option<(&FnCall, &CallMagic)> {
    self.reverse_functions.get(gd_name).and_then(|name| self.get_fn(name))
  }

  /// Sets the variable with the given GDLisp name, returning the old
  /// value if one existed.
  pub fn set_var(&mut self, name: String, value: LocalVar) -> Option<LocalVar> {
    let new_gd_name = value.simple_name().map(|x| x.to_owned());
    let old_value = self.locals.insert(name.clone(), value);
    if let Some(old_value) = &old_value {
      if let Some(old_gd_name) = old_value.simple_name() {
        self.reverse_locals.remove(old_gd_name);
      }
    }
    if let Some(new_gd_name) = new_gd_name {
      self.reverse_locals.insert(new_gd_name, name);
    }
    old_value
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

  /// Adds a synthetic GDScript variable to the symbol table.
  /// Synthetic GDScript variables do not appear in the symbol table's
  /// maps but will respond to [`SymbolTable::has_var_with_gd_name`].
  pub fn add_synthetic_var(&mut self, name: String, is_local: bool) {
    self.synthetic_locals.insert(name, is_local);
  }

  /// Removes a synthetic GDScript variable. See
  /// [`SymbolTable::add_synthetic_var`].
  pub fn remove_synthetic_var(&mut self, name: &str) {
    self.synthetic_locals.remove(name);
  }

  /// Removes all synthetic variables marked with `is_local`.
  pub fn clear_synthetic_locals(&mut self) {
    self.synthetic_locals.retain(|_, is_local| !*is_local);
  }

  /// Gets the function call and call magic associated with the given
  /// GDLisp function name.
  pub fn get_fn(&self, name: &str) -> Option<(&FnCall, &CallMagic)> {
    self.functions.get(name).map(|(call, magic)| {
      (call, magic)
    })
  }

  /// Sets the function call and call magic associated with the given
  /// function name.
  pub fn set_fn(&mut self, name: String, value: FnCall, magic: CallMagic) {
    let new_gd_name = value.function.clone();
    let old_function = self.functions.insert(name.clone(), (value, magic));
    if let Some((old_function, _)) = old_function {
      self.reverse_functions.remove(&old_function.function);
    }
    self.reverse_functions.insert(new_gd_name, name);
  }

  /// Deletes any function call info and call magic associated with
  /// the given function name.
  pub fn del_fn(&mut self, name: &str) {
    let old_function = self.functions.remove(name);
    if let Some((old_function, _)) = old_function {
      self.reverse_locals.remove(&old_function.function);
    }
  }

  /// Adds a synthetic GDScript function to the symbol table.
  /// Synthetic GDScript functions do not appear in the symbol table's
  /// maps but will respond to [`SymbolTable::has_fn_with_gd_name`].
  pub fn add_synthetic_fn(&mut self, name: String) {
    self.synthetic_functions.insert(name);
  }

  /// Removes a synthetic GDScript function. See
  /// [`SymbolTable::add_synthetic_fn`].
  pub fn remove_synthetic_fn(&mut self, name: &str) {
    self.synthetic_functions.remove(name);
  }

  /// An iterator over the variable namespace of this table.
  pub fn vars(&self) -> impl Iterator<Item=(&str, &LocalVar)> {
    return self.locals.iter().map(|x| (x.0.borrow(), x.1));
  }

  /// An iterator over the function namespace of this table.
  pub fn fns(&self) -> impl Iterator<Item=(&str, &FnCall, &CallMagic)> {
    self.functions.iter().map(|(name, value)| {
      (name.borrow(), &value.0, &value.1)
    })
  }

  /// An iterator over the function namespace of this table, providing
  /// mutable access to the function call information and the call
  /// magic.
  pub fn fns_mut(&mut self) -> impl Iterator<Item=(&str, &mut FnCall, &mut CallMagic)> {
    self.functions.iter_mut().map(|(name, value)| {
      (name.borrow(), &mut value.0, &mut value.1)
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
      self.set_fn(name.to_owned(), call.to_owned(), magic.to_owned());
    }
  }

  /// Returns true if the symbol table has a variable with the given
  /// GDScript name.
  pub fn has_var_with_gd_name(&self, name: &str) -> bool {
    self.reverse_locals.contains_key(name) ||
      self.synthetic_locals.contains_key(name)
  }

  /// Returns true if the symbol table has a function with the given
  /// GDScript name.
  pub fn has_fn_with_gd_name(&self, name: &str) -> bool {
    self.reverse_functions.contains_key(name) ||
      self.synthetic_functions.contains(name)
  }

}

/// Trait for objects which have a symbol table. Currently, there is
/// only one implementor of this trait: [`SymbolTable`] itself.
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
    let previous = self.get_symbol_table_mut().get_fn(&name).map(|(p, m)| (p.clone(), m.clone()));
    self.get_symbol_table_mut().set_fn(name.clone(), value, CallMagic::DefaultCall);
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
    assert_eq!(table.get_var_by_gd_name("foo"), None);
    assert_eq!(table.get_var_by_gd_name("bar"), None);
    assert_eq!(table.get_var_by_gd_name("baz"), None);

    assert_eq!(table.set_var("foo".to_owned(), LocalVar::read("bar".to_owned())), None);
    assert_eq!(table.get_var("foo"), Some(&LocalVar::read("bar".to_owned())));
    assert_eq!(table.get_var_by_gd_name("foo"), None);
    assert_eq!(table.get_var_by_gd_name("bar"), Some(&LocalVar::read("bar".to_owned())));
    assert_eq!(table.get_var_by_gd_name("baz"), None);

    assert_eq!(table.set_var("foo".to_owned(), LocalVar::read("baz".to_owned())),
               Some(LocalVar::read("bar".to_owned())));
    assert_eq!(table.get_var_by_gd_name("foo"), None);
    assert_eq!(table.get_var_by_gd_name("bar"), None);
    assert_eq!(table.get_var_by_gd_name("baz"), Some(&LocalVar::read("baz".to_owned())));

    table.del_var("foo");
    assert_eq!(table.get_var("foo"), None);
    assert_eq!(table.get_var_by_gd_name("foo"), None);
    assert_eq!(table.get_var_by_gd_name("bar"), None);
    assert_eq!(table.get_var_by_gd_name("baz"), None);

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

    table.set_fn("foo".to_owned(), sample_fn(), CallMagic::DefaultCall);
    assert_eq!(table.get_fn("foo").map(|x| x.0), Some(&sample_fn()));
    assert_eq!(table.get_fn_by_gd_name("foo").map(|x| x.0), None);
    assert_eq!(table.get_fn_by_gd_name("foobar").map(|x| x.0), Some(&sample_fn()));

    table.set_fn("foo".to_owned(), sample_fn(), CallMagic::DefaultCall);
    table.del_fn("foo");
    assert_eq!(table.get_fn("foo").map(|x| x.0), None);
    assert_eq!(table.get_fn_by_gd_name("foo").map(|x| x.0), None);
    assert_eq!(table.get_fn_by_gd_name("foobar").map(|x| x.0), None);

  }

  #[test]
  fn test_iter_fns() {
    let mut table = SymbolTable::new();
    table.set_fn("foo".to_owned(), sample_fn(), CallMagic::DefaultCall);
    table.set_fn("foo1".to_owned(), sample_fn(), CallMagic::DefaultCall);
    table.set_fn("foo2".to_owned(), sample_fn(), CallMagic::DefaultCall);
    let mut vec: Vec<_> = table.fns().map(|(x, y, _)| (x, y)).collect();
    vec.sort_unstable_by(|a, b| a.0.cmp(b.0));
    let tmp = sample_fn();
    assert_eq!(vec, vec!(("foo", &tmp), ("foo1", &tmp), ("foo2", &tmp)));
  }

  #[test]
  fn test_synthetic_vars() {
    let mut table = SymbolTable::new();
    assert_eq!(table.has_var_with_gd_name("foo"), false);
    table.add_synthetic_var(String::from("foo"), false);
    assert_eq!(table.has_var_with_gd_name("foo"), true);
    table.remove_synthetic_var("foo");
    assert_eq!(table.has_var_with_gd_name("foo"), false);
  }

  #[test]
  fn test_synthetic_fns() {
    let mut table = SymbolTable::new();
    assert_eq!(table.has_fn_with_gd_name("foo"), false);
    table.add_synthetic_fn(String::from("foo"));
    assert_eq!(table.has_fn_with_gd_name("foo"), true);
    table.remove_synthetic_fn("foo");
    assert_eq!(table.has_fn_with_gd_name("foo"), false);
  }

}
