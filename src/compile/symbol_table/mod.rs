
pub mod function_call;
pub mod call_magic;
pub mod local_var;

use function_call::FnCall;
use call_magic::{CallMagic, DefaultCall};
use local_var::{LocalVar, ValueHint, ValueHintsTable};
use crate::util::debug_wrapper::DebugWrapper;

use std::collections::HashMap;
use std::borrow::Borrow;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  locals: HashMap<String, LocalVar>,
  reverse_locals: HashMap<String, String>, // key: GDScript name, value: GDLisp name (to use in locals)
  functions: HashMap<String, (FnCall, DebugWrapper<Box<dyn CallMagic + 'static>>)>,
}

// When we move into a class scope, we need to keep two symbol tables:
// one for use in static contexts and one for use in instance
// (non-static) contexts. This struct encapsulates that concept.
pub struct ClassTablePair<'a, 'b> {
  pub static_table: &'a mut SymbolTable,
  pub instance_table: &'b mut SymbolTable,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  pub fn get_var(&self, name: &str) -> Option<&LocalVar> {
    self.locals.get(name)
  }

  pub fn get_var_by_gd_name(&self, gd_name: &str) -> Option<&LocalVar> {
    self.reverse_locals.get(gd_name).and_then(|name| self.locals.get(name))
  }

  pub fn set_var(&mut self, name: String, value: LocalVar) -> Option<LocalVar> {
    if let Some(gd_name) = value.simple_name() {
      self.reverse_locals.insert(gd_name.to_owned(), name.clone());
    }
    self.locals.insert(name, value)
  }

  pub fn del_var(&mut self, name: &str) {
    let value = self.locals.remove(name);
    if let Some(value) = value {
      if let Some(gd_name) = value.simple_name() {
        self.reverse_locals.remove(gd_name);
      }
    }
  }

  pub fn get_fn(&self, name: &str) -> Option<(&FnCall, &(dyn CallMagic + 'static))> {
    self.functions.get(name).map(|(call, magic)| {
      (call, &*magic.0)
    })
  }

  pub fn set_fn(&mut self, name: String, value: FnCall, magic: Box<dyn CallMagic + 'static>) {
    self.functions.insert(name, (value, DebugWrapper(magic)));
  }

  pub fn del_fn(&mut self, name: &str) {
    self.functions.remove(name);
  }

  pub fn vars(&self) -> impl Iterator<Item=(&str, &LocalVar)> {
    return self.locals.iter().map(|x| (x.0.borrow(), x.1));
  }

  pub fn fns(&self) -> impl Iterator<Item=(&str, &FnCall, &(dyn CallMagic + 'static))> {
    self.functions.iter().map(|(name, value)| {
      (name.borrow(), &value.0, &*value.1.0)
    })
  }

  pub fn fns_mut(&mut self) -> impl Iterator<Item=(&str, &mut FnCall, &mut (dyn CallMagic + 'static))> {
    self.functions.iter_mut().map(|(name, value)| {
      (name.borrow(), &mut value.0, &mut *value.1.0)
    })
  }

}

// So this probably doesn't need to be a trait anymore. It could
// almost be merged into SymbolTable. It was necessary back when the
// compiler directly stored a SymbolTable, which we don't do anymore.
pub trait HasSymbolTable {

  fn get_symbol_table(&self) -> &SymbolTable;

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable;

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
