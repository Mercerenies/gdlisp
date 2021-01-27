
pub mod function_call;

use function_call::FnCall;
use crate::ir::locals::AccessType;
use crate::gdscript::expr::Expr;
use crate::gdscript::library::CELL_CONTENTS;

use std::collections::HashMap;
use std::borrow::Borrow;

#[derive(Clone, Debug)]
pub struct SymbolTable {
  locals: HashMap<String, LocalVar>,
  functions: HashMap<String, FnCall>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LocalVar {
  pub name: String,
  pub access_type: AccessType,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable { locals: HashMap::new(), functions: HashMap::new() }
  }

  pub fn get_var(&self, name: &str) -> Option<&LocalVar> {
    self.locals.get(name)
  }

  pub fn set_var(&mut self, name: String, value: LocalVar) -> Option<LocalVar> {
    self.locals.insert(name, value)
  }

  pub fn del_var(&mut self, name: &str) {
    self.locals.remove(name);
  }

  pub fn get_fn(&self, name: &str) -> Option<&FnCall> {
    self.functions.get(name)
  }

  pub fn set_fn(&mut self, name: String, value: FnCall) -> Option<FnCall> {
    self.functions.insert(name, value)
  }

  pub fn del_fn(&mut self, name: &str) {
    self.functions.remove(name);
  }

  pub fn vars<'a>(&'a self) -> impl Iterator<Item=(&'a str, &'a LocalVar)> {
    return self.locals.iter().map(|x| (x.0.borrow(), x.1));
  }

  pub fn fns<'a>(&'a self) -> impl Iterator<Item=(&'a str, &'a FnCall)> {
    return self.functions.iter().map(|x| (x.0.borrow(), x.1));
  }

}

impl LocalVar {

  pub fn read(name: String) -> LocalVar {
    LocalVar { name, access_type: AccessType::Read }
  }

  pub fn rw(name: String) -> LocalVar {
    LocalVar { name, access_type: AccessType::RW }
  }

  pub fn closed_rw(name: String) -> LocalVar {
    LocalVar { name, access_type: AccessType::ClosedRW }
  }

  pub fn new(name: String, access_type: AccessType) -> LocalVar {
    LocalVar { name, access_type }
  }

  pub fn expr(&self) -> Expr {
    if self.access_type.requires_cell() {
        Expr::Attribute(Box::new(Expr::var(&self.name)), CELL_CONTENTS.to_owned())
    } else {
      Expr::var(&self.name)
    }
  }

  // TODO Put all of the declaration-site stuff here as well, like
  // .expr() for access, so we have it all in one place (i.e. the
  // difference between "var x = ..." and "var x = Cell.new(...)")

}

// So this probably doesn't need to be a trait anymore. It could
// almost be merged into SymbolTable. It was necessary back when the
// compiler directly stored a SymbolTable, which we don't do anymore.
pub trait HasSymbolTable {

  fn get_symbol_table(&self) -> &SymbolTable;

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable;

  fn with_local_var<B>(&mut self,
                       name: String,
                       value: LocalVar,
                       block: impl FnOnce(&mut Self) -> B) -> B {
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
    let previous = self.get_symbol_table_mut().set_fn(name.clone(), value);
    let result = block(self);
    if let Some(previous) = previous {
      self.get_symbol_table_mut().set_fn(name, previous);
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

#[cfg(test)]
mod tests {
  use super::*;
  use function_call::{FnSpecs, FnScope};

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
    let mut vec: Vec<_> = table.vars().map(|x| (x.0, x.1.name.borrow())).collect();
    vec.sort_unstable();
    assert_eq!(vec, vec!(("foo", "bar"), ("foo1", "baz"), ("foo2", "abcdef")));
  }

  fn sample_fn() -> FnCall {
    FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, "foobar".to_owned())
  }

  #[test]
  fn test_fns() {
    let mut table = SymbolTable::new();
    assert_eq!(table.get_fn("foo"), None);
    assert_eq!(table.set_fn("foo".to_owned(), sample_fn()), None);
    assert_eq!(table.get_fn("foo"), Some(&sample_fn()));
    assert_eq!(table.set_fn("foo".to_owned(), sample_fn()), Some(sample_fn()));
    table.del_fn("foo");
    assert_eq!(table.get_fn("foo"), None);
  }

  #[test]
  fn test_iter_fns() {
    let mut table = SymbolTable::new();
    table.set_fn("foo".to_owned(), sample_fn());
    table.set_fn("foo1".to_owned(), sample_fn());
    table.set_fn("foo2".to_owned(), sample_fn());
    let mut vec: Vec<_> = table.fns().collect();
    vec.sort_unstable_by(|a, b| a.0.cmp(b.0));
    let tmp = sample_fn();
    assert_eq!(vec, vec!(("foo", &tmp), ("foo1", &tmp), ("foo2", &tmp)));
  }

}
