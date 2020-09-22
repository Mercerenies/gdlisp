
use std::collections::HashMap;

pub struct SymbolTable {
  data: HashMap<String, String>,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable { data: HashMap::new() }
  }

  pub fn get_var(&self, name: &str) -> Option<&str> {
    self.data.get(name).map(|x| x.as_str())
  }

  pub fn set_var(&mut self, name: String, value: String) -> Option<String> {
    self.data.insert(name, value)
  }

  pub fn del_var(&mut self, name: &str) {
    self.data.remove(name);
  }

}

pub trait HasSymbolTable {

  fn get_symbol_table(&self) -> &SymbolTable;

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable;

  fn with_local_var<B>(&mut self,
                       name: String,
                       value: String,
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
                        vars: &mut dyn Iterator<Item=(String, String)>,
                        block: impl FnOnce(&mut Self) -> B) -> B {
    if let Some((name, value)) = vars.next() {
      self.with_local_var(name, value, |curr| {
        curr.with_local_vars(vars, block)
      })
    } else {
      block(self)
    }
  }

  #[deprecated(note="Simply make a new symbol table")]
  fn with_disjoint_scope<B>(&mut self,
                            block: impl FnOnce(&mut Self) -> B) -> B {
    let old_data = {
      let table = self.get_symbol_table_mut();
      let tmp = table.data.clone(); // Don't own, so can't move :(
      table.data = HashMap::new();
      tmp
    };
    let result = block(self);
    self.get_symbol_table_mut().data = old_data;
    result
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
