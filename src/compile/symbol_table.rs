
use std::collections::HashMap;

pub struct SymbolTable {
  data: HashMap<String, String>,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable { data: HashMap::new() }
  }

  pub fn get_table(&self) -> &HashMap<String, String> {
    &self.data
  }

}

pub trait HasSymbolTable {

  fn get_symbol_table(&self) -> &SymbolTable;

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable;

  fn with_local_var<B>(&mut self,
                       name: String,
                       value: String,
                       block: impl FnOnce(&mut Self) -> B) -> B {
    let previous = self.get_symbol_table_mut().data.insert(name.clone(), value);
    let result = block(self);
    if let Some(previous) = previous {
      self.get_symbol_table_mut().data.insert(name, previous);
    } else {
      self.get_symbol_table_mut().data.remove(&name);
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

  fn get_var(&self, name: &str) -> Option<&str> {
    self.get_symbol_table().data.get(name).map(|x| x.as_str())
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
