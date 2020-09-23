
pub mod concrete;
pub mod monitored;

pub trait SymbolTable {

  // get_var requires &mut self so we can do monitoring tricks to
  // detect closure arguments.
  fn get_var(&mut self, name: &str) -> Option<&str>;
  fn set_var(&mut self, name: String, value: String) -> Option<String>;
  fn del_var(&mut self, name: &str);

  // Would be nice if we could get this thing to return an iterator,
  // but between GATs being unstable, no impl trait return on trait
  // methods, and just generally me still learning Rust, we're going
  // to keep it simple.
  fn vars<'a>(&'a self) -> Vec<(&'a str, &'a str)>;

}

// So this probably doesn't need to be a trait anymore. It could
// almost be merged into SymbolTable. It was necessary back when the
// compiler directly stored a SymbolTable, which we don't do anymore.
pub trait HasSymbolTable {

  type Table: SymbolTable;

  fn get_symbol_table(&self) -> &Self::Table;

  fn get_symbol_table_mut(&mut self) -> &mut Self::Table;

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

}

impl<T: SymbolTable> HasSymbolTable for T {
  type Table = T;

  fn get_symbol_table(&self) -> &Self::Table {
    self
  }

  fn get_symbol_table_mut(&mut self) -> &mut Self::Table {
    self
  }

}
