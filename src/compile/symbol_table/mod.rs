
pub mod concrete;
pub mod monitored;

// Won't need this lifetime parameter once we have https://github.com/rust-lang/rust/issues/44265
pub trait SymbolTable<'a> {

  type Iter: Iterator<Item=(&'a String, &'a String)>;

  // get_var requires &mut self so we can do monitoring tricks to
  // detect closure arguments.
  fn get_var(&mut self, name: &str) -> Option<&str>;
  fn set_var(&mut self, name: String, value: String) -> Option<String>;
  fn del_var(&mut self, name: &str);

  fn vars(&'a self) -> Self::Iter;

}

// So this probably doesn't need to be a trait anymore. It could
// almost be merged into SymbolTable. It was necessary back when the
// compiler directly stored a SymbolTable, which we don't do anymore.
pub trait HasSymbolTable<'a> {

  type Table: SymbolTable<'a>;

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

impl<'a, T: SymbolTable<'a>> HasSymbolTable<'a> for T {
  type Table = T;

  fn get_symbol_table(&self) -> &Self::Table {
    self
  }

  fn get_symbol_table_mut(&mut self) -> &mut Self::Table {
    self
  }

}
