
use super::SymbolTable;

use std::collections::HashMap;

// Note: The keys in the monitor HashMap are actually the GD names,
// NOT the AST ones. This is to ensure that if an AST name gets
// locally shadowed, only the *original* GD name is monitored, not the
// local shadow. Consider
//
// (let ((foo 1)) (lambda () (progn foo (let ((foo 2)) foo))))
//
// The first instance of foo in the lambda is captured by the closure
// and should be subject to monitoring so we can store it with the
// closure object. The second is a simple local variable that happens
// to share its name with a possible capture. The local variable
// should not be monitored.
pub struct MonitoredTable<Table: SymbolTable> {
  table: Table,
  monitor: HashMap<String, bool>,
}

impl<Table: SymbolTable> MonitoredTable<Table> {

  pub fn new(table: Table) -> MonitoredTable<Table> {
    MonitoredTable {
      table: table,
      monitor: HashMap::new(),
    }
  }

  pub fn monitor_var(&mut self, name: String) {
    if !self.monitor.contains_key(&name) {
      self.monitor.insert(name, false);
    }
  }

  pub fn monitor_vars(&mut self, names: impl IntoIterator<Item = String>) {
    for name in names {
      self.monitor_var(name)
    }
  }

  pub fn is_monitoring(&self, name: &str) -> bool {
    self.monitor.contains_key(name)
  }

  pub fn was_var_used(&self, name: &str) -> bool {
    *self.monitor.get(name).unwrap_or(&false)
  }

  pub fn get_used_vars(&self) -> impl Iterator<Item = &String> {
    self.monitor.iter().filter(|x| *x.1).map(|x| x.0)
  }

}

impl<Table: SymbolTable> SymbolTable for MonitoredTable<Table> {

  fn get_var(&mut self, name: &str) -> Option<&str> {
    let res_name = self.table.get_var(name);
    if let Some(res_name) = res_name {
      if self.monitor.contains_key(res_name) {
        // Seems like an avoidable copy, but meh
        self.monitor.insert(String::from(res_name), true);
      }
    }
    res_name
  }

  fn set_var(&mut self, name: String, value: String) -> Option<String> {
    self.table.set_var(name, value)
  }

  fn del_var(&mut self, name: &str) {
    self.table.del_var(name)
  }

  fn vars<'a>(&'a self) -> Vec<(&'a str, &'a str)> {
    self.table.vars()
  }

}
