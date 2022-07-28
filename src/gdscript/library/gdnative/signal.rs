
use super::argument::Argument;

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Signal {
  pub name: String,
  pub arguments: Vec<Argument>,
}
