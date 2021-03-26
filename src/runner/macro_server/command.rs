
use std::vec;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServerCommand {
  Quit,
  Ping,
  Eval(String),
  Load(String),
}

impl ServerCommand {

  fn to_vec(&self) -> Vec<String> {
    match self {
      ServerCommand::Quit => vec!(String::from("quit")),
      ServerCommand::Ping => vec!(String::from("ping")),
      ServerCommand::Eval(s) => vec!(String::from("eval"), String::from(s)),
      ServerCommand::Load(s) => vec!(String::from("load"), String::from(s)),
    }
  }

  pub fn to_command(&self) -> vec::IntoIter<String> {
    self.to_vec().into_iter()
  }
}
