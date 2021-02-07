
use std::slice;
use std::iter;
use std::vec;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServerCommand {
  Quit,
  Ping,
}

impl ServerCommand {

  fn to_vec(&self) -> Vec<String> {
    match self {
      ServerCommand::Quit => vec!(String::from("quit")),
      ServerCommand::Ping => vec!(String::from("ping")),
    }
  }

}

pub trait IsServerCommand {
  type Iter: Iterator<Item=String>;

  fn to_command(self) -> Self::Iter;

}

impl<'a> IsServerCommand for &'a Vec<String> {
  type Iter = iter::Cloned<slice::Iter<'a, String>>;

  fn to_command(self) -> Self::Iter {
    self.iter().cloned()
  }
}

impl<'a> IsServerCommand for &'a ServerCommand {
  type Iter = vec::IntoIter<String>;

  fn to_command(self) -> Self::Iter {
    self.to_vec().into_iter()
  }
}
