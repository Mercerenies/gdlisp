
use json::JsonValue;

use std::vec;

/*
 * Commands are sent as JSON objects which have the following keys.
 *
 * command (required) - The name of the command.
 *
 * args (required) - An array of values, usually strings. Its
 * interpretation depends on the command being run.
 */

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServerCommand {
  Quit,
  Ping,
  Eval(String),
  Load(String),
}

impl ServerCommand {

  pub fn name(&self) -> &'static str {
    match self {
      ServerCommand::Quit => "quit",
      ServerCommand::Ping => "ping",
      ServerCommand::Eval(_) => "eval",
      ServerCommand::Load(_) => "load",
    }
  }

  pub fn arguments(&self) -> Vec<&str> {
    match self {
      ServerCommand::Quit => vec!(),
      ServerCommand::Ping => vec!(),
      ServerCommand::Eval(s) => vec!(&s),
      ServerCommand::Load(s) => vec!(&s),
    }
  }

  pub fn to_json(&self) -> JsonValue {
    let command = String::from(self.name());
    let args = self.arguments().into_iter().map(JsonValue::from).collect();
    json::object!{
      "command" => command,
      "args" => JsonValue::Array(args),
    }
  }

}
