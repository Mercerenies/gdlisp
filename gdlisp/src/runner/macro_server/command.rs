// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! The types representing commands which can be sent to a
//! [`MacroServer`](super::MacroServer).
//!
//! [`ServerCommand`] represents the various commands which can be
//! sent to the macro server. Commands are eventually encoded as JSON
//! and passed onto the server. Eventually, the server will receive a
//! [`ServerResponse`](super::response::ServerResponse) back as reply.
//!
//! Note that, while it is *highly* unlikely that this will ever come
//! into play, the total length of a command, including the JSON
//! formatting and command name, cannot exceed 2^32 characters. Thus,
//! any string arguments to `ServerCommand` should not approach that
//! length.

use json::JsonValue;

/// A server command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServerCommand {
  Quit,
  Ping,
  Eval(String),
  Exec(String),
  Load(String),
}

impl ServerCommand {

  /// The name of the command, as a string.
  pub fn name(&self) -> &'static str {
    match self {
      ServerCommand::Quit => "quit",
      ServerCommand::Ping => "ping",
      ServerCommand::Eval(_) => "eval",
      ServerCommand::Exec(_) => "exec",
      ServerCommand::Load(_) => "load",
    }
  }

  /// The arguments to the command, as a sequence of strings.
  pub fn arguments(&self) -> Vec<&str> {
    match self {
      ServerCommand::Quit => vec!(),
      ServerCommand::Ping => vec!(),
      ServerCommand::Eval(s) => vec!(s),
      ServerCommand::Exec(s) => vec!(s),
      ServerCommand::Load(s) => vec!(s),
    }
  }

  /// Convert the command to JSON.
  ///
  /// Commands are sent as JSON objects which have the following keys.
  ///
  /// * command (required) - The name of the command.
  ///
  /// * args (required) - An array of values, usually strings. Its
  /// interpretation depends on the command being run.
  pub fn to_json(&self) -> JsonValue {
    let command = String::from(self.name());
    let args = self.arguments().into_iter().map(JsonValue::from).collect();
    json::object!{
      "command" => command,
      "args" => JsonValue::Array(args),
    }
  }

}
