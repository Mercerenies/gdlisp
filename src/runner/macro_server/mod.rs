
pub mod command;
pub mod lazy;

use super::run_project_process;

use command::{ServerCommand, IsServerCommand};

use std::io::{self, Write, Read};
use std::path::PathBuf;
use std::process::{Child, ExitStatus};
use std::net::{TcpListener, TcpStream};
use std::convert::TryInto;
use std::fs;

// TODO Make port number configurable
pub const PORT_NUMBER: u16 = 61992;

pub struct MacroServer {
  tcp_server: TcpStream,
  godot_server: Child,
}

impl MacroServer {

  pub fn new() -> io::Result<MacroServer> {
    fs::copy(PathBuf::from("GDLisp.gd"), PathBuf::from("MacroServer/GDLisp.gd"))?;
    let tcp_listener = TcpListener::bind(("127.0.0.1", PORT_NUMBER))?;
    let gd_server = run_project_process(PathBuf::from("MacroServer/"))?;
    let (tcp_server, _) = tcp_listener.accept()?;
    Ok(MacroServer {
      tcp_server: tcp_server,
      godot_server: gd_server,
    })
  }

  fn send_string(&mut self, string: &str) -> io::Result<()> {
    let mut buf = Vec::new();
    let len: u32 = string.len().try_into().expect("String too long to send to Godot TCP server");
    buf.extend(&len.to_be_bytes());
    buf.extend(string.bytes());
    self.tcp_server.write_all(&buf)?;
    Ok(())
  }

  fn receive_string(&mut self) -> io::Result<String> {
    let mut len_buf = [0; 4];
    self.tcp_server.read_exact(&mut len_buf)?;
    let len: usize = u32::from_be_bytes(len_buf).try_into().expect("String too long to receive from Godot TCP server");
    let mut buf = vec![0; len];
    self.tcp_server.read_exact(&mut buf)?;
    String::from_utf8(buf).map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Error in UTF8 conversion"))
  }

  pub fn issue_command(&mut self, command: impl IsServerCommand) -> io::Result<String> {
    for s in command.to_command() {
      self.send_string(&s)?;
    }
    self.receive_string()
  }

  pub fn shutdown(mut self) -> io::Result<ExitStatus> {
    self.issue_command(&ServerCommand::Quit)?;
    self.godot_server.wait()
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::reify::Reify;
  use crate::parser;

  fn roundtrip_value(server: &mut MacroServer, value: &str) {
    let parser = parser::ASTParser::new();
    let ast = parser.parse(value).unwrap();
    let result = server.issue_command(&ServerCommand::Eval(ast.reify().to_gd())).unwrap();
    assert_eq!(value, &result);
  }

  #[test]
  #[ignore]
  fn spawn_server_test() {
    MacroServer::new().unwrap().shutdown().unwrap();

    let mut server1 = MacroServer::new().unwrap();
    let response1 = server1.issue_command(&vec!(String::from("ping"))).unwrap();
    assert_eq!(response1, "pong");
    server1.shutdown().unwrap();

    let mut server2 = MacroServer::new().unwrap();
    let response2_1 = server2.issue_command(&ServerCommand::Ping).unwrap();
    assert_eq!(response2_1, "pong");
    let response2_2 = server2.issue_command(&ServerCommand::Eval(String::from("1 + 1"))).unwrap();
    assert_eq!(response2_2, "2");

    roundtrip_value(&mut server2, "(1 . ())");
    roundtrip_value(&mut server2, "[#t #f abc def]");
    roundtrip_value(&mut server2, "[10 20 (30 . (40 . ())) \"ABC\"]");
    // TODO Test roundtrip on string escaping (once we support parsing
    // escape sequence)

    server2.shutdown().unwrap();

  }

}
