
pub mod command;
pub mod response;
pub mod lazy;
pub mod named_file_server;

use super::run_project_process;

use command::ServerCommand;

use std::io::{self, Write, Read, ErrorKind};
use std::path::PathBuf;
use std::process::{Child, ExitStatus};
use std::net::{TcpListener, TcpStream};
use std::convert::TryInto;
use std::fs;
use std::mem::ManuallyDrop;

// TODO Make port number configurable
pub const PORT_NUMBER: u16 = 61992;

pub struct MacroServer {
  tcp_server: TcpStream,
  godot_server: Child,
}

impl MacroServer {

  pub fn new() -> io::Result<MacroServer> {
    fs::copy(PathBuf::from("GDLisp.gd"), PathBuf::from("MacroServer/GDLisp.gd"))?;
    let (tcp_listener, port) = MacroServer::try_to_bind_port(PORT_NUMBER, u16::MAX)?;
    let env = vec!(("GDLISP_PORT_NUMBER", port.to_string()));
    let gd_server = run_project_process(PathBuf::from("MacroServer/"), env.into_iter())?;
    let (tcp_server, _) = tcp_listener.accept()?;
    Ok(MacroServer {
      tcp_server: tcp_server,
      godot_server: gd_server,
    })
  }

  fn try_to_bind_port(start: u16, end: u16) -> io::Result<(TcpListener, u16)> {
    for port in start..=end {
      match TcpListener::bind(("127.0.0.1", port)) {
        Ok(listener) => {
          return Ok((listener, port));
        }
        Err(err) if err.kind() == ErrorKind::AddrInUse => {
          // Continue.
        }
        Err(err) => {
          // Unhandled error
          return Err(err);
        }
      }
    }
    Err(io::Error::new(ErrorKind::AddrInUse, "Could not find TCP port to bind"))
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

  pub fn issue_command(&mut self, command: &ServerCommand) -> io::Result<String> {
    let json = command.to_json();
    self.send_string(&json.to_string())?;
    self.receive_string()
  }

  // Unsafe to use the server afterward, so we only expose a public
  // version that takes ownership.
  fn _shutdown(&mut self) -> io::Result<ExitStatus> {
    self.issue_command(&ServerCommand::Quit)?;
    self.godot_server.wait()
  }

  pub fn shutdown(self) -> io::Result<ExitStatus> {
    let mut server = ManuallyDrop::new(self);
    server._shutdown()
  }

}

impl Drop for MacroServer {

  fn drop(&mut self) {
    let _r = self._shutdown(); // Ignore io::Result (we're in Drop so we can't handle it)
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
  fn spawn_server_simple_test() {
    MacroServer::new().unwrap().shutdown().unwrap();
  }

  #[test]
  #[ignore]
  fn spawn_server_ping_pong_test() {
    let mut server1 = MacroServer::new().unwrap();
    let response1 = server1.issue_command(&ServerCommand::Ping).unwrap();
    assert_eq!(response1, "pong");
    server1.shutdown().unwrap();
  }

  #[test]
  #[ignore]
  fn spawn_server_test() {
    let mut server2 = MacroServer::new().unwrap();
    let response2_1 = server2.issue_command(&ServerCommand::Ping).unwrap();
    assert_eq!(response2_1, "pong");
    let response2_2 = server2.issue_command(&ServerCommand::Eval(String::from("1 + 1"))).unwrap();
    assert_eq!(response2_2, "2");

    roundtrip_value(&mut server2, "(1 . ())");
    roundtrip_value(&mut server2, "[#t #f abc def]");
    roundtrip_value(&mut server2, "[10 20 (30 . (40 . ())) \"ABC\"]");
    roundtrip_value(&mut server2, "V{1 2}");
    roundtrip_value(&mut server2, "V{1 2 3}");
    // TODO Test roundtrip on string escaping (once we support parsing
    // escape sequences)

    let response2_3 = server2.issue_command(&ServerCommand::Load(String::from("res://TestLoadedFile.gd"))).unwrap();
    assert_eq!(response2_3, "0");
    let response2_4 = server2.issue_command(&ServerCommand::Eval(String::from(r#"MAIN.loaded_files[0].example()"#))).unwrap();
    assert_eq!(response2_4, "\"Test succeeded\"");

    server2.shutdown().unwrap();

  }

}
