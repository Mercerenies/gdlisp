
//! Functionality for constructing and interacting with a Godot server
//! process.
//!
//! This module provides [`MacroServer`], for spawning Godot server
//! processes. A `MacroServer` can be sent [`ServerCommand`] commands
//! and will receive responses in the form of [`ServerResponse`].
//!
//! In addition to the primitive interface, this module also provides
//! submodules for richer server interaction. [`lazy`] provides a
//! means of lazily constructing a macro server, delaying construction
//! of the child process until absolutely necessary.
//! [`named_file_server`] provides a high-level API for providing
//! macros and other resources to a server and for calling macros on a
//! server.

pub mod command;
pub mod response;
pub mod lazy;
pub mod named_file_server;

use super::godot::GodotCommand;
use crate::gdscript::library;

use command::ServerCommand;
use response::ServerResponse;

use std::io::{self, Write, Read, ErrorKind};
use std::process::{Child, Stdio, ExitStatus};
use std::net::{TcpListener, TcpStream};
use std::convert::{TryFrom, TryInto};
use std::mem::ManuallyDrop;
use std::sync::{Mutex, MutexGuard};
use std::env::current_exe;

/// The TCP port used for communicating with the macro server.
pub const DEFAULT_PORT_NUMBER: u16 = 61992;

lazy_static! {
  static ref MACRO_SERVER_LOCK: Mutex<()> = Mutex::new(());
}

/// A `MacroServer` instance manages a child Godot process and a TCP
/// connection to that process. Using this instance, callers can send
/// commands and receive responses via
/// [`issue_command`](MacroServer::issue_command).
pub struct MacroServer {
  tcp_server: TcpStream,
  godot_server: Child,
}

impl MacroServer {

  /// Construct a new `MacroServer`. This function immediately spawns
  /// a child process. For a lazily-constructed server that only
  /// spawns once necessary, consider using [`lazy::LazyServer`]
  /// instead.
  pub fn new() -> io::Result<MacroServer> {
    Self::new_on_ports(DEFAULT_PORT_NUMBER, u16::MAX)
  }

  pub fn new_on_ports(min_port: u16, max_port: u16) -> io::Result<MacroServer> {

    // This mutex protects our access to GDLisp.gd. Especially when
    // testing (but also just in general), multiple threads can easily
    // try to run the fs::copy command below at the same time,
    // resulting in an inconsistent state of the GDLisp.gd file. With
    // this mutex, the following steps are atomic:
    //
    // 1. Initialize stdlib (if uninitialized)
    // 2. Copy the GDLisp.gd file into the subdirectory
    // 3. Load Godot and allow it to read GDLisp.gd
    //
    // No other thread can start copying GDLisp.gd until our current
    // thread allows its subprocess to parse it fully.
    let _lock_guard = MacroServer::lock_macro_server_init()?;

    library::ensure_stdlib_loaded();
    let (tcp_listener, port) = MacroServer::try_to_bind_port(min_port, max_port)?;
    let gd_server = run_godot_child_process(port)?;
    let (tcp_server, _) = tcp_listener.accept()?;
    Ok(MacroServer {
      tcp_server: tcp_server,
      godot_server: gd_server,
    })
  }

  fn lock_macro_server_init() -> io::Result<MutexGuard<'static, ()>> {
    MACRO_SERVER_LOCK.lock().map_err(|_| {
      io::Error::new(io::ErrorKind::Other, "MACRO_SERVER_LOCK was poisoned")
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
    buf.extend(len.to_be_bytes());
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

  /// Issues the given command to the macro server, waits on a
  /// response, and returns a [`ServerResponse`] indicating success or
  /// failure.
  pub fn issue_command(&mut self, command: &ServerCommand) -> io::Result<ServerResponse> {
    let json = command.to_json();
    self.send_string(&json.to_string())?;

    let result = self.receive_string()?;
    let result = json::parse(&result).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;
    ServerResponse::try_from(result).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))
  }

  // Unsafe to use the server afterward, so we only expose a public
  // version that takes ownership.
  fn _shutdown(&mut self) -> io::Result<ExitStatus> {
    self.issue_command(&ServerCommand::Quit)?;
    self.godot_server.wait()
  }

  /// Shuts down the server process. This is equivalent to simply
  /// dropping `self` except that this method allows the caller to
  /// handle any error conditions that arise from shutting down the
  /// server.
  pub fn shutdown(self) -> io::Result<ExitStatus> {
    let mut server = ManuallyDrop::new(self);
    server._shutdown()
  }

  /// Returns true if the Godot subprocess is running and is healthy.
  /// If the process has terminated for any reason or the GDLisp
  /// process cannot communicate with it, then this function returns false.
  pub fn is_process_healthy(&mut self) -> bool {
    match self.godot_server.try_wait() {
      Ok(Some(_)) => {
        // Process has terminated.
        false
      }
      Ok(None) => {
        // Process has not terminated.
        true
      }
      Err(_) => {
        // Error occurred trying to get process status; assume
        // unhealthy.
        false
      }
    }
  }

}

/// Dropping a `MacroServer` kills the child process, suppressing any
/// errors that result from doing so.
impl Drop for MacroServer {

  fn drop(&mut self) {
    let _r = self._shutdown(); // Ignore io::Result (we're in Drop so we can't handle it)
  }

}

fn run_godot_child_process(port: u16) -> io::Result<Child> {
  let exe_path = current_exe()?;
  let exe_dir = exe_path.parent().ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Could not locate executable path"))?;
  GodotCommand::base()
    .project_dir(&exe_dir.join("MacroServer"))
    .env("GDLISP_PORT_NUMBER", port.to_string())
    .stdout(Stdio::null())
    .spawn()
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::reify::Reify;
  use crate::AST_PARSER;
  use json::object;
  use std::convert::TryFrom;
  use std::thread::sleep;
  use std::time::Duration;

  fn issue_command_and_unwrap(server: &mut MacroServer, value: &ServerCommand) -> String {
    let result = server.issue_command(value).unwrap();
    String::try_from(result).unwrap()
  }

  fn roundtrip_value(server: &mut MacroServer, value: &str) {
    let ast = AST_PARSER.parse(value).unwrap();
    let result = issue_command_and_unwrap(server, &ServerCommand::Eval(ast.reify().to_gd()));
    assert_eq!(value, &result);
  }

  #[test]
  fn spawn_server_simple_test() {
    MacroServer::new().unwrap().shutdown().unwrap();
  }

  #[test]
  fn spawn_server_ping_pong_test() {
    let mut server = MacroServer::new().unwrap();
    let response = issue_command_and_unwrap(&mut server, &ServerCommand::Ping);
    assert_eq!(response, "pong");
    server.shutdown().unwrap();
  }

  #[test]
  fn spawn_server_eval_test() {
    let mut server = MacroServer::new().unwrap();
    let response = issue_command_and_unwrap(&mut server, &ServerCommand::Eval(String::from("1 + 1")));
    assert_eq!(response, "2");
  }

  #[test]
  fn spawn_server_roundtrip_test_1() {
    let mut server = MacroServer::new().unwrap();
    roundtrip_value(&mut server, "(1)");
    roundtrip_value(&mut server, "(1 2 . 3)");
    roundtrip_value(&mut server, "(1 . 2)");
    roundtrip_value(&mut server, "(array #t #f abc def)");
    roundtrip_value(&mut server, "(array 10 20 (30 40) \"ABC\")");
    roundtrip_value(&mut server, "(array 10 20 (30 40 . 50) \"ABC\")");
    roundtrip_value(&mut server, "(array 10 20 (30 40 50 (60 70)) \"ABC\")");
  }

  #[test]
  fn spawn_server_roundtrip_test_2() {
    let mut server = MacroServer::new().unwrap();
    roundtrip_value(&mut server, "\"ABC\"");
    roundtrip_value(&mut server, "\"αβγ ⊕\"");
    roundtrip_value(&mut server, "\"😁😁😁😁😁😁\"");
    roundtrip_value(&mut server, r#""abc\"def""#);
    roundtrip_value(&mut server, r#""''\"'\"'""#);
    roundtrip_value(&mut server, r#""\\""#);
    roundtrip_value(&mut server, r#""\"\\\"""#);
    roundtrip_value(&mut server, r#""\\\\\\""#);
  }

  #[test]
  fn spawn_server_roundtrip_test_3() {
    let mut server = MacroServer::new().unwrap();
    roundtrip_value(&mut server, r#""\n\t\t\n""#);
    roundtrip_value(&mut server, r#""\r\r\r\r""#);
    roundtrip_value(&mut server, r#""abc \a def""#);
    roundtrip_value(&mut server, r#""\t\v\f\f""#);
    roundtrip_value(&mut server, r#""xxx\b\b\b""#);
    roundtrip_value(&mut server, r#""\v""#);
    roundtrip_value(&mut server, r#""\\v""#);
    roundtrip_value(&mut server, r#""\\\v""#);
    roundtrip_value(&mut server, r#""\\\\v""#);
  }

  #[test]
  fn spawn_server_load_test() {
    let mut server = MacroServer::new().unwrap();
    let load_response = issue_command_and_unwrap(&mut server, &ServerCommand::Load(String::from("res://TestLoadedFile.gd")));
    assert_eq!(load_response, "0");
    let eval_response = issue_command_and_unwrap(&mut server, &ServerCommand::Eval(String::from(r#"MAIN.loaded_files[0].example()"#)));
    assert_eq!(eval_response, "\"Test succeeded\"");
    server.shutdown().unwrap();
  }

  #[test]
  fn spawn_server_exec_test() {
    let mut server = MacroServer::new().unwrap();
    let command = ServerCommand::Exec(String::from("    var tmp_var = 1 + 1\n    return tmp_var"));
    let response = issue_command_and_unwrap(&mut server, &command);
    assert_eq!(response, "2");
  }

  #[test]
  fn spawn_server_bad_json_test() {
    let mut server = MacroServer::new().unwrap();
    let command = "[{INVALID_JSON";
    server.send_string(command).unwrap();
    let response = server.receive_string().unwrap();
    let response = json::parse(&response).unwrap();
    let expected_response =
      object!{
        "error_code" => 30, // ERR_INVALID_DATA
        "error_string" => "Invalid JSON Expected key",
        "response_string" => "",
      };
    assert_eq!(response, expected_response);
  }

  #[test]
  fn healthy_server_test() {
    let mut server = MacroServer::new().unwrap();
    assert!(server.is_process_healthy());
  }

  #[test]
  #[ignore = "Race condition (see issue #127)"]
  fn unhealthy_server_test() {
    let mut server = MacroServer::new().unwrap();
    let command = ServerCommand::Eval(String::from("GDLisp.get_tree().quit()"));
    issue_command_and_unwrap(&mut server, &command);

    // The quit command will terminate the Godot subprocess within
    // 1/60th of a second (Godot will terminate at the end of the
    // current frame, and the default framerate is 60 frames per
    // second). So we'll wait 300 milliseconds to make sure it has
    // time to terminate.
    sleep(Duration::from_millis(300));

    assert!(!server.is_process_healthy());
  }

}
