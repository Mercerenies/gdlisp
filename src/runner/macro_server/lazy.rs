
use super::MacroServer;

use std::io;
use std::process::ExitStatus;

pub struct LazyServer(Option<MacroServer>);

// Default doesn't make as much sense for this type, as it's meant to
// behave like a server, not a simple datatype. The fact that it *can*
// be default-allocated is an implementation detail.
#[allow(clippy::new_without_default)]
impl LazyServer {

  pub fn new() -> LazyServer {
    LazyServer(None)
  }

  pub fn get_mut(&mut self) -> io::Result<&mut MacroServer> {
    if self.0.is_none() {
      self.0 = Some(MacroServer::new()?);
    }
    Ok(self.0.as_mut().unwrap())
  }

  pub fn shutdown(self) -> io::Result<Option<ExitStatus>> {
    match self.0 {
      None => Ok(None),
      Some(x) => x.shutdown().map(Some),
    }
  }

}
