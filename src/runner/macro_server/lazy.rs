
use super::MacroServer;

use std::io;

pub struct LazyServer(Option<MacroServer>);

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

}
