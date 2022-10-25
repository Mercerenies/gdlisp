
//! Provides [`LazyServer`] for lazily constructing
//! [`MacroServer`](super::MacroServer) instances.

use super::MacroServer;

use std::io;
use std::process::ExitStatus;

/// A macro server that may or may not have been initialized yet.
///
/// A `LazyServer` is similar to a
/// [`Lazy<MacroServer>`](https://doc.rust-lang.org/std/lazy/struct.Lazy.html),
/// in that it doesn't actually construct a macro server until the
/// value is actually forced. This provides the key benefit that the
/// server will not be forced as soon as the compiler starts up,
/// instead waiting until it's actually required for macro resolution,
/// macro definition, or for storage of a completed file.
///
/// Note that, unlike `Lazy`, this type does *not* provide
/// [`Deref`](std::ops::Deref). Macro servers must be explicitly
/// queried with [`LazyServer::get_mut`]. This is because the macro server
/// construction is a nontrivial operation, so we require the user to
/// clearly assert that they understand the cost.
pub struct LazyServer(Option<MacroServer>);

// **Note:** [`Default`] doesn't make as much sense for this type, as
// it's meant to behave like a server, not a simple datatype. The fact
// that it *can* be default-allocated is an implementation detail.
#[allow(clippy::new_without_default)]
impl LazyServer {

  /// Construct a new, empty `LazyServer`. No additional processes
  /// will be spawned.
  pub fn new() -> LazyServer {
    LazyServer(None)
  }

  /// Query whether or not the server has been started.
  pub fn is_running(&self) -> bool {
    self.0.is_some()
  }

  /// Get the macro server, starting a new server process if one has
  /// not been started yet.
  ///
  /// If the server has already been started
  /// ([`is_running`](LazyServer::is_running) is true), then this
  /// method is guaranteed to succeed. If the server has not been
  /// started yet, then IO errors while spawning the process will be
  /// propagated to the return value of this method. If an error
  /// occurs, the `LazyServer` is guaranteed to be left in an
  /// uninitialized state, and future calls to `get_mut` will attempt
  /// to spawn the process once again.
  pub fn get_mut(&mut self) -> io::Result<&mut MacroServer> {
    if self.0.is_none() {
      self.0 = Some(MacroServer::new()?);
    }
    Ok(self.0.as_mut().unwrap())
  }

  /// Returns the macro server, but only if it has already been
  /// started. If the macro server has not yet been started, then this
  /// will return `None`.
  ///
  /// For a function that lazily initializes the server if it hasn't
  /// been initialized, see [`LazyServer::get_mut`].
  pub fn get_mut_if_initialized(&mut self) -> Option<&mut MacroServer> {
    self.0.as_mut()
  }

  /// Consume the `LazyServer` and shut down the server, if it's
  /// running. This is equivalent to simply dropping the `LazyServer`
  /// instance but allows custom error handling to be implemented.
  ///
  /// If the lazy server is uninitialized, then `Ok(None)` will always
  /// be returned. Otherwise, the server will be shut down via
  /// [`MacroServer::shutdown`] and any errors will be propagated out.
  /// Assuming there are no IO errors during shutdown, the result will
  /// be `Ok(Some(exit_status))`, where `exit_status` is the exit
  /// status of the macro server.
  pub fn shutdown(self) -> io::Result<Option<ExitStatus>> {
    match self.0 {
      None => Ok(None),
      Some(x) => x.shutdown().map(Some),
    }
  }

}
