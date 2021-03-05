
// Normally, we resolve preloads relative to the project root.
// However, for macro loading, we may end up needing to load using an
// alternative translation scheme, probably to some temporary
// directory.

use crate::runner::path::RPathBuf;

use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct DefaultPreloadResolver;

#[derive(Debug, Clone)]
pub struct LookupPreloadResolver(pub HashMap<PathBuf, PathBuf>);

pub trait PreloadResolver {
  fn resolve_preload(&self, path: &RPathBuf) -> Option<String>;
}

impl PreloadResolver for DefaultPreloadResolver {

  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    Some(path.to_string())
  }

}

impl PreloadResolver for LookupPreloadResolver {

  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    // TODO This to_string-ish thing is the same as in Display for RPathBuf; make a common helper function
    self.0.get(path.path()).map(|x| x.to_string_lossy().replace("\\", "/"))
  }

}
