
// Normally, we resolve preloads relative to the project root.
// However, for macro loading, we may end up needing to load using an
// alternative translation scheme, probably to some temporary
// directory.

use crate::runner::path::RPathBuf;

#[derive(Debug, Clone)]
pub struct DefaultPreloadResolver;

pub trait PreloadResolver {
  fn resolve_preload(&self, path: &RPathBuf) -> Option<String>;
}

impl PreloadResolver for DefaultPreloadResolver {

  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    Some(path.to_string())
  }

}
