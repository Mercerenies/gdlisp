
// Normally, we resolve preloads relative to the project root.
// However, for macro loading, we may end up needing to load using an
// alternative translation scheme, probably to some temporary
// directory.

use crate::runner::path::RPathBuf;
use crate::compile::resource_type::ResourceType;

use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct DefaultPreloadResolver;

#[derive(Debug, Clone)]
pub struct LookupPreloadResolver(pub HashMap<PathBuf, PathBuf>);

pub trait PreloadResolver {
  fn resolve_preload(&self, path: &RPathBuf) -> Option<String>;
  fn include_resource(&self, res: ResourceType) -> bool;
}

impl LookupPreloadResolver {

  pub fn insert(&mut self, key: PathBuf, value: PathBuf) -> Option<PathBuf> {
    self.0.insert(key, value)
  }

}

impl PreloadResolver for DefaultPreloadResolver {

  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    Some(path.to_string())
  }

  fn include_resource(&self, _res: ResourceType) -> bool {
    true
  }

}

impl PreloadResolver for LookupPreloadResolver {

  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    self.0.get(path.path()).map(RPathBuf::path_to_string)
  }

  fn include_resource(&self, res: ResourceType) -> bool {
    res == ResourceType::GDLispSource
  }

}
