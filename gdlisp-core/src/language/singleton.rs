
use super::GDLispScriptLanguage;

use godot::prelude::*;
use godot::engine::Engine;
use lazy_static::lazy_static;

use std::sync::Mutex;

#[derive(Debug, Default)]
struct GDLispScriptLanguageSingleton {
  instance: Option<Gd<GDLispScriptLanguage>>,
}

// Welcome, future Silvio. If you're looking at this file, something
// horrible has broken. I'm guessing the year is 2024, but if it's
// later than that then I'm impressed. I apologize for writing this
// unsafe impl. Good luck fixing it :)
//
// Sincerely, May 8 2023 Silvio
unsafe impl Send for GDLispScriptLanguageSingleton {}

lazy_static! {
    static ref LANGUAGE_SINGLETON: Mutex<GDLispScriptLanguageSingleton> =
      Mutex::new(GDLispScriptLanguageSingleton::new());
}

impl GDLispScriptLanguageSingleton {

  fn new() -> Self {
    Self::default()
  }

}

pub(super) fn singleton() -> Gd<GDLispScriptLanguage> {
  let mut lock = LANGUAGE_SINGLETON.lock().unwrap();
  if let Some(script_language) = lock.instance.as_ref() {
    script_language.clone()
  } else {
    println!("A");
    let script_language = GDLispScriptLanguage::alloc_gd();
    println!("B");
    Engine::singleton().register_script_language(script_language.clone().upcast());
    println!("C");
    lock.instance = Some(script_language.clone());
    println!("D");
    script_language
  }
}

pub(super) fn free_singleton() {
  let mut lock = LANGUAGE_SINGLETON.lock().unwrap();
  if let Some(script_language) = lock.instance.take() {
    Engine::singleton().unregister_script_language(script_language.clone().upcast());
    script_language.free();
  }
}
