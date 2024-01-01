
mod ser_test;

use godot::init::{gdextension, ExtensionLibrary};

struct Tests;

#[gdextension(entry_point=tests_init)]
unsafe impl ExtensionLibrary for Tests {}
