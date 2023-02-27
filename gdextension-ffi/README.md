
# gdextension-ffi

The parts of GDLisp that communicate with the GDExtension API.

## Build Parameters

The build process accepts an optional environment variable:
``GDLISP_BUILD_CONFIGURATION``. If provided, it must be one of the
Godot build configurations: ``float_32``, ``float_64``, ``double_32``,
or ``double_64``. It **must** match the configuration with which Godot
was built. The default is ``float_64``. If you don't know what any of
this means, the default is probably fine in your case.
