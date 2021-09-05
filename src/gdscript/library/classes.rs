
//! A list of all built-in GDScript classes.

// TODO Should we make the Mono-only names conditionally available? It seems CSharpScript and GodotSharp are the relevant names that don't exist in non-Mono environments

/// A list of all built-in GDScript classes which are always available
/// from the top-level scope.
///
/// This list comes from the Godot Docs and is reproduced here under CC-BY 3.0.
///
/// © Copyright 2014-2020, Juan Linietsky, Ariel Manzur and the Godot community (CC-BY 3.0) Revision 3239b4b5.
///
/// [https://docs.godotengine.org/en/stable/classes/index.html]
///
/// Also note: `@GlobalScope` and `@GDScript` are not included here, as we
/// cannot access them directly from GDScript anyway.
///
/// This list is not perfect. There are some things here that are not
/// first-class objects on the Godot side. I already removed a few
/// (`int`, `float`, `Vector2`, `Vector3`, etc.), but I'm sure there are more
/// that need to be dealt with.
pub static GDSCRIPT_CLASS_NAMES: [&str; 622] = [
//"AABB",
  "AcceptDialog",
  "AnimatedSprite",
  "AnimatedSprite3D",
  "AnimatedTexture",
  "Animation",
  "AnimationNode",
  "AnimationNodeAdd2",
  "AnimationNodeAdd3",
  "AnimationNodeAnimation",
  "AnimationNodeBlend2",
  "AnimationNodeBlend3",
  "AnimationNodeBlendSpace1D",
  "AnimationNodeBlendSpace2D",
  "AnimationNodeBlendTree",
  "AnimationNodeOneShot",
  "AnimationNodeOutput",
  "AnimationNodeStateMachine",
  "AnimationNodeStateMachinePlayback",
  "AnimationNodeStateMachineTransition",
  "AnimationNodeTimeScale",
  "AnimationNodeTimeSeek",
  "AnimationNodeTransition",
  "AnimationPlayer",
  "AnimationRootNode",
  "AnimationTrackEditPlugin",
  "AnimationTree",
  "AnimationTreePlayer",
  "Area",
  "Area2D",
//"Array",
  "ArrayMesh",
  "ARVRAnchor",
  "ARVRCamera",
  "ARVRController",
  "ARVRInterface",
  "ARVRInterfaceGDNative",
  "ARVROrigin",
  "ARVRPositionalTracker",
  "ARVRServer",
  "AStar",
  "AStar2D",
  "AtlasTexture",
  "AudioBusLayout",
  "AudioEffect",
  "AudioEffectAmplify",
  "AudioEffectBandLimitFilter",
  "AudioEffectBandPassFilter",
  "AudioEffectChorus",
  "AudioEffectCompressor",
  "AudioEffectDelay",
  "AudioEffectDistortion",
  "AudioEffectEQ",
  "AudioEffectEQ10",
  "AudioEffectEQ21",
  "AudioEffectEQ6",
  "AudioEffectFilter",
  "AudioEffectHighPassFilter",
  "AudioEffectHighShelfFilter",
  "AudioEffectInstance",
  "AudioEffectLimiter",
  "AudioEffectLowPassFilter",
  "AudioEffectLowShelfFilter",
  "AudioEffectNotchFilter",
  "AudioEffectPanner",
  "AudioEffectPhaser",
  "AudioEffectPitchShift",
  "AudioEffectRecord",
  "AudioEffectReverb",
  "AudioEffectSpectrumAnalyzer",
  "AudioEffectSpectrumAnalyzerInstance",
  "AudioEffectStereoEnhance",
  "AudioServer",
  "AudioStream",
  "AudioStreamGenerator",
  "AudioStreamGeneratorPlayback",
  "AudioStreamMicrophone",
  "AudioStreamMP3",
  "AudioStreamOGGVorbis",
  "AudioStreamPlayback",
  "AudioStreamPlaybackResampled",
  "AudioStreamPlayer",
  "AudioStreamPlayer2D",
  "AudioStreamPlayer3D",
  "AudioStreamRandomPitch",
  "AudioStreamSample",
  "BackBufferCopy",
  "BakedLightmap",
  "BakedLightmapData",
  "BaseButton",
//"Basis",
  "BitMap",
  "BitmapFont",
  "Bone2D",
  "BoneAttachment",
//"bool",
  "BoxContainer",
  "BoxShape",
  "BulletPhysicsServer",
  "Button",
  "ButtonGroup",
  "Camera",
  "Camera2D",
  "CameraFeed",
  "CameraServer",
  "CameraTexture",
  "CanvasItem",
  "CanvasItemMaterial",
  "CanvasLayer",
  "CanvasModulate",
  "CapsuleMesh",
  "CapsuleShape",
  "CapsuleShape2D",
  "CenterContainer",
  "CharFXTransform",
  "CheckBox",
  "CheckButton",
  "CircleShape2D",
  "ClassDB",
  "ClippedCamera",
  "CollisionObject",
  "CollisionObject2D",
  "CollisionPolygon",
  "CollisionPolygon2D",
  "CollisionShape",
  "CollisionShape2D",
//"Color",
  "ColorPicker",
  "ColorPickerButton",
  "ColorRect",
  "ConcavePolygonShape",
  "ConcavePolygonShape2D",
  "ConeTwistJoint",
  "ConfigFile",
  "ConfirmationDialog",
  "Container",
  "Control",
  "ConvexPolygonShape",
  "ConvexPolygonShape2D",
  "CPUParticles",
  "CPUParticles2D",
  "Crypto",
  "CryptoKey",
  "CSGBox",
  "CSGCombiner",
  "CSGCylinder",
  "CSGMesh",
  "CSGPolygon",
  "CSGPrimitive",
  "CSGShape",
  "CSGSphere",
  "CSGTorus",
//"CSharpScript",
  "CubeMap",
  "CubeMesh",
  "Curve",
  "Curve2D",
  "Curve3D",
  "CurveTexture",
  "CylinderMesh",
  "CylinderShape",
  "DampedSpringJoint2D",
//"Dictionary",
  "DirectionalLight",
  "Directory",
  "DTLSServer",
  "DynamicFont",
  "DynamicFontData",
  "EditorExportPlugin",
  "EditorFeatureProfile",
  "EditorFileDialog",
  "EditorFileSystem",
  "EditorFileSystemDirectory",
  "EditorImportPlugin",
  "EditorInspector",
  "EditorInspectorPlugin",
  "EditorInterface",
  "EditorNavigationMeshGenerator",
  "EditorPlugin",
  "EditorProperty",
  "EditorResourceConversionPlugin",
  "EditorResourcePreview",
  "EditorResourcePreviewGenerator",
  "EditorSceneImporter",
  "EditorSceneImporterFBX",
  "EditorScenePostImport",
  "EditorScript",
  "EditorSelection",
  "EditorSettings",
  "EditorSpatialGizmo",
  "EditorSpatialGizmoPlugin",
  "EditorSpinSlider",
  "EditorVCSInterface",
  "EncodedObjectAsID",
  "Engine",
  "Environment",
  "Expression",
  "ExternalTexture",
  "File",
  "FileDialog",
  "FileSystemDock",
//"float",
  "Font",
  "FuncRef",
  "GDNative",
  "GDNativeLibrary",
  "GDScript",
  "GDScriptFunctionState",
  "Generic6DOFJoint",
  "Geometry",
  "GeometryInstance",
  "GIProbe",
  "GIProbeData",
//"GodotSharp",
  "Gradient",
  "GradientTexture",
  "GraphEdit",
  "GraphNode",
  "GridContainer",
  "GridMap",
  "GrooveJoint2D",
  "HashingContext",
  "HBoxContainer",
  "HeightMapShape",
  "HingeJoint",
  "HScrollBar",
  "HSeparator",
  "HSlider",
  "HSplitContainer",
  "HTTPClient",
  "HTTPRequest",
  "Image",
  "ImageTexture",
  "ImmediateGeometry",
  "Input",
  "InputEvent",
  "InputEventAction",
  "InputEventGesture",
  "InputEventJoypadButton",
  "InputEventJoypadMotion",
  "InputEventKey",
  "InputEventMagnifyGesture",
  "InputEventMIDI",
  "InputEventMouse",
  "InputEventMouseButton",
  "InputEventMouseMotion",
  "InputEventPanGesture",
  "InputEventScreenDrag",
  "InputEventScreenTouch",
  "InputEventWithModifiers",
  "InputMap",
  "InstancePlaceholder",
//"int",
  "InterpolatedCamera",
  "IP",
  "ItemList",
  "JavaClass",
  "JavaClassWrapper",
  "JavaScript",
  "JNISingleton",
  "Joint",
  "Joint2D",
  "JSON",
  "JSONParseResult",
  "JSONRPC",
  "KinematicBody",
  "KinematicBody2D",
  "KinematicCollision",
  "KinematicCollision2D",
  "Label",
  "LargeTexture",
  "Light",
  "Light2D",
  "LightOccluder2D",
  "Line2D",
  "LineEdit",
  "LineShape2D",
  "LinkButton",
  "Listener",
  "MainLoop",
  "MarginContainer",
  "Marshalls",
  "Material",
  "MenuButton",
  "Mesh",
  "MeshDataTool",
  "MeshInstance",
  "MeshInstance2D",
  "MeshLibrary",
  "MeshTexture",
  "MobileVRInterface",
  "MultiMesh",
  "MultiMeshInstance",
  "MultiMeshInstance2D",
  "MultiplayerAPI",
  "MultiplayerPeerGDNative",
  "Mutex",
  "NativeScript",
  "Navigation",
  "Navigation2D",
  "NavigationMesh",
  "NavigationMeshInstance",
  "NavigationPolygon",
  "NavigationPolygonInstance",
  "NetworkedMultiplayerENet",
  "NetworkedMultiplayerPeer",
  "NinePatchRect",
  "Node",
  "Node2D",
//"NodePath",
  "NoiseTexture",
//"Object",
  "OccluderPolygon2D",
  "OmniLight",
  "OpenSimplexNoise",
  "OptionButton",
  "OS",
  "PackedDataContainer",
  "PackedDataContainerRef",
  "PackedScene",
  "PacketPeer",
  "PacketPeerDTLS",
  "PacketPeerGDNative",
  "PacketPeerStream",
  "PacketPeerUDP",
  "Panel",
  "PanelContainer",
  "PanoramaSky",
  "ParallaxBackground",
  "ParallaxLayer",
  "Particles",
  "Particles2D",
  "ParticlesMaterial",
  "Path",
  "Path2D",
  "PathFollow",
  "PathFollow2D",
  "PCKPacker",
  "Performance",
  "PHashTranslation",
  "PhysicalBone",
  "Physics2DDirectBodyState",
  "Physics2DDirectSpaceState",
  "Physics2DServer",
  "Physics2DShapeQueryParameters",
  "Physics2DShapeQueryResult",
  "Physics2DTestMotionResult",
  "PhysicsBody",
  "PhysicsBody2D",
  "PhysicsDirectBodyState",
  "PhysicsDirectSpaceState",
  "PhysicsMaterial",
  "PhysicsServer",
  "PhysicsShapeQueryParameters",
  "PhysicsShapeQueryResult",
  "PinJoint",
  "PinJoint2D",
//"Plane",
  "PlaneMesh",
  "PlaneShape",
  "PluginScript",
  "PointMesh",
  "Polygon2D",
  "PolygonPathFinder",
//"PoolByteArray",
//"PoolColorArray",
//"PoolIntArray",
//"PoolRealArray",
//"PoolStringArray",
//"PoolVector2Array",
//"PoolVector3Array",
  "Popup",
  "PopupDialog",
  "PopupMenu",
  "PopupPanel",
  "Position2D",
  "Position3D",
  "PrimitiveMesh",
  "PrismMesh",
  "ProceduralSky",
  "ProgressBar",
  "ProjectSettings",
  "ProximityGroup",
  "ProxyTexture",
  "QuadMesh",
//"Quat",
  "RandomNumberGenerator",
  "Range",
  "RayCast",
  "RayCast2D",
  "RayShape",
  "RayShape2D",
//"Rect2",
  "RectangleShape2D",
  "Reference",
  "ReferenceRect",
  "ReflectionProbe",
  "RegEx",
  "RegExMatch",
  "RemoteTransform",
  "RemoteTransform2D",
  "Resource",
  "ResourceFormatLoader",
  "ResourceFormatSaver",
  "ResourceImporter",
  "ResourceInteractiveLoader",
  "ResourceLoader",
  "ResourcePreloader",
  "ResourceSaver",
  "RichTextEffect",
  "RichTextLabel",
//"RID",
  "RigidBody",
  "RigidBody2D",
  "RootMotionView",
  "SceneState",
  "SceneTree",
  "SceneTreeTimer",
  "Script",
  "ScriptCreateDialog",
  "ScriptEditor",
  "ScrollBar",
  "ScrollContainer",
  "SegmentShape2D",
  "Semaphore",
  "Separator",
  "Shader",
  "ShaderMaterial",
  "Shape",
  "Shape2D",
  "ShortCut",
  "Skeleton",
  "Skeleton2D",
  "SkeletonIK",
  "Skin",
  "SkinReference",
  "Sky",
  "Slider",
  "SliderJoint",
  "SoftBody",
  "Spatial",
  "SpatialGizmo",
  "SpatialMaterial",
  "SpatialVelocityTracker",
  "SphereMesh",
  "SphereShape",
  "SpinBox",
  "SplitContainer",
  "SpotLight",
  "SpringArm",
  "Sprite",
  "Sprite3D",
  "SpriteBase3D",
  "SpriteFrames",
  "StaticBody",
  "StaticBody2D",
  "StreamPeer",
  "StreamPeerBuffer",
  "StreamPeerGDNative",
  "StreamPeerSSL",
  "StreamPeerTCP",
  "StreamTexture",
//"String",
  "StyleBox",
  "StyleBoxEmpty",
  "StyleBoxFlat",
  "StyleBoxLine",
  "StyleBoxTexture",
  "SurfaceTool",
  "TabContainer",
  "Tabs",
  "TCP_Server",
  "TextEdit",
  "TextFile",
  "Texture",
  "Texture3D",
  "TextureArray",
  "TextureButton",
  "TextureLayered",
  "TextureProgress",
  "TextureRect",
  "Theme",
  "Thread",
  "TileMap",
  "TileSet",
  "Timer",
  "ToolButton",
  "TouchScreenButton",
//"Transform",
//"Transform2D",
  "Translation",
  "TranslationServer",
  "Tree",
  "TreeItem",
  "TriangleMesh",
  "Tween",
  "UDPServer",
  "UndoRedo",
  "UPNP",
  "UPNPDevice",
//"Variant",
  "VBoxContainer",
//"Vector2",
//"Vector3",
  "VehicleBody",
  "VehicleWheel",
  "VideoPlayer",
  "VideoStream",
  "VideoStreamGDNative",
  "VideoStreamTheora",
  "VideoStreamWebm",
  "Viewport",
  "ViewportContainer",
  "ViewportTexture",
  "VisibilityEnabler",
  "VisibilityEnabler2D",
  "VisibilityNotifier",
  "VisibilityNotifier2D",
  "VisualInstance",
  "VisualScript",
  "VisualScriptBasicTypeConstant",
  "VisualScriptBuiltinFunc",
  "VisualScriptClassConstant",
  "VisualScriptComment",
  "VisualScriptComposeArray",
  "VisualScriptCondition",
  "VisualScriptConstant",
  "VisualScriptConstructor",
  "VisualScriptCustomNode",
  "VisualScriptDeconstruct",
  "VisualScriptEditor",
  "VisualScriptEmitSignal",
  "VisualScriptEngineSingleton",
  "VisualScriptExpression",
  "VisualScriptFunction",
  "VisualScriptFunctionCall",
  "VisualScriptFunctionState",
  "VisualScriptGlobalConstant",
  "VisualScriptIndexGet",
  "VisualScriptIndexSet",
  "VisualScriptInputAction",
  "VisualScriptIterator",
  "VisualScriptLists",
  "VisualScriptLocalVar",
  "VisualScriptLocalVarSet",
  "VisualScriptMathConstant",
  "VisualScriptNode",
  "VisualScriptOperator",
  "VisualScriptPreload",
  "VisualScriptPropertyGet",
  "VisualScriptPropertySet",
  "VisualScriptResourcePath",
  "VisualScriptReturn",
  "VisualScriptSceneNode",
  "VisualScriptSceneTree",
  "VisualScriptSelect",
  "VisualScriptSelf",
  "VisualScriptSequence",
  "VisualScriptSubCall",
  "VisualScriptSwitch",
  "VisualScriptTypeCast",
  "VisualScriptVariableGet",
  "VisualScriptVariableSet",
  "VisualScriptWhile",
  "VisualScriptYield",
  "VisualScriptYieldSignal",
  "VisualServer",
  "VisualShader",
  "VisualShaderNode",
  "VisualShaderNodeBooleanConstant",
  "VisualShaderNodeBooleanUniform",
  "VisualShaderNodeColorConstant",
  "VisualShaderNodeColorFunc",
  "VisualShaderNodeColorOp",
  "VisualShaderNodeColorUniform",
  "VisualShaderNodeCompare",
  "VisualShaderNodeCubeMap",
  "VisualShaderNodeCubeMapUniform",
  "VisualShaderNodeCustom",
  "VisualShaderNodeDeterminant",
  "VisualShaderNodeDotProduct",
  "VisualShaderNodeExpression",
  "VisualShaderNodeFaceForward",
  "VisualShaderNodeFresnel",
  "VisualShaderNodeGlobalExpression",
  "VisualShaderNodeGroupBase",
  "VisualShaderNodeIf",
  "VisualShaderNodeInput",
  "VisualShaderNodeIs",
  "VisualShaderNodeOuterProduct",
  "VisualShaderNodeOutput",
  "VisualShaderNodeScalarClamp",
  "VisualShaderNodeScalarConstant",
  "VisualShaderNodeScalarDerivativeFunc",
  "VisualShaderNodeScalarFunc",
  "VisualShaderNodeScalarInterp",
  "VisualShaderNodeScalarOp",
  "VisualShaderNodeScalarSmoothStep",
  "VisualShaderNodeScalarSwitch",
  "VisualShaderNodeScalarUniform",
  "VisualShaderNodeSwitch",
  "VisualShaderNodeTexture",
  "VisualShaderNodeTextureUniform",
  "VisualShaderNodeTextureUniformTriplanar",
  "VisualShaderNodeTransformCompose",
  "VisualShaderNodeTransformConstant",
  "VisualShaderNodeTransformDecompose",
  "VisualShaderNodeTransformFunc",
  "VisualShaderNodeTransformMult",
  "VisualShaderNodeTransformUniform",
  "VisualShaderNodeTransformVecMult",
  "VisualShaderNodeUniform",
  "VisualShaderNodeUniformRef",
  "VisualShaderNodeVec3Constant",
  "VisualShaderNodeVec3Uniform",
  "VisualShaderNodeVectorClamp",
  "VisualShaderNodeVectorCompose",
  "VisualShaderNodeVectorDecompose",
  "VisualShaderNodeVectorDerivativeFunc",
  "VisualShaderNodeVectorDistance",
  "VisualShaderNodeVectorFunc",
  "VisualShaderNodeVectorInterp",
  "VisualShaderNodeVectorLen",
  "VisualShaderNodeVectorOp",
  "VisualShaderNodeVectorRefract",
  "VisualShaderNodeVectorScalarMix",
  "VisualShaderNodeVectorScalarSmoothStep",
  "VisualShaderNodeVectorScalarStep",
  "VisualShaderNodeVectorSmoothStep",
  "VScrollBar",
  "VSeparator",
  "VSlider",
  "VSplitContainer",
  "WeakRef",
  "WebRTCDataChannel",
  "WebRTCDataChannelGDNative",
  "WebRTCMultiplayer",
  "WebRTCPeerConnection",
  "WebRTCPeerConnectionGDNative",
  "WebSocketClient",
  "WebSocketMultiplayerPeer",
  "WebSocketPeer",
  "WebSocketServer",
  "WebXRInterface",
  "WindowDialog",
  "World",
  "World2D",
  "WorldEnvironment",
  "X509Certificate",
  "XMLParser",
  "YSort",
];
