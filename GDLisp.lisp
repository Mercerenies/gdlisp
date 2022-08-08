
;; Library file for GDLisp. This file must be included as a singleton
;; in any project which uses GDLisp compiled files.

(sys/nostdlib)

(defconst nil ())

(defclass Cons (Reference)
  (defvar car)
  (defvar cdr)
  (defn _init (car cdr)
    (set self:car car)
    (set self:cdr cdr)))

(defclass Function (Reference)
  (defvar __is_gdlisp_function #t)
  (defvar __gdlisp_required 0)
  (defvar __gdlisp_optional 0)
  (defvar __gdlisp_rest 1)
  ;; Constants for the different types of "rest" arguments a function
  ;; can take. (i.e. the valid values for __gdlisp_rest)
  (defconst __gdlisp_vararg_no   0)
  (defconst __gdlisp_vararg_rest 1)
  (defconst __gdlisp_vararg_arr  2))

(defclass Cell (Reference)
  (defvar contents)
  (defn _init (contents)
    (set self:contents contents)))

(defclass Symbol (Reference)
  ;; Note: This will be obsolete once we have StringName in GDScript,
  ;; which seems to be coming in Godot 4. For now, this manual wrapper
  ;; stores symbols in the least efficient way possible.
  (defvar contents)
  (defn _init (contents)
    (set self:contents contents)))

(defclass FreshNameGenerator (Reference)
  ;; This is meant to be identical to FreshNameGenerator in the Rust
  ;; source. We want to be able to consistently generate names in the
  ;; same way on both Rust and Godot and to be able to communicate
  ;; FreshNameGenerator state between the two (via to_json /
  ;; from_json)

  (defconst DEFAULT_PREFIX "_G")

  (defvar reserved)
  (defvar index)

  (defn _init (reserved index) ; NOTE: Lack of arglist modifier
    (set self:reserved reserved)
    (set self:index index))

  (defn generate ()
    (self:generate_with FreshNameGenerator:DEFAULT_PREFIX))

  (defn generate_with (prefix)
    (let ((name ("{}_{}":format [prefix self:index] "{}")))
      (set self:index (+ self:index 1))
      (cond
        ((member? name self:reserved) (self:generate_with prefix))
        (#t name))))

  (defn to_json ()
    {"reserved" self:reserved "index" self:index})

  (defn from_json (json) static
    (FreshNameGenerator:new
     (elt json "reserved")
     (elt json "index"))))

(sys/declare superglobal GDLisp public) ; This file :)

;;; GLOBAL ENUMS ;;;

(sys/bootstrap constants)

(sys/bootstrap constant-enums)

(defclass _GDLisp (Node) main
  (defvar global_name_generator)

  (defvar symbol_table)

  ;; Primitive types
  (defvar Null)
  (defvar Bool)
  (defvar Int)
  (defvar Float)
  (defvar String)
  (defvar Vector2)
  (defvar Rect2)
  (defvar Vector3)
  (defvar Transform2D)
  (defvar Plane)
  (defvar Quat)
  (defvar AABB)
  (defvar Basis)
  (defvar Transform)
  (defvar Color)
  (defvar NodePath)
  (defvar RID)
  (defvar Object)
  (defvar Dictionary)
  (defvar Array)
  (defvar PoolByteArray)
  (defvar PoolIntArray)
  (defvar PoolRealArray)
  (defvar PoolStringArray)
  (defvar PoolVector2Array)
  (defvar PoolVector3Array)
  (defvar PoolColorArray)

  ;; Synthetic types
  (defvar Any)
  (defvar AnyRef)
  (defvar AnyVal)
  (defvar Number)
  (defvar BaseArray)
  (defvar Nothing)

  ;; GDScript native types lookup table
  (defvar native_types_lookup)
  (defvar primitive_types_lookup)

  (defn _init ()
    (set self:global_name_generator (FreshNameGenerator:new [] 0))
    (set self:symbol_table {})

    (set self:Null (PrimitiveType:new TYPE_NIL))
    (set self:Bool (PrimitiveType:new TYPE_BOOL))
    (set self:Int (PrimitiveType:new TYPE_INT))
    (set self:Float (PrimitiveType:new TYPE_REAL))
    (set self:String (PrimitiveType:new TYPE_STRING))
    (set self:Vector2 (PrimitiveType:new TYPE_VECTOR2))
    (set self:Rect2 (PrimitiveType:new TYPE_RECT2))
    (set self:Vector3 (PrimitiveType:new TYPE_VECTOR3))
    (set self:Transform2D (PrimitiveType:new TYPE_TRANSFORM2D))
    (set self:Plane (PrimitiveType:new TYPE_PLANE))
    (set self:Quat (PrimitiveType:new TYPE_QUAT))
    (set self:AABB (PrimitiveType:new TYPE_AABB))
    (set self:Basis (PrimitiveType:new TYPE_BASIS))
    (set self:Transform (PrimitiveType:new TYPE_TRANSFORM))
    (set self:Color (PrimitiveType:new TYPE_COLOR))
    (set self:NodePath (PrimitiveType:new TYPE_NODE_PATH))
    (set self:RID (PrimitiveType:new TYPE_RID))
    (set self:Object (PrimitiveType:new TYPE_OBJECT))
    (set self:Dictionary (PrimitiveType:new TYPE_DICTIONARY))
    (set self:Array (PrimitiveType:new TYPE_ARRAY))
    (set self:PoolByteArray (PrimitiveType:new TYPE_RAW_ARRAY))
    (set self:PoolIntArray (PrimitiveType:new TYPE_INT_ARRAY))
    (set self:PoolRealArray (PrimitiveType:new TYPE_REAL_ARRAY))
    (set self:PoolStringArray (PrimitiveType:new TYPE_STRING_ARRAY))
    (set self:PoolVector2Array (PrimitiveType:new TYPE_VECTOR2_ARRAY))
    (set self:PoolVector3Array (PrimitiveType:new TYPE_VECTOR3_ARRAY))
    (set self:PoolColorArray (PrimitiveType:new TYPE_COLOR_ARRAY))
    (set self:Any (AnyType:new))
    (set self:AnyRef (AnyRefType:new))
    (set self:AnyVal (AnyValType:new))
    (set self:Number (NumberType:new))
    (set self:BaseArray (BaseArrayType:new))
    (set self:Nothing (NothingType:new))

    (set self:primitive_types_lookup
         {TYPE_NIL @Null TYPE_BOOL @Bool TYPE_INT @Int TYPE_REAL @Float TYPE_STRING @String TYPE_VECTOR2 @Vector2
          TYPE_RECT2 @Rect2 TYPE_VECTOR3 @Vector3 TYPE_TRANSFORM2D @Transform2D TYPE_PLANE @Plane TYPE_QUAT @Quat
          TYPE_AABB @AABB TYPE_BASIS @Basis TYPE_TRANSFORM @Transform TYPE_COLOR @Color TYPE_NODE_PATH @NodePath
          TYPE_RID @RID TYPE_OBJECT @Object TYPE_DICTIONARY @Dictionary TYPE_ARRAY @Array
          TYPE_RAW_ARRAY @PoolByteArray TYPE_INT_ARRAY @PoolIntArray TYPE_REAL_ARRAY @PoolRealArray
          TYPE_STRING_ARRAY @PoolStringArray TYPE_VECTOR2_ARRAY @PoolVector2Array
          TYPE_VECTOR3_ARRAY @PoolVector3Array TYPE_COLOR_ARRAY @PoolColorArray})

    (set self:native_types_lookup
         {"AcceptDialog" AcceptDialog "AnimatedSprite" AnimatedSprite "AnimatedSprite3D" AnimatedSprite3D
          "AnimatedTexture" AnimatedTexture "Animation" Animation "AnimationNode" AnimationNode
          "AnimationNodeAdd2" AnimationNodeAdd2 "AnimationNodeAdd3" AnimationNodeAdd3 "AnimationNodeAnimation" AnimationNodeAnimation
          "AnimationNodeBlend2" AnimationNodeBlend2 "AnimationNodeBlend3" AnimationNodeBlend3 "AnimationNodeBlendSpace1D" AnimationNodeBlendSpace1D
          "AnimationNodeBlendSpace2D" AnimationNodeBlendSpace2D "AnimationNodeBlendTree" AnimationNodeBlendTree "AnimationNodeOneShot" AnimationNodeOneShot
          "AnimationNodeOutput" AnimationNodeOutput "AnimationNodeStateMachine" AnimationNodeStateMachine "AnimationNodeStateMachinePlayback" AnimationNodeStateMachinePlayback
          "AnimationNodeStateMachineTransition" AnimationNodeStateMachineTransition "AnimationNodeTimeScale" AnimationNodeTimeScale "AnimationNodeTimeSeek" AnimationNodeTimeSeek
          "AnimationNodeTransition" AnimationNodeTransition "AnimationPlayer" AnimationPlayer "AnimationRootNode" AnimationRootNode
          "AnimationTrackEditPlugin" AnimationTrackEditPlugin "AnimationTree" AnimationTree "AnimationTreePlayer" AnimationTreePlayer
          "Area" Area "Area2D" Area2D "ArrayMesh" ArrayMesh
          "ARVRAnchor" ARVRAnchor "ARVRCamera" ARVRCamera "ARVRController" ARVRController
          "ARVRInterface" ARVRInterface "ARVRInterfaceGDNative" ARVRInterfaceGDNative "ARVROrigin" ARVROrigin
          "ARVRPositionalTracker" ARVRPositionalTracker "ARVRServer" ARVRServer "AStar" AStar
          "AStar2D" AStar2D "AtlasTexture" AtlasTexture "AudioBusLayout" AudioBusLayout
          "AudioEffect" AudioEffect "AudioEffectAmplify" AudioEffectAmplify "AudioEffectBandLimitFilter" AudioEffectBandLimitFilter
          "AudioEffectBandPassFilter" AudioEffectBandPassFilter "AudioEffectChorus" AudioEffectChorus "AudioEffectCompressor" AudioEffectCompressor
          "AudioEffectDelay" AudioEffectDelay "AudioEffectDistortion" AudioEffectDistortion "AudioEffectEQ" AudioEffectEQ
          "AudioEffectEQ10" AudioEffectEQ10 "AudioEffectEQ21" AudioEffectEQ21 "AudioEffectEQ6" AudioEffectEQ6
          "AudioEffectFilter" AudioEffectFilter "AudioEffectHighPassFilter" AudioEffectHighPassFilter "AudioEffectHighShelfFilter" AudioEffectHighShelfFilter
          "AudioEffectInstance" AudioEffectInstance "AudioEffectLimiter" AudioEffectLimiter "AudioEffectLowPassFilter" AudioEffectLowPassFilter
          "AudioEffectLowShelfFilter" AudioEffectLowShelfFilter "AudioEffectNotchFilter" AudioEffectNotchFilter "AudioEffectPanner" AudioEffectPanner
          "AudioEffectPhaser" AudioEffectPhaser "AudioEffectPitchShift" AudioEffectPitchShift "AudioEffectRecord" AudioEffectRecord
          "AudioEffectReverb" AudioEffectReverb "AudioEffectSpectrumAnalyzer" AudioEffectSpectrumAnalyzer "AudioEffectSpectrumAnalyzerInstance" AudioEffectSpectrumAnalyzerInstance
          "AudioEffectStereoEnhance" AudioEffectStereoEnhance "AudioServer" AudioServer "AudioStream" AudioStream
          "AudioStreamGenerator" AudioStreamGenerator "AudioStreamGeneratorPlayback" AudioStreamGeneratorPlayback "AudioStreamMicrophone" AudioStreamMicrophone
          "AudioStreamMP3" AudioStreamMP3 "AudioStreamOGGVorbis" AudioStreamOGGVorbis "AudioStreamPlayback" AudioStreamPlayback
          "AudioStreamPlaybackResampled" AudioStreamPlaybackResampled "AudioStreamPlayer" AudioStreamPlayer "AudioStreamPlayer2D" AudioStreamPlayer2D
          "AudioStreamPlayer3D" AudioStreamPlayer3D "AudioStreamRandomPitch" AudioStreamRandomPitch "AudioStreamSample" AudioStreamSample
          "BackBufferCopy" BackBufferCopy "BakedLightmap" BakedLightmap "BakedLightmapData" BakedLightmapData
          "BaseButton" BaseButton "BitMap" BitMap "BitmapFont" BitmapFont
          "Bone2D" Bone2D "BoneAttachment" BoneAttachment "BoxContainer" BoxContainer
          "BoxShape" BoxShape "BulletPhysicsServer" BulletPhysicsServer "Button" Button
          "ButtonGroup" ButtonGroup "Camera" Camera "Camera2D" Camera2D
          "CameraFeed" CameraFeed "CameraServer" CameraServer "CameraTexture" CameraTexture
          "CanvasItem" CanvasItem "CanvasItemMaterial" CanvasItemMaterial "CanvasLayer" CanvasLayer
          "CanvasModulate" CanvasModulate "CapsuleMesh" CapsuleMesh "CapsuleShape" CapsuleShape
          "CapsuleShape2D" CapsuleShape2D "CenterContainer" CenterContainer "CharFXTransform" CharFXTransform
          "CheckBox" CheckBox "CheckButton" CheckButton "CircleShape2D" CircleShape2D
          "ClassDB" ClassDB "ClippedCamera" ClippedCamera "CollisionObject" CollisionObject
          "CollisionObject2D" CollisionObject2D "CollisionPolygon" CollisionPolygon "CollisionPolygon2D" CollisionPolygon2D
          "CollisionShape" CollisionShape "CollisionShape2D" CollisionShape2D "ColorPicker" ColorPicker
          "ColorPickerButton" ColorPickerButton "ColorRect" ColorRect "ConcavePolygonShape" ConcavePolygonShape
          "ConcavePolygonShape2D" ConcavePolygonShape2D "ConeTwistJoint" ConeTwistJoint "ConfigFile" ConfigFile
          "ConfirmationDialog" ConfirmationDialog "Container" Container "Control" Control
          "ConvexPolygonShape" ConvexPolygonShape "ConvexPolygonShape2D" ConvexPolygonShape2D "CPUParticles" CPUParticles
          "CPUParticles2D" CPUParticles2D "Crypto" Crypto "CryptoKey" CryptoKey
          "CSGBox" CSGBox "CSGCombiner" CSGCombiner "CSGCylinder" CSGCylinder
          "CSGMesh" CSGMesh "CSGPolygon" CSGPolygon "CSGPrimitive" CSGPrimitive
          "CSGShape" CSGShape "CSGSphere" CSGSphere "CSGTorus" CSGTorus
          "CubeMap" CubeMap "CubeMesh" CubeMesh
          "Curve" Curve "Curve2D" Curve2D "Curve3D" Curve3D
          "CurveTexture" CurveTexture "CylinderMesh" CylinderMesh "CylinderShape" CylinderShape
          "DampedSpringJoint2D" DampedSpringJoint2D "DirectionalLight" DirectionalLight "Directory" Directory
          "DTLSServer" DTLSServer "DynamicFont" DynamicFont "DynamicFontData" DynamicFontData
          "EditorExportPlugin" EditorExportPlugin "EditorFeatureProfile" EditorFeatureProfile "EditorFileDialog" EditorFileDialog
          "EditorFileSystem" EditorFileSystem "EditorFileSystemDirectory" EditorFileSystemDirectory "EditorImportPlugin" EditorImportPlugin
          "EditorInspector" EditorInspector "EditorInspectorPlugin" EditorInspectorPlugin "EditorInterface" EditorInterface
          "EditorNavigationMeshGenerator" EditorNavigationMeshGenerator "EditorPlugin" EditorPlugin "EditorProperty" EditorProperty
          "EditorResourceConversionPlugin" EditorResourceConversionPlugin "EditorResourcePreview" EditorResourcePreview "EditorResourcePreviewGenerator" EditorResourcePreviewGenerator
          "EditorSceneImporter" EditorSceneImporter "EditorSceneImporterFBX" EditorSceneImporterFBX "EditorScenePostImport" EditorScenePostImport
          "EditorScript" EditorScript "EditorSelection" EditorSelection "EditorSettings" EditorSettings
          "EditorSpatialGizmo" EditorSpatialGizmo "EditorSpatialGizmoPlugin" EditorSpatialGizmoPlugin "EditorSpinSlider" EditorSpinSlider
          "EditorVCSInterface" EditorVCSInterface "EncodedObjectAsID" EncodedObjectAsID "Engine" Engine
          "Environment" Environment "Expression" Expression "ExternalTexture" ExternalTexture
          "File" File "FileDialog" FileDialog "FileSystemDock" FileSystemDock
          "Font" Font "FuncRef" FuncRef "GDNative" GDNative
          "GDNativeLibrary" GDNativeLibrary "GDScript" GDScript "GDScriptFunctionState" GDScriptFunctionState
          "Generic6DOFJoint" Generic6DOFJoint "Geometry" Geometry "GeometryInstance" GeometryInstance
          "GIProbe" GIProbe "GIProbeData" GIProbeData
          "Gradient" Gradient "GradientTexture" GradientTexture "GraphEdit" GraphEdit
          "GraphNode" GraphNode "GridContainer" GridContainer "GridMap" GridMap
          "GrooveJoint2D" GrooveJoint2D "HashingContext" HashingContext "HBoxContainer" HBoxContainer
          "HeightMapShape" HeightMapShape "HingeJoint" HingeJoint "HScrollBar" HScrollBar
          "HSeparator" HSeparator "HSlider" HSlider "HSplitContainer" HSplitContainer
          "HTTPClient" HTTPClient "HTTPRequest" HTTPRequest "Image" Image
          "ImageTexture" ImageTexture "ImmediateGeometry" ImmediateGeometry "Input" Input
          "InputEvent" InputEvent "InputEventAction" InputEventAction "InputEventGesture" InputEventGesture
          "InputEventJoypadButton" InputEventJoypadButton "InputEventJoypadMotion" InputEventJoypadMotion "InputEventKey" InputEventKey
          "InputEventMagnifyGesture" InputEventMagnifyGesture "InputEventMIDI" InputEventMIDI "InputEventMouse" InputEventMouse
          "InputEventMouseButton" InputEventMouseButton "InputEventMouseMotion" InputEventMouseMotion "InputEventPanGesture" InputEventPanGesture
          "InputEventScreenDrag" InputEventScreenDrag "InputEventScreenTouch" InputEventScreenTouch "InputEventWithModifiers" InputEventWithModifiers
          "InputMap" InputMap "InstancePlaceholder" InstancePlaceholder "InterpolatedCamera" InterpolatedCamera
          "IP" IP "ItemList" ItemList "JavaClass" JavaClass
          "JavaClassWrapper" JavaClassWrapper "JavaScript" JavaScript "JNISingleton" JNISingleton
          "Joint" Joint "Joint2D" Joint2D "JSON" JSON
          "JSONParseResult" JSONParseResult "JSONRPC" JSONRPC "KinematicBody" KinematicBody
          "KinematicBody2D" KinematicBody2D "KinematicCollision" KinematicCollision "KinematicCollision2D" KinematicCollision2D
          "Label" Label "LargeTexture" LargeTexture "Light" Light
          "Light2D" Light2D "LightOccluder2D" LightOccluder2D "Line2D" Line2D
          "LineEdit" LineEdit "LineShape2D" LineShape2D "LinkButton" LinkButton
          "Listener" Listener "MainLoop" MainLoop "MarginContainer" MarginContainer
          "Marshalls" Marshalls "Material" Material "MenuButton" MenuButton
          "Mesh" Mesh "MeshDataTool" MeshDataTool "MeshInstance" MeshInstance
          "MeshInstance2D" MeshInstance2D "MeshLibrary" MeshLibrary "MeshTexture" MeshTexture
          "MobileVRInterface" MobileVRInterface "MultiMesh" MultiMesh "MultiMeshInstance" MultiMeshInstance
          "MultiMeshInstance2D" MultiMeshInstance2D "MultiplayerAPI" MultiplayerAPI "MultiplayerPeerGDNative" MultiplayerPeerGDNative
          "Mutex" Mutex "NativeScript" NativeScript "Navigation" Navigation
          "Navigation2D" Navigation2D "NavigationMesh" NavigationMesh "NavigationMeshInstance" NavigationMeshInstance
          "NavigationPolygon" NavigationPolygon "NavigationPolygonInstance" NavigationPolygonInstance "NetworkedMultiplayerENet" NetworkedMultiplayerENet
          "NetworkedMultiplayerPeer" NetworkedMultiplayerPeer "NinePatchRect" NinePatchRect "Node" Node
          "Node2D" Node2D "NoiseTexture" NoiseTexture "OccluderPolygon2D" OccluderPolygon2D
          "OmniLight" OmniLight "OpenSimplexNoise" OpenSimplexNoise "OptionButton" OptionButton
          "OS" OS "PackedDataContainer" PackedDataContainer "PackedDataContainerRef" PackedDataContainerRef
          "PackedScene" PackedScene "PacketPeer" PacketPeer "PacketPeerDTLS" PacketPeerDTLS
          "PacketPeerGDNative" PacketPeerGDNative "PacketPeerStream" PacketPeerStream "PacketPeerUDP" PacketPeerUDP
          "Panel" Panel "PanelContainer" PanelContainer "PanoramaSky" PanoramaSky
          "ParallaxBackground" ParallaxBackground "ParallaxLayer" ParallaxLayer "Particles" Particles
          "Particles2D" Particles2D "ParticlesMaterial" ParticlesMaterial "Path" Path
          "Path2D" Path2D "PathFollow" PathFollow "PathFollow2D" PathFollow2D
          "PCKPacker" PCKPacker "Performance" Performance "PHashTranslation" PHashTranslation
          "PhysicalBone" PhysicalBone "Physics2DDirectBodyState" Physics2DDirectBodyState "Physics2DDirectSpaceState" Physics2DDirectSpaceState
          "Physics2DServer" Physics2DServer "Physics2DShapeQueryParameters" Physics2DShapeQueryParameters
          "Physics2DTestMotionResult" Physics2DTestMotionResult "PhysicsBody" PhysicsBody "PhysicsBody2D" PhysicsBody2D
          "PhysicsDirectBodyState" PhysicsDirectBodyState "PhysicsDirectSpaceState" PhysicsDirectSpaceState "PhysicsMaterial" PhysicsMaterial
          "PhysicsServer" PhysicsServer "PhysicsShapeQueryParameters" PhysicsShapeQueryParameters
          "PinJoint" PinJoint "PinJoint2D" PinJoint2D "PlaneMesh" PlaneMesh
          "PlaneShape" PlaneShape "PluginScript" PluginScript "PointMesh" PointMesh
          "Polygon2D" Polygon2D "PolygonPathFinder" PolygonPathFinder "Popup" Popup
          "PopupDialog" PopupDialog "PopupMenu" PopupMenu "PopupPanel" PopupPanel
          "Position2D" Position2D "Position3D" Position3D "PrimitiveMesh" PrimitiveMesh
          "PrismMesh" PrismMesh "ProceduralSky" ProceduralSky "ProgressBar" ProgressBar
          "ProjectSettings" ProjectSettings "ProximityGroup" ProximityGroup "ProxyTexture" ProxyTexture
          "QuadMesh" QuadMesh "RandomNumberGenerator" RandomNumberGenerator "Range" Range
          "RayCast" RayCast "RayCast2D" RayCast2D "RayShape" RayShape
          "RayShape2D" RayShape2D "RectangleShape2D" RectangleShape2D "Reference" Reference
          "ReferenceRect" ReferenceRect "ReflectionProbe" ReflectionProbe "RegEx" RegEx
          "RegExMatch" RegExMatch "RemoteTransform" RemoteTransform "RemoteTransform2D" RemoteTransform2D
          "Resource" Resource "ResourceFormatLoader" ResourceFormatLoader "ResourceFormatSaver" ResourceFormatSaver
          "ResourceImporter" ResourceImporter "ResourceInteractiveLoader" ResourceInteractiveLoader "ResourceLoader" ResourceLoader
          "ResourcePreloader" ResourcePreloader "ResourceSaver" ResourceSaver "RichTextEffect" RichTextEffect
          "RichTextLabel" RichTextLabel "RigidBody" RigidBody "RigidBody2D" RigidBody2D
          "RootMotionView" RootMotionView "SceneState" SceneState "SceneTree" SceneTree
          "SceneTreeTimer" SceneTreeTimer "Script" Script "ScriptCreateDialog" ScriptCreateDialog
          "ScriptEditor" ScriptEditor "ScrollBar" ScrollBar "ScrollContainer" ScrollContainer
          "SegmentShape2D" SegmentShape2D "Semaphore" Semaphore "Separator" Separator
          "Shader" Shader "ShaderMaterial" ShaderMaterial "Shape" Shape
          "Shape2D" Shape2D "ShortCut" ShortCut "Skeleton" Skeleton
          "Skeleton2D" Skeleton2D "SkeletonIK" SkeletonIK "Skin" Skin
          "SkinReference" SkinReference "Sky" Sky "Slider" Slider
          "SliderJoint" SliderJoint "SoftBody" SoftBody "Spatial" Spatial
          "SpatialGizmo" SpatialGizmo "SpatialMaterial" SpatialMaterial "SpatialVelocityTracker" SpatialVelocityTracker
          "SphereMesh" SphereMesh "SphereShape" SphereShape "SpinBox" SpinBox
          "SplitContainer" SplitContainer "SpotLight" SpotLight "SpringArm" SpringArm
          "Sprite" Sprite "Sprite3D" Sprite3D "SpriteBase3D" SpriteBase3D
          "SpriteFrames" SpriteFrames "StaticBody" StaticBody "StaticBody2D" StaticBody2D
          "StreamPeer" StreamPeer "StreamPeerBuffer" StreamPeerBuffer "StreamPeerGDNative" StreamPeerGDNative
          "StreamPeerSSL" StreamPeerSSL "StreamPeerTCP" StreamPeerTCP "StreamTexture" StreamTexture
          "StyleBox" StyleBox "StyleBoxEmpty" StyleBoxEmpty "StyleBoxFlat" StyleBoxFlat
          "StyleBoxLine" StyleBoxLine "StyleBoxTexture" StyleBoxTexture "SurfaceTool" SurfaceTool
          "TabContainer" TabContainer "Tabs" Tabs "TCP_Server" TCP_Server
          "TextEdit" TextEdit "TextFile" TextFile "Texture" Texture
          "Texture3D" Texture3D "TextureArray" TextureArray "TextureButton" TextureButton
          "TextureLayered" TextureLayered "TextureProgress" TextureProgress "TextureRect" TextureRect
          "Theme" Theme "Thread" Thread "TileMap" TileMap
          "TileSet" TileSet "Timer" Timer "ToolButton" ToolButton
          "TouchScreenButton" TouchScreenButton "Translation" Translation "TranslationServer" TranslationServer
          "Tree" Tree "TreeItem" TreeItem "TriangleMesh" TriangleMesh
          "Tween" Tween "UDPServer" UDPServer "UndoRedo" UndoRedo
          "UPNP" UPNP "UPNPDevice" UPNPDevice
          "VBoxContainer" VBoxContainer "VehicleBody" VehicleBody "VehicleWheel" VehicleWheel
          "VideoPlayer" VideoPlayer "VideoStream" VideoStream "VideoStreamGDNative" VideoStreamGDNative
          "VideoStreamTheora" VideoStreamTheora "VideoStreamWebm" VideoStreamWebm "Viewport" Viewport
          "ViewportContainer" ViewportContainer "ViewportTexture" ViewportTexture "VisibilityEnabler" VisibilityEnabler
          "VisibilityEnabler2D" VisibilityEnabler2D "VisibilityNotifier" VisibilityNotifier "VisibilityNotifier2D" VisibilityNotifier2D
          "VisualInstance" VisualInstance "VisualScript" VisualScript "VisualScriptBasicTypeConstant" VisualScriptBasicTypeConstant
          "VisualScriptBuiltinFunc" VisualScriptBuiltinFunc "VisualScriptClassConstant" VisualScriptClassConstant "VisualScriptComment" VisualScriptComment
          "VisualScriptComposeArray" VisualScriptComposeArray "VisualScriptCondition" VisualScriptCondition "VisualScriptConstant" VisualScriptConstant
          "VisualScriptConstructor" VisualScriptConstructor "VisualScriptCustomNode" VisualScriptCustomNode "VisualScriptDeconstruct" VisualScriptDeconstruct
          "VisualScriptEditor" VisualScriptEditor "VisualScriptEmitSignal" VisualScriptEmitSignal "VisualScriptEngineSingleton" VisualScriptEngineSingleton
          "VisualScriptExpression" VisualScriptExpression "VisualScriptFunction" VisualScriptFunction "VisualScriptFunctionCall" VisualScriptFunctionCall
          "VisualScriptFunctionState" VisualScriptFunctionState "VisualScriptGlobalConstant" VisualScriptGlobalConstant "VisualScriptIndexGet" VisualScriptIndexGet
          "VisualScriptIndexSet" VisualScriptIndexSet "VisualScriptInputAction" VisualScriptInputAction "VisualScriptIterator" VisualScriptIterator
          "VisualScriptLists" VisualScriptLists "VisualScriptLocalVar" VisualScriptLocalVar "VisualScriptLocalVarSet" VisualScriptLocalVarSet
          "VisualScriptMathConstant" VisualScriptMathConstant "VisualScriptNode" VisualScriptNode "VisualScriptOperator" VisualScriptOperator
          "VisualScriptPreload" VisualScriptPreload "VisualScriptPropertyGet" VisualScriptPropertyGet "VisualScriptPropertySet" VisualScriptPropertySet
          "VisualScriptResourcePath" VisualScriptResourcePath "VisualScriptReturn" VisualScriptReturn "VisualScriptSceneNode" VisualScriptSceneNode
          "VisualScriptSceneTree" VisualScriptSceneTree "VisualScriptSelect" VisualScriptSelect "VisualScriptSelf" VisualScriptSelf
          "VisualScriptSequence" VisualScriptSequence "VisualScriptSubCall" VisualScriptSubCall "VisualScriptSwitch" VisualScriptSwitch
          "VisualScriptTypeCast" VisualScriptTypeCast "VisualScriptVariableGet" VisualScriptVariableGet "VisualScriptVariableSet" VisualScriptVariableSet
          "VisualScriptWhile" VisualScriptWhile "VisualScriptYield" VisualScriptYield "VisualScriptYieldSignal" VisualScriptYieldSignal
          "VisualServer" VisualServer "VisualShader" VisualShader "VisualShaderNode" VisualShaderNode
          "VisualShaderNodeBooleanConstant" VisualShaderNodeBooleanConstant "VisualShaderNodeBooleanUniform" VisualShaderNodeBooleanUniform "VisualShaderNodeColorConstant" VisualShaderNodeColorConstant
          "VisualShaderNodeColorFunc" VisualShaderNodeColorFunc "VisualShaderNodeColorOp" VisualShaderNodeColorOp "VisualShaderNodeColorUniform" VisualShaderNodeColorUniform
          "VisualShaderNodeCompare" VisualShaderNodeCompare "VisualShaderNodeCubeMap" VisualShaderNodeCubeMap "VisualShaderNodeCubeMapUniform" VisualShaderNodeCubeMapUniform
          "VisualShaderNodeCustom" VisualShaderNodeCustom "VisualShaderNodeDeterminant" VisualShaderNodeDeterminant "VisualShaderNodeDotProduct" VisualShaderNodeDotProduct
          "VisualShaderNodeExpression" VisualShaderNodeExpression "VisualShaderNodeFaceForward" VisualShaderNodeFaceForward "VisualShaderNodeFresnel" VisualShaderNodeFresnel
          "VisualShaderNodeGlobalExpression" VisualShaderNodeGlobalExpression "VisualShaderNodeGroupBase" VisualShaderNodeGroupBase "VisualShaderNodeIf" VisualShaderNodeIf
          "VisualShaderNodeInput" VisualShaderNodeInput "VisualShaderNodeIs" VisualShaderNodeIs "VisualShaderNodeOuterProduct" VisualShaderNodeOuterProduct
          "VisualShaderNodeOutput" VisualShaderNodeOutput "VisualShaderNodeScalarClamp" VisualShaderNodeScalarClamp "VisualShaderNodeScalarConstant" VisualShaderNodeScalarConstant
          "VisualShaderNodeScalarDerivativeFunc" VisualShaderNodeScalarDerivativeFunc "VisualShaderNodeScalarFunc" VisualShaderNodeScalarFunc "VisualShaderNodeScalarInterp" VisualShaderNodeScalarInterp
          "VisualShaderNodeScalarOp" VisualShaderNodeScalarOp "VisualShaderNodeScalarSmoothStep" VisualShaderNodeScalarSmoothStep "VisualShaderNodeScalarSwitch" VisualShaderNodeScalarSwitch
          "VisualShaderNodeScalarUniform" VisualShaderNodeScalarUniform "VisualShaderNodeSwitch" VisualShaderNodeSwitch "VisualShaderNodeTexture" VisualShaderNodeTexture
          "VisualShaderNodeTextureUniform" VisualShaderNodeTextureUniform "VisualShaderNodeTextureUniformTriplanar" VisualShaderNodeTextureUniformTriplanar "VisualShaderNodeTransformCompose" VisualShaderNodeTransformCompose
          "VisualShaderNodeTransformConstant" VisualShaderNodeTransformConstant "VisualShaderNodeTransformDecompose" VisualShaderNodeTransformDecompose "VisualShaderNodeTransformFunc" VisualShaderNodeTransformFunc
          "VisualShaderNodeTransformMult" VisualShaderNodeTransformMult "VisualShaderNodeTransformUniform" VisualShaderNodeTransformUniform "VisualShaderNodeTransformVecMult" VisualShaderNodeTransformVecMult
          "VisualShaderNodeUniform" VisualShaderNodeUniform "VisualShaderNodeUniformRef" VisualShaderNodeUniformRef "VisualShaderNodeVec3Constant" VisualShaderNodeVec3Constant
          "VisualShaderNodeVec3Uniform" VisualShaderNodeVec3Uniform "VisualShaderNodeVectorClamp" VisualShaderNodeVectorClamp "VisualShaderNodeVectorCompose" VisualShaderNodeVectorCompose
          "VisualShaderNodeVectorDecompose" VisualShaderNodeVectorDecompose "VisualShaderNodeVectorDerivativeFunc" VisualShaderNodeVectorDerivativeFunc "VisualShaderNodeVectorDistance" VisualShaderNodeVectorDistance
          "VisualShaderNodeVectorFunc" VisualShaderNodeVectorFunc "VisualShaderNodeVectorInterp" VisualShaderNodeVectorInterp "VisualShaderNodeVectorLen" VisualShaderNodeVectorLen
          "VisualShaderNodeVectorOp" VisualShaderNodeVectorOp "VisualShaderNodeVectorRefract" VisualShaderNodeVectorRefract "VisualShaderNodeVectorScalarMix" VisualShaderNodeVectorScalarMix
          "VisualShaderNodeVectorScalarSmoothStep" VisualShaderNodeVectorScalarSmoothStep "VisualShaderNodeVectorScalarStep" VisualShaderNodeVectorScalarStep "VisualShaderNodeVectorSmoothStep" VisualShaderNodeVectorSmoothStep
          "VScrollBar" VScrollBar "VSeparator" VSeparator "VSlider" VSlider
          "VSplitContainer" VSplitContainer "WeakRef" WeakRef "WebRTCDataChannel" WebRTCDataChannel
          "WebRTCDataChannelGDNative" WebRTCDataChannelGDNative "WebRTCMultiplayer" WebRTCMultiplayer "WebRTCPeerConnection" WebRTCPeerConnection
          "WebRTCPeerConnectionGDNative" WebRTCPeerConnectionGDNative "WebSocketClient" WebSocketClient "WebSocketMultiplayerPeer" WebSocketMultiplayerPeer
          "WebSocketPeer" WebSocketPeer "WebSocketServer" WebSocketServer "WebXRInterface" WebXRInterface
          "WindowDialog" WindowDialog "World" World "World2D" World2D
          "WorldEnvironment" WorldEnvironment "X509Certificate" X509Certificate "XMLParser" XMLParser
          "YSort" YSort}))

  (defn typeof (value)
    (let ((t ((literally typeof) value)))
      (cond
        ((/= t TYPE_OBJECT) (elt self:primitive_types_lookup t))
        ((value:get_script))
        (#t (self:native_types_lookup:get (value:get_class) self:Any))))))

(defn cons (a b)
  (Cons:new a b))

(defn car (a)
  a:car)

(defn cdr (a)
  a:cdr)

(defn init (a)
  (cond
    ((sys/instance-direct? (cdr a) Cons) (cons (car a) (init (cdr a))))
    (#t nil)))

(defn tail (a)
  (cond
    ((sys/instance-direct? (cdr a) Cons) (tail (cdr a)))
    (#t (car a))))

(defn set-car (b a)
  (set a:car b))

(defn set-cdr (b a)
  (set a:cdr b))

(defn intern (a)
  (cond
    ((member? a GDLisp:symbol_table) (elt GDLisp:symbol_table a))
    (#t (set (elt GDLisp:symbol_table a) (Symbol:new a)))))

(defn length (x)
  (let ((result 0))
    (while (sys/instance-direct? x Cons)
      (set result (+ result 1))
      (set x x:cdr))
    result))

(defn funcall (f &rest args)
  (apply f args))

;; funcall alias used in IIFEs. The user can shadow funcall (though
;; it's a bad idea), but shadowing names in sys/ is undefined behavior.
(defn sys/funcall (f &rest args)
  (apply f args))

(defn apply (f &rest args)
  (let ((args1 (init args))
        (args2 (tail args)))
    (cond
      ((sys/instance-direct? f Function) (f:call_funcv (append args1 args2)))
      (#t (push-error "Attempt to call non-function")))))

(defn + (&rest args)
  (sys/call-magic ADDITION)
  (cond
    ((sys/instance-direct? args Cons)
     (let ((result args:car))
       (set args args:cdr)
       (while (sys/instance-direct? args Cons)
         (set result (+ result args:car))
         (set args args:cdr))
       result))
    (#t 0)))

(defn * (&rest args)
  (sys/call-magic MULTIPLICATION)
  (let ((result 1))
    (while (sys/instance-direct? args Cons)
      (set result (* result args:car))
      (set args args:cdr))
    result))

(defn - (x &rest args)
  (sys/call-magic SUBTRACTION)
  (cond
    ((sys/instance-direct? args Cons)
     (let ((result x))
       (while (sys/instance-direct? args Cons)
         (set result (- result args:car))
         (set args args:cdr))
       result))
    (#t (- x))))

(defn / (x &rest args)
  (sys/call-magic DIVISION)
  (cond
    ((sys/instance-direct? args Cons)
     (let ((result x))
       (while (sys/instance-direct? args Cons)
         (set result (/ result args:car))
         (set args args:cdr))
       result))
    (#t (/ x))))

(defn div (x &rest args)
  (sys/call-magic INTEGER-DIVISION)
  (cond
    ((sys/instance-direct? args Cons)
     (let ((result x))
       (while (sys/instance-direct? args Cons)
         (set result (div result args:car))
         (set args args:cdr))
       result))
    (#t (div x))))

(defn mod (x y)
  (sys/call-magic MODULO)
  (mod x y))

(defn min (&rest args)
  (sys/call-magic MIN-FUNCTION)
  (cond
    ((sys/instance-direct? args Cons)
     (let ((result args:car))
       (set args args:cdr)
       (while (sys/instance-direct? args Cons)
         (set result (min result args:car))
         (set args args:cdr))
       result))
    (#t (literally INF))))

(defn max (&rest args)
  (sys/call-magic MAX-FUNCTION)
  (cond
    ((sys/instance-direct? args Cons)
     (let ((result args:car))
       (set args args:cdr)
       (while (sys/instance-direct? args Cons)
         (set result (max result args:car))
         (set args args:cdr))
       result))
    (#t (- (literally INF)))))

(defn = (x &rest args)
  (sys/call-magic EQUAL)
  (while (sys/instance-direct? args Cons)
    (cond
      ((= x args:car) ())
      (#t (return #f)))
    (set x args:car)
    (set args args:cdr))
  #t)

(defn < (x &rest args)
  (sys/call-magic LESS-THAN)
  (while (sys/instance-direct? args Cons)
    (cond
      ((< x args:car) ())
      (#t (return #f)))
    (set x args:car)
    (set args args:cdr))
  #t)

(defn > (x &rest args)
  (sys/call-magic GREATER-THAN)
  (while (sys/instance-direct? args Cons)
    (cond
      ((> x args:car) ())
      (#t (return #f)))
    (set x args:car)
    (set args args:cdr))
  #t)

(defn <= (x &rest args)
  (sys/call-magic LESS-THAN-OR-EQUAL)
  (while (sys/instance-direct? args Cons)
    (cond
      ((<= x args:car) ())
      (#t (return #f)))
    (set x args:car)
    (set args args:cdr))
  #t)

(defn >= (x &rest args)
  (sys/call-magic GREATER-THAN-OR-EQUAL)
  (while (sys/instance-direct? args Cons)
    (cond
      ((>= x args:car) ())
      (#t (return #f)))
    (set x args:car)
    (set args args:cdr))
  #t)

(defn /= (x &rest args)
  (sys/call-magic NOT-EQUAL)
  (let ((outer (cons x args)))
    (while (sys/instance-direct? outer Cons)
      (let ((inner outer:cdr))
        (while (sys/instance-direct? inner Cons)
          (cond
            ((/= outer:car inner:car) ())
            (#t (return #f)))
          (set inner inner:cdr)))
      (set outer outer:cdr)))
  #t)

(defn equal? (x &rest args)
  (while (sys/instance-direct? args Cons)
    (cond
      ((bin-equal x args:car) ())
      (#t (return #f)))
    (set x args:car)
    (set args args:cdr))
  #t)

(defn bin-equal (a b) private
  (cond
    ((cond ((instance? a GDLisp:BaseArray) (instance? b GDLisp:BaseArray)) (#t #f))
     (array-equal a b))
    ((cond ((instance? a GDLisp:Dictionary) (instance? b GDLisp:Dictionary)) (#t #f))
     (dict-equal a b))
    ((cond ((instance? a Cons) (instance? b Cons)) (#t #f))
     (cons-equal a b))
    ((cond ((instance? a GDLisp:Number) (instance? b GDLisp:Number)) (#t #f))
     (= a b))
    ((= (GDLisp:typeof a) (GDLisp:typeof b))
     (= a b))
    (#t #f)))

(defn array-equal (a b) private
  (cond
    ((/= (len a) (len b)) #f)
    (#t (let ((i 0)
              (upper (len a)))
          (while (< i upper)
            (cond ((not (bin-equal (elt a i) (elt b i))) (return #f)))
            (set i (+ i 1)))
          #t))))

(defn dict-equal (a b) private
  (cond
    ((/= (len a) (len b)) #f)
    (#t (for key (a:keys)
          (cond ((not (bin-equal (elt a key) (elt b key))) (return #f))))
        #t)))

(defn cons-equal (a b) private
  (cond
    ((bin-equal (car a) (car b)) (bin-equal (cdr a) (cdr b)))
    (#t #f)))

(defn not (x)
  (sys/call-magic BOOLEAN-NOT)
  (not x))

(defn list (&rest args)
  (sys/call-magic LIST)
  args)

(defn vector (x y &opt z)
  (sys/call-magic VECTOR)
  (cond
    ((= z ()) V{x y})
    (#t V{x y z})))

(defn list->array (list)
  (let ((arr []))
    (while (sys/instance-direct? list Cons)
      (arr:push_back list:car)
      (set list list:cdr))
    arr))

(defn array->list (arr)
  (let ((outer (cons () ())))
    (let ((curr outer))
      (for elem arr
        (set curr:cdr (cons elem ()))
        (set curr curr:cdr))
      outer:cdr)))

(defn elt (arr n)
  (sys/call-magic ARRAY-SUBSCRIPT)
  (elt arr n))

(defn set-elt (x arr n)
  (sys/call-magic ARRAY-SUBSCRIPT-ASSIGNMENT)
  (set-elt x arr n))

(defn member? (value arr)
  (sys/call-magic ARRAY-MEMBER-CHECK)
  (member? value arr))

(defn sys/get-node (obj path)
  (sys/call-magic GET-NODE-SYNTAX)
  (obj:get-node path))

(defn sys/native-class-private () private
  ;; TODO This is a messy hack. Godot chokes if we write
  ;; "GDScript.get_class()" because it thinks we're calling a
  ;; non-static function in a static context. So we store the
  ;; "GDScript" object in a local variable. Optimizations may clobber
  ;; this hack eventually, so we need a better way to reify
  ;; pseudo-objects in Godot like GDScript and (especially) things
  ;; like Object or Vector2.
  (let ((x GDScript))
    (x:get_class)))

(defn instance? (value type)
  (cond
    ((sys/instance-direct? type GDLispSpecialType) (type:satisfies? value))
    (#t (sys/instance-direct? value type))))

(defn sys/instance-direct? (value type)
  (sys/call-magic DIRECT-INSTANCE-CHECK)
  (sys/instance-direct? value type))

(sys/declare function typeof (value) public)

(defn convert (what type)
  (cond
    ((sys/instance-direct? type PrimitiveType) ((literally convert) what type:primitive-value))
    (#t ((literally convert) what type))))

(defn gensym (&opt prefix)
  (cond
    ((= prefix ()) (Symbol:new (GDLisp:global_name_generator:generate)))
    (#t (Symbol:new (GDLisp:global_name_generator:generate_with prefix)))))

(defn map (f xs)
  (cond
    ((cond ((sys/instance-direct? xs Cons) #t) ((= xs nil) #t) (#f #t))
     ;; List map
     (let ((outer (cons nil nil)))
       (let ((curr outer))
         (while (/= xs nil)
           (set curr:cdr (cons (funcall f xs:car) nil))
           (set curr curr:cdr)
           (set xs xs:cdr))
         outer:cdr)))
    (#t
     ;; Array map
     (let ((result []))
       (for i (len xs)
         (result:push_back (funcall f (elt xs i))))
       result))))

(defn filter (p xs)
  (cond
    ((cond ((sys/instance-direct? xs Cons) #t) ((= xs nil) #t) (#t #f))
     ;; List filter
     (let ((outer (cons nil nil)))
       (let ((curr outer))
         (while (/= xs nil)
           (cond
             ((funcall p xs:car)
              (set curr:cdr (cons xs:car nil))
              (set curr curr:cdr)))
           (set xs xs:cdr))
         outer:cdr)))
    (#t
     (let ((result []))
       (for i (len xs)
            (cond
              ((funcall p (elt xs i))
               (result:push_back (elt xs i)))))
       result))))

(defn reverse (arg)
  ;; Only works on lists right now (TODO Arrays)
  (let ((rev nil))
    (while (/= arg nil)
      (set rev `(,(car arg) . ,rev))
      (set arg arg:cdr))
    rev))

(defn append (&rest args)
  (let ((outer (cons nil nil)))
    (let ((curr outer))
      (while (/= args nil)
        (let ((inner-value args:car))
          (while (/= inner-value nil)
            (set curr:cdr (cons inner-value:car nil))
            (set curr curr:cdr)
            (set inner-value inner-value:cdr)))
        (set args args:cdr))
      outer:cdr)))

(defn sys/qq-smart-list (a)
  (cond
    ((instance? a GDLisp:BaseArray) (array->list a))
    (#t a)))

(defn sys/qq-smart-array (a)
  (cond
    ((instance? a GDLisp:BaseArray) a)
    (#t (list->array a))))

;; GDScript built-ins that we use unmodified

;; Note: These all repeat the name of the function. By default,
;; `sys/declare` will refuse to declare a name that conflicts with a
;; GDScript language keyword (adding an `_` to the name
;; automatically). But in our case, we want these names to represent
;; the GDScript keywords for real, so we override this using the
;; explicit `sys/declare` naming syntax and force it to use the name
;; anyway.

(sys/declare superfunction (int int) (a) public)
(sys/declare superfunction (bool bool) (a) public)
(sys/declare superfunction (randomize randomize) () public)
(sys/declare superfunction (randi randi) () public)
(sys/declare superfunction (randf randf) () public)
(sys/declare superfunction (rand-range rand-range) (a b) public)
(sys/declare superfunction (clamp clamp) (a b c) public)
(sys/declare superfunction (abs abs) (a) public)
(sys/declare superfunction (len len) (a) public) ; TODO Eventually, we'll want this to be a multimethod which works on lists as well as arrays. (And possibly elt as well?)
(sys/declare superfunction (get-global-mouse-position get-global-mouse-position) () public) ; TODO Definitely want to wrap this (and all of the mouse functions) in a nice namespace or module or something
(sys/declare superfunction (push-error push-error) (a) public)
(sys/declare superfunction (push-warning push-warning) (a) public)
(sys/declare superfunction (load load) (a) public)
(sys/declare superfunction (acos acos) (a) public)
(sys/declare superfunction (asin asin) (a) public)
(sys/declare superfunction (atan atan) (a) public)
(sys/declare superfunction (atan2 atan2) (a b) public)
(sys/declare superfunction (cos cos) (a) public)
(sys/declare superfunction (cosh cosh) (a) public)
(sys/declare superfunction (sin sin) (a) public)
(sys/declare superfunction (sinh sinh) (a) public)
(sys/declare superfunction (tan tan) (a) public)
(sys/declare superfunction (tanh tanh) (a) public)
(sys/declare superfunction (ceil ceil) (a) public)
(sys/declare superfunction (char char) (a) public)
(sys/declare superfunction (exp exp) (a) public)
(sys/declare superfunction (floor floor) (a) public)
(sys/declare superfunction (sqrt sqrt) (a) public)
(sys/declare superfunction (fmod fmod) (a b) public)
(sys/declare superfunction (fposmod fposmod) (a b) public)
(sys/declare superfunction (posmod posmod) (a b) public)
(sys/declare superfunction (sign sign) (a) public)
(sys/declare superfunction (ord ord) (a) public)
(sys/declare superfunction (hash hash) (a) public)
(sys/declare superfunction (get-stack get-stack) () public)
(sys/declare superfunction (is-nan is-nan) (a) public)
(sys/declare superfunction (is-inf is-inf) (a) public)
(sys/declare superfunction (is-equal-approx is-equal-approx) (a b) public)
(sys/declare superfunction (is-zero-approx is-zero-approx) (a) public)
(sys/declare superfunction (inverse-lerp inverse-lerp) (a b c) public)
(sys/declare superfunction (lerp lerp) (a b c) public)
(sys/declare superfunction (lerp-angle lerp-angle) (a b c) public)
(sys/declare superfunction (pow pow) (a b) public)
(sys/declare superfunction (stepify stepify) (a b) public)
(sys/declare superfunction (step-decimals step-decimals) (a) public)
(sys/declare superfunction (seed seed) (a) public)
(sys/declare superfunction (rand-seed rand-seed) (a) public)
(sys/declare superfunction (deg2rad deg2rad) (a) public)
(sys/declare superfunction (rad2deg rad2deg) (a) public)
(sys/declare superfunction (db2linear db2linear) (a) public)
(sys/declare superfunction (linear2db linear2db) (a) public)
(sys/declare superfunction (is-instance-valid is-instance-valid) (a) public)
(sys/declare superfunction (log log) (a) public)
(sys/declare superfunction (wrapf wrapf) (a b c) public)
(sys/declare superfunction (wrapi wrapi) (a b c) public)
(sys/declare superfunction (print-stack print-stack) () public)
(sys/declare superfunction (round round) (a) public)
(sys/declare superfunction (cartesian2polar cartesian2polar) (a b) public)
(sys/declare superfunction (polar2cartesian polar2cartesian) (a b) public)
(sys/declare superfunction (range-lerp range-lerp) (a b c d e) public)
(sys/declare superfunction (move-toward move-toward) (a b c) public)
(sys/declare superfunction (nearest-po2 nearest-po2) (a) public)
(sys/declare superfunction (instance-from-id instance-from-id) (a) public)
(sys/declare superfunction (parse-json parse-json) (a) public)
(sys/declare superfunction (to-json to-json) (a) public)
(sys/declare superfunction (validate-json validate-json) (a) public)
(sys/declare superfunction (dict2inst dict2inst) (a) public)
(sys/declare superfunction (inst2dict inst2dict) (a) public)
(sys/declare superfunction (str2var str2var) (a) public)
(sys/declare superfunction (var2str var2str) (a) public)
(sys/declare superfunction (weakref weakref) (a) public)
(sys/declare superfunction (ease ease) (a b) public)
(sys/declare superfunction (funcref funcref) (a b) public)
(sys/declare superfunction (type-exists type-exists) (a) public)
(sys/declare superfunction (smoothstep smoothstep) (a b c) public)

;; Varargs functions (see
;; https://github.com/Mercerenies/gdlisp/issues/79 for details on why
;; we have to wrap these ourselves)

(defn str (x &arr args)
  (sys/call-magic VARARG-STR)
  (let ((result (str x)))
    (for arg args
       (set result (+ result (str arg))))
    result))

(defn printerr (&arr args)
  (sys/call-magic VARARG-PRINTERR)
  (let ((result ""))
    (for arg args
       (set result (+ result (str arg))))
    (printerr result)))

(defn printraw (&arr args)
  (sys/call-magic VARARG-PRINTRAW)
  (let ((result ""))
    (for arg args
       (set result (+ result (str arg))))
    (printraw result)))

(defn print-debug (&arr args)
  (sys/call-magic VARARG-PRINTDEBUG)
  (let ((result ""))
    (for arg args
       (set result (+ result (str arg))))
    (print-debug result)))

(defn print (&arr args)
  (sys/call-magic VARARG-PRINT)
  (let ((result ""))
    (for arg args
       (set result (+ result (str arg))))
    (print result)))

(defn prints (&arr args)
  (sys/call-magic VARARG-PRINTS)
  (let ((result "")
        (first #t))
    (for arg args
       (set result (+ result (cond (first "") (#t " ")) (str arg)))
       (set first #f))
    (print result)))

(defn printt (&arr args)
  (sys/call-magic VARARG-PRINTT)
  (let ((result "")
        (first #t))
    (for arg args
       (set result (+ result (cond (first "") (#t "\t")) (str arg)))
       (set first #f))
    (print result)))

(defn range (a &opt b c)
  (sys/call-magic VARARG-RANGE)
  (cond
    ((= b nil) (range a))
    ((= c nil) (range a b))
    (#t (range a b c))))

(defn Color8 (a b c &opt d)
  (sys/call-magic VARARG-COLOR8)
  (cond
    ((= d nil) (Color8 a b c))
    (#t (Color8 a b c d))))

(defn ColorN (a &opt b)
  (sys/call-magic VARARG-COLORN)
  (cond
    ((= b nil) (ColorN a))
    (#t (ColorN a b))))

(defn bytes2var (a &opt b)
  (sys/call-magic VARARG-BYTES2VAR)
  (cond
    ((= b nil) (bytes2var a))
    (#t (bytes2var a b))))

(defn var2bytes (a &opt b)
  (sys/call-magic VARARG-VAR2BYTES)
  (cond
    ((= b nil) (var2bytes a))
    (#t (var2bytes a b))))

;; Global constants

(sys/declare superglobal (PI PI) public)
(sys/declare superglobal (TAU TAU) public)
(sys/declare superglobal (INF INF) public)

;; TYPE_* Constants

(defclass GDLispSpecialType (Reference) private)

(defclass PrimitiveType (GDLispSpecialType) private
  (defvar primitive-value)

  (defn _init (primitive-value)
    (set self:primitive-value primitive-value))

  (defn satisfies? (value)
    (= ((literally typeof) value) self:primitive-value)))

;; Named types like _Engine whose name can be returned from get_class
;; but which do not exist in the runtime namespace exposed to
;; GDScript.
(defclass NamedSyntheticType (GDLispSpecialType) private
  (defvar name)

  (defn _init (name)
    (set self:name name))

  (defn satisfies? (value)
    (= (value:get-class) self:name)))

;; Note: All of these synthetic types would theoretically be defobject
;; if we weren't writing them in the standard library. But for
;; complicated reasons, we can't use macro expansion at all in stdlib,
;; and defobject is behind three layers of macro (defobject and
;; deflazy, and the expansion includes define-symbol-macro), so it's
;; off-limits.

(defclass AnyType (GDLispSpecialType) private
  (defn satisfies? (value)
    #t))

(defclass AnyRefType (GDLispSpecialType) private
  (defn satisfies? (value)
    (= ((literally typeof) value) TYPE_OBJECT)))

(defclass AnyValType (GDLispSpecialType) private
  (defn satisfies? (value)
    (/= ((literally typeof) value) TYPE_OBJECT)))

(defclass NumberType (GDLispSpecialType) private
  (defn satisfies? (value)
    (let ((t ((literally typeof) value)))
      (cond
        ((= t TYPE_INT) #t)
        ((= t TYPE_REAL) #t)
        (#t #f)))))

(defclass BaseArrayType (GDLispSpecialType) private
  (defn satisfies? (value)
    (<= TYPE_ARRAY ((literally typeof) value) TYPE_COLOR_ARRAY)))

(defclass NothingType (GDLispSpecialType) private
  (defn satisfies? (value)
    #f))

(sys/declare value Any public)
(sys/declare value AnyRef public)
(sys/declare value AnyVal public)
(sys/declare value Number public)
(sys/declare value BaseArray public)
(sys/declare value Nothing public)

(sys/declare value Null public)
(sys/declare value Bool public)
(sys/declare value Int public)
(sys/declare value Float public)
(sys/declare value String public)
(sys/declare value Vector2 public)
(sys/declare value Rect2 public)
(sys/declare value Vector3 public)
(sys/declare value Transform2D public)
(sys/declare value Plane public)
(sys/declare value Quat public)
(sys/declare value AABB public)
(sys/declare value Basis public)
(sys/declare value Transform public)
(sys/declare value Color public)
(sys/declare value NodePath public)
(sys/declare value RID public)
(sys/declare value Object public)
(sys/declare value Dictionary public)
(sys/declare value Array public)
(sys/declare value PoolByteArray public)
(sys/declare value PoolIntArray public)
(sys/declare value PoolRealArray public)
(sys/declare value PoolStringArray public)
(sys/declare value PoolVector2Array public)
(sys/declare value PoolVector3Array public)
(sys/declare value PoolColorArray public)

;; BUILT-IN MACROS

(defmacro or (&rest args)
  (let ((args (reverse args)))
    (cond
      (args
       (let ((result `((#t ,(car args)))))
         (set args (cdr args))
         (while (/= args nil)
           (set result `((,(car args)) . ,result))
           (set args (cdr args)))
         `(cond . ,result)))
      (#t #f))))

(defmacro and (&rest args)
  (let ((args (reverse args)))
    (cond
      (args
       (let ((result `((#t ,(car args)))))
         (set args (cdr args))
         (while (/= args nil)
           (set result `(((not ,(car args)) #f) . ,result))
           (set args (cdr args)))
         `(cond . ,result)))
       (#t #t))))

(defmacro let* (vars &rest body)
  (cond
    ((= vars nil) `(progn ,.body))
    (#t `(let (,(car vars))
           (let* ,(cdr vars) ,.body)))))

(defmacro defvars (&rest args)
  (let ((arr []))
    (while (/= args nil)
      (arr:push_back (list 'defvar args:car))
      (set args args:cdr))
    `(progn ,.arr)))

(defmacro when (cnd &rest args)
  `(cond
     (,cnd (progn ,.args))))

(defmacro unless (cnd &rest args)
  `(cond
     (,cnd ())
     (#t (progn ,.args))))

(defmacro if (cnd t &opt f)
  `(cond
     (,cnd ,t)
     (#t ,f)))

(defmacro yield* (arg)
  (let ((symbol (gensym "_yield")))
    `(let ((,symbol ,arg))
       (while (and (instance? ,symbol GDScriptFunctionState) ((unquote symbol):is-valid))
         (yield)
         (set ,symbol ((unquote symbol):resume)))
       ,symbol)))

(defmacro this-file ()
  '(sys/special-ref this-file))

(defmacro this-filename ()
  '(sys/special-ref this-filename))

(defmacro this-true-filename ()
  '(sys/special-ref this-true-filename))

;; TODO Document the semantics of this macro and what preconditions
;; are necessary for it to be safe to use.
(defmacro contextual-load (arg)
  `(load (sys/context-filename ,arg)))

(defmacro deflazy (name value &rest modifiers)
  (let ((fn-name (gensym "_lazy"))
        (this-file (gensym "_this_file"))
        (value-var (gensym "_value"))
        (meta-name ("__gdlisp_Lazy_{}":format [(gensym):contents] "{}"))) ; TODO Find a better way to convert symbol to string than accessing a, theoretically, private field
    `(progn
       (defn ,fn-name ()
         (let ((,this-file (this-file)))
           (if ((unquote this-file):has-meta ,meta-name)
               ((unquote this-file):get-meta ,meta-name)
               (let ((,value-var ,value))
                 ((unquote this-file):set-meta ,meta-name ,value-var)
                 ,value-var))))
       (define-symbol-macro ,name (list (list 'access-slot (list 'contextual-load (this-true-filename)) ',fn-name)) ,.modifiers))))

(defmacro defobject (name parent &opt visibility &rest body)
  (cond
    ((= visibility nil)
     (set visibility 'public))
    ((not (instance? visibility Symbol))
     (set body (cons visibility body)) ; It's not a modifier, so it's part of the body
     (set visibility 'public))
    ((= visibility 'public)
     nil)
    ((= visibility 'private)
     nil)
    (#t
     (set body (cons visibility body)) ; It's not a modifier, so it's part of the body
     (set visibility 'public)))
  `(deflazy ,name (new ,parent ,.body) ,visibility))
