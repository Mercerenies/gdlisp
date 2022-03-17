extends Node
const nil = null
class Cons extends Reference:
    func _init(car_0, cdr_1):
        self.car = car_0
        self.cdr = cdr_1
    var car
    var cdr
class Function extends Reference:
    func _init():
        pass
    var __is_gdlisp_function = true
    var __gdlisp_required = 0
    var __gdlisp_optional = 0
    var __gdlisp_rest = 1
    const __gdlisp_vararg_no = 0
    const __gdlisp_vararg_rest = 1
    const __gdlisp_vararg_arr = 2
class Cell extends Reference:
    func _init(contents_2):
        self.contents = contents_2
    var contents
class Symbol extends Reference:
    func _init(contents_3):
        self.contents = contents_3
    var contents
class FreshNameGenerator extends Reference:
    func _init(reserved_5, index_6):
        self.reserved = reserved_5
        self.index = index_6
    const DEFAULT_PREFIX = "_G"
    var reserved
    var index
    func generate():
        return self.generate_with(FreshNameGenerator.DEFAULT_PREFIX)
    func generate_with(prefix_7):
        var name_8 = "{}_{}".format([prefix_7, self.index], "{}")
        self.index = self.index + 1
        var _cond_9 = self.generate_with(prefix_7) if name_8 in self.reserved else name_8
        return _cond_9
    func to_json():
        return {"reserved": self.reserved, "index": self.index}
    static func from_json(json_10):
        return FreshNameGenerator.new(json_10["reserved"], json_10["index"])
    var __gdlisp_outer_class_4 = load("res://GDLisp.gd")
enum Mouse {
    LEFT = BUTTON_LEFT,
    RIGHT = BUTTON_RIGHT,
    MIDDLE = BUTTON_MIDDLE,
    XBUTTON1 = BUTTON_XBUTTON1,
    XBUTTON2 = BUTTON_XBUTTON2,
    WHEEL_UP = BUTTON_WHEEL_UP,
    WHEEL_DOWN = BUTTON_WHEEL_DOWN,
    WHEEL_LEFT = BUTTON_WHEEL_LEFT,
    WHEEL_RIGHT = BUTTON_WHEEL_RIGHT,
    MASK_LEFT = BUTTON_MASK_LEFT,
    MASK_RIGHT = BUTTON_MASK_RIGHT,
    MASK_MIDDLE = BUTTON_MASK_MIDDLE,
    MASK_XBUTTON1 = BUTTON_MASK_XBUTTON1,
    MASK_XBUTTON2 = BUTTON_MASK_XBUTTON2,
}
enum Corner {
    TOP_LEFT = CORNER_TOP_LEFT,
    TOP_RIGHT = CORNER_TOP_RIGHT,
    BOTTOM_RIGHT = CORNER_BOTTOM_RIGHT,
    BOTTOM_LEFT = CORNER_BOTTOM_LEFT,
}
enum Joy {
    BUTTON_0 = JOY_BUTTON_0,
    BUTTON_1 = JOY_BUTTON_1,
    BUTTON_2 = JOY_BUTTON_2,
    BUTTON_3 = JOY_BUTTON_3,
    BUTTON_4 = JOY_BUTTON_4,
    BUTTON_5 = JOY_BUTTON_5,
    BUTTON_6 = JOY_BUTTON_6,
    BUTTON_7 = JOY_BUTTON_7,
    BUTTON_8 = JOY_BUTTON_8,
    BUTTON_9 = JOY_BUTTON_9,
    BUTTON_10 = JOY_BUTTON_10,
    BUTTON_11 = JOY_BUTTON_11,
    BUTTON_12 = JOY_BUTTON_12,
    BUTTON_13 = JOY_BUTTON_13,
    BUTTON_14 = JOY_BUTTON_14,
    BUTTON_15 = JOY_BUTTON_15,
    BUTTON_MAX = JOY_BUTTON_MAX,
    SONY_CIRCLE = JOY_SONY_CIRCLE,
    SONY_X = JOY_SONY_X,
    SONY_SQUARE = JOY_SONY_SQUARE,
    SONY_TRIANGLE = JOY_SONY_TRIANGLE,
    XBOX_B = JOY_XBOX_B,
    XBOX_A = JOY_XBOX_A,
    XBOX_X = JOY_XBOX_X,
    XBOX_Y = JOY_XBOX_Y,
    DS_A = JOY_DS_A,
    DS_B = JOY_DS_B,
    DS_X = JOY_DS_X,
    DS_Y = JOY_DS_Y,
    VR_GRIP = JOY_VR_GRIP,
    VR_PAD = JOY_VR_PAD,
    VR_TRIGGER = JOY_VR_TRIGGER,
    OCULUS_AX = JOY_OCULUS_AX,
    OCULUS_BY = JOY_OCULUS_BY,
    OCULUS_MENU = JOY_OCULUS_MENU,
    OPENVR_MENU = JOY_OPENVR_MENU,
    SELECT = JOY_SELECT,
    START = JOY_START,
    DPAD_UP = JOY_DPAD_UP,
    DPAD_DOWN = JOY_DPAD_DOWN,
    DPAD_LEFT = JOY_DPAD_LEFT,
    DPAD_RIGHT = JOY_DPAD_RIGHT,
    L = JOY_L,
    L2 = JOY_L2,
    L3 = JOY_L3,
    R = JOY_R,
    R2 = JOY_R2,
    R3 = JOY_R3,
    AXIS_0 = JOY_AXIS_0,
    AXIS_1 = JOY_AXIS_1,
    AXIS_2 = JOY_AXIS_2,
    AXIS_3 = JOY_AXIS_3,
    AXIS_4 = JOY_AXIS_4,
    AXIS_5 = JOY_AXIS_5,
    AXIS_6 = JOY_AXIS_6,
    AXIS_7 = JOY_AXIS_7,
    AXIS_8 = JOY_AXIS_8,
    AXIS_9 = JOY_AXIS_9,
    AXIS_MAX = JOY_AXIS_MAX,
    ANALOG_LX = JOY_ANALOG_LX,
    ANALOG_LY = JOY_ANALOG_LY,
    ANALOG_RX = JOY_ANALOG_RX,
    ANALOG_RY = JOY_ANALOG_RY,
    ANALOG_L2 = JOY_ANALOG_L2,
    ANALOG_R2 = JOY_ANALOG_R2,
    VR_ANALOG_TRIGGER = JOY_VR_ANALOG_TRIGGER,
    VR_ANALOG_GRIP = JOY_VR_ANALOG_GRIP,
    OPENVR_TOUCHPADX = JOY_OPENVR_TOUCHPADX,
    OPENVR_TOUCHPADY = JOY_OPENVR_TOUCHPADY,
}
enum Margin {
    LEFT = MARGIN_LEFT,
    TOP = MARGIN_TOP,
    RIGHT = MARGIN_RIGHT,
    BOTTOM = MARGIN_BOTTOM,
}
enum KeyMask {
    CODE_MASK = KEY_CODE_MASK,
    MODIFIER_MASK = KEY_MODIFIER_MASK,
    SHIFT = KEY_MASK_SHIFT,
    ALT = KEY_MASK_ALT,
    META = KEY_MASK_META,
    CTRL = KEY_MASK_CTRL,
    CMD = KEY_MASK_CMD,
    KPAD = KEY_MASK_KPAD,
    GROUP_SWITCH = KEY_MASK_GROUP_SWITCH,
}
enum Orientation {
    VERTICAL = VERTICAL,
    HORIZONTAL = HORIZONTAL,
}
enum VAlign {
    TOP = VALIGN_TOP,
    CENTER = VALIGN_CENTER,
    BOTTOM = VALIGN_BOTTOM,
}
enum Key {
    ESCAPE = KEY_ESCAPE,
    TAB = KEY_TAB,
    BACKTAB = KEY_BACKTAB,
    BACKSPACE = KEY_BACKSPACE,
    ENTER = KEY_ENTER,
    KP_ENTER = KEY_KP_ENTER,
    INSERT = KEY_INSERT,
    DELETE = KEY_DELETE,
    PAUSE = KEY_PAUSE,
    PRINT = KEY_PRINT,
    SYSREQ = KEY_SYSREQ,
    CLEAR = KEY_CLEAR,
    HOME = KEY_HOME,
    END = KEY_END,
    LEFT = KEY_LEFT,
    UP = KEY_UP,
    RIGHT = KEY_RIGHT,
    DOWN = KEY_DOWN,
    PAGEUP = KEY_PAGEUP,
    PAGEDOWN = KEY_PAGEDOWN,
    SHIFT = KEY_SHIFT,
    CONTROL = KEY_CONTROL,
    META = KEY_META,
    ALT = KEY_ALT,
    CAPSLOCK = KEY_CAPSLOCK,
    NUMLOCK = KEY_NUMLOCK,
    SCROLLLOCK = KEY_SCROLLLOCK,
    F1 = KEY_F1,
    F2 = KEY_F2,
    F3 = KEY_F3,
    F4 = KEY_F4,
    F5 = KEY_F5,
    F6 = KEY_F6,
    F7 = KEY_F7,
    F8 = KEY_F8,
    F9 = KEY_F9,
    F10 = KEY_F10,
    F11 = KEY_F11,
    F12 = KEY_F12,
    F13 = KEY_F13,
    F14 = KEY_F14,
    F15 = KEY_F15,
    F16 = KEY_F16,
    KP_MULTIPLY = KEY_KP_MULTIPLY,
    KP_DIVIDE = KEY_KP_DIVIDE,
    KP_SUBTRACT = KEY_KP_SUBTRACT,
    KP_PERIOD = KEY_KP_PERIOD,
    KP_ADD = KEY_KP_ADD,
    KP_0 = KEY_KP_0,
    KP_1 = KEY_KP_1,
    KP_2 = KEY_KP_2,
    KP_3 = KEY_KP_3,
    KP_4 = KEY_KP_4,
    KP_5 = KEY_KP_5,
    KP_6 = KEY_KP_6,
    KP_7 = KEY_KP_7,
    KP_8 = KEY_KP_8,
    KP_9 = KEY_KP_9,
    SUPER_L = KEY_SUPER_L,
    SUPER_R = KEY_SUPER_R,
    MENU = KEY_MENU,
    HYPER_L = KEY_HYPER_L,
    HYPER_R = KEY_HYPER_R,
    HELP = KEY_HELP,
    DIRECTION_L = KEY_DIRECTION_L,
    DIRECTION_R = KEY_DIRECTION_R,
    BACK = KEY_BACK,
    FORWARD = KEY_FORWARD,
    STOP = KEY_STOP,
    REFRESH = KEY_REFRESH,
    VOLUMEDOWN = KEY_VOLUMEDOWN,
    VOLUMEMUTE = KEY_VOLUMEMUTE,
    VOLUMEUP = KEY_VOLUMEUP,
    BASSBOOST = KEY_BASSBOOST,
    BASSUP = KEY_BASSUP,
    BASSDOWN = KEY_BASSDOWN,
    TREBLEUP = KEY_TREBLEUP,
    TREBLEDOWN = KEY_TREBLEDOWN,
    MEDIAPLAY = KEY_MEDIAPLAY,
    MEDIASTOP = KEY_MEDIASTOP,
    MEDIAPREVIOUS = KEY_MEDIAPREVIOUS,
    MEDIANEXT = KEY_MEDIANEXT,
    MEDIARECORD = KEY_MEDIARECORD,
    HOMEPAGE = KEY_HOMEPAGE,
    FAVORITES = KEY_FAVORITES,
    SEARCH = KEY_SEARCH,
    STANDBY = KEY_STANDBY,
    OPENURL = KEY_OPENURL,
    LAUNCHMAIL = KEY_LAUNCHMAIL,
    LAUNCHMEDIA = KEY_LAUNCHMEDIA,
    LAUNCH0 = KEY_LAUNCH0,
    LAUNCH1 = KEY_LAUNCH1,
    LAUNCH2 = KEY_LAUNCH2,
    LAUNCH3 = KEY_LAUNCH3,
    LAUNCH4 = KEY_LAUNCH4,
    LAUNCH5 = KEY_LAUNCH5,
    LAUNCH6 = KEY_LAUNCH6,
    LAUNCH7 = KEY_LAUNCH7,
    LAUNCH8 = KEY_LAUNCH8,
    LAUNCH9 = KEY_LAUNCH9,
    LAUNCHA = KEY_LAUNCHA,
    LAUNCHB = KEY_LAUNCHB,
    LAUNCHC = KEY_LAUNCHC,
    LAUNCHD = KEY_LAUNCHD,
    LAUNCHE = KEY_LAUNCHE,
    LAUNCHF = KEY_LAUNCHF,
    UNKNOWN = KEY_UNKNOWN,
    SPACE = KEY_SPACE,
    EXCLAM = KEY_EXCLAM,
    QUOTEDBL = KEY_QUOTEDBL,
    NUMBERSIGN = KEY_NUMBERSIGN,
    DOLLAR = KEY_DOLLAR,
    PERCENT = KEY_PERCENT,
    AMPERSAND = KEY_AMPERSAND,
    APOSTROPHE = KEY_APOSTROPHE,
    PARENLEFT = KEY_PARENLEFT,
    PARENRIGHT = KEY_PARENRIGHT,
    ASTERISK = KEY_ASTERISK,
    PLUS = KEY_PLUS,
    COMMA = KEY_COMMA,
    MINUS = KEY_MINUS,
    PERIOD = KEY_PERIOD,
    SLASH = KEY_SLASH,
    K0 = KEY_0,
    K1 = KEY_1,
    K2 = KEY_2,
    K3 = KEY_3,
    K4 = KEY_4,
    K5 = KEY_5,
    K6 = KEY_6,
    K7 = KEY_7,
    K8 = KEY_8,
    K9 = KEY_9,
    COLON = KEY_COLON,
    SEMICOLON = KEY_SEMICOLON,
    LESS = KEY_LESS,
    EQUAL = KEY_EQUAL,
    GREATER = KEY_GREATER,
    QUESTION = KEY_QUESTION,
    AT = KEY_AT,
    A = KEY_A,
    B = KEY_B,
    C = KEY_C,
    D = KEY_D,
    E = KEY_E,
    F = KEY_F,
    G = KEY_G,
    H = KEY_H,
    I = KEY_I,
    J = KEY_J,
    K = KEY_K,
    L = KEY_L,
    M = KEY_M,
    N = KEY_N,
    O = KEY_O,
    P = KEY_P,
    Q = KEY_Q,
    R = KEY_R,
    S = KEY_S,
    T = KEY_T,
    U = KEY_U,
    V = KEY_V,
    W = KEY_W,
    X = KEY_X,
    Y = KEY_Y,
    Z = KEY_Z,
    BRACKETLEFT = KEY_BRACKETLEFT,
    BACKSLASH = KEY_BACKSLASH,
    BRACKETRIGHT = KEY_BRACKETRIGHT,
    ASCIICIRCUM = KEY_ASCIICIRCUM,
    UNDERSCORE = KEY_UNDERSCORE,
    QUOTELEFT = KEY_QUOTELEFT,
    BRACELEFT = KEY_BRACELEFT,
    BAR = KEY_BAR,
    BRACERIGHT = KEY_BRACERIGHT,
    ASCIITILDE = KEY_ASCIITILDE,
    NOBREAKSPACE = KEY_NOBREAKSPACE,
    EXCLAMDOWN = KEY_EXCLAMDOWN,
    CENT = KEY_CENT,
    STERLING = KEY_STERLING,
    CURRENCY = KEY_CURRENCY,
    YEN = KEY_YEN,
    BROKENBAR = KEY_BROKENBAR,
    SECTION = KEY_SECTION,
    DIAERESIS = KEY_DIAERESIS,
    COPYRIGHT = KEY_COPYRIGHT,
    ORDFEMININE = KEY_ORDFEMININE,
    GUILLEMOTLEFT = KEY_GUILLEMOTLEFT,
    NOTSIGN = KEY_NOTSIGN,
    HYPHEN = KEY_HYPHEN,
    REGISTERED = KEY_REGISTERED,
    MACRON = KEY_MACRON,
    DEGREE = KEY_DEGREE,
    PLUSMINUS = KEY_PLUSMINUS,
    TWOSUPERIOR = KEY_TWOSUPERIOR,
    THREESUPERIOR = KEY_THREESUPERIOR,
    ACUTE = KEY_ACUTE,
    MU = KEY_MU,
    PARAGRAPH = KEY_PARAGRAPH,
    PERIODCENTERED = KEY_PERIODCENTERED,
    CEDILLA = KEY_CEDILLA,
    ONESUPERIOR = KEY_ONESUPERIOR,
    MASCULINE = KEY_MASCULINE,
    GUILLEMOTRIGHT = KEY_GUILLEMOTRIGHT,
    ONEQUARTER = KEY_ONEQUARTER,
    ONEHALF = KEY_ONEHALF,
    THREEQUARTERS = KEY_THREEQUARTERS,
    QUESTIONDOWN = KEY_QUESTIONDOWN,
    AGRAVE = KEY_AGRAVE,
    AACUTE = KEY_AACUTE,
    ACIRCUMFLEX = KEY_ACIRCUMFLEX,
    ATILDE = KEY_ATILDE,
    ADIAERESIS = KEY_ADIAERESIS,
    ARING = KEY_ARING,
    AE = KEY_AE,
    CCEDILLA = KEY_CCEDILLA,
    EGRAVE = KEY_EGRAVE,
    EACUTE = KEY_EACUTE,
    ECIRCUMFLEX = KEY_ECIRCUMFLEX,
    EDIAERESIS = KEY_EDIAERESIS,
    IGRAVE = KEY_IGRAVE,
    IACUTE = KEY_IACUTE,
    ICIRCUMFLEX = KEY_ICIRCUMFLEX,
    IDIAERESIS = KEY_IDIAERESIS,
    ETH = KEY_ETH,
    NTILDE = KEY_NTILDE,
    OGRAVE = KEY_OGRAVE,
    OACUTE = KEY_OACUTE,
    OCIRCUMFLEX = KEY_OCIRCUMFLEX,
    OTILDE = KEY_OTILDE,
    ODIAERESIS = KEY_ODIAERESIS,
    MULTIPLY = KEY_MULTIPLY,
    OOBLIQUE = KEY_OOBLIQUE,
    UGRAVE = KEY_UGRAVE,
    UACUTE = KEY_UACUTE,
    UCIRCUMFLEX = KEY_UCIRCUMFLEX,
    UDIAERESIS = KEY_UDIAERESIS,
    YACUTE = KEY_YACUTE,
    THORN = KEY_THORN,
    SSHARP = KEY_SSHARP,
    DIVISION = KEY_DIVISION,
    YDIAERESIS = KEY_YDIAERESIS,
}
enum HAlign {
    LEFT = HALIGN_LEFT,
    CENTER = HALIGN_CENTER,
    RIGHT = HALIGN_RIGHT,
}
enum MIDIMessage {
    NOTE_OFF = MIDI_MESSAGE_NOTE_OFF,
    NOTE_ON = MIDI_MESSAGE_NOTE_ON,
    AFTERTOUCH = MIDI_MESSAGE_AFTERTOUCH,
    CONTROL_CHANGE = MIDI_MESSAGE_CONTROL_CHANGE,
    PROGRAM_CHANGE = MIDI_MESSAGE_PROGRAM_CHANGE,
    CHANNEL_PRESSURE = MIDI_MESSAGE_CHANNEL_PRESSURE,
    PITCH_BEND = MIDI_MESSAGE_PITCH_BEND,
}
func _init():
    self.global_name_generator = FreshNameGenerator.new([], 0)
    self.Null = PrimitiveType.new(TYPE_NIL)
    self.Bool = PrimitiveType.new(TYPE_BOOL)
    self.Int = PrimitiveType.new(TYPE_INT)
    self.Float = PrimitiveType.new(TYPE_REAL)
    self._String = PrimitiveType.new(TYPE_STRING)
    self._Vector2 = PrimitiveType.new(TYPE_VECTOR2)
    self._Rect2 = PrimitiveType.new(TYPE_RECT2)
    self._Vector3 = PrimitiveType.new(TYPE_VECTOR3)
    self._Transform2D = PrimitiveType.new(TYPE_TRANSFORM2D)
    self._Plane = PrimitiveType.new(TYPE_PLANE)
    self._Quat = PrimitiveType.new(TYPE_QUAT)
    self._AABB = PrimitiveType.new(TYPE_AABB)
    self._Basis = PrimitiveType.new(TYPE_BASIS)
    self._Transform = PrimitiveType.new(TYPE_TRANSFORM)
    self._Color = PrimitiveType.new(TYPE_COLOR)
    self._NodePath = PrimitiveType.new(TYPE_NODE_PATH)
    self._RID = PrimitiveType.new(TYPE_RID)
    self._Object = PrimitiveType.new(TYPE_OBJECT)
    self._Dictionary = PrimitiveType.new(TYPE_DICTIONARY)
    self._Array = PrimitiveType.new(TYPE_ARRAY)
    self._PoolByteArray = PrimitiveType.new(TYPE_RAW_ARRAY)
    self._PoolIntArray = PrimitiveType.new(TYPE_INT_ARRAY)
    self._PoolRealArray = PrimitiveType.new(TYPE_REAL_ARRAY)
    self._PoolStringArray = PrimitiveType.new(TYPE_STRING_ARRAY)
    self._PoolVector2Array = PrimitiveType.new(TYPE_VECTOR2_ARRAY)
    self._PoolVector3Array = PrimitiveType.new(TYPE_VECTOR3_ARRAY)
    self._PoolColorArray = PrimitiveType.new(TYPE_COLOR_ARRAY)
    self.Any = AnyType.new()
    self.AnyRef = AnyRefType.new()
    self.AnyVal = AnyValType.new()
    self.Number = NumberType.new()
    self.BaseArray = BaseArrayType.new()
    self.Nothing = NothingType.new()
    self.native_types_lookup = {"AcceptDialog": AcceptDialog, "AnimatedSprite": AnimatedSprite, "AnimatedSprite3D": AnimatedSprite3D, "AnimatedTexture": AnimatedTexture, "Animation": Animation, "AnimationNode": AnimationNode, "AnimationNodeAdd2": AnimationNodeAdd2, "AnimationNodeAdd3": AnimationNodeAdd3, "AnimationNodeAnimation": AnimationNodeAnimation, "AnimationNodeBlend2": AnimationNodeBlend2, "AnimationNodeBlend3": AnimationNodeBlend3, "AnimationNodeBlendSpace1D": AnimationNodeBlendSpace1D, "AnimationNodeBlendSpace2D": AnimationNodeBlendSpace2D, "AnimationNodeBlendTree": AnimationNodeBlendTree, "AnimationNodeOneShot": AnimationNodeOneShot, "AnimationNodeOutput": AnimationNodeOutput, "AnimationNodeStateMachine": AnimationNodeStateMachine, "AnimationNodeStateMachinePlayback": AnimationNodeStateMachinePlayback, "AnimationNodeStateMachineTransition": AnimationNodeStateMachineTransition, "AnimationNodeTimeScale": AnimationNodeTimeScale, "AnimationNodeTimeSeek": AnimationNodeTimeSeek, "AnimationNodeTransition": AnimationNodeTransition, "AnimationPlayer": AnimationPlayer, "AnimationRootNode": AnimationRootNode, "AnimationTrackEditPlugin": AnimationTrackEditPlugin, "AnimationTree": AnimationTree, "AnimationTreePlayer": AnimationTreePlayer, "Area": Area, "Area2D": Area2D, "ArrayMesh": ArrayMesh, "ARVRAnchor": ARVRAnchor, "ARVRCamera": ARVRCamera, "ARVRController": ARVRController, "ARVRInterface": ARVRInterface, "ARVRInterfaceGDNative": ARVRInterfaceGDNative, "ARVROrigin": ARVROrigin, "ARVRPositionalTracker": ARVRPositionalTracker, "ARVRServer": ARVRServer, "AStar": AStar, "AStar2D": AStar2D, "AtlasTexture": AtlasTexture, "AudioBusLayout": AudioBusLayout, "AudioEffect": AudioEffect, "AudioEffectAmplify": AudioEffectAmplify, "AudioEffectBandLimitFilter": AudioEffectBandLimitFilter, "AudioEffectBandPassFilter": AudioEffectBandPassFilter, "AudioEffectChorus": AudioEffectChorus, "AudioEffectCompressor": AudioEffectCompressor, "AudioEffectDelay": AudioEffectDelay, "AudioEffectDistortion": AudioEffectDistortion, "AudioEffectEQ": AudioEffectEQ, "AudioEffectEQ10": AudioEffectEQ10, "AudioEffectEQ21": AudioEffectEQ21, "AudioEffectEQ6": AudioEffectEQ6, "AudioEffectFilter": AudioEffectFilter, "AudioEffectHighPassFilter": AudioEffectHighPassFilter, "AudioEffectHighShelfFilter": AudioEffectHighShelfFilter, "AudioEffectInstance": AudioEffectInstance, "AudioEffectLimiter": AudioEffectLimiter, "AudioEffectLowPassFilter": AudioEffectLowPassFilter, "AudioEffectLowShelfFilter": AudioEffectLowShelfFilter, "AudioEffectNotchFilter": AudioEffectNotchFilter, "AudioEffectPanner": AudioEffectPanner, "AudioEffectPhaser": AudioEffectPhaser, "AudioEffectPitchShift": AudioEffectPitchShift, "AudioEffectRecord": AudioEffectRecord, "AudioEffectReverb": AudioEffectReverb, "AudioEffectSpectrumAnalyzer": AudioEffectSpectrumAnalyzer, "AudioEffectSpectrumAnalyzerInstance": AudioEffectSpectrumAnalyzerInstance, "AudioEffectStereoEnhance": AudioEffectStereoEnhance, "AudioServer": AudioServer, "AudioStream": AudioStream, "AudioStreamGenerator": AudioStreamGenerator, "AudioStreamGeneratorPlayback": AudioStreamGeneratorPlayback, "AudioStreamMicrophone": AudioStreamMicrophone, "AudioStreamMP3": AudioStreamMP3, "AudioStreamOGGVorbis": AudioStreamOGGVorbis, "AudioStreamPlayback": AudioStreamPlayback, "AudioStreamPlaybackResampled": AudioStreamPlaybackResampled, "AudioStreamPlayer": AudioStreamPlayer, "AudioStreamPlayer2D": AudioStreamPlayer2D, "AudioStreamPlayer3D": AudioStreamPlayer3D, "AudioStreamRandomPitch": AudioStreamRandomPitch, "AudioStreamSample": AudioStreamSample, "BackBufferCopy": BackBufferCopy, "BakedLightmap": BakedLightmap, "BakedLightmapData": BakedLightmapData, "BaseButton": BaseButton, "BitMap": BitMap, "BitmapFont": BitmapFont, "Bone2D": Bone2D, "BoneAttachment": BoneAttachment, "BoxContainer": BoxContainer, "BoxShape": BoxShape, "BulletPhysicsServer": BulletPhysicsServer, "Button": Button, "ButtonGroup": ButtonGroup, "Camera": Camera, "Camera2D": Camera2D, "CameraFeed": CameraFeed, "CameraServer": CameraServer, "CameraTexture": CameraTexture, "CanvasItem": CanvasItem, "CanvasItemMaterial": CanvasItemMaterial, "CanvasLayer": CanvasLayer, "CanvasModulate": CanvasModulate, "CapsuleMesh": CapsuleMesh, "CapsuleShape": CapsuleShape, "CapsuleShape2D": CapsuleShape2D, "CenterContainer": CenterContainer, "CharFXTransform": CharFXTransform, "CheckBox": CheckBox, "CheckButton": CheckButton, "CircleShape2D": CircleShape2D, "ClassDB": ClassDB, "ClippedCamera": ClippedCamera, "CollisionObject": CollisionObject, "CollisionObject2D": CollisionObject2D, "CollisionPolygon": CollisionPolygon, "CollisionPolygon2D": CollisionPolygon2D, "CollisionShape": CollisionShape, "CollisionShape2D": CollisionShape2D, "ColorPicker": ColorPicker, "ColorPickerButton": ColorPickerButton, "ColorRect": ColorRect, "ConcavePolygonShape": ConcavePolygonShape, "ConcavePolygonShape2D": ConcavePolygonShape2D, "ConeTwistJoint": ConeTwistJoint, "ConfigFile": ConfigFile, "ConfirmationDialog": ConfirmationDialog, "Container": Container, "Control": Control, "ConvexPolygonShape": ConvexPolygonShape, "ConvexPolygonShape2D": ConvexPolygonShape2D, "CPUParticles": CPUParticles, "CPUParticles2D": CPUParticles2D, "Crypto": Crypto, "CryptoKey": CryptoKey, "CSGBox": CSGBox, "CSGCombiner": CSGCombiner, "CSGCylinder": CSGCylinder, "CSGMesh": CSGMesh, "CSGPolygon": CSGPolygon, "CSGPrimitive": CSGPrimitive, "CSGShape": CSGShape, "CSGSphere": CSGSphere, "CSGTorus": CSGTorus, "CubeMap": CubeMap, "CubeMesh": CubeMesh, "Curve": Curve, "Curve2D": Curve2D, "Curve3D": Curve3D, "CurveTexture": CurveTexture, "CylinderMesh": CylinderMesh, "CylinderShape": CylinderShape, "DampedSpringJoint2D": DampedSpringJoint2D, "DirectionalLight": DirectionalLight, "Directory": Directory, "DTLSServer": DTLSServer, "DynamicFont": DynamicFont, "DynamicFontData": DynamicFontData, "EditorExportPlugin": EditorExportPlugin, "EditorFeatureProfile": EditorFeatureProfile, "EditorFileDialog": EditorFileDialog, "EditorFileSystem": EditorFileSystem, "EditorFileSystemDirectory": EditorFileSystemDirectory, "EditorImportPlugin": EditorImportPlugin, "EditorInspector": EditorInspector, "EditorInspectorPlugin": EditorInspectorPlugin, "EditorInterface": EditorInterface, "EditorNavigationMeshGenerator": EditorNavigationMeshGenerator, "EditorPlugin": EditorPlugin, "EditorProperty": EditorProperty, "EditorResourceConversionPlugin": EditorResourceConversionPlugin, "EditorResourcePreview": EditorResourcePreview, "EditorResourcePreviewGenerator": EditorResourcePreviewGenerator, "EditorSceneImporter": EditorSceneImporter, "EditorSceneImporterFBX": EditorSceneImporterFBX, "EditorScenePostImport": EditorScenePostImport, "EditorScript": EditorScript, "EditorSelection": EditorSelection, "EditorSettings": EditorSettings, "EditorSpatialGizmo": EditorSpatialGizmo, "EditorSpatialGizmoPlugin": EditorSpatialGizmoPlugin, "EditorSpinSlider": EditorSpinSlider, "EditorVCSInterface": EditorVCSInterface, "EncodedObjectAsID": EncodedObjectAsID, "Engine": Engine, "Environment": Environment, "Expression": Expression, "ExternalTexture": ExternalTexture, "File": File, "FileDialog": FileDialog, "FileSystemDock": FileSystemDock, "Font": Font, "FuncRef": FuncRef, "GDNative": GDNative, "GDNativeLibrary": GDNativeLibrary, "GDScript": GDScript, "GDScriptFunctionState": GDScriptFunctionState, "Generic6DOFJoint": Generic6DOFJoint, "Geometry": Geometry, "GeometryInstance": GeometryInstance, "GIProbe": GIProbe, "GIProbeData": GIProbeData, "Gradient": Gradient, "GradientTexture": GradientTexture, "GraphEdit": GraphEdit, "GraphNode": GraphNode, "GridContainer": GridContainer, "GridMap": GridMap, "GrooveJoint2D": GrooveJoint2D, "HashingContext": HashingContext, "HBoxContainer": HBoxContainer, "HeightMapShape": HeightMapShape, "HingeJoint": HingeJoint, "HScrollBar": HScrollBar, "HSeparator": HSeparator, "HSlider": HSlider, "HSplitContainer": HSplitContainer, "HTTPClient": HTTPClient, "HTTPRequest": HTTPRequest, "Image": Image, "ImageTexture": ImageTexture, "ImmediateGeometry": ImmediateGeometry, "Input": Input, "InputEvent": InputEvent, "InputEventAction": InputEventAction, "InputEventGesture": InputEventGesture, "InputEventJoypadButton": InputEventJoypadButton, "InputEventJoypadMotion": InputEventJoypadMotion, "InputEventKey": InputEventKey, "InputEventMagnifyGesture": InputEventMagnifyGesture, "InputEventMIDI": InputEventMIDI, "InputEventMouse": InputEventMouse, "InputEventMouseButton": InputEventMouseButton, "InputEventMouseMotion": InputEventMouseMotion, "InputEventPanGesture": InputEventPanGesture, "InputEventScreenDrag": InputEventScreenDrag, "InputEventScreenTouch": InputEventScreenTouch, "InputEventWithModifiers": InputEventWithModifiers, "InputMap": InputMap, "InstancePlaceholder": InstancePlaceholder, "InterpolatedCamera": InterpolatedCamera, "IP": IP, "ItemList": ItemList, "JavaClass": JavaClass, "JavaClassWrapper": JavaClassWrapper, "JavaScript": JavaScript, "JNISingleton": JNISingleton, "Joint": Joint, "Joint2D": Joint2D, "JSON": JSON, "JSONParseResult": JSONParseResult, "JSONRPC": JSONRPC, "KinematicBody": KinematicBody, "KinematicBody2D": KinematicBody2D, "KinematicCollision": KinematicCollision, "KinematicCollision2D": KinematicCollision2D, "Label": Label, "LargeTexture": LargeTexture, "Light": Light, "Light2D": Light2D, "LightOccluder2D": LightOccluder2D, "Line2D": Line2D, "LineEdit": LineEdit, "LineShape2D": LineShape2D, "LinkButton": LinkButton, "Listener": Listener, "MainLoop": MainLoop, "MarginContainer": MarginContainer, "Marshalls": Marshalls, "Material": Material, "MenuButton": MenuButton, "Mesh": Mesh, "MeshDataTool": MeshDataTool, "MeshInstance": MeshInstance, "MeshInstance2D": MeshInstance2D, "MeshLibrary": MeshLibrary, "MeshTexture": MeshTexture, "MobileVRInterface": MobileVRInterface, "MultiMesh": MultiMesh, "MultiMeshInstance": MultiMeshInstance, "MultiMeshInstance2D": MultiMeshInstance2D, "MultiplayerAPI": MultiplayerAPI, "MultiplayerPeerGDNative": MultiplayerPeerGDNative, "Mutex": Mutex, "NativeScript": NativeScript, "Navigation": Navigation, "Navigation2D": Navigation2D, "NavigationMesh": NavigationMesh, "NavigationMeshInstance": NavigationMeshInstance, "NavigationPolygon": NavigationPolygon, "NavigationPolygonInstance": NavigationPolygonInstance, "NetworkedMultiplayerENet": NetworkedMultiplayerENet, "NetworkedMultiplayerPeer": NetworkedMultiplayerPeer, "NinePatchRect": NinePatchRect, "Node": Node, "Node2D": Node2D, "NoiseTexture": NoiseTexture, "OccluderPolygon2D": OccluderPolygon2D, "OmniLight": OmniLight, "OpenSimplexNoise": OpenSimplexNoise, "OptionButton": OptionButton, "OS": OS, "PackedDataContainer": PackedDataContainer, "PackedDataContainerRef": PackedDataContainerRef, "PackedScene": PackedScene, "PacketPeer": PacketPeer, "PacketPeerDTLS": PacketPeerDTLS, "PacketPeerGDNative": PacketPeerGDNative, "PacketPeerStream": PacketPeerStream, "PacketPeerUDP": PacketPeerUDP, "Panel": Panel, "PanelContainer": PanelContainer, "PanoramaSky": PanoramaSky, "ParallaxBackground": ParallaxBackground, "ParallaxLayer": ParallaxLayer, "Particles": Particles, "Particles2D": Particles2D, "ParticlesMaterial": ParticlesMaterial, "Path": Path, "Path2D": Path2D, "PathFollow": PathFollow, "PathFollow2D": PathFollow2D, "PCKPacker": PCKPacker, "Performance": Performance, "PHashTranslation": PHashTranslation, "PhysicalBone": PhysicalBone, "Physics2DDirectBodyState": Physics2DDirectBodyState, "Physics2DDirectSpaceState": Physics2DDirectSpaceState, "Physics2DServer": Physics2DServer, "Physics2DShapeQueryParameters": Physics2DShapeQueryParameters, "Physics2DTestMotionResult": Physics2DTestMotionResult, "PhysicsBody": PhysicsBody, "PhysicsBody2D": PhysicsBody2D, "PhysicsDirectBodyState": PhysicsDirectBodyState, "PhysicsDirectSpaceState": PhysicsDirectSpaceState, "PhysicsMaterial": PhysicsMaterial, "PhysicsServer": PhysicsServer, "PhysicsShapeQueryParameters": PhysicsShapeQueryParameters, "PinJoint": PinJoint, "PinJoint2D": PinJoint2D, "PlaneMesh": PlaneMesh, "PlaneShape": PlaneShape, "PluginScript": PluginScript, "PointMesh": PointMesh, "Polygon2D": Polygon2D, "PolygonPathFinder": PolygonPathFinder, "Popup": Popup, "PopupDialog": PopupDialog, "PopupMenu": PopupMenu, "PopupPanel": PopupPanel, "Position2D": Position2D, "Position3D": Position3D, "PrimitiveMesh": PrimitiveMesh, "PrismMesh": PrismMesh, "ProceduralSky": ProceduralSky, "ProgressBar": ProgressBar, "ProjectSettings": ProjectSettings, "ProximityGroup": ProximityGroup, "ProxyTexture": ProxyTexture, "QuadMesh": QuadMesh, "RandomNumberGenerator": RandomNumberGenerator, "Range": Range, "RayCast": RayCast, "RayCast2D": RayCast2D, "RayShape": RayShape, "RayShape2D": RayShape2D, "RectangleShape2D": RectangleShape2D, "Reference": Reference, "ReferenceRect": ReferenceRect, "ReflectionProbe": ReflectionProbe, "RegEx": RegEx, "RegExMatch": RegExMatch, "RemoteTransform": RemoteTransform, "RemoteTransform2D": RemoteTransform2D, "Resource": Resource, "ResourceFormatLoader": ResourceFormatLoader, "ResourceFormatSaver": ResourceFormatSaver, "ResourceImporter": ResourceImporter, "ResourceInteractiveLoader": ResourceInteractiveLoader, "ResourceLoader": ResourceLoader, "ResourcePreloader": ResourcePreloader, "ResourceSaver": ResourceSaver, "RichTextEffect": RichTextEffect, "RichTextLabel": RichTextLabel, "RigidBody": RigidBody, "RigidBody2D": RigidBody2D, "RootMotionView": RootMotionView, "SceneState": SceneState, "SceneTree": SceneTree, "SceneTreeTimer": SceneTreeTimer, "Script": Script, "ScriptCreateDialog": ScriptCreateDialog, "ScriptEditor": ScriptEditor, "ScrollBar": ScrollBar, "ScrollContainer": ScrollContainer, "SegmentShape2D": SegmentShape2D, "Semaphore": Semaphore, "Separator": Separator, "Shader": Shader, "ShaderMaterial": ShaderMaterial, "Shape": Shape, "Shape2D": Shape2D, "ShortCut": ShortCut, "Skeleton": Skeleton, "Skeleton2D": Skeleton2D, "SkeletonIK": SkeletonIK, "Skin": Skin, "SkinReference": SkinReference, "Sky": Sky, "Slider": Slider, "SliderJoint": SliderJoint, "SoftBody": SoftBody, "Spatial": Spatial, "SpatialGizmo": SpatialGizmo, "SpatialMaterial": SpatialMaterial, "SpatialVelocityTracker": SpatialVelocityTracker, "SphereMesh": SphereMesh, "SphereShape": SphereShape, "SpinBox": SpinBox, "SplitContainer": SplitContainer, "SpotLight": SpotLight, "SpringArm": SpringArm, "Sprite": Sprite, "Sprite3D": Sprite3D, "SpriteBase3D": SpriteBase3D, "SpriteFrames": SpriteFrames, "StaticBody": StaticBody, "StaticBody2D": StaticBody2D, "StreamPeer": StreamPeer, "StreamPeerBuffer": StreamPeerBuffer, "StreamPeerGDNative": StreamPeerGDNative, "StreamPeerSSL": StreamPeerSSL, "StreamPeerTCP": StreamPeerTCP, "StreamTexture": StreamTexture, "StyleBox": StyleBox, "StyleBoxEmpty": StyleBoxEmpty, "StyleBoxFlat": StyleBoxFlat, "StyleBoxLine": StyleBoxLine, "StyleBoxTexture": StyleBoxTexture, "SurfaceTool": SurfaceTool, "TabContainer": TabContainer, "Tabs": Tabs, "TCP_Server": TCP_Server, "TextEdit": TextEdit, "TextFile": TextFile, "Texture": Texture, "Texture3D": Texture3D, "TextureArray": TextureArray, "TextureButton": TextureButton, "TextureLayered": TextureLayered, "TextureProgress": TextureProgress, "TextureRect": TextureRect, "Theme": Theme, "Thread": Thread, "TileMap": TileMap, "TileSet": TileSet, "Timer": Timer, "ToolButton": ToolButton, "TouchScreenButton": TouchScreenButton, "Translation": Translation, "TranslationServer": TranslationServer, "Tree": Tree, "TreeItem": TreeItem, "TriangleMesh": TriangleMesh, "Tween": Tween, "UDPServer": UDPServer, "UndoRedo": UndoRedo, "UPNP": UPNP, "UPNPDevice": UPNPDevice, "VBoxContainer": VBoxContainer, "VehicleBody": VehicleBody, "VehicleWheel": VehicleWheel, "VideoPlayer": VideoPlayer, "VideoStream": VideoStream, "VideoStreamGDNative": VideoStreamGDNative, "VideoStreamTheora": VideoStreamTheora, "VideoStreamWebm": VideoStreamWebm, "Viewport": Viewport, "ViewportContainer": ViewportContainer, "ViewportTexture": ViewportTexture, "VisibilityEnabler": VisibilityEnabler, "VisibilityEnabler2D": VisibilityEnabler2D, "VisibilityNotifier": VisibilityNotifier, "VisibilityNotifier2D": VisibilityNotifier2D, "VisualInstance": VisualInstance, "VisualScript": VisualScript, "VisualScriptBasicTypeConstant": VisualScriptBasicTypeConstant, "VisualScriptBuiltinFunc": VisualScriptBuiltinFunc, "VisualScriptClassConstant": VisualScriptClassConstant, "VisualScriptComment": VisualScriptComment, "VisualScriptComposeArray": VisualScriptComposeArray, "VisualScriptCondition": VisualScriptCondition, "VisualScriptConstant": VisualScriptConstant, "VisualScriptConstructor": VisualScriptConstructor, "VisualScriptCustomNode": VisualScriptCustomNode, "VisualScriptDeconstruct": VisualScriptDeconstruct, "VisualScriptEditor": VisualScriptEditor, "VisualScriptEmitSignal": VisualScriptEmitSignal, "VisualScriptEngineSingleton": VisualScriptEngineSingleton, "VisualScriptExpression": VisualScriptExpression, "VisualScriptFunction": VisualScriptFunction, "VisualScriptFunctionCall": VisualScriptFunctionCall, "VisualScriptFunctionState": VisualScriptFunctionState, "VisualScriptGlobalConstant": VisualScriptGlobalConstant, "VisualScriptIndexGet": VisualScriptIndexGet, "VisualScriptIndexSet": VisualScriptIndexSet, "VisualScriptInputAction": VisualScriptInputAction, "VisualScriptIterator": VisualScriptIterator, "VisualScriptLists": VisualScriptLists, "VisualScriptLocalVar": VisualScriptLocalVar, "VisualScriptLocalVarSet": VisualScriptLocalVarSet, "VisualScriptMathConstant": VisualScriptMathConstant, "VisualScriptNode": VisualScriptNode, "VisualScriptOperator": VisualScriptOperator, "VisualScriptPreload": VisualScriptPreload, "VisualScriptPropertyGet": VisualScriptPropertyGet, "VisualScriptPropertySet": VisualScriptPropertySet, "VisualScriptResourcePath": VisualScriptResourcePath, "VisualScriptReturn": VisualScriptReturn, "VisualScriptSceneNode": VisualScriptSceneNode, "VisualScriptSceneTree": VisualScriptSceneTree, "VisualScriptSelect": VisualScriptSelect, "VisualScriptSelf": VisualScriptSelf, "VisualScriptSequence": VisualScriptSequence, "VisualScriptSubCall": VisualScriptSubCall, "VisualScriptSwitch": VisualScriptSwitch, "VisualScriptTypeCast": VisualScriptTypeCast, "VisualScriptVariableGet": VisualScriptVariableGet, "VisualScriptVariableSet": VisualScriptVariableSet, "VisualScriptWhile": VisualScriptWhile, "VisualScriptYield": VisualScriptYield, "VisualScriptYieldSignal": VisualScriptYieldSignal, "VisualServer": VisualServer, "VisualShader": VisualShader, "VisualShaderNode": VisualShaderNode, "VisualShaderNodeBooleanConstant": VisualShaderNodeBooleanConstant, "VisualShaderNodeBooleanUniform": VisualShaderNodeBooleanUniform, "VisualShaderNodeColorConstant": VisualShaderNodeColorConstant, "VisualShaderNodeColorFunc": VisualShaderNodeColorFunc, "VisualShaderNodeColorOp": VisualShaderNodeColorOp, "VisualShaderNodeColorUniform": VisualShaderNodeColorUniform, "VisualShaderNodeCompare": VisualShaderNodeCompare, "VisualShaderNodeCubeMap": VisualShaderNodeCubeMap, "VisualShaderNodeCubeMapUniform": VisualShaderNodeCubeMapUniform, "VisualShaderNodeCustom": VisualShaderNodeCustom, "VisualShaderNodeDeterminant": VisualShaderNodeDeterminant, "VisualShaderNodeDotProduct": VisualShaderNodeDotProduct, "VisualShaderNodeExpression": VisualShaderNodeExpression, "VisualShaderNodeFaceForward": VisualShaderNodeFaceForward, "VisualShaderNodeFresnel": VisualShaderNodeFresnel, "VisualShaderNodeGlobalExpression": VisualShaderNodeGlobalExpression, "VisualShaderNodeGroupBase": VisualShaderNodeGroupBase, "VisualShaderNodeIf": VisualShaderNodeIf, "VisualShaderNodeInput": VisualShaderNodeInput, "VisualShaderNodeIs": VisualShaderNodeIs, "VisualShaderNodeOuterProduct": VisualShaderNodeOuterProduct, "VisualShaderNodeOutput": VisualShaderNodeOutput, "VisualShaderNodeScalarClamp": VisualShaderNodeScalarClamp, "VisualShaderNodeScalarConstant": VisualShaderNodeScalarConstant, "VisualShaderNodeScalarDerivativeFunc": VisualShaderNodeScalarDerivativeFunc, "VisualShaderNodeScalarFunc": VisualShaderNodeScalarFunc, "VisualShaderNodeScalarInterp": VisualShaderNodeScalarInterp, "VisualShaderNodeScalarOp": VisualShaderNodeScalarOp, "VisualShaderNodeScalarSmoothStep": VisualShaderNodeScalarSmoothStep, "VisualShaderNodeScalarSwitch": VisualShaderNodeScalarSwitch, "VisualShaderNodeScalarUniform": VisualShaderNodeScalarUniform, "VisualShaderNodeSwitch": VisualShaderNodeSwitch, "VisualShaderNodeTexture": VisualShaderNodeTexture, "VisualShaderNodeTextureUniform": VisualShaderNodeTextureUniform, "VisualShaderNodeTextureUniformTriplanar": VisualShaderNodeTextureUniformTriplanar, "VisualShaderNodeTransformCompose": VisualShaderNodeTransformCompose, "VisualShaderNodeTransformConstant": VisualShaderNodeTransformConstant, "VisualShaderNodeTransformDecompose": VisualShaderNodeTransformDecompose, "VisualShaderNodeTransformFunc": VisualShaderNodeTransformFunc, "VisualShaderNodeTransformMult": VisualShaderNodeTransformMult, "VisualShaderNodeTransformUniform": VisualShaderNodeTransformUniform, "VisualShaderNodeTransformVecMult": VisualShaderNodeTransformVecMult, "VisualShaderNodeUniform": VisualShaderNodeUniform, "VisualShaderNodeUniformRef": VisualShaderNodeUniformRef, "VisualShaderNodeVec3Constant": VisualShaderNodeVec3Constant, "VisualShaderNodeVec3Uniform": VisualShaderNodeVec3Uniform, "VisualShaderNodeVectorClamp": VisualShaderNodeVectorClamp, "VisualShaderNodeVectorCompose": VisualShaderNodeVectorCompose, "VisualShaderNodeVectorDecompose": VisualShaderNodeVectorDecompose, "VisualShaderNodeVectorDerivativeFunc": VisualShaderNodeVectorDerivativeFunc, "VisualShaderNodeVectorDistance": VisualShaderNodeVectorDistance, "VisualShaderNodeVectorFunc": VisualShaderNodeVectorFunc, "VisualShaderNodeVectorInterp": VisualShaderNodeVectorInterp, "VisualShaderNodeVectorLen": VisualShaderNodeVectorLen, "VisualShaderNodeVectorOp": VisualShaderNodeVectorOp, "VisualShaderNodeVectorRefract": VisualShaderNodeVectorRefract, "VisualShaderNodeVectorScalarMix": VisualShaderNodeVectorScalarMix, "VisualShaderNodeVectorScalarSmoothStep": VisualShaderNodeVectorScalarSmoothStep, "VisualShaderNodeVectorScalarStep": VisualShaderNodeVectorScalarStep, "VisualShaderNodeVectorSmoothStep": VisualShaderNodeVectorSmoothStep, "VScrollBar": VScrollBar, "VSeparator": VSeparator, "VSlider": VSlider, "VSplitContainer": VSplitContainer, "WeakRef": WeakRef, "WebRTCDataChannel": WebRTCDataChannel, "WebRTCDataChannelGDNative": WebRTCDataChannelGDNative, "WebRTCMultiplayer": WebRTCMultiplayer, "WebRTCPeerConnection": WebRTCPeerConnection, "WebRTCPeerConnectionGDNative": WebRTCPeerConnectionGDNative, "WebSocketClient": WebSocketClient, "WebSocketMultiplayerPeer": WebSocketMultiplayerPeer, "WebSocketPeer": WebSocketPeer, "WebSocketServer": WebSocketServer, "WebXRInterface": WebXRInterface, "WindowDialog": WindowDialog, "World": World, "World2D": World2D, "WorldEnvironment": WorldEnvironment, "X509Certificate": X509Certificate, "XMLParser": XMLParser, "YSort": YSort}
var global_name_generator
var Null
var Bool
var Int
var Float
var _String
var _Vector2
var _Rect2
var _Vector3
var _Transform2D
var _Plane
var _Quat
var _AABB
var _Basis
var _Transform
var _Color
var _NodePath
var _RID
var _Object
var _Dictionary
var _Array
var _PoolByteArray
var _PoolIntArray
var _PoolRealArray
var _PoolStringArray
var _PoolVector2Array
var _PoolVector3Array
var _PoolColorArray
var Any
var AnyRef
var AnyVal
var Number
var BaseArray
var Nothing
var native_types_lookup
func _typeof(value_11):
    var t_12 = typeof(value_11)
    var _cond_13 = null
    if t_12 != TYPE_OBJECT:
        _cond_13 = PrimitiveType.new(t_12)
    else:
        var _cond_14 = value_11.get_script()
        _cond_13 = _cond_14 if _cond_14 else self.native_types_lookup.get(value_11.get_class(), self.Any)
    return _cond_13
static func cons(a_15, b_16):
    return Cons.new(a_15, b_16)
static func car(a_17):
    return a_17.car
static func cdr(a_18):
    return a_18.cdr
static func init(a_19):
    var _cond_20 = cons(car(a_19), init(cdr(a_19))) if cdr(a_19) is Cons else nil
    return _cond_20
static func tail(a_21):
    var _cond_22 = tail(cdr(a_21)) if cdr(a_21) is Cons else car(a_21)
    return _cond_22
static func set_car(b_23, a_24):
    a_24.car = b_23
    return b_23
static func set_cdr(b_25, a_26):
    a_26.cdr = b_25
    return b_25
static func intern(a_27):
    return Symbol.new(a_27)
static func length(x_28):
    var result_29 = 0
    while x_28 is Cons:
        result_29 = result_29 + 1
        x_28 = x_28.cdr
    return result_29
static func funcall(f_30, args_31):
    return apply(f_30, GDLisp.Cons.new(args_31, null))
static func sys_DIV_funcall(f_32, args_33):
    return apply(f_32, GDLisp.Cons.new(args_33, null))
static func apply(f_34, args_35):
    var args1_36 = init(args_35)
    var args2_37 = tail(args_35)
    var _cond_38 = f_34.call_funcv(append(GDLisp.Cons.new(args1_36, GDLisp.Cons.new(args2_37, null)))) if f_34 is Function else push_error("Attempt to call non-function")
    return _cond_38
static func _PLUS_(args_39):
    var _cond_40 = null
    if args_39 is Cons:
        var result_41 = args_39.car
        args_39 = args_39.cdr
        while args_39 is Cons:
            result_41 = result_41 + args_39.car
            args_39 = args_39.cdr
        _cond_40 = result_41
    else:
        _cond_40 = 0
    return _cond_40
static func _TIMES_(args_42):
    var result_43 = 1
    while args_42 is Cons:
        result_43 = result_43 * args_42.car
        args_42 = args_42.cdr
    return result_43
static func _(x_44, args_45):
    var _cond_46 = null
    if args_45 is Cons:
        var result_47 = x_44
        while args_45 is Cons:
            result_47 = result_47 - args_45.car
            args_45 = args_45.cdr
        _cond_46 = result_47
    else:
        _cond_46 = -x_44
    return _cond_46
static func _DIV_(x_48, args_49):
    var _cond_50 = null
    if args_49 is Cons:
        var result_51 = x_48
        while args_49 is Cons:
            result_51 = result_51 / float(args_49.car)
            args_49 = args_49.cdr
        _cond_50 = result_51
    else:
        _cond_50 = 1 / float(x_48)
    return _cond_50
static func div(x_52, args_53):
    var _cond_54 = null
    if args_53 is Cons:
        var result_55 = x_52
        while args_53 is Cons:
            result_55 = result_55 / int(args_53.car)
            args_53 = args_53.cdr
        _cond_54 = result_55
    else:
        _cond_54 = 1 / int(x_52)
    return _cond_54
static func mod(x_56, y_57):
    return x_56 % y_57
static func _EQ_(x_58, args_59):
    while args_59 is Cons:
        if x_58 == args_59.car:
            pass
        else:
            return false
        x_58 = args_59.car
        args_59 = args_59.cdr
    return true
static func _LT_(x_60, args_61):
    while args_61 is Cons:
        if x_60 < args_61.car:
            pass
        else:
            return false
        x_60 = args_61.car
        args_61 = args_61.cdr
    return true
static func _GT_(x_62, args_63):
    while args_63 is Cons:
        if x_62 > args_63.car:
            pass
        else:
            return false
        x_62 = args_63.car
        args_63 = args_63.cdr
    return true
static func _LT__EQ_(x_64, args_65):
    while args_65 is Cons:
        if x_64 <= args_65.car:
            pass
        else:
            return false
        x_64 = args_65.car
        args_65 = args_65.cdr
    return true
static func _GT__EQ_(x_66, args_67):
    while args_67 is Cons:
        if x_66 >= args_67.car:
            pass
        else:
            return false
        x_66 = args_67.car
        args_67 = args_67.cdr
    return true
static func _DIV__EQ_(x_68, args_69):
    var outer_70 = cons(x_68, args_69)
    while outer_70 is Cons:
        var inner_71 = outer_70.cdr
        while inner_71 is Cons:
            if outer_70.car != inner_71.car:
                pass
            else:
                return false
            inner_71 = inner_71.cdr
        outer_70 = outer_70.cdr
    return true
static func _not(x_72):
    return !x_72
static func list(args_73):
    return args_73
static func vector(x_74, y_75, z_76):
    var _cond_77 = Vector2(x_74, y_75) if z_76 == null else Vector3(x_74, y_75, z_76)
    return _cond_77
static func list_to_array(list_78):
    var arr_79 = []
    while list_78 is Cons:
        arr_79.push_back(list_78.car)
        list_78 = list_78.cdr
    return arr_79
static func array_to_list(arr_80):
    var outer_81 = cons(null, null)
    var curr_82 = outer_81
    for elem_83 in arr_80:
        curr_82.cdr = cons(elem_83, null)
        curr_82 = curr_82.cdr
    return outer_81.cdr
static func elt(arr_84, n_85):
    return arr_84[n_85]
static func set_elt(x_86, arr_87, n_88):
    arr_87[n_88] = x_86
    return arr_87[n_88]
static func is_member(value_89, arr_90):
    return value_89 in arr_90
static func sys_DIV_get_node(obj_91, path_92):
    return obj_91.get_node(path_92)
static func sys_DIV_native_class_private():
    var x_93 = GDScript
    return x_93.get_class()
static func is_instance(value_94, type_95):
    var _cond_96 = type_95.is_satisfies(value_94) if type_95 is GDLispSpecialType else value_94 is type_95
    return _cond_96
static func is_sys_DIV_instance_direct(value_97, type_98):
    return value_97 is type_98
static func gensym(prefix_99):
    var _cond_100 = Symbol.new(GDLisp.global_name_generator.generate()) if prefix_99 == null else Symbol.new(GDLisp.global_name_generator.generate_with(prefix_99))
    return _cond_100
static func map(f_101, xs_102):
    var _cond_103 = null
    var _cond_106 = true if xs_102 is Cons else true if xs_102 == nil else null
    if _cond_106:
        var outer_107 = cons(nil, nil)
        var curr_108 = outer_107
        while xs_102 != nil:
            curr_108.cdr = cons(funcall(f_101, GDLisp.Cons.new(xs_102.car, null)), nil)
            curr_108 = curr_108.cdr
            xs_102 = xs_102.cdr
        _cond_103 = outer_107.cdr
    else:
        var result_104 = []
        for i_105 in len(xs_102):
            result_104.push_back(funcall(f_101, GDLisp.Cons.new(xs_102[i_105], null)))
        _cond_103 = result_104
    return _cond_103
static func filter(p_109, xs_110):
    var _cond_111 = null
    var _cond_114 = true if xs_110 is Cons else true if xs_110 == nil else false
    if _cond_114:
        var outer_115 = cons(nil, nil)
        var curr_116 = outer_115
        while xs_110 != nil:
            if funcall(p_109, GDLisp.Cons.new(xs_110.car, null)):
                curr_116.cdr = cons(xs_110.car, nil)
                curr_116 = curr_116.cdr
            xs_110 = xs_110.cdr
        _cond_111 = outer_115.cdr
    else:
        var result_112 = []
        for i_113 in len(xs_110):
            if funcall(p_109, GDLisp.Cons.new(xs_110[i_113], null)):
                result_112.push_back(xs_110[i_113])
        _cond_111 = result_112
    return _cond_111
static func reverse(arg_117):
    var rev_118 = nil
    while arg_117 != nil:
        rev_118 = cons(car(arg_117), rev_118)
        arg_117 = arg_117.cdr
    return rev_118
static func append(args_119):
    var outer_120 = cons(nil, nil)
    var curr_121 = outer_120
    while args_119 != nil:
        var inner_value_122 = args_119.car
        while inner_value_122 != nil:
            curr_121.cdr = cons(inner_value_122.car, nil)
            curr_121 = curr_121.cdr
            inner_value_122 = inner_value_122.cdr
        args_119 = args_119.cdr
    return outer_120.cdr
static func sys_DIV_qq_smart_list(a_123):
    var t_124 = typeof(a_123)
    var _cond_125 = array_to_list(a_123) if TYPE_ARRAY <= t_124 && t_124 <= TYPE_COLOR_ARRAY else a_123
    return _cond_125
static func sys_DIV_qq_smart_array(a_126):
    var t_127 = typeof(a_126)
    var _cond_128 = a_126 if TYPE_ARRAY <= t_127 && t_127 <= TYPE_COLOR_ARRAY else list_to_array(a_126)
    return _cond_128
static func _PI():
    return GDLisp.cons(GDLisp.intern("literally"), GDLisp.cons(GDLisp.intern("PI"), null))
static func _SPKEY():
    return GDLisp.cons(GDLisp.intern("literally"), GDLisp.cons(GDLisp.intern("SPKEY"), null))
class GDLispSpecialType extends Reference:
    func _init():
        pass
class PrimitiveType extends GDLispSpecialType:
    func _init(primitive_value_130):
        self.primitive_value = primitive_value_130
    var primitive_value
    func is_satisfies(value_131):
        return typeof(value_131) == self.primitive_value
    var __gdlisp_outer_class_129 = load("res://GDLisp.gd")
class AnyType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_132):
        return true
class AnyRefType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_134):
        return typeof(value_134) == TYPE_OBJECT
    var __gdlisp_outer_class_133 = load("res://GDLisp.gd")
class AnyValType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_136):
        return typeof(value_136) != TYPE_OBJECT
    var __gdlisp_outer_class_135 = load("res://GDLisp.gd")
class NumberType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_138):
        var t_139 = typeof(value_138)
        var _cond_140 = true if t_139 == TYPE_INT else true if t_139 == TYPE_REAL else false
        return _cond_140
    var __gdlisp_outer_class_137 = load("res://GDLisp.gd")
class BaseArrayType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_142):
        var _cmp_143 = typeof(value_142)
        return TYPE_ARRAY <= _cmp_143 && _cmp_143 <= TYPE_COLOR_ARRAY
    var __gdlisp_outer_class_141 = load("res://GDLisp.gd")
class NothingType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_144):
        return false
static func _or(args_145):
    var args_146 = reverse(args_145)
    var _cond_147 = null
    if args_146:
        var result_148 = cons(cons(true, cons(car(args_146), null)), null)
        args_146 = cdr(args_146)
        while args_146 != nil:
            result_148 = cons(cons(car(args_146), null), result_148)
            args_146 = cdr(args_146)
        _cond_147 = cons(GDLisp.intern("cond"), result_148)
    else:
        _cond_147 = false
    return _cond_147
static func _and(args_149):
    var args_150 = reverse(args_149)
    var _cond_151 = null
    if args_150:
        var result_152 = cons(cons(true, cons(car(args_150), null)), null)
        args_150 = cdr(args_150)
        while args_150 != nil:
            result_152 = cons(cons(cons(GDLisp.intern("not"), cons(car(args_150), null)), cons(false, null)), result_152)
            args_150 = cdr(args_150)
        _cond_151 = cons(GDLisp.intern("cond"), result_152)
    else:
        _cond_151 = true
    return _cond_151
static func let_TIMES_(vars_153, body_154):
    var _cond_155 = null
    if vars_153 == nil:
        _cond_155 = cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_DIV_qq_smart_list(body_154), GDLisp.Cons.new(null, null))))
    else:
        var _quasiquote_156 = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(body_154), GDLisp.Cons.new(null, null)))
        _cond_155 = cons(GDLisp.intern("let"), cons(cons(car(vars_153), null), cons(cons(GDLisp.intern("let*"), cons(cdr(vars_153), _quasiquote_156)), null)))
    return _cond_155
static func defvars(args_157):
    var arr_158 = []
    while args_157 != nil:
        arr_158.push_back(GDLisp.Cons.new(GDLisp.intern("defvar"), GDLisp.Cons.new(args_157.car, null)))
        args_157 = args_157.cdr
    return cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_DIV_qq_smart_list(arr_158), GDLisp.Cons.new(null, null))))
static func when(cnd_159, args_160):
    var _quasiquote_161 = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(args_160), GDLisp.Cons.new(null, null)))
    return cons(GDLisp.intern("cond"), cons(cons(cnd_159, cons(cons(GDLisp.intern("progn"), _quasiquote_161), null)), null))
static func unless(cnd_162, args_163):
    var _quasiquote_164 = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(args_163), GDLisp.Cons.new(null, null)))
    return cons(GDLisp.intern("cond"), cons(cons(cnd_162, cons(null, null)), cons(cons(true, cons(cons(GDLisp.intern("progn"), _quasiquote_164), null)), null)))
static func _if(cnd_166, t_167, f_168):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_166, cons(t_167, null)), cons(cons(true, cons(f_168, null)), null)))
static func yield_TIMES_(arg_170):
    var symbol_171 = gensym("_yield")
    var _quasiquote_173 = cons(GDLisp.intern("is-valid"), null)
    var _quasiquote_174 = cons(cons(GDLisp.intern("instance?"), cons(symbol_171, cons(GDLisp.intern("GDScriptFunctionState"), null))), cons(cons(cons(GDLisp.intern("access-slot"), cons(symbol_171, _quasiquote_173)), null), null))
    var _quasiquote_175 = cons(symbol_171, cons(GDLisp.intern("resume"), null))
    var _quasiquote_178 = cons(cons(GDLisp.intern("yield"), null), cons(cons(GDLisp.intern("set"), cons(symbol_171, cons(cons(cons(GDLisp.intern("access-slot"), _quasiquote_175), null), null))), null))
    return cons(GDLisp.intern("let"), cons(cons(cons(symbol_171, cons(arg_170, null)), null), cons(cons(GDLisp.intern("while"), cons(cons(GDLisp.intern("and"), _quasiquote_174), _quasiquote_178)), cons(symbol_171, null))))
static func this_file():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-file"), null))
static func this_filename():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-filename"), null))
static func this_true_filename():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-true-filename"), null))
static func contextual_load(arg_179):
    return cons(GDLisp.intern("load"), cons(cons(GDLisp.intern("sys/context-filename"), cons(arg_179, null)), null))
static func deflazy(name_180, value_181, modifiers_182):
    var fn_name_183 = gensym("_lazy")
    var this_file_184 = gensym("_this_file")
    var value_var_185 = gensym("_value")
    var meta_name_186 = "__gdlisp_Lazy_{}".format([gensym(null).contents], "{}")
    var _quasiquote_187 = cons(cons(GDLisp.intern("this-file"), null), null)
    var _quasiquote_189 = cons(GDLisp.intern("get-meta"), null)
    var _quasiquote_190 = cons(value_181, null)
    var _quasiquote_193 = cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file_184, cons(GDLisp.intern("set-meta"), null))), cons(meta_name_186, cons(value_var_185, null))), cons(value_var_185, null))
    var _quasiquote_194 = cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file_184, cons(GDLisp.intern("has-meta"), null))), cons(meta_name_186, null)), cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file_184, _quasiquote_189)), cons(meta_name_186, null)), cons(cons(GDLisp.intern("let"), cons(cons(cons(value_var_185, _quasiquote_190), null), _quasiquote_193)), null)))
    var _quasiquote_195 = cons(cons(GDLisp.intern("let"), cons(cons(cons(this_file_184, _quasiquote_187), null), cons(cons(GDLisp.intern("if"), _quasiquote_194), null))), null)
    var _quasiquote_196 = cons(GDLisp.intern("access-slot"), null)
    var _quasiquote_199 = cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("quote"), cons(GDLisp.intern("contextual-load"), null)), cons(cons(GDLisp.intern("this-true-filename"), null), null))), cons(cons(GDLisp.intern("quote"), cons(fn_name_183, null)), null))
    var _quasiquote_200 = cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("quote"), _quasiquote_196), _quasiquote_199)), null)), append(GDLisp.Cons.new(sys_DIV_qq_smart_list(modifiers_182), GDLisp.Cons.new(null, null))))
    return cons(GDLisp.intern("progn"), cons(cons(GDLisp.intern("defn"), cons(fn_name_183, cons(null, _quasiquote_195))), cons(cons(GDLisp.intern("define-symbol-macro"), cons(name_180, _quasiquote_200)), null)))
static func defobject(name_201, parent_202, visibility_203, body_204):
    if visibility_203 == nil:
        visibility_203 = GDLisp.intern("public")
    elif !is_instance(visibility_203, Symbol):
        body_204 = cons(visibility_203, body_204)
        visibility_203 = GDLisp.intern("public")
    elif visibility_203.contents == "public":
        pass
    elif visibility_203.contents == "private":
        pass
    else:
        body_204 = cons(visibility_203, body_204)
        visibility_203 = GDLisp.intern("public")
    var _quasiquote_205 = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(body_204), GDLisp.Cons.new(null, null)))
    return cons(GDLisp.intern("deflazy"), cons(name_201, cons(cons(GDLisp.intern("new"), cons(parent_202, _quasiquote_205)), cons(visibility_203, null))))
static func run():
    return null
