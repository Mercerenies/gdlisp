extends Node
const nil = null
class Cons extends Reference:
    func _init(car, cdr):
        self.car = car
        self.cdr = cdr
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
    func _init(contents):
        self.contents = contents
    var contents
class Symbol extends Reference:
    func _init(contents):
        self.contents = contents
    var contents
class FreshNameGenerator extends Reference:
    func _init(reserved, index):
        self.reserved = reserved
        self.index = index
    const DEFAULT_PREFIX = "_G"
    var reserved
    var index
    func generate():
        return self.generate_with(FreshNameGenerator.DEFAULT_PREFIX)
    func generate_with(prefix):
        var name = "{}_{}".format([prefix, self.index], "{}")
        self.index = self.index + 1
        var _cond = self.generate_with(prefix) if name in self.reserved else name
        return _cond
    func to_json():
        return {"reserved": self.reserved, "index": self.index}
    static func from_json(json):
        return FreshNameGenerator.new(json["reserved"], json["index"])
    var __gdlisp_outer_class_0 = load("res://GDLisp.gd")
enum Corner {
    BOTTOM_LEFT = CORNER_BOTTOM_LEFT,
    BOTTOM_RIGHT = CORNER_BOTTOM_RIGHT,
    TOP_LEFT = CORNER_TOP_LEFT,
    TOP_RIGHT = CORNER_TOP_RIGHT,
}
enum Err {
    ALREADY_EXISTS = ERR_ALREADY_EXISTS,
    ALREADY_IN_USE = ERR_ALREADY_IN_USE,
    BUG = ERR_BUG,
    BUSY = ERR_BUSY,
    CANT_ACQUIRE_RESOURCE = ERR_CANT_ACQUIRE_RESOURCE,
    CANT_CONNECT = ERR_CANT_CONNECT,
    CANT_CREATE = ERR_CANT_CREATE,
    CANT_FORK = ERR_CANT_FORK,
    CANT_OPEN = ERR_CANT_OPEN,
    CANT_RESOLVE = ERR_CANT_RESOLVE,
    COMPILATION_FAILED = ERR_COMPILATION_FAILED,
    CONNECTION_ERROR = ERR_CONNECTION_ERROR,
    CYCLIC_LINK = ERR_CYCLIC_LINK,
    DATABASE_CANT_READ = ERR_DATABASE_CANT_READ,
    DATABASE_CANT_WRITE = ERR_DATABASE_CANT_WRITE,
    DOES_NOT_EXIST = ERR_DOES_NOT_EXIST,
    DUPLICATE_SYMBOL = ERR_DUPLICATE_SYMBOL,
    _FAILED = FAILED,
    FILE_ALREADY_IN_USE = ERR_FILE_ALREADY_IN_USE,
    FILE_BAD_DRIVE = ERR_FILE_BAD_DRIVE,
    FILE_BAD_PATH = ERR_FILE_BAD_PATH,
    FILE_CANT_OPEN = ERR_FILE_CANT_OPEN,
    FILE_CANT_READ = ERR_FILE_CANT_READ,
    FILE_CANT_WRITE = ERR_FILE_CANT_WRITE,
    FILE_CORRUPT = ERR_FILE_CORRUPT,
    FILE_EOF = ERR_FILE_EOF,
    FILE_MISSING_DEPENDENCIES = ERR_FILE_MISSING_DEPENDENCIES,
    FILE_NOT_FOUND = ERR_FILE_NOT_FOUND,
    FILE_NO_PERMISSION = ERR_FILE_NO_PERMISSION,
    FILE_UNRECOGNIZED = ERR_FILE_UNRECOGNIZED,
    HELP = ERR_HELP,
    INVALID_DATA = ERR_INVALID_DATA,
    INVALID_DECLARATION = ERR_INVALID_DECLARATION,
    INVALID_PARAMETER = ERR_INVALID_PARAMETER,
    LINK_FAILED = ERR_LINK_FAILED,
    LOCKED = ERR_LOCKED,
    METHOD_NOT_FOUND = ERR_METHOD_NOT_FOUND,
    _OK = OK,
    OUT_OF_MEMORY = ERR_OUT_OF_MEMORY,
    PARAMETER_RANGE_ERROR = ERR_PARAMETER_RANGE_ERROR,
    PARSE_ERROR = ERR_PARSE_ERROR,
    PRINTER_ON_FIRE = ERR_PRINTER_ON_FIRE,
    QUERY_FAILED = ERR_QUERY_FAILED,
    SCRIPT_FAILED = ERR_SCRIPT_FAILED,
    SKIP = ERR_SKIP,
    TIMEOUT = ERR_TIMEOUT,
    UNAUTHORIZED = ERR_UNAUTHORIZED,
    UNAVAILABLE = ERR_UNAVAILABLE,
    UNCONFIGURED = ERR_UNCONFIGURED,
}
enum HAlign {
    CENTER = HALIGN_CENTER,
    LEFT = HALIGN_LEFT,
    RIGHT = HALIGN_RIGHT,
}
enum Joy {
    ANALOG_L2 = JOY_ANALOG_L2,
    ANALOG_LX = JOY_ANALOG_LX,
    ANALOG_LY = JOY_ANALOG_LY,
    ANALOG_R2 = JOY_ANALOG_R2,
    ANALOG_RX = JOY_ANALOG_RX,
    ANALOG_RY = JOY_ANALOG_RY,
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
    BUTTON_0 = JOY_BUTTON_0,
    BUTTON_1 = JOY_BUTTON_1,
    BUTTON_10 = JOY_BUTTON_10,
    BUTTON_11 = JOY_BUTTON_11,
    BUTTON_12 = JOY_BUTTON_12,
    BUTTON_13 = JOY_BUTTON_13,
    BUTTON_14 = JOY_BUTTON_14,
    BUTTON_15 = JOY_BUTTON_15,
    BUTTON_16 = JOY_BUTTON_16,
    BUTTON_17 = JOY_BUTTON_17,
    BUTTON_18 = JOY_BUTTON_18,
    BUTTON_19 = JOY_BUTTON_19,
    BUTTON_2 = JOY_BUTTON_2,
    BUTTON_20 = JOY_BUTTON_20,
    BUTTON_21 = JOY_BUTTON_21,
    BUTTON_22 = JOY_BUTTON_22,
    BUTTON_3 = JOY_BUTTON_3,
    BUTTON_4 = JOY_BUTTON_4,
    BUTTON_5 = JOY_BUTTON_5,
    BUTTON_6 = JOY_BUTTON_6,
    BUTTON_7 = JOY_BUTTON_7,
    BUTTON_8 = JOY_BUTTON_8,
    BUTTON_9 = JOY_BUTTON_9,
    BUTTON_MAX = JOY_BUTTON_MAX,
    DPAD_DOWN = JOY_DPAD_DOWN,
    DPAD_LEFT = JOY_DPAD_LEFT,
    DPAD_RIGHT = JOY_DPAD_RIGHT,
    DPAD_UP = JOY_DPAD_UP,
    DS_A = JOY_DS_A,
    DS_B = JOY_DS_B,
    DS_X = JOY_DS_X,
    DS_Y = JOY_DS_Y,
    GUIDE = JOY_GUIDE,
    INVALID_OPTION = JOY_INVALID_OPTION,
    L = JOY_L,
    L2 = JOY_L2,
    L3 = JOY_L3,
    MISC1 = JOY_MISC1,
    OCULUS_AX = JOY_OCULUS_AX,
    OCULUS_BY = JOY_OCULUS_BY,
    OCULUS_MENU = JOY_OCULUS_MENU,
    OPENVR_MENU = JOY_OPENVR_MENU,
    OPENVR_TOUCHPADX = JOY_OPENVR_TOUCHPADX,
    OPENVR_TOUCHPADY = JOY_OPENVR_TOUCHPADY,
    PADDLE1 = JOY_PADDLE1,
    PADDLE2 = JOY_PADDLE2,
    PADDLE3 = JOY_PADDLE3,
    PADDLE4 = JOY_PADDLE4,
    R = JOY_R,
    R2 = JOY_R2,
    R3 = JOY_R3,
    SELECT = JOY_SELECT,
    SONY_CIRCLE = JOY_SONY_CIRCLE,
    SONY_SQUARE = JOY_SONY_SQUARE,
    SONY_TRIANGLE = JOY_SONY_TRIANGLE,
    SONY_X = JOY_SONY_X,
    START = JOY_START,
    TOUCHPAD = JOY_TOUCHPAD,
    VR_ANALOG_GRIP = JOY_VR_ANALOG_GRIP,
    VR_ANALOG_TRIGGER = JOY_VR_ANALOG_TRIGGER,
    VR_GRIP = JOY_VR_GRIP,
    VR_PAD = JOY_VR_PAD,
    VR_TRIGGER = JOY_VR_TRIGGER,
    XBOX_A = JOY_XBOX_A,
    XBOX_B = JOY_XBOX_B,
    XBOX_X = JOY_XBOX_X,
    XBOX_Y = JOY_XBOX_Y,
}
enum Key {
    _0 = KEY_0,
    _1 = KEY_1,
    _2 = KEY_2,
    _3 = KEY_3,
    _4 = KEY_4,
    _5 = KEY_5,
    _6 = KEY_6,
    _7 = KEY_7,
    _8 = KEY_8,
    _9 = KEY_9,
    A = KEY_A,
    AACUTE = KEY_AACUTE,
    ACIRCUMFLEX = KEY_ACIRCUMFLEX,
    ACUTE = KEY_ACUTE,
    ADIAERESIS = KEY_ADIAERESIS,
    AE = KEY_AE,
    AGRAVE = KEY_AGRAVE,
    ALT = KEY_ALT,
    AMPERSAND = KEY_AMPERSAND,
    APOSTROPHE = KEY_APOSTROPHE,
    ARING = KEY_ARING,
    ASCIICIRCUM = KEY_ASCIICIRCUM,
    ASCIITILDE = KEY_ASCIITILDE,
    ASTERISK = KEY_ASTERISK,
    AT = KEY_AT,
    ATILDE = KEY_ATILDE,
    B = KEY_B,
    BACK = KEY_BACK,
    BACKSLASH = KEY_BACKSLASH,
    BACKSPACE = KEY_BACKSPACE,
    BACKTAB = KEY_BACKTAB,
    BAR = KEY_BAR,
    BASSBOOST = KEY_BASSBOOST,
    BASSDOWN = KEY_BASSDOWN,
    BASSUP = KEY_BASSUP,
    BRACELEFT = KEY_BRACELEFT,
    BRACERIGHT = KEY_BRACERIGHT,
    BRACKETLEFT = KEY_BRACKETLEFT,
    BRACKETRIGHT = KEY_BRACKETRIGHT,
    BROKENBAR = KEY_BROKENBAR,
    C = KEY_C,
    CAPSLOCK = KEY_CAPSLOCK,
    CCEDILLA = KEY_CCEDILLA,
    CEDILLA = KEY_CEDILLA,
    CENT = KEY_CENT,
    CLEAR = KEY_CLEAR,
    CODE_MASK = KEY_CODE_MASK,
    COLON = KEY_COLON,
    COMMA = KEY_COMMA,
    CONTROL = KEY_CONTROL,
    COPYRIGHT = KEY_COPYRIGHT,
    CURRENCY = KEY_CURRENCY,
    D = KEY_D,
    DEGREE = KEY_DEGREE,
    DELETE = KEY_DELETE,
    DIAERESIS = KEY_DIAERESIS,
    DIRECTION_L = KEY_DIRECTION_L,
    DIRECTION_R = KEY_DIRECTION_R,
    DIVISION = KEY_DIVISION,
    DOLLAR = KEY_DOLLAR,
    DOWN = KEY_DOWN,
    E = KEY_E,
    EACUTE = KEY_EACUTE,
    ECIRCUMFLEX = KEY_ECIRCUMFLEX,
    EDIAERESIS = KEY_EDIAERESIS,
    EGRAVE = KEY_EGRAVE,
    END = KEY_END,
    ENTER = KEY_ENTER,
    EQUAL = KEY_EQUAL,
    ESCAPE = KEY_ESCAPE,
    ETH = KEY_ETH,
    EXCLAM = KEY_EXCLAM,
    EXCLAMDOWN = KEY_EXCLAMDOWN,
    F = KEY_F,
    F1 = KEY_F1,
    F10 = KEY_F10,
    F11 = KEY_F11,
    F12 = KEY_F12,
    F13 = KEY_F13,
    F14 = KEY_F14,
    F15 = KEY_F15,
    F16 = KEY_F16,
    F2 = KEY_F2,
    F3 = KEY_F3,
    F4 = KEY_F4,
    F5 = KEY_F5,
    F6 = KEY_F6,
    F7 = KEY_F7,
    F8 = KEY_F8,
    F9 = KEY_F9,
    FAVORITES = KEY_FAVORITES,
    FORWARD = KEY_FORWARD,
    G = KEY_G,
    GREATER = KEY_GREATER,
    GUILLEMOTLEFT = KEY_GUILLEMOTLEFT,
    GUILLEMOTRIGHT = KEY_GUILLEMOTRIGHT,
    H = KEY_H,
    HELP = KEY_HELP,
    HOME = KEY_HOME,
    HOMEPAGE = KEY_HOMEPAGE,
    HYPER_L = KEY_HYPER_L,
    HYPER_R = KEY_HYPER_R,
    HYPHEN = KEY_HYPHEN,
    I = KEY_I,
    IACUTE = KEY_IACUTE,
    ICIRCUMFLEX = KEY_ICIRCUMFLEX,
    IDIAERESIS = KEY_IDIAERESIS,
    IGRAVE = KEY_IGRAVE,
    INSERT = KEY_INSERT,
    J = KEY_J,
    K = KEY_K,
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
    KP_ADD = KEY_KP_ADD,
    KP_DIVIDE = KEY_KP_DIVIDE,
    KP_ENTER = KEY_KP_ENTER,
    KP_MULTIPLY = KEY_KP_MULTIPLY,
    KP_PERIOD = KEY_KP_PERIOD,
    KP_SUBTRACT = KEY_KP_SUBTRACT,
    L = KEY_L,
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
    LAUNCHMAIL = KEY_LAUNCHMAIL,
    LAUNCHMEDIA = KEY_LAUNCHMEDIA,
    LEFT = KEY_LEFT,
    LESS = KEY_LESS,
    M = KEY_M,
    MACRON = KEY_MACRON,
    MASCULINE = KEY_MASCULINE,
    MEDIANEXT = KEY_MEDIANEXT,
    MEDIAPLAY = KEY_MEDIAPLAY,
    MEDIAPREVIOUS = KEY_MEDIAPREVIOUS,
    MEDIARECORD = KEY_MEDIARECORD,
    MEDIASTOP = KEY_MEDIASTOP,
    MENU = KEY_MENU,
    META = KEY_META,
    MINUS = KEY_MINUS,
    MODIFIER_MASK = KEY_MODIFIER_MASK,
    MU = KEY_MU,
    MULTIPLY = KEY_MULTIPLY,
    N = KEY_N,
    NOBREAKSPACE = KEY_NOBREAKSPACE,
    NOTSIGN = KEY_NOTSIGN,
    NTILDE = KEY_NTILDE,
    NUMBERSIGN = KEY_NUMBERSIGN,
    NUMLOCK = KEY_NUMLOCK,
    O = KEY_O,
    OACUTE = KEY_OACUTE,
    OCIRCUMFLEX = KEY_OCIRCUMFLEX,
    ODIAERESIS = KEY_ODIAERESIS,
    OGRAVE = KEY_OGRAVE,
    ONEHALF = KEY_ONEHALF,
    ONEQUARTER = KEY_ONEQUARTER,
    ONESUPERIOR = KEY_ONESUPERIOR,
    OOBLIQUE = KEY_OOBLIQUE,
    OPENURL = KEY_OPENURL,
    ORDFEMININE = KEY_ORDFEMININE,
    OTILDE = KEY_OTILDE,
    P = KEY_P,
    PAGEDOWN = KEY_PAGEDOWN,
    PAGEUP = KEY_PAGEUP,
    PARAGRAPH = KEY_PARAGRAPH,
    PARENLEFT = KEY_PARENLEFT,
    PARENRIGHT = KEY_PARENRIGHT,
    PAUSE = KEY_PAUSE,
    PERCENT = KEY_PERCENT,
    PERIOD = KEY_PERIOD,
    PERIODCENTERED = KEY_PERIODCENTERED,
    PLUS = KEY_PLUS,
    PLUSMINUS = KEY_PLUSMINUS,
    PRINT = KEY_PRINT,
    Q = KEY_Q,
    QUESTION = KEY_QUESTION,
    QUESTIONDOWN = KEY_QUESTIONDOWN,
    QUOTEDBL = KEY_QUOTEDBL,
    QUOTELEFT = KEY_QUOTELEFT,
    R = KEY_R,
    REFRESH = KEY_REFRESH,
    REGISTERED = KEY_REGISTERED,
    RIGHT = KEY_RIGHT,
    S = KEY_S,
    SCROLLLOCK = KEY_SCROLLLOCK,
    SEARCH = KEY_SEARCH,
    SECTION = KEY_SECTION,
    SEMICOLON = KEY_SEMICOLON,
    SHIFT = KEY_SHIFT,
    SLASH = KEY_SLASH,
    SPACE = KEY_SPACE,
    SSHARP = KEY_SSHARP,
    STANDBY = KEY_STANDBY,
    STERLING = KEY_STERLING,
    STOP = KEY_STOP,
    SUPER_L = KEY_SUPER_L,
    SUPER_R = KEY_SUPER_R,
    SYSREQ = KEY_SYSREQ,
    T = KEY_T,
    TAB = KEY_TAB,
    THORN = KEY_THORN,
    THREEQUARTERS = KEY_THREEQUARTERS,
    THREESUPERIOR = KEY_THREESUPERIOR,
    TREBLEDOWN = KEY_TREBLEDOWN,
    TREBLEUP = KEY_TREBLEUP,
    TWOSUPERIOR = KEY_TWOSUPERIOR,
    U = KEY_U,
    UACUTE = KEY_UACUTE,
    UCIRCUMFLEX = KEY_UCIRCUMFLEX,
    UDIAERESIS = KEY_UDIAERESIS,
    UGRAVE = KEY_UGRAVE,
    UNDERSCORE = KEY_UNDERSCORE,
    UNKNOWN = KEY_UNKNOWN,
    UP = KEY_UP,
    V = KEY_V,
    VOLUMEDOWN = KEY_VOLUMEDOWN,
    VOLUMEMUTE = KEY_VOLUMEMUTE,
    VOLUMEUP = KEY_VOLUMEUP,
    W = KEY_W,
    X = KEY_X,
    Y = KEY_Y,
    YACUTE = KEY_YACUTE,
    YDIAERESIS = KEY_YDIAERESIS,
    YEN = KEY_YEN,
    Z = KEY_Z,
}
enum KeyMask {
    ALT = KEY_MASK_ALT,
    CMD = KEY_MASK_CMD,
    CTRL = KEY_MASK_CTRL,
    GROUP_SWITCH = KEY_MASK_GROUP_SWITCH,
    KPAD = KEY_MASK_KPAD,
    META = KEY_MASK_META,
    SHIFT = KEY_MASK_SHIFT,
}
enum Margin {
    BOTTOM = MARGIN_BOTTOM,
    LEFT = MARGIN_LEFT,
    RIGHT = MARGIN_RIGHT,
    TOP = MARGIN_TOP,
}
enum MethodFlag {
    CONST = METHOD_FLAG_CONST,
    EDITOR = METHOD_FLAG_EDITOR,
    FROM_SCRIPT = METHOD_FLAG_FROM_SCRIPT,
    _METHOD_FLAGS_DEFAULT = METHOD_FLAGS_DEFAULT,
    NORMAL = METHOD_FLAG_NORMAL,
    NOSCRIPT = METHOD_FLAG_NOSCRIPT,
    REVERSE = METHOD_FLAG_REVERSE,
    VIRTUAL = METHOD_FLAG_VIRTUAL,
}
enum MidiMessage {
    AFTERTOUCH = MIDI_MESSAGE_AFTERTOUCH,
    CHANNEL_PRESSURE = MIDI_MESSAGE_CHANNEL_PRESSURE,
    CONTROL_CHANGE = MIDI_MESSAGE_CONTROL_CHANGE,
    NOTE_OFF = MIDI_MESSAGE_NOTE_OFF,
    NOTE_ON = MIDI_MESSAGE_NOTE_ON,
    PITCH_BEND = MIDI_MESSAGE_PITCH_BEND,
    PROGRAM_CHANGE = MIDI_MESSAGE_PROGRAM_CHANGE,
}
enum Mouse {
    LEFT = BUTTON_LEFT,
    MASK_LEFT = BUTTON_MASK_LEFT,
    MASK_MIDDLE = BUTTON_MASK_MIDDLE,
    MASK_RIGHT = BUTTON_MASK_RIGHT,
    MASK_XBUTTON1 = BUTTON_MASK_XBUTTON1,
    MASK_XBUTTON2 = BUTTON_MASK_XBUTTON2,
    MIDDLE = BUTTON_MIDDLE,
    RIGHT = BUTTON_RIGHT,
    WHEEL_DOWN = BUTTON_WHEEL_DOWN,
    WHEEL_LEFT = BUTTON_WHEEL_LEFT,
    WHEEL_RIGHT = BUTTON_WHEEL_RIGHT,
    WHEEL_UP = BUTTON_WHEEL_UP,
    XBUTTON1 = BUTTON_XBUTTON1,
    XBUTTON2 = BUTTON_XBUTTON2,
}
enum Op {
    ADD = OP_ADD,
    AND = OP_AND,
    BIT_AND = OP_BIT_AND,
    BIT_NEGATE = OP_BIT_NEGATE,
    BIT_OR = OP_BIT_OR,
    BIT_XOR = OP_BIT_XOR,
    DIVIDE = OP_DIVIDE,
    EQUAL = OP_EQUAL,
    GREATER = OP_GREATER,
    GREATER_EQUAL = OP_GREATER_EQUAL,
    IN = OP_IN,
    LESS = OP_LESS,
    LESS_EQUAL = OP_LESS_EQUAL,
    MAX = OP_MAX,
    MODULE = OP_MODULE,
    MULTIPLY = OP_MULTIPLY,
    NEGATE = OP_NEGATE,
    NOT = OP_NOT,
    NOT_EQUAL = OP_NOT_EQUAL,
    OR = OP_OR,
    POSITIVE = OP_POSITIVE,
    SHIFT_LEFT = OP_SHIFT_LEFT,
    SHIFT_RIGHT = OP_SHIFT_RIGHT,
    STRING_CONCAT = OP_STRING_CONCAT,
    SUBTRACT = OP_SUBTRACT,
    XOR = OP_XOR,
}
enum Orientation {
    _HORIZONTAL = HORIZONTAL,
    _VERTICAL = VERTICAL,
}
enum PropertyHint {
    COLOR_NO_ALPHA = PROPERTY_HINT_COLOR_NO_ALPHA,
    DIR = PROPERTY_HINT_DIR,
    ENUM = PROPERTY_HINT_ENUM,
    EXP_EASING = PROPERTY_HINT_EXP_EASING,
    EXP_RANGE = PROPERTY_HINT_EXP_RANGE,
    FILE = PROPERTY_HINT_FILE,
    FLAGS = PROPERTY_HINT_FLAGS,
    GLOBAL_DIR = PROPERTY_HINT_GLOBAL_DIR,
    GLOBAL_FILE = PROPERTY_HINT_GLOBAL_FILE,
    IMAGE_COMPRESS_LOSSLESS = PROPERTY_HINT_IMAGE_COMPRESS_LOSSLESS,
    IMAGE_COMPRESS_LOSSY = PROPERTY_HINT_IMAGE_COMPRESS_LOSSY,
    KEY_ACCEL = PROPERTY_HINT_KEY_ACCEL,
    LAYERS_2D_PHYSICS = PROPERTY_HINT_LAYERS_2D_PHYSICS,
    LAYERS_2D_RENDER = PROPERTY_HINT_LAYERS_2D_RENDER,
    LAYERS_3D_PHYSICS = PROPERTY_HINT_LAYERS_3D_PHYSICS,
    LAYERS_3D_RENDER = PROPERTY_HINT_LAYERS_3D_RENDER,
    LENGTH = PROPERTY_HINT_LENGTH,
    MULTILINE_TEXT = PROPERTY_HINT_MULTILINE_TEXT,
    NONE = PROPERTY_HINT_NONE,
    PLACEHOLDER_TEXT = PROPERTY_HINT_PLACEHOLDER_TEXT,
    RANGE = PROPERTY_HINT_RANGE,
    RESOURCE_TYPE = PROPERTY_HINT_RESOURCE_TYPE,
}
enum PropertyUsage {
    CATEGORY = PROPERTY_USAGE_CATEGORY,
    CHECKABLE = PROPERTY_USAGE_CHECKABLE,
    CHECKED = PROPERTY_USAGE_CHECKED,
    DEFAULT = PROPERTY_USAGE_DEFAULT,
    DEFAULT_INTL = PROPERTY_USAGE_DEFAULT_INTL,
    EDITOR = PROPERTY_USAGE_EDITOR,
    EDITOR_HELPER = PROPERTY_USAGE_EDITOR_HELPER,
    GROUP = PROPERTY_USAGE_GROUP,
    INTERNATIONALIZED = PROPERTY_USAGE_INTERNATIONALIZED,
    NETWORK = PROPERTY_USAGE_NETWORK,
    NOEDITOR = PROPERTY_USAGE_NOEDITOR,
    NO_INSTANCE_STATE = PROPERTY_USAGE_NO_INSTANCE_STATE,
    RESTART_IF_CHANGED = PROPERTY_USAGE_RESTART_IF_CHANGED,
    SCRIPT_VARIABLE = PROPERTY_USAGE_SCRIPT_VARIABLE,
    STORAGE = PROPERTY_USAGE_STORAGE,
}
enum Type {
    _AABB = TYPE_AABB,
    ARRAY = TYPE_ARRAY,
    BASIS = TYPE_BASIS,
    BOOL = TYPE_BOOL,
    COLOR = TYPE_COLOR,
    COLOR_ARRAY = TYPE_COLOR_ARRAY,
    DICTIONARY = TYPE_DICTIONARY,
    INT = TYPE_INT,
    INT_ARRAY = TYPE_INT_ARRAY,
    MAX = TYPE_MAX,
    NIL = TYPE_NIL,
    NODE_PATH = TYPE_NODE_PATH,
    OBJECT = TYPE_OBJECT,
    PLANE = TYPE_PLANE,
    QUAT = TYPE_QUAT,
    RAW_ARRAY = TYPE_RAW_ARRAY,
    REAL = TYPE_REAL,
    REAL_ARRAY = TYPE_REAL_ARRAY,
    RECT2 = TYPE_RECT2,
    _RID = TYPE_RID,
    STRING = TYPE_STRING,
    STRING_ARRAY = TYPE_STRING_ARRAY,
    TRANSFORM = TYPE_TRANSFORM,
    TRANSFORM2D = TYPE_TRANSFORM2D,
    VECTOR2 = TYPE_VECTOR2,
    VECTOR2_ARRAY = TYPE_VECTOR2_ARRAY,
    VECTOR3 = TYPE_VECTOR3,
    VECTOR3_ARRAY = TYPE_VECTOR3_ARRAY,
}
enum VAlign {
    BOTTOM = VALIGN_BOTTOM,
    CENTER = VALIGN_CENTER,
    TOP = VALIGN_TOP,
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
    self._ARVRServer = NamedSyntheticType.new("ARVRServer")
    self._AudioServer = NamedSyntheticType.new("AudioServer")
    self._CameraServer = NamedSyntheticType.new("CameraServer")
    self._IP = NamedSyntheticType.new("IP")
    self._Input = NamedSyntheticType.new("Input")
    self._InputMap = NamedSyntheticType.new("InputMap")
    self._JavaClassWrapper = NamedSyntheticType.new("JavaClassWrapper")
    self._JavaScript = NamedSyntheticType.new("JavaScript")
    self._Performance = NamedSyntheticType.new("Performance")
    self._Physics2DServer = NamedSyntheticType.new("Physics2DServer")
    self._PhysicsServer = NamedSyntheticType.new("PhysicsServer")
    self._ProjectSettings = NamedSyntheticType.new("ProjectSettings")
    self._TranslationServer = NamedSyntheticType.new("TranslationServer")
    self._VisualServer = NamedSyntheticType.new("VisualServer")
    self._ClassDB = NamedSyntheticType.new("_ClassDB")
    self._Engine = NamedSyntheticType.new("_Engine")
    self._Geometry = NamedSyntheticType.new("_Geometry")
    self._JSON = NamedSyntheticType.new("_JSON")
    self._Marshalls = NamedSyntheticType.new("_Marshalls")
    self._OS = NamedSyntheticType.new("_OS")
    self._ResourceLoader = NamedSyntheticType.new("_ResourceLoader")
    self._ResourceSaver = NamedSyntheticType.new("_ResourceSaver")
    self._VisualScriptEditor = NamedSyntheticType.new("_VisualScriptEditor")
    self.primitive_types_lookup = {TYPE_NIL: self.Null, TYPE_BOOL: self.Bool, TYPE_INT: self.Int, TYPE_REAL: self.Float, TYPE_STRING: self._String, TYPE_VECTOR2: self._Vector2, TYPE_RECT2: self._Rect2, TYPE_VECTOR3: self._Vector3, TYPE_TRANSFORM2D: self._Transform2D, TYPE_PLANE: self._Plane, TYPE_QUAT: self._Quat, TYPE_AABB: self._AABB, TYPE_BASIS: self._Basis, TYPE_TRANSFORM: self._Transform, TYPE_COLOR: self._Color, TYPE_NODE_PATH: self._NodePath, TYPE_RID: self._RID, TYPE_OBJECT: self._Object, TYPE_DICTIONARY: self._Dictionary, TYPE_ARRAY: self._Array, TYPE_RAW_ARRAY: self._PoolByteArray, TYPE_INT_ARRAY: self._PoolIntArray, TYPE_REAL_ARRAY: self._PoolRealArray, TYPE_STRING_ARRAY: self._PoolStringArray, TYPE_VECTOR2_ARRAY: self._PoolVector2Array, TYPE_VECTOR3_ARRAY: self._PoolVector3Array, TYPE_COLOR_ARRAY: self._PoolColorArray}
    self.native_types_lookup = {"ColorPickerButton": ColorPickerButton, "VisualShaderNodeDeterminant": VisualShaderNodeDeterminant, "Room": Room, "CPUParticles2D": CPUParticles2D, "VisualScriptFunction": VisualScriptFunction, "SpriteFrames": SpriteFrames, "TriangleMesh": TriangleMesh, "AudioStream": AudioStream, "AudioStreamPlayback": AudioStreamPlayback, "SkinReference": SkinReference, "HScrollBar": HScrollBar, "AnimationNodeAnimation": AnimationNodeAnimation, "GLTFBufferView": GLTFBufferView, "CenterContainer": CenterContainer, "AudioStreamPlayer3D": AudioStreamPlayer3D, "PopupMenu": PopupMenu, "GLTFDocument": GLTFDocument, "Resource": Resource, "StyleBoxFlat": StyleBoxFlat, "VisualScriptClassConstant": VisualScriptClassConstant, "HSlider": HSlider, "EditorScenePostImport": EditorScenePostImport, "ConvexPolygonShape2D": ConvexPolygonShape2D, "PacketPeer": PacketPeer, "CSGBox": CSGBox, "PacketPeerDTLS": PacketPeerDTLS, "EditorInterface": EditorInterface, "PhysicsBody": PhysicsBody, "ScrollContainer": ScrollContainer, "StreamPeerTCP": StreamPeerTCP, "VisualServer": self._VisualServer, "VisualShaderNodeBooleanUniform": VisualShaderNodeBooleanUniform, "VisualShaderNodeTransformFunc": VisualShaderNodeTransformFunc, "WindowDialog": WindowDialog, "MeshInstance2D": MeshInstance2D, "RandomNumberGenerator": RandomNumberGenerator, "VisualShaderNodeScalarSwitch": VisualShaderNodeScalarSwitch, "SkeletonIK": SkeletonIK, "VisualScriptSelf": VisualScriptSelf, "PopupDialog": PopupDialog, "ProjectSettings": self._ProjectSettings, "SliderJoint": SliderJoint, "CSGSphere": CSGSphere, "BoxContainer": BoxContainer, "Navigation2D": Navigation2D, "AudioStreamPlaybackResampled": AudioStreamPlaybackResampled, "NoiseTexture": NoiseTexture, "Physics2DDirectBodyState": Physics2DDirectBodyState, "AudioEffectEQ": AudioEffectEQ, "GrooveJoint2D": GrooveJoint2D, "InputMap": self._InputMap, "AudioEffectPanner": AudioEffectPanner, "VisualScriptConstructor": VisualScriptConstructor, "VisualScriptNode": VisualScriptNode, "VisualShaderNodeCubeMapUniform": VisualShaderNodeCubeMapUniform, "UPNPDevice": UPNPDevice, "AudioEffectLimiter": AudioEffectLimiter, "CameraServer": self._CameraServer, "AESContext": AESContext, "VisualScriptSwitch": VisualScriptSwitch, "ARVRCamera": ARVRCamera, "ScrollBar": ScrollBar, "TranslationServer": self._TranslationServer, "VehicleBody": VehicleBody, "AnimationTrackEditPlugin": AnimationTrackEditPlugin, "RigidBody2D": RigidBody2D, "KinematicBody2D": KinematicBody2D, "MultiplayerAPI": MultiplayerAPI, "OccluderShapeSphere": OccluderShapeSphere, "Timer": Timer, "AcceptDialog": AcceptDialog, "VScrollBar": VScrollBar, "VideoStreamGDNative": VideoStreamGDNative, "VideoStreamWebm": VideoStreamWebm, "AudioEffect": AudioEffect, "AStar2D": AStar2D, "VisualScriptBasicTypeConstant": VisualScriptBasicTypeConstant, "VisualScriptFunctionCall": VisualScriptFunctionCall, "WebRTCMultiplayer": WebRTCMultiplayer, "AnimatedSprite3D": AnimatedSprite3D, "HSeparator": HSeparator, "VisualShaderNodeScalarConstant": VisualShaderNodeScalarConstant, "ViewportTexture": ViewportTexture, "VisualShaderNodeTransformCompose": VisualShaderNodeTransformCompose, "KinematicBody": KinematicBody, "CSGShape": CSGShape, "Curve": Curve, "VisualShaderNodeColorOp": VisualShaderNodeColorOp, "Joint2D": Joint2D, "VisualShaderNodeVectorScalarMix": VisualShaderNodeVectorScalarMix, "KinematicCollision": KinematicCollision, "ImageTexture": ImageTexture, "VisualScriptCustomNode": VisualScriptCustomNode, "AudioEffectBandPassFilter": AudioEffectBandPassFilter, "CSGPrimitive": CSGPrimitive, "TextEdit": TextEdit, "VisualScriptCondition": VisualScriptCondition, "AudioEffectLowShelfFilter": AudioEffectLowShelfFilter, "AudioEffectEQ10": AudioEffectEQ10, "AnimationNodeBlendSpace2D": AnimationNodeBlendSpace2D, "MainLoop": MainLoop, "AnimationNodeTimeScale": AnimationNodeTimeScale, "StyleBoxTexture": StyleBoxTexture, "EditorImportPlugin": EditorImportPlugin, "InputEventScreenTouch": InputEventScreenTouch, "SpatialGizmo": SpatialGizmo, "VisibilityEnabler": VisibilityEnabler, "VisualScriptDeconstruct": VisualScriptDeconstruct, "VisualScriptIndexGet": VisualScriptIndexGet, "WebXRInterface": WebXRInterface, "Tabs": Tabs, "TreeItem": TreeItem, "AudioEffectFilter": AudioEffectFilter, "MenuButton": MenuButton, "VehicleWheel": VehicleWheel, "SoftBody": SoftBody, "PopupPanel": PopupPanel, "CurveTexture": CurveTexture, "AudioEffectDistortion": AudioEffectDistortion, "JavaClassWrapper": self._JavaClassWrapper, "Mesh": Mesh, "CSGCylinder": CSGCylinder, "AnimationNodeStateMachine": AnimationNodeStateMachine, "ResourceImporter": ResourceImporter, "InputEventAction": InputEventAction, "CanvasLayer": CanvasLayer, "VisualShaderNodeTransformMult": VisualShaderNodeTransformMult, "PhysicsDirectSpaceState": PhysicsDirectSpaceState, "Node": Node, "MeshDataTool": MeshDataTool, "VisualScriptOperator": VisualScriptOperator, "Camera": Camera, "AnimationNodeAdd3": AnimationNodeAdd3, "InputEventMouseButton": InputEventMouseButton, "InputEventMagnifyGesture": InputEventMagnifyGesture, "VisualScriptVariableSet": VisualScriptVariableSet, "NetworkedMultiplayerPeer": NetworkedMultiplayerPeer, "CubeMap": CubeMap, "Sprite": Sprite, "Crypto": Crypto, "CollisionObject2D": CollisionObject2D, "HashingContext": HashingContext, "RoomManager": RoomManager, "TCP_Server": TCP_Server, "SpriteBase3D": SpriteBase3D, "OccluderShape": OccluderShape, "OmniLight": OmniLight, "VisualShaderNodeFresnel": VisualShaderNodeFresnel, "CapsuleMesh": CapsuleMesh, "AudioStreamGenerator": AudioStreamGenerator, "AudioEffectHighPassFilter": AudioEffectHighPassFilter, "_Directory": Directory, "AnimatedTexture": AnimatedTexture, "GridMap": GridMap, "DynamicFontData": DynamicFontData, "EditorVCSInterface": EditorVCSInterface, "AnimationNodeBlendTree": AnimationNodeBlendTree, "GIProbeData": GIProbeData, "VisualInstance": VisualInstance, "VisualScriptFunctionState": VisualScriptFunctionState, "BackBufferCopy": BackBufferCopy, "Spatial": Spatial, "_Geometry": self._Geometry, "TextureRect": TextureRect, "VisualShaderNodeVectorOp": VisualShaderNodeVectorOp, "AnimationNodeBlend2": AnimationNodeBlend2, "Physics2DShapeQueryParameters": Physics2DShapeQueryParameters, "GLTFAccessor": GLTFAccessor, "VisualShaderNodeVectorDistance": VisualShaderNodeVectorDistance, "Node2D": Node2D, "Texture": Texture, "Camera2D": Camera2D, "VisibilityEnabler2D": VisibilityEnabler2D, "VisualShaderNodeIs": VisualShaderNodeIs, "VisualShaderNodeVectorFunc": VisualShaderNodeVectorFunc, "AudioEffectStereoEnhance": AudioEffectStereoEnhance, "GDScriptFunctionState": GDScriptFunctionState, "Area2D": Area2D, "AnimationNode": AnimationNode, "MultiMeshInstance2D": MultiMeshInstance2D, "ARVRServer": self._ARVRServer, "Viewport": Viewport, "VisualShaderNodeTexture": VisualShaderNodeTexture, "_ClassDB": self._ClassDB, "VisibilityNotifier": VisibilityNotifier, "BitMap": BitMap, "CameraTexture": CameraTexture, "AnimationNodeAdd2": AnimationNodeAdd2, "EditorSceneImporter": EditorSceneImporter, "Physics2DServer": self._Physics2DServer, "AnimationNodeOneShot": AnimationNodeOneShot, "TextureProgress": TextureProgress, "VisualShaderNodeCustom": VisualShaderNodeCustom, "HSplitContainer": HSplitContainer, "VSlider": VSlider, "CharFXTransform": CharFXTransform, "JNISingleton": JNISingleton, "SpatialVelocityTracker": SpatialVelocityTracker, "EditorSpinSlider": EditorSpinSlider, "GLTFSkeleton": GLTFSkeleton, "GLTFCamera": GLTFCamera, "SegmentShape2D": SegmentShape2D, "BulletPhysicsServer": BulletPhysicsServer, "UPNP": UPNP, "TextureButton": TextureButton, "VisualScriptBuiltinFunc": VisualScriptBuiltinFunc, "EditorFileSystemDirectory": EditorFileSystemDirectory, "CubeMesh": CubeMesh, "VisualShaderNodeScalarFunc": VisualShaderNodeScalarFunc, "RichTextEffect": RichTextEffect, "VisualShaderNodeTransformConstant": VisualShaderNodeTransformConstant, "Reference": Reference, "AudioStreamOGGVorbis": AudioStreamOGGVorbis, "ScriptEditor": ScriptEditor, "VisualScriptMathConstant": VisualScriptMathConstant, "VisualScriptSceneTree": VisualScriptSceneTree, "VisualShaderNodeTransformDecompose": VisualShaderNodeTransformDecompose, "SpatialMaterial": SpatialMaterial, "Navigation": Navigation, "AudioEffectLowPassFilter": AudioEffectLowPassFilter, "AudioStreamPlayer": AudioStreamPlayer, "ShaderMaterial": ShaderMaterial, "CheckButton": CheckButton, "CollisionShape2D": CollisionShape2D, "IP_Unix": IP_Unix, "CollisionPolygon2D": CollisionPolygon2D, "ProceduralSky": ProceduralSky, "DampedSpringJoint2D": DampedSpringJoint2D, "SpotLight": SpotLight, "GLTFMesh": GLTFMesh, "NavigationMesh": NavigationMesh, "PlaneShape": PlaneShape, "VisualScriptExpression": VisualScriptExpression, "MeshInstance": MeshInstance, "VisualScriptEmitSignal": VisualScriptEmitSignal, "VisualScriptLocalVarSet": VisualScriptLocalVarSet, "CSGMesh": CSGMesh, "VisualShaderNodeSwitch": VisualShaderNodeSwitch, "VisualShaderNodeTransformUniform": VisualShaderNodeTransformUniform, "RichTextLabel": RichTextLabel, "_File": File, "VisualShaderNodeVectorScalarSmoothStep": VisualShaderNodeVectorScalarSmoothStep, "EditorResourcePreviewGenerator": EditorResourcePreviewGenerator, "CylinderMesh": CylinderMesh, "Physics2DDirectBodyStateSW": Physics2DDirectBodyStateSW, "VisualShaderNodeTransformVecMult": VisualShaderNodeTransformVecMult, "VisualShaderNodeVectorScalarStep": VisualShaderNodeVectorScalarStep, "StreamPeerSSL": StreamPeerSSL, "CanvasModulate": CanvasModulate, "CollisionPolygon": CollisionPolygon, "VisualShaderNode": VisualShaderNode, "InputEventPanGesture": InputEventPanGesture, "WebRTCPeerConnection": WebRTCPeerConnection, "VisualShaderNodeUniformRef": VisualShaderNodeUniformRef, "SceneTreeTimer": SceneTreeTimer, "_JSON": self._JSON, "VisualShaderNodeVec3Constant": VisualShaderNodeVec3Constant, "AtlasTexture": AtlasTexture, "Skeleton2D": Skeleton2D, "VisualShaderNodeVectorDecompose": VisualShaderNodeVectorDecompose, "Particles2D": Particles2D, "ArrayMesh": ArrayMesh, "PhysicsDirectBodyState": PhysicsDirectBodyState, "VisualShaderNodeVectorRefract": VisualShaderNodeVectorRefract, "StreamPeerGDNative": StreamPeerGDNative, "AnimationNodeBlendSpace1D": AnimationNodeBlendSpace1D, "Skin": Skin, "RemoteTransform2D": RemoteTransform2D, "StyleBoxLine": StyleBoxLine, "VisualShaderNodeScalarUniform": VisualShaderNodeScalarUniform, "RectangleShape2D": RectangleShape2D, "WebSocketClient": WebSocketClient, "ParticlesMaterial": ParticlesMaterial, "_Engine": self._Engine, "EditorSceneImporterGLTF": EditorSceneImporterGLTF, "DTLSServer": DTLSServer, "ARVRAnchor": ARVRAnchor, "PhysicsMaterial": PhysicsMaterial, "SpringArm": SpringArm, "BaseButton": BaseButton, "VisualScriptSceneNode": VisualScriptSceneNode, "VisualScriptLists": VisualScriptLists, "PinJoint2D": PinJoint2D, "HeightMapShape": HeightMapShape, "Area": Area, "AnimatedSprite": AnimatedSprite, "CanvasItemMaterial": CanvasItemMaterial, "EditorInspector": EditorInspector, "Listener2D": Listener2D, "Label": Label, "TabContainer": TabContainer, "Popup": Popup, "EditorFileDialog": EditorFileDialog, "StaticBody2D": StaticBody2D, "ButtonGroup": ButtonGroup, "Translation": Translation, "VisualScriptYieldSignal": VisualScriptYieldSignal, "AudioStreamRandomPitch": AudioStreamRandomPitch, "RigidBody": RigidBody, "Container": Container, "InputDefault": InputDefault, "SplitContainer": SplitContainer, "Line2D": Line2D, "BoneAttachment": BoneAttachment, "TextureArray": TextureArray, "CollisionObject": CollisionObject, "AudioStreamPlayer2D": AudioStreamPlayer2D, "_ResourceLoader": self._ResourceLoader, "VisualShaderNodeVectorLen": VisualShaderNodeVectorLen, "PHashTranslation": PHashTranslation, "Portal": Portal, "ImmediateGeometry": ImmediateGeometry, "Panel": Panel, "Physics2DTestMotionResult": Physics2DTestMotionResult, "StaticBody": StaticBody, "VideoPlayer": VideoPlayer, "VisualShaderNodeColorUniform": VisualShaderNodeColorUniform, "YSort": YSort, "EditorResourceConversionPlugin": EditorResourceConversionPlugin, "HMACContext": HMACContext, "VisualShaderNodeTextureUniform": VisualShaderNodeTextureUniform, "AnimationNodeStateMachinePlayback": AnimationNodeStateMachinePlayback, "AudioEffectRecord": AudioEffectRecord, "GLTFSkin": GLTFSkin, "WebRTCDataChannelGDNative": WebRTCDataChannelGDNative, "PhysicsServer": self._PhysicsServer, "ARVRController": ARVRController, "AudioEffectHighShelfFilter": AudioEffectHighShelfFilter, "TileMap": TileMap, "InputEventWithModifiers": InputEventWithModifiers, "Environment": Environment, "RegEx": RegEx, "VisualScriptLocalVar": VisualScriptLocalVar, "VisualScriptPropertySet": VisualScriptPropertySet, "VisualShaderNodeIf": VisualShaderNodeIf, "AudioEffectNotchFilter": AudioEffectNotchFilter, "ARVROrigin": ARVROrigin, "GDNativeLibrary": GDNativeLibrary, "Separator": Separator, "AudioEffectChorus": AudioEffectChorus, "ReflectionProbe": ReflectionProbe, "HBoxContainer": HBoxContainer, "Gradient": Gradient, "NinePatchRect": NinePatchRect, "Path2D": Path2D, "VisualShaderNodeOutput": VisualShaderNodeOutput, "ConeTwistJoint": ConeTwistJoint, "SphereShape": SphereShape, "GDScript": GDScript, "AudioBusLayout": AudioBusLayout, "ProxyTexture": ProxyTexture, "RoomGroup": RoomGroup, "ProgressBar": ProgressBar, "InputEventMouse": InputEventMouse, "SphereMesh": SphereMesh, "CapsuleShape2D": CapsuleShape2D, "MultiplayerPeerGDNative": MultiplayerPeerGDNative, "AudioEffectCompressor": AudioEffectCompressor, "Button": Button, "Performance": self._Performance, "VisualScript": VisualScript, "EditorResourcePicker": EditorResourcePicker, "VisualScriptPropertyGet": VisualScriptPropertyGet, "WorldEnvironment": WorldEnvironment, "PackedDataContainerRef": PackedDataContainerRef, "AStar": AStar, "X509Certificate": X509Certificate, "XMLParser": XMLParser, "AudioEffectDelay": AudioEffectDelay, "VisualScriptConstant": VisualScriptConstant, "VisualShader": VisualShader, "AudioStreamSample": AudioStreamSample, "StyleBoxEmpty": StyleBoxEmpty, "EditorFeatureProfile": EditorFeatureProfile, "MeshLibrary": MeshLibrary, "AudioEffectSpectrumAnalyzer": AudioEffectSpectrumAnalyzer, "PointMesh": PointMesh, "PacketPeerUDP": PacketPeerUDP, "InputEventMIDI": InputEventMIDI, "VisualScriptIterator": VisualScriptIterator, "ParallaxLayer": ParallaxLayer, "PluginScript": PluginScript, "VisualShaderNodeGroupBase": VisualShaderNodeGroupBase, "Tween": Tween, "LightOccluder2D": LightOccluder2D, "StyleBox": StyleBox, "VisualScriptReturn": VisualScriptReturn, "Sky": Sky, "World": World, "AnimationNodeTimeSeek": AnimationNodeTimeSeek, "VisualScriptPreload": VisualScriptPreload, "EditorExportPlugin": EditorExportPlugin, "ResourcePreloader": ResourcePreloader, "StreamPeerBuffer": StreamPeerBuffer, "TextFile": TextFile, "EditorSettings": EditorSettings, "PackedDataContainer": PackedDataContainer, "Shape2D": Shape2D, "GraphEdit": GraphEdit, "PackedScene": PackedScene, "EditorSpatialGizmo": EditorSpatialGizmo, "RegExMatch": RegExMatch, "ColorPicker": ColorPicker, "PhysicsTestMotionResult": PhysicsTestMotionResult, "Curve3D": Curve3D, "LargeTexture": LargeTexture, "Physics2DDirectSpaceState": Physics2DDirectSpaceState, "AudioEffectBandLimitFilter": AudioEffectBandLimitFilter, "OccluderPolygon2D": OccluderPolygon2D, "SpinBox": SpinBox, "ReferenceRect": ReferenceRect, "ConvexPolygonShape": ConvexPolygonShape, "VisualShaderNodeColorConstant": VisualShaderNodeColorConstant, "InterpolatedCamera": InterpolatedCamera, "Sprite3D": Sprite3D, "BakedLightmapData": BakedLightmapData, "CPUParticles": CPUParticles, "Polygon2D": Polygon2D, "ConcavePolygonShape2D": ConcavePolygonShape2D, "GraphNode": GraphNode, "AudioEffectEQ6": AudioEffectEQ6, "EditorInspectorPlugin": EditorInspectorPlugin, "InputEventGesture": InputEventGesture, "CircleShape2D": CircleShape2D, "Material": Material, "MobileVRInterface": MobileVRInterface, "ConfirmationDialog": ConfirmationDialog, "RootMotionView": RootMotionView, "AudioEffectPitchShift": AudioEffectPitchShift, "Expression": Expression, "HingeJoint": HingeJoint, "Joint": Joint, "UndoRedo": UndoRedo, "MultiMesh": MultiMesh, "VisibilityNotifier2D": VisibilityNotifier2D, "VisualScriptWhile": VisualScriptWhile, "EditorSceneImporterFBX": EditorSceneImporterFBX, "WebSocketServer": WebSocketServer, "InstancePlaceholder": InstancePlaceholder, "AudioStreamGeneratorPlayback": AudioStreamGeneratorPlayback, "AudioEffectSpectrumAnalyzerInstance": AudioEffectSpectrumAnalyzerInstance, "_Thread": Thread, "EditorFileSystem": EditorFileSystem, "ScriptCreateDialog": ScriptCreateDialog, "AudioEffectAmplify": AudioEffectAmplify, "VisualShaderNodeScalarSmoothStep": VisualShaderNodeScalarSmoothStep, "ARVRInterface": ARVRInterface, "AudioEffectPhaser": AudioEffectPhaser, "LinkButton": LinkButton, "PCKPacker": PCKPacker, "AnimationTree": AnimationTree, "VisualShaderNodeColorFunc": VisualShaderNodeColorFunc, "EditorResourcePreview": EditorResourcePreview, "ClippedCamera": ClippedCamera, "Font": Font, "IP": self._IP, "ResourceInteractiveLoader": ResourceInteractiveLoader, "StreamPeer": StreamPeer, "_Semaphore": Semaphore, "VisualShaderNodeUniform": VisualShaderNodeUniform, "RayShape": RayShape, "Control": Control, "OpenSimplexNoise": OpenSimplexNoise, "PacketPeerGDNative": PacketPeerGDNative, "Physics2DServerSW": Physics2DServerSW, "PackedSceneGLTF": PackedSceneGLTF, "EncodedObjectAsID": EncodedObjectAsID, "VideoStreamTheora": VideoStreamTheora, "VisualShaderNodeExpression": VisualShaderNodeExpression, "TouchScreenButton": TouchScreenButton, "AnimationNodeStateMachineTransition": AnimationNodeStateMachineTransition, "AudioEffectEQ21": AudioEffectEQ21, "Skeleton": Skeleton, "EditorScriptPicker": EditorScriptPicker, "HTTPClient": HTTPClient, "Input": self._Input, "CSGCombiner": CSGCombiner, "VisualScriptInputAction": VisualScriptInputAction, "VisualScriptSubCall": VisualScriptSubCall, "VisualShaderNodeCubeMap": VisualShaderNodeCubeMap, "PanoramaSky": PanoramaSky, "ItemList": ItemList, "VSplitContainer": VSplitContainer, "NativeScript": NativeScript, "JavaClass": JavaClass, "VisualShaderNodeFaceForward": VisualShaderNodeFaceForward, "VisualScriptVariableGet": VisualScriptVariableGet, "ARVRInterfaceGDNative": ARVRInterfaceGDNative, "Curve2D": Curve2D, "NavigationPolygon": NavigationPolygon, "Range": Range, "WebRTCPeerConnectionGDNative": WebRTCPeerConnectionGDNative, "DynamicFont": DynamicFont, "Particles": Particles, "GLTFNode": GLTFNode, "FileSystemDock": FileSystemDock, "World2D": World2D, "CanvasItem": CanvasItem, "GLTFState": GLTFState, "LineShape2D": LineShape2D, "NetworkedMultiplayerENet": NetworkedMultiplayerENet, "Tree": Tree, "RayCast": RayCast, "Light": Light, "CapsuleShape": CapsuleShape, "ConfigFile": ConfigFile, "VisualShaderNodeScalarInterp": VisualShaderNodeScalarInterp, "VisualShaderNodeVectorDerivativeFunc": VisualShaderNodeVectorDerivativeFunc, "ToolButton": ToolButton, "CollisionShape": CollisionShape, "VisualScriptComment": VisualScriptComment, "MeshTexture": MeshTexture, "AnimationNodeBlend3": AnimationNodeBlend3, "KinematicCollision2D": KinematicCollision2D, "Path": Path, "VisualScriptResourcePath": VisualScriptResourcePath, "VisualScriptGlobalConstant": VisualScriptGlobalConstant, "VisualShaderNodeGlobalExpression": VisualShaderNodeGlobalExpression, "GLTFSpecGloss": GLTFSpecGloss, "WebRTCDataChannel": WebRTCDataChannel, "EditorSpatialGizmoPlugin": EditorSpatialGizmoPlugin, "Generic6DOFJoint": Generic6DOFJoint, "Shape": Shape, "AudioEffectInstance": AudioEffectInstance, "ViewportContainer": ViewportContainer, "VisualScriptTypeCast": VisualScriptTypeCast, "VisualShaderNodeVectorInterp": VisualShaderNodeVectorInterp, "CSGTorus": CSGTorus, "JSONRPC": JSONRPC, "NavigationMeshInstance": NavigationMeshInstance, "AudioEffectCapture": AudioEffectCapture, "CullInstance": CullInstance, "PlaneMesh": PlaneMesh, "VisualShaderNodeTextureUniformTriplanar": VisualShaderNodeTextureUniformTriplanar, "Theme": Theme, "ColorRect": ColorRect, "InputEventKey": InputEventKey, "SceneTree": SceneTree, "ShortCut": ShortCut, "VisualShaderNodeVectorSmoothStep": VisualShaderNodeVectorSmoothStep, "EditorNavigationMeshGenerator": EditorNavigationMeshGenerator, "Position3D": Position3D, "NavigationPolygonInstance": NavigationPolygonInstance, "CameraFeed": CameraFeed, "JavaScript": self._JavaScript, "RayShape2D": RayShape2D, "AudioServer": self._AudioServer, "UDPServer": UDPServer, "AudioEffectReverb": AudioEffectReverb, "AnimationRootNode": AnimationRootNode, "CheckBox": CheckBox, "HTTPRequest": HTTPRequest, "InputEventJoypadButton": InputEventJoypadButton, "EditorProperty": EditorProperty, "VisualScriptSequence": VisualScriptSequence, "_Mutex": Mutex, "ConcavePolygonShape": ConcavePolygonShape, "RemoteTransform": RemoteTransform, "PacketPeerStream": PacketPeerStream, "PrismMesh": PrismMesh, "VSeparator": VSeparator, "AnimationPlayer": AnimationPlayer, "ARVRPositionalTracker": ARVRPositionalTracker, "GradientTexture": GradientTexture, "GridContainer": GridContainer, "EditorPlugin": EditorPlugin, "GLTFAnimation": GLTFAnimation, "VBoxContainer": VBoxContainer, "VisualScriptComposeArray": VisualScriptComposeArray, "Occluder": Occluder, "PanelContainer": PanelContainer, "ProximityGroup": ProximityGroup, "VisualShaderNodeVectorCompose": VisualShaderNodeVectorCompose, "Texture3D": Texture3D, "AspectRatioContainer": AspectRatioContainer, "ExternalTexture": ExternalTexture, "ResourceFormatSaver": ResourceFormatSaver, "BoxShape": BoxShape, "OptionButton": OptionButton, "VisualScriptYield": VisualScriptYield, "VisualShaderNodeInput": VisualShaderNodeInput, "WeakRef": WeakRef, "PrimitiveMesh": PrimitiveMesh, "InputEvent": InputEvent, "VisualScriptEngineSingleton": VisualScriptEngineSingleton, "WebSocketPeer": WebSocketPeer, "Light2D": Light2D, "PinJoint": PinJoint, "_OS": self._OS, "DirectionalLight": DirectionalLight, "AnimationTreePlayer": AnimationTreePlayer, "MultiMeshInstance": MultiMeshInstance, "PolygonPathFinder": PolygonPathFinder, "StreamTexture": StreamTexture, "FuncRef": FuncRef, "VisualShaderNodeCompare": VisualShaderNodeCompare, "GIProbe": GIProbe, "EditorScript": EditorScript, "GLTFTexture": GLTFTexture, "InputEventScreenDrag": InputEventScreenDrag, "JavaScriptObject": JavaScriptObject, "Position2D": Position2D, "VisualShaderNodeVectorClamp": VisualShaderNodeVectorClamp, "AudioStreamMicrophone": AudioStreamMicrophone, "VisualScriptIndexSet": VisualScriptIndexSet, "GeometryInstance": GeometryInstance, "QuadMesh": QuadMesh, "SurfaceTool": SurfaceTool, "RayCast2D": RayCast2D, "VisualScriptSelect": VisualScriptSelect, "VisualShaderNodeVec3Uniform": VisualShaderNodeVec3Uniform, "TextureLayered": TextureLayered, "PhysicsShapeQueryParameters": PhysicsShapeQueryParameters, "Bone2D": Bone2D, "SceneState": SceneState, "InputEventJoypadMotion": InputEventJoypadMotion, "Animation": Animation, "Listener": Listener, "CSGPolygon": CSGPolygon, "BulletPhysicsDirectBodyState": BulletPhysicsDirectBodyState, "PhysicalBone": PhysicalBone, "ResourceFormatLoader": ResourceFormatLoader, "Script": Script, "_ResourceSaver": self._ResourceSaver, "JSONParseResult": JSONParseResult, "Slider": Slider, "LineEdit": LineEdit, "EditorSelection": EditorSelection, "WebSocketMultiplayerPeer": WebSocketMultiplayerPeer, "VisualShaderNodeScalarDerivativeFunc": VisualShaderNodeScalarDerivativeFunc, "VisualShaderNodeDotProduct": VisualShaderNodeDotProduct, "BakedLightmap": BakedLightmap, "FileDialog": FileDialog, "Image": Image, "GDNative": GDNative, "MarginContainer": MarginContainer, "TileSet": TileSet, "_Marshalls": self._Marshalls, "_VisualScriptEditor": self._VisualScriptEditor, "InputEventMouseMotion": InputEventMouseMotion, "VideoStream": VideoStream, "PathFollow": PathFollow, "AnimationNodeTransition": AnimationNodeTransition, "AudioStreamMP3": AudioStreamMP3, "GLTFLight": GLTFLight, "VisualShaderNodeScalarOp": VisualShaderNodeScalarOp, "VisualShaderNodeScalarClamp": VisualShaderNodeScalarClamp, "AnimationNodeOutput": AnimationNodeOutput, "CylinderShape": CylinderShape, "BitmapFont": BitmapFont, "ParallaxBackground": ParallaxBackground, "PathFollow2D": PathFollow2D, "VisualShaderNodeOuterProduct": VisualShaderNodeOuterProduct, "VisualShaderNodeBooleanConstant": VisualShaderNodeBooleanConstant, "PhysicsBody2D": PhysicsBody2D, "Shader": Shader, "CryptoKey": CryptoKey}
    self.native_types_lookup["Object"] = self._Object
var global_name_generator
var symbol_table = {}
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
var primitive_types_lookup
var _ARVRServer
var _AudioServer
var _CameraServer
var _IP
var _Input
var _InputMap
var _JavaClassWrapper
var _JavaScript
var _Performance
var _Physics2DServer
var _PhysicsServer
var _ProjectSettings
var _TranslationServer
var _VisualServer
var _ClassDB
var _Engine
var _Geometry
var _JSON
var _Marshalls
var _OS
var _ResourceLoader
var _ResourceSaver
var _VisualScriptEditor
func _typeof(value):
    var t = typeof(value)
    var _cond = null
    if t != TYPE_OBJECT:
        _cond = self.primitive_types_lookup[t]
    else:
        var _cond_0 = value.get_script()
        _cond = _cond_0 if _cond_0 else self.native_types_lookup.get(value.get_class(), self.Any)
    return _cond
static func cons(a, b):
    return Cons.new(a, b)
static func car(a):
    return a.car
static func cdr(a):
    return a.cdr
static func init(a):
    var _cond = cons(car(a), init(cdr(a))) if cdr(a) is Cons else nil
    return _cond
static func tail(a):
    var _cond = tail(cdr(a)) if cdr(a) is Cons else car(a)
    return _cond
static func set_car(b, a):
    a.car = b
    return b
static func set_cdr(b, a):
    a.cdr = b
    return b
static func intern(a):
    var _cond = null
    if a in GDLisp.symbol_table:
        _cond = GDLisp.symbol_table[a]
    else:
        GDLisp.symbol_table[a] = Symbol.new(a)
        _cond = GDLisp.symbol_table[a]
    return _cond
static func length(x):
    var result = 0
    while x is Cons:
        result = result + 1
        x = x.cdr
    return result
static func funcall(f, args):
    return apply(f, GDLisp.Cons.new(args, null))
static func sys_DIV_funcall(f, args):
    return apply(f, GDLisp.Cons.new(args, null))
static func apply(f, args):
    var args1 = init(args)
    var args2 = tail(args)
    var _cond = f.call_funcv(append(GDLisp.Cons.new(args1, GDLisp.Cons.new(args2, null)))) if f is Function else push_error("Attempt to call non-function")
    return _cond
static func _PLUS_(args):
    var _cond = null
    if args is Cons:
        var result = args.car
        args = args.cdr
        while args is Cons:
            result = result + args.car
            args = args.cdr
        _cond = result
    else:
        _cond = 0
    return _cond
static func _TIMES_(args):
    var result = 1
    while args is Cons:
        result = result * args.car
        args = args.cdr
    return result
static func _(x, args):
    var _cond = null
    if args is Cons:
        var result = x
        while args is Cons:
            result = result - args.car
            args = args.cdr
        _cond = result
    else:
        _cond = -x
    return _cond
static func _DIV_(x, args):
    var _cond = null
    if args is Cons:
        var result = x
        while args is Cons:
            result = result / float(args.car)
            args = args.cdr
        _cond = result
    else:
        _cond = 1 / float(x)
    return _cond
static func div(x, args):
    var _cond = null
    if args is Cons:
        var result = x
        while args is Cons:
            result = result / int(args.car)
            args = args.cdr
        _cond = result
    else:
        _cond = 1 / int(x)
    return _cond
static func mod(x, y):
    return x % y
static func min(args):
    var _cond = null
    if args is Cons:
        var result = args.car
        args = args.cdr
        while args is Cons:
            result = min(result, args.car)
            args = args.cdr
        _cond = result
    else:
        _cond = INF
    return _cond
static func max(args):
    var _cond = null
    if args is Cons:
        var result = args.car
        args = args.cdr
        while args is Cons:
            result = max(result, args.car)
            args = args.cdr
        _cond = result
    else:
        _cond = -INF
    return _cond
static func _EQ_(x, args):
    while args is Cons:
        if x == args.car:
            pass
        else:
            return false
        x = args.car
        args = args.cdr
    return true
static func _LT_(x, args):
    while args is Cons:
        if x < args.car:
            pass
        else:
            return false
        x = args.car
        args = args.cdr
    return true
static func _GT_(x, args):
    while args is Cons:
        if x > args.car:
            pass
        else:
            return false
        x = args.car
        args = args.cdr
    return true
static func _LT__EQ_(x, args):
    while args is Cons:
        if x <= args.car:
            pass
        else:
            return false
        x = args.car
        args = args.cdr
    return true
static func _GT__EQ_(x, args):
    while args is Cons:
        if x >= args.car:
            pass
        else:
            return false
        x = args.car
        args = args.cdr
    return true
static func _DIV__EQ_(x, args):
    var outer = cons(x, args)
    while outer is Cons:
        var inner = outer.cdr
        while inner is Cons:
            if outer.car != inner.car:
                pass
            else:
                return false
            inner = inner.cdr
        outer = outer.cdr
    return true
static func is_equal(x, args):
    while args is Cons:
        if bin_equal(x, args.car):
            pass
        else:
            return false
        x = args.car
        args = args.cdr
    return true
static func bin_equal(a, b):
    var _cond = null
    var _cond_3 = is_instance(b, GDLisp.BaseArray) if is_instance(a, GDLisp.BaseArray) else false
    if _cond_3:
        _cond = array_equal(a, b)
    else:
        var _cond_2 = is_instance(b, GDLisp._Dictionary) if is_instance(a, GDLisp._Dictionary) else false
        if _cond_2:
            _cond = dict_equal(a, b)
        else:
            var _cond_1 = is_instance(b, Cons) if is_instance(a, Cons) else false
            if _cond_1:
                _cond = cons_equal(a, b)
            else:
                var _cond_0 = is_instance(b, GDLisp.Number) if is_instance(a, GDLisp.Number) else false
                _cond = a == b if _cond_0 else a == b if GDLisp._typeof(a) == GDLisp._typeof(b) else false
    return _cond
static func array_equal(a, b):
    var _cond = null
    if len(a) != len(b):
        _cond = false
    else:
        var i = 0
        var upper = len(a)
        while i < upper:
            if !bin_equal(a[i], b[i]):
                return false
            i = i + 1
        _cond = true
    return _cond
static func dict_equal(a, b):
    var _cond = null
    if len(a) != len(b):
        _cond = false
    else:
        for key in a.keys():
            if !bin_equal(a[key], b[key]):
                return false
        _cond = true
    return _cond
static func cons_equal(a, b):
    var _cond = bin_equal(cdr(a), cdr(b)) if bin_equal(car(a), car(b)) else false
    return _cond
static func _not(x):
    return !x
static func list(args):
    return args
static func vector(x, y, z):
    var _cond = Vector2(x, y) if z == null else Vector3(x, y, z)
    return _cond
static func list_to_array(list):
    var arr = []
    while list is Cons:
        arr.push_back(list.car)
        list = list.cdr
    return arr
static func array_to_list(arr):
    var outer = cons(null, null)
    var curr = outer
    for elem in arr:
        curr.cdr = cons(elem, null)
        curr = curr.cdr
    return outer.cdr
static func elt(arr, n):
    return arr[n]
static func set_elt(x, arr, n):
    arr[n] = x
    return arr[n]
static func is_member(value, arr):
    return value in arr
static func sys_DIV_get_node(obj, path):
    return obj.get_node(path)
static func sys_DIV_native_class_private():
    var x = GDScript
    return x.get_class()
static func is_instance(value, type):
    var _cond = type.is_satisfies(value) if type is GDLispSpecialType else value is type
    return _cond
static func is_sys_DIV_instance_direct(value, type):
    return value is type
static func convert(what, type):
    var _cond = convert(what, type.primitive_value) if type is PrimitiveType else convert(what, type)
    return _cond
static func gensym(prefix):
    var _cond = Symbol.new(GDLisp.global_name_generator.generate()) if prefix == null else Symbol.new(GDLisp.global_name_generator.generate_with(prefix))
    return _cond
static func map(f, xs):
    var _cond = null
    var _cond_0 = true if xs is Cons else true if xs == nil else null
    if _cond_0:
        var outer = cons(nil, nil)
        var curr = outer
        while xs != nil:
            curr.cdr = cons(funcall(f, GDLisp.Cons.new(xs.car, null)), nil)
            curr = curr.cdr
            xs = xs.cdr
        _cond = outer.cdr
    else:
        var result = []
        for i in len(xs):
            result.push_back(funcall(f, GDLisp.Cons.new(xs[i], null)))
        _cond = result
    return _cond
static func filter(p, xs):
    var _cond = null
    var _cond_0 = true if xs is Cons else true if xs == nil else false
    if _cond_0:
        var outer = cons(nil, nil)
        var curr = outer
        while xs != nil:
            if funcall(p, GDLisp.Cons.new(xs.car, null)):
                curr.cdr = cons(xs.car, nil)
                curr = curr.cdr
            xs = xs.cdr
        _cond = outer.cdr
    else:
        var result = []
        for i_0 in len(xs):
            if funcall(p, GDLisp.Cons.new(xs[i_0], null)):
                result.push_back(xs[i_0])
        _cond = result
    return _cond
static func reverse(arg):
    var rev = nil
    while arg != nil:
        rev = cons(car(arg), rev)
        arg = arg.cdr
    return rev
static func append(args):
    var outer = cons(nil, nil)
    var curr = outer
    while args != nil:
        var inner_value = args.car
        while inner_value != nil:
            curr.cdr = cons(inner_value.car, nil)
            curr = curr.cdr
            inner_value = inner_value.cdr
        args = args.cdr
    return outer.cdr
static func sys_DIV_qq_smart_list(a):
    var _cond = array_to_list(a) if is_instance(a, GDLisp.BaseArray) else a
    return _cond
static func sys_DIV_qq_smart_array(a):
    var _cond = a if is_instance(a, GDLisp.BaseArray) else list_to_array(a)
    return _cond
static func _str(x, args):
    var result = str(x)
    for arg in args:
        result = result + str(arg)
    return result
static func _printerr(args):
    var result = ""
    for arg_0 in args:
        result = result + str(arg_0)
    return printerr(result)
static func _printraw(args):
    var result = ""
    for arg_1 in args:
        result = result + str(arg_1)
    return printraw(result)
static func _print_debug(args):
    var result = ""
    for arg_2 in args:
        result = result + str(arg_2)
    return print_debug(result)
static func _print(args):
    var result = ""
    for arg_3 in args:
        result = result + str(arg_3)
    return print(result)
static func _prints(args):
    var result = ""
    var first = true
    for arg_4 in args:
        var _cond = "" if first else " "
        result = result + _cond + str(arg_4)
        first = false
    return print(result)
static func _printt(args):
    var result = ""
    var first = true
    for arg_5 in args:
        var _cond = "" if first else "\t"
        result = result + _cond + str(arg_5)
        first = false
    return print(result)
static func _range(a, b, c):
    var _cond = range(a) if b == nil else range(a, b) if c == nil else range(a, b, c)
    return _cond
static func _Color8(a, b, c, d):
    var _cond = Color8(a, b, c) if d == nil else Color8(a, b, c, d)
    return _cond
static func _ColorN(a, b):
    var _cond = ColorN(a) if b == nil else ColorN(a, b)
    return _cond
static func _bytes2var(a, b):
    var _cond = bytes2var(a) if b == nil else bytes2var(a, b)
    return _cond
static func _var2bytes(a, b):
    var _cond = var2bytes(a) if b == nil else var2bytes(a, b)
    return _cond
class GDLispSpecialType extends Reference:
    func _init():
        pass
class PrimitiveType extends GDLispSpecialType:
    func _init(primitive_value):
        self.primitive_value = primitive_value
    var primitive_value
    func is_satisfies(value):
        return typeof(value) == self.primitive_value
    var __gdlisp_outer_class_1 = load("res://GDLisp.gd")
class NamedSyntheticType extends GDLispSpecialType:
    func _init(name):
        self.name = name
    var name
    func is_satisfies(value):
        return value.get_class() == self.name
    var __gdlisp_outer_class_2 = load("res://GDLisp.gd")
class AnyType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value):
        return true
class AnyRefType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value):
        return typeof(value) == TYPE_OBJECT
    var __gdlisp_outer_class_3 = load("res://GDLisp.gd")
class AnyValType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value):
        return typeof(value) != TYPE_OBJECT
    var __gdlisp_outer_class_4 = load("res://GDLisp.gd")
class NumberType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value):
        var t = typeof(value)
        var _cond = true if t == TYPE_INT else true if t == TYPE_REAL else false
        return _cond
    var __gdlisp_outer_class_5 = load("res://GDLisp.gd")
class BaseArrayType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value):
        var _cmp = typeof(value)
        return TYPE_ARRAY <= _cmp && _cmp <= TYPE_COLOR_ARRAY
    var __gdlisp_outer_class_6 = load("res://GDLisp.gd")
class NothingType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value):
        return false
static func _or(args):
    var args_0 = reverse(args)
    var _cond = null
    if args_0:
        var result = cons(cons(true, cons(car(args_0), null)), null)
        args_0 = cdr(args_0)
        while args_0 != nil:
            result = cons(cons(car(args_0), null), result)
            args_0 = cdr(args_0)
        _cond = cons(GDLisp.intern("cond"), result)
    else:
        _cond = false
    return _cond
static func _and(args):
    var args_0 = reverse(args)
    var _cond = null
    if args_0:
        var result = cons(cons(true, cons(car(args_0), null)), null)
        args_0 = cdr(args_0)
        while args_0 != nil:
            result = cons(cons(cons(GDLisp.intern("not"), cons(car(args_0), null)), cons(false, null)), result)
            args_0 = cdr(args_0)
        _cond = cons(GDLisp.intern("cond"), result)
    else:
        _cond = true
    return _cond
static func let_TIMES_(vars, body):
    var _cond = null
    if vars == nil:
        _cond = cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_DIV_qq_smart_list(body), GDLisp.Cons.new(null, null))))
    else:
        var _quasiquote = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(body), GDLisp.Cons.new(null, null)))
        _cond = cons(GDLisp.intern("let"), cons(cons(car(vars), null), cons(cons(GDLisp.intern("let*"), cons(cdr(vars), _quasiquote)), null)))
    return _cond
static func defvars(args):
    var arr = []
    while args != nil:
        arr.push_back(GDLisp.Cons.new(GDLisp.intern("defvar"), GDLisp.Cons.new(args.car, null)))
        args = args.cdr
    return cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_DIV_qq_smart_list(arr), GDLisp.Cons.new(null, null))))
static func when(cnd, args):
    var _quasiquote = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(args), GDLisp.Cons.new(null, null)))
    return cons(GDLisp.intern("cond"), cons(cons(cnd, cons(cons(GDLisp.intern("progn"), _quasiquote), null)), null))
static func unless(cnd, args):
    var _quasiquote = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(args), GDLisp.Cons.new(null, null)))
    return cons(GDLisp.intern("cond"), cons(cons(cnd, cons(null, null)), cons(cons(true, cons(cons(GDLisp.intern("progn"), _quasiquote), null)), null)))
static func _if(cnd, t, f):
    return cons(GDLisp.intern("cond"), cons(cons(cnd, cons(t, null)), cons(cons(true, cons(f, null)), null)))
static func yield_TIMES_(arg):
    var symbol = gensym("_yield")
    var _quasiquote_0 = cons(GDLisp.intern("is-valid"), null)
    var _quasiquote_1 = cons(cons(GDLisp.intern("instance?"), cons(symbol, cons(GDLisp.intern("GDScriptFunctionState"), null))), cons(cons(cons(GDLisp.intern("access-slot"), cons(symbol, _quasiquote_0)), null), null))
    var _quasiquote_2 = cons(symbol, cons(GDLisp.intern("resume"), null))
    var _quasiquote_5 = cons(cons(GDLisp.intern("yield"), null), cons(cons(GDLisp.intern("set"), cons(symbol, cons(cons(cons(GDLisp.intern("access-slot"), _quasiquote_2), null), null))), null))
    return cons(GDLisp.intern("let"), cons(cons(cons(symbol, cons(arg, null)), null), cons(cons(GDLisp.intern("while"), cons(cons(GDLisp.intern("and"), _quasiquote_1), _quasiquote_5)), cons(symbol, null))))
static func this_file():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-file"), null))
static func this_filename():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-filename"), null))
static func this_true_filename():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-true-filename"), null))
static func contextual_load(arg):
    return cons(GDLisp.intern("load"), cons(cons(GDLisp.intern("sys/context-filename"), cons(arg, null)), null))
static func deflazy(name, value, modifiers):
    var fn_name = gensym("_lazy")
    var this_file = gensym("_this_file")
    var value_var = gensym("_value")
    var meta_name = "__gdlisp_Lazy_{}".format([gensym(null).contents], "{}")
    var _quasiquote = cons(cons(GDLisp.intern("this-file"), null), null)
    var _quasiquote_1 = cons(GDLisp.intern("get-meta"), null)
    var _quasiquote_2 = cons(value, null)
    var _quasiquote_5 = cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file, cons(GDLisp.intern("set-meta"), null))), cons(meta_name, cons(value_var, null))), cons(value_var, null))
    var _quasiquote_6 = cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file, cons(GDLisp.intern("has-meta"), null))), cons(meta_name, null)), cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file, _quasiquote_1)), cons(meta_name, null)), cons(cons(GDLisp.intern("let"), cons(cons(cons(value_var, _quasiquote_2), null), _quasiquote_5)), null)))
    var _quasiquote_7 = cons(cons(GDLisp.intern("let"), cons(cons(cons(this_file, _quasiquote), null), cons(cons(GDLisp.intern("if"), _quasiquote_6), null))), null)
    var _quasiquote_8 = cons(GDLisp.intern("access-slot"), null)
    var _quasiquote_11 = cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("quote"), cons(GDLisp.intern("contextual-load"), null)), cons(cons(GDLisp.intern("this-true-filename"), null), null))), cons(cons(GDLisp.intern("quote"), cons(fn_name, null)), null))
    var _quasiquote_12 = cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("quote"), _quasiquote_8), _quasiquote_11)), null)), append(GDLisp.Cons.new(sys_DIV_qq_smart_list(modifiers), GDLisp.Cons.new(null, null))))
    return cons(GDLisp.intern("progn"), cons(cons(GDLisp.intern("defn"), cons(fn_name, cons(null, _quasiquote_7))), cons(cons(GDLisp.intern("define-symbol-macro"), cons(name, _quasiquote_12)), null)))
static func defobject(name, parent, visibility, body):
    if visibility == nil:
        visibility = GDLisp.intern("public")
    elif !is_instance(visibility, Symbol):
        body = cons(visibility, body)
        visibility = GDLisp.intern("public")
    elif visibility == GDLisp.intern("public"):
        pass
    elif visibility == GDLisp.intern("private"):
        pass
    else:
        body = cons(visibility, body)
        visibility = GDLisp.intern("public")
    var _quasiquote = append(GDLisp.Cons.new(sys_DIV_qq_smart_list(body), GDLisp.Cons.new(null, null)))
    return cons(GDLisp.intern("deflazy"), cons(name, cons(cons(GDLisp.intern("new"), cons(parent, _quasiquote)), cons(visibility, null))))
static func run():
    return null
