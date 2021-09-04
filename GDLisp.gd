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
static func cons(a_11, b_12):
    return Cons.new(a_11, b_12)
static func car(a_13):
    return a_13.car
static func cdr(a_14):
    return a_14.cdr
static func set_car(b_15, a_16):
    a_16.car = b_15
    return a_16.car
static func set_cdr(b_17, a_18):
    a_18.cdr = b_17
    return a_18.cdr
static func intern(a_19):
    return Symbol.new(a_19)
static func length(x_20):
    var result_21 = 0
    while x_20 is Cons:
        result_21 = result_21 + 1
        x_20 = x_20.cdr
    return result_21
static func funcall(f_22, args_23):
    var _cond_24 = f_22.call_funcv(args_23) if f_22 is Function else push_error("Attempt to call non-function")
    return _cond_24
static func _u002B(args_25):
    var _cond_26 = null
    if args_25 is Cons:
        var result_27 = args_25.car
        args_25 = args_25.cdr
        while args_25 is Cons:
            result_27 = result_27 + args_25.car
            args_25 = args_25.cdr
        _cond_26 = result_27
    else:
        _cond_26 = 0
    return _cond_26
static func _u002A(args_28):
    var result_29 = 1
    while args_28 is Cons:
        result_29 = result_29 * args_28.car
        args_28 = args_28.cdr
    return result_29
static func _(x_30, args_31):
    var _cond_32 = null
    if args_31 is Cons:
        var result_33 = x_30
        while args_31 is Cons:
            result_33 = result_33 - args_31.car
            args_31 = args_31.cdr
        _cond_32 = result_33
    else:
        _cond_32 = -x_30
    return _cond_32
static func _u002F(x_34, args_35):
    var _cond_36 = null
    if args_35 is Cons:
        var result_37 = x_34
        while args_35 is Cons:
            result_37 = result_37 / float(args_35.car)
            args_35 = args_35.cdr
        _cond_36 = result_37
    else:
        _cond_36 = 1 / float(x_34)
    return _cond_36
static func div(x_38, args_39):
    var _cond_40 = null
    if args_39 is Cons:
        var result_41 = x_38
        while args_39 is Cons:
            result_41 = result_41 / int(args_39.car)
            args_39 = args_39.cdr
        _cond_40 = result_41
    else:
        _cond_40 = 1 / int(x_38)
    return _cond_40
static func mod(x_42, y_43):
    return x_42 % y_43
static func _EQ_(x_44, args_45):
    while args_45 is Cons:
        if x_44 == args_45.car:
            pass
        else:
            return false
        x_44 = args_45.car
        args_45 = args_45.cdr
    return true
static func _LT_(x_46, args_47):
    while args_47 is Cons:
        if x_46 < args_47.car:
            pass
        else:
            return false
        x_46 = args_47.car
        args_47 = args_47.cdr
    return true
static func _GT_(x_48, args_49):
    while args_49 is Cons:
        if x_48 > args_49.car:
            pass
        else:
            return false
        x_48 = args_49.car
        args_49 = args_49.cdr
    return true
static func _LT__EQ_(x_50, args_51):
    while args_51 is Cons:
        if x_50 <= args_51.car:
            pass
        else:
            return false
        x_50 = args_51.car
        args_51 = args_51.cdr
    return true
static func _GT__EQ_(x_52, args_53):
    while args_53 is Cons:
        if x_52 >= args_53.car:
            pass
        else:
            return false
        x_52 = args_53.car
        args_53 = args_53.cdr
    return true
static func _u002F_EQ_(x_54, args_55):
    var outer_56 = cons(x_54, args_55)
    while outer_56 is Cons:
        var inner_57 = outer_56.cdr
        while inner_57 is Cons:
            if outer_56.car != inner_57.car:
                pass
            else:
                return false
            inner_57 = inner_57.cdr
        outer_56 = outer_56.cdr
    return true
static func _not(x_58):
    return !x_58
static func list(args_59):
    return args_59
static func vector(x_60, y_61, z_62):
    var _cond_63 = Vector2(x_60, y_61) if z_62 == null else Vector3(x_60, y_61, z_62)
    return _cond_63
static func list_to_array(list_64):
    var arr_65 = []
    while list_64 is Cons:
        arr_65.push_back(list_64.car)
        list_64 = list_64.cdr
    return arr_65
static func array_to_list(arr_66):
    var outer_67 = cons(null, null)
    var curr_68 = outer_67
    for elem_69 in arr_66:
        curr_68.cdr = cons(elem_69, null)
        curr_68 = curr_68.cdr
    return outer_67.cdr
static func elt(arr_70, n_71):
    return arr_70[n_71]
static func set_elt(x_72, arr_73, n_74):
    arr_73[n_74] = x_72
    return arr_73[n_74]
static func is_member(value_75, arr_76):
    return value_75 in arr_76
static func sys_u002Fnative_class_private():
    var x_77 = GDScript
    return x_77.get_class()
static func is_instance(value_78, type_79):
    var _cond_80 = type_79.is_satisfies(value_78) if type_79 is GDLispSpecialType else value_78 is type_79
    return _cond_80
static func is_sys_u002Finstance_direct(value_81, type_82):
    return value_81 is type_82
static func gensym(prefix_83):
    var _cond_84 = Symbol.new(GDLisp.global_name_generator.generate()) if prefix_83 == null else Symbol.new(GDLisp.global_name_generator.generate_with(prefix_83))
    return _cond_84
static func map(f_85, xs_86):
    var _cond_87 = null
    var _cond_90 = true if xs_86 is Cons else true if xs_86 == nil else null
    if _cond_90:
        var outer_91 = cons(nil, nil)
        var curr_92 = outer_91
        while xs_86 != nil:
            curr_92.cdr = cons(funcall(f_85, GDLisp.Cons.new(xs_86.car, null)), nil)
            curr_92 = curr_92.cdr
            xs_86 = xs_86.cdr
        _cond_87 = outer_91.cdr
    else:
        var result_88 = []
        for i_89 in len(xs_86):
            result_88.push_back(funcall(f_85, GDLisp.Cons.new(xs_86[i_89], null)))
        _cond_87 = result_88
    return _cond_87
static func filter(p_93, xs_94):
    var _cond_95 = null
    var _cond_98 = true if xs_94 is Cons else true if xs_94 == nil else false
    if _cond_98:
        var outer_99 = cons(nil, nil)
        var curr_100 = outer_99
        while xs_94 != nil:
            if funcall(p_93, GDLisp.Cons.new(xs_94.car, null)):
                curr_100.cdr = cons(xs_94.car, nil)
                curr_100 = curr_100.cdr
            xs_94 = xs_94.cdr
        _cond_95 = outer_99.cdr
    else:
        var result_96 = []
        for i_97 in len(xs_94):
            if funcall(p_93, GDLisp.Cons.new(xs_94[i_97], null)):
                result_96.push_back(xs_94[i_97])
        _cond_95 = result_96
    return _cond_95
static func reverse(arg_101):
    var rev_102 = nil
    while arg_101 != nil:
        rev_102 = cons(car(arg_101), rev_102)
        arg_101 = arg_101.cdr
    return rev_102
static func append(args_103):
    var outer_104 = cons(nil, nil)
    var curr_105 = outer_104
    while args_103 != nil:
        var inner_value_106 = args_103.car
        while inner_value_106 != nil:
            curr_105.cdr = cons(inner_value_106.car, nil)
            curr_105 = curr_105.cdr
            inner_value_106 = inner_value_106.cdr
        args_103 = args_103.cdr
    return outer_104.cdr
static func sys_u002Fqq_smart_list(a_107):
    var t_108 = typeof(a_107)
    var _cond_109 = array_to_list(a_107) if TYPE_ARRAY <= t_108 && t_108 <= TYPE_COLOR_ARRAY else a_107
    return _cond_109
static func sys_u002Fqq_smart_array(a_110):
    var t_111 = typeof(a_110)
    var _cond_112 = a_110 if TYPE_ARRAY <= t_111 && t_111 <= TYPE_COLOR_ARRAY else list_to_array(a_110)
    return _cond_112
static func _PI():
    return GDLisp.cons(GDLisp.intern("literally"), GDLisp.cons(GDLisp.intern("PI"), null))
static func _SPKEY():
    return GDLisp.cons(GDLisp.intern("literally"), GDLisp.cons(GDLisp.intern("SPKEY"), null))
class GDLispSpecialType extends Reference:
    func _init():
        pass
class PrimitiveType extends GDLispSpecialType:
    func _init(primitive_value_114):
        self.primitive_value = primitive_value_114
    var primitive_value
    func is_satisfies(value_115):
        return typeof(value_115) == self.primitive_value
    var __gdlisp_outer_class_113 = load("res://GDLisp.gd")
class AnyType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_116):
        return true
class AnyRefType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_118):
        return typeof(value_118) == TYPE_OBJECT
    var __gdlisp_outer_class_117 = load("res://GDLisp.gd")
class AnyValType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_120):
        return typeof(value_120) != TYPE_OBJECT
    var __gdlisp_outer_class_119 = load("res://GDLisp.gd")
class NumberType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_122):
        var t_123 = typeof(value_122)
        var _cond_124 = true if t_123 == TYPE_INT else true if t_123 == TYPE_REAL else false
        return _cond_124
    var __gdlisp_outer_class_121 = load("res://GDLisp.gd")
class BaseArrayType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_126):
        var _cmp_127 = typeof(value_126)
        return TYPE_ARRAY <= _cmp_127 && _cmp_127 <= TYPE_COLOR_ARRAY
    var __gdlisp_outer_class_125 = load("res://GDLisp.gd")
class NothingType extends GDLispSpecialType:
    func _init():
        pass
    func is_satisfies(value_128):
        return false
static func _or(args_129):
    var args_130 = reverse(args_129)
    var _cond_131 = null
    if args_130:
        var result_132 = cons(cons(true, cons(car(args_130), null)), null)
        args_130 = cdr(args_130)
        while args_130 != nil:
            result_132 = cons(cons(car(args_130), null), result_132)
            args_130 = cdr(args_130)
        _cond_131 = cons(GDLisp.intern("cond"), result_132)
    else:
        _cond_131 = false
    return _cond_131
static func _and(args_133):
    var args_134 = reverse(args_133)
    var _cond_135 = null
    if args_134:
        var result_136 = cons(cons(true, cons(car(args_134), null)), null)
        args_134 = cdr(args_134)
        while args_134 != nil:
            result_136 = cons(cons(cons(GDLisp.intern("not"), cons(car(args_134), null)), cons(false, null)), result_136)
            args_134 = cdr(args_134)
        _cond_135 = cons(GDLisp.intern("cond"), result_136)
    else:
        _cond_135 = true
    return _cond_135
static func let_u002A(vars_137, body_138):
    var _cond_139 = cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(body_138), GDLisp.Cons.new(null, null)))) if vars_137 == nil else cons(GDLisp.intern("let"), cons(cons(car(vars_137), null), cons(cons(GDLisp.intern("let*"), cons(cdr(vars_137), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(body_138), GDLisp.Cons.new(null, null))))), null)))
    return _cond_139
static func defvars(args_140):
    var arr_141 = []
    while args_140 != nil:
        arr_141.push_back(GDLisp.Cons.new(GDLisp.intern("defvar"), GDLisp.Cons.new(args_140.car, null)))
        args_140 = args_140.cdr
    return cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(arr_141), GDLisp.Cons.new(null, null))))
static func when(cnd_142, args_143):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_142, cons(cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(args_143), GDLisp.Cons.new(null, null)))), null)), null))
static func unless(cnd_144, args_145):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_144, cons(null, null)), cons(cons(true, cons(cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(args_145), GDLisp.Cons.new(null, null)))), null)), null)))
static func _if(cnd_146, t_147, f_148):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_146, cons(t_147, null)), cons(cons(true, cons(f_148, null)), null)))
static func yield_u002A(arg_149):
    var symbol_150 = gensym("_yield")
    return cons(GDLisp.intern("let"), cons(cons(cons(symbol_150, cons(arg_149, null)), null), cons(cons(GDLisp.intern("while"), cons(cons(GDLisp.intern("and"), cons(cons(GDLisp.intern("instance?"), cons(symbol_150, cons(GDLisp.intern("GDScriptFunctionState"), null))), cons(cons(cons(GDLisp.intern("access-slot"), cons(symbol_150, cons(GDLisp.intern("is-valid"), null))), null), null))), cons(cons(GDLisp.intern("yield"), null), cons(cons(GDLisp.intern("set"), cons(symbol_150, cons(cons(cons(GDLisp.intern("access-slot"), cons(symbol_150, cons(GDLisp.intern("resume"), null))), null), null))), null)))), cons(symbol_150, null))))
static func this_file():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-file"), null))
static func this_filename():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-filename"), null))
static func this_true_filename():
    return GDLisp.cons(GDLisp.intern("sys/special-ref"), GDLisp.cons(GDLisp.intern("this-true-filename"), null))
static func contextual_load(arg_151):
    return cons(GDLisp.intern("load"), cons(cons(GDLisp.intern("sys/context-filename"), cons(arg_151, null)), null))
static func deflazy(name_152, value_153, modifiers_154):
    var fn_name_155 = gensym("_lazy")
    var this_file_156 = gensym("_this_file")
    var value_var_157 = gensym("_value")
    var meta_name_158 = "__gdlisp_Lazy_{}".format([gensym(null).contents], "{}")
    return cons(GDLisp.intern("progn"), cons(cons(GDLisp.intern("defn"), cons(fn_name_155, cons(null, cons(cons(GDLisp.intern("let"), cons(cons(cons(this_file_156, cons(cons(GDLisp.intern("this-file"), null), null)), null), cons(cons(GDLisp.intern("if"), cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file_156, cons(GDLisp.intern("has-meta"), null))), cons(meta_name_158, null)), cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file_156, cons(GDLisp.intern("get-meta"), null))), cons(meta_name_158, null)), cons(cons(GDLisp.intern("let"), cons(cons(cons(value_var_157, cons(value_153, null)), null), cons(cons(cons(GDLisp.intern("access-slot"), cons(this_file_156, cons(GDLisp.intern("set-meta"), null))), cons(meta_name_158, cons(value_var_157, null))), cons(value_var_157, null)))), null)))), null))), null)))), cons(cons(GDLisp.intern("define-symbol-macro"), cons(name_152, cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("quote"), cons(GDLisp.intern("access-slot"), null)), cons(cons(GDLisp.intern("list"), cons(cons(GDLisp.intern("quote"), cons(GDLisp.intern("contextual-load"), null)), cons(cons(GDLisp.intern("this-true-filename"), null), null))), cons(cons(GDLisp.intern("quote"), cons(fn_name_155, null)), null)))), null)), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(modifiers_154), GDLisp.Cons.new(null, null)))))), null)))
static func defobject(name_159, parent_160, visibility_161, body_162):
    if visibility_161 == nil:
        visibility_161 = GDLisp.intern("public")
    elif !is_instance(visibility_161, Symbol):
        body_162 = cons(visibility_161, body_162)
        visibility_161 = GDLisp.intern("public")
    elif visibility_161.contents == "public":
        pass
    elif visibility_161.contents == "private":
        pass
    else:
        body_162 = cons(visibility_161, body_162)
        visibility_161 = GDLisp.intern("public")
    return cons(GDLisp.intern("deflazy"), cons(name_159, cons(cons(GDLisp.intern("new"), cons(parent_160, append(GDLisp.Cons.new(sys_u002Fqq_smart_list(body_162), GDLisp.Cons.new(null, null))))), cons(visibility_161, null))))
static func run():
    return null
