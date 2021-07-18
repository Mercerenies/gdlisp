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
var global_name_generator
static func cons(a_11, b_12):
    return Cons.new(a_11, b_12)
static func car(a_13):
    return a_13.car
static func cdr(a_14):
    return a_14.cdr
static func intern(a_15):
    return Symbol.new(a_15)
static func length(x_16):
    var result_17 = 0
    while x_16 is Cons:
        result_17 = result_17 + 1
        x_16 = x_16.cdr
    return result_17
static func funcall(f_18, args_19):
    var _cond_20 = f_18.call_funcv(args_19) if f_18 is Function else push_error("Attempt to call non-function")
    return _cond_20
static func _u002B(args_21):
    var _cond_22 = null
    if args_21 is Cons:
        var result_23 = args_21.car
        args_21 = args_21.cdr
        while args_21 is Cons:
            result_23 = result_23 + args_21.car
            args_21 = args_21.cdr
        _cond_22 = result_23
    else:
        _cond_22 = 0
    return _cond_22
static func _u002A(args_24):
    var result_25 = 1
    while args_24 is Cons:
        result_25 = result_25 * args_24.car
        args_24 = args_24.cdr
    return result_25
static func _(x_26, args_27):
    var _cond_28 = null
    if args_27 is Cons:
        var result_29 = x_26
        while args_27 is Cons:
            result_29 = result_29 - args_27.car
            args_27 = args_27.cdr
        _cond_28 = result_29
    else:
        _cond_28 = -x_26
    return _cond_28
static func _u002F(x_30, args_31):
    var _cond_32 = null
    if args_31 is Cons:
        var result_33 = x_30
        while args_31 is Cons:
            result_33 = result_33 / float(args_31.car)
            args_31 = args_31.cdr
        _cond_32 = result_33
    else:
        _cond_32 = 1 / float(x_30)
    return _cond_32
static func div(x_34, args_35):
    var _cond_36 = null
    if args_35 is Cons:
        var result_37 = x_34
        while args_35 is Cons:
            result_37 = result_37 / int(args_35.car)
            args_35 = args_35.cdr
        _cond_36 = result_37
    else:
        _cond_36 = 1 / int(x_34)
    return _cond_36
static func mod(x_38, y_39):
    return x_38 % y_39
static func _EQ_(x_40, args_41):
    while args_41 is Cons:
        if x_40 == args_41.car:
            pass
        else:
            return false
        x_40 = args_41.car
        args_41 = args_41.cdr
    return true
static func _LT_(x_42, args_43):
    while args_43 is Cons:
        if x_42 < args_43.car:
            pass
        else:
            return false
        x_42 = args_43.car
        args_43 = args_43.cdr
    return true
static func _GT_(x_44, args_45):
    while args_45 is Cons:
        if x_44 > args_45.car:
            pass
        else:
            return false
        x_44 = args_45.car
        args_45 = args_45.cdr
    return true
static func _LT__EQ_(x_46, args_47):
    while args_47 is Cons:
        if x_46 <= args_47.car:
            pass
        else:
            return false
        x_46 = args_47.car
        args_47 = args_47.cdr
    return true
static func _GT__EQ_(x_48, args_49):
    while args_49 is Cons:
        if x_48 >= args_49.car:
            pass
        else:
            return false
        x_48 = args_49.car
        args_49 = args_49.cdr
    return true
static func _u002F_EQ_(x_50, args_51):
    var outer_52 = cons(x_50, args_51)
    while outer_52 is Cons:
        var inner_53 = outer_52.cdr
        while inner_53 is Cons:
            if outer_52.car != inner_53.car:
                pass
            else:
                return false
            inner_53 = inner_53.cdr
        outer_52 = outer_52.cdr
    return true
static func _not(x_54):
    return !x_54
static func list(args_55):
    return args_55
static func vector(x_56, y_57, z_58):
    var _cond_59 = Vector2(x_56, y_57) if z_58 == null else Vector3(x_56, y_57, z_58)
    return _cond_59
static func list_to_array(list_60):
    var arr_61 = []
    while list_60 is Cons:
        arr_61.push_back(list_60.car)
        list_60 = list_60.cdr
    return arr_61
static func array_to_list(arr_62):
    var outer_63 = cons(null, null)
    var curr_64 = outer_63
    for elem_65 in arr_62:
        curr_64.cdr = cons(elem_65, null)
        curr_64 = curr_64.cdr
    return outer_63.cdr
static func elt(arr_66, n_67):
    return arr_66[n_67]
static func set_elt(x_68, arr_69, n_70):
    arr_69[n_70] = x_68
    return arr_69[n_70]
static func is_member(value_71, arr_72):
    return value_71 in arr_72
static func is_instance(value_73, type_74):
    var _cond_75 = typeof(value_73) == type_74 if typeof(type_74) == Int else value_73 is type_74
    return _cond_75
static func is_sys_u002Finstance_direct(value_76, type_77):
    return value_76 is type_77
static func gensym(prefix_78):
    var _cond_79 = Symbol.new(GDLisp.global_name_generator.generate()) if prefix_78 == null else Symbol.new(GDLisp.global_name_generator.generate_with(prefix_78))
    return _cond_79
static func map(f_80, xs_81):
    var _cond_82 = null
    var _cond_85 = true if xs_81 is Cons else true if xs_81 == nil else null
    if _cond_85:
        var outer_86 = cons(nil, nil)
        var curr_87 = outer_86
        while xs_81 != nil:
            curr_87.cdr = cons(funcall(f_80, GDLisp.Cons.new(xs_81.car, null)), nil)
            curr_87 = curr_87.cdr
            xs_81 = xs_81.cdr
        _cond_82 = outer_86.cdr
    else:
        var result_83 = []
        for i_84 in len(xs_81):
            result_83.push_back(funcall(f_80, GDLisp.Cons.new(xs_81[i_84], null)))
        _cond_82 = result_83
    return _cond_82
static func filter(p_88, xs_89):
    var _cond_90 = null
    var _cond_93 = true if xs_89 is Cons else true if xs_89 == nil else false
    if _cond_93:
        var outer_94 = cons(nil, nil)
        var curr_95 = outer_94
        while xs_89 != nil:
            if funcall(p_88, GDLisp.Cons.new(xs_89.car, null)):
                curr_95.cdr = cons(xs_89.car, nil)
                curr_95 = curr_95.cdr
            xs_89 = xs_89.cdr
        _cond_90 = outer_94.cdr
    else:
        var result_91 = []
        for i_92 in len(xs_89):
            if funcall(p_88, GDLisp.Cons.new(xs_89[i_92], null)):
                result_91.push_back(xs_89[i_92])
        _cond_90 = result_91
    return _cond_90
static func reverse(arg_96):
    var rev_97 = nil
    while arg_96 != nil:
        rev_97 = cons(car(arg_96), rev_97)
        arg_96 = arg_96.cdr
    return rev_97
static func append(args_98):
    var outer_99 = cons(nil, nil)
    var curr_100 = outer_99
    while args_98 != nil:
        var inner_value_101 = args_98.car
        while inner_value_101 != nil:
            curr_100.cdr = cons(inner_value_101.car, nil)
            curr_100 = curr_100.cdr
            inner_value_101 = inner_value_101.cdr
        args_98 = args_98.cdr
    return outer_99.cdr
static func sys_u002Fqq_smart_list(a_102):
    var t_103 = typeof(a_102)
    var _cond_104 = array_to_list(a_102) if _Array <= t_103 && t_103 <= _PoolColorArray else a_102
    return _cond_104
static func sys_u002Fqq_smart_array(a_105):
    var t_106 = typeof(a_105)
    var _cond_107 = a_105 if _Array <= t_106 && t_106 <= _PoolColorArray else list_to_array(a_105)
    return _cond_107
const Null = TYPE_NIL
const Bool = TYPE_BOOL
const Int = TYPE_INT
const Float = TYPE_REAL
const _String = TYPE_STRING
const _Vector2 = TYPE_VECTOR2
const _Rect2 = TYPE_RECT2
const _Vector3 = TYPE_VECTOR3
const _Transform2D = TYPE_TRANSFORM2D
const _Plane = TYPE_PLANE
const _Quat = TYPE_QUAT
const _AABB = TYPE_AABB
const _Basis = TYPE_BASIS
const _Transform = TYPE_TRANSFORM
const _Color = TYPE_COLOR
const _NodePath = TYPE_NODE_PATH
const _RID = TYPE_RID
const _Object = TYPE_OBJECT
const _Dictionary = TYPE_DICTIONARY
const _Array = TYPE_ARRAY
const _PoolByteArray = TYPE_RAW_ARRAY
const _PoolIntArray = TYPE_INT_ARRAY
const _PoolRealArray = TYPE_REAL_ARRAY
const _PoolStringArray = TYPE_STRING_ARRAY
const _PoolVector2Array = TYPE_VECTOR2_ARRAY
const _PoolVector3Array = TYPE_VECTOR3_ARRAY
const _PoolColorArray = TYPE_COLOR_ARRAY
static func _or(args_108):
    var args_109 = reverse(args_108)
    var _cond_110 = null
    if args_109:
        var result_111 = cons(cons(true, cons(car(args_109), null)), null)
        args_109 = cdr(args_109)
        while args_109 != nil:
            result_111 = cons(cons(car(args_109), null), result_111)
            args_109 = cdr(args_109)
        _cond_110 = cons(GDLisp.intern("cond"), result_111)
    else:
        _cond_110 = false
    return _cond_110
static func _and(args_112):
    var args_113 = reverse(args_112)
    var _cond_114 = null
    if args_113:
        var result_115 = cons(cons(true, cons(car(args_113), null)), null)
        args_113 = cdr(args_113)
        while args_113 != nil:
            result_115 = cons(cons(cons(GDLisp.intern("not"), cons(car(args_113), null)), cons(false, null)), result_115)
            args_113 = cdr(args_113)
        _cond_114 = cons(GDLisp.intern("cond"), result_115)
    else:
        _cond_114 = true
    return _cond_114
static func let_u002A(vars_116, body_117):
    var _cond_118 = cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(body_117), GDLisp.Cons.new(null, null)))) if vars_116 == nil else cons(GDLisp.intern("let"), cons(cons(car(vars_116), null), cons(cons(GDLisp.intern("let*"), cons(cdr(vars_116), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(body_117), GDLisp.Cons.new(null, null))))), null)))
    return _cond_118
static func defvars(args_119):
    var arr_120 = []
    while args_119 != nil:
        arr_120.push_back(GDLisp.Cons.new(GDLisp.intern("defvar"), GDLisp.Cons.new(args_119.car, null)))
        args_119 = args_119.cdr
    return cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(arr_120), GDLisp.Cons.new(null, null))))
static func when(cnd_121, args_122):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_121, cons(cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(args_122), GDLisp.Cons.new(null, null)))), null)), null))
static func unless(cnd_123, args_124):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_123, cons(null, null)), cons(cons(true, cons(cons(GDLisp.intern("progn"), append(GDLisp.Cons.new(sys_u002Fqq_smart_list(args_124), GDLisp.Cons.new(null, null)))), null)), null)))
static func _if(cnd_125, t_126, f_127):
    return cons(GDLisp.intern("cond"), cons(cons(cnd_125, cons(t_126, null)), cons(cons(true, cons(f_127, null)), null)))
static func yield_u002A(arg_128):
    var symbol_129 = gensym("_yield")
    return cons(GDLisp.intern("let"), cons(cons(cons(symbol_129, cons(arg_128, null)), null), cons(cons(GDLisp.intern("while"), cons(cons(GDLisp.intern("and"), cons(cons(GDLisp.intern("instance?"), cons(symbol_129, cons(GDLisp.intern("GDScriptFunctionState"), null))), cons(cons(cons(GDLisp.intern("access-slot"), cons(symbol_129, cons(GDLisp.intern("is-valid"), null))), null), null))), cons(cons(GDLisp.intern("yield"), null), cons(cons(GDLisp.intern("set"), cons(symbol_129, cons(cons(cons(GDLisp.intern("access-slot"), cons(symbol_129, cons(GDLisp.intern("resume"), null))), null), null))), null)))), cons(symbol_129, null))))
static func run():
    return null
