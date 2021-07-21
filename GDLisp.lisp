
;; TEST FILE FOR BOOTSTRAPPING ;;

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
  (defvar __gdlisp_rest 1))

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

(sys/declare superglobal BUTTON_LEFT)
(sys/declare superglobal BUTTON_RIGHT)
(sys/declare superglobal BUTTON_MIDDLE)
(sys/declare superglobal BUTTON_XBUTTON1)
(sys/declare superglobal BUTTON_XBUTTON2)
(sys/declare superglobal BUTTON_WHEEL_UP)
(sys/declare superglobal BUTTON_WHEEL_DOWN)
(sys/declare superglobal BUTTON_WHEEL_LEFT)
(sys/declare superglobal BUTTON_WHEEL_RIGHT)
(sys/declare superglobal BUTTON_MASK_LEFT)
(sys/declare superglobal BUTTON_MASK_RIGHT)
(sys/declare superglobal BUTTON_MASK_MIDDLE)
(sys/declare superglobal BUTTON_MASK_XBUTTON1)
(sys/declare superglobal BUTTON_MASK_XBUTTON2)
(sys/declare superglobal CORNER_TOP_LEFT)
(sys/declare superglobal CORNER_TOP_RIGHT)
(sys/declare superglobal CORNER_BOTTOM_RIGHT)
(sys/declare superglobal CORNER_BOTTOM_LEFT)
(sys/declare superglobal JOY_BUTTON_0)
(sys/declare superglobal JOY_BUTTON_1)
(sys/declare superglobal JOY_BUTTON_2)
(sys/declare superglobal JOY_BUTTON_3)
(sys/declare superglobal JOY_BUTTON_4)
(sys/declare superglobal JOY_BUTTON_5)
(sys/declare superglobal JOY_BUTTON_6)
(sys/declare superglobal JOY_BUTTON_7)
(sys/declare superglobal JOY_BUTTON_8)
(sys/declare superglobal JOY_BUTTON_9)
(sys/declare superglobal JOY_BUTTON_10)
(sys/declare superglobal JOY_BUTTON_11)
(sys/declare superglobal JOY_BUTTON_12)
(sys/declare superglobal JOY_BUTTON_13)
(sys/declare superglobal JOY_BUTTON_14)
(sys/declare superglobal JOY_BUTTON_15)
(sys/declare superglobal JOY_BUTTON_MAX)
(sys/declare superglobal JOY_SONY_CIRCLE)
(sys/declare superglobal JOY_SONY_X)
(sys/declare superglobal JOY_SONY_SQUARE)
(sys/declare superglobal JOY_SONY_TRIANGLE)
(sys/declare superglobal JOY_XBOX_B)
(sys/declare superglobal JOY_XBOX_A)
(sys/declare superglobal JOY_XBOX_X)
(sys/declare superglobal JOY_XBOX_Y)
(sys/declare superglobal JOY_DS_A)
(sys/declare superglobal JOY_DS_B)
(sys/declare superglobal JOY_DS_X)
(sys/declare superglobal JOY_DS_Y)
(sys/declare superglobal JOY_VR_GRIP)
(sys/declare superglobal JOY_VR_PAD)
(sys/declare superglobal JOY_VR_TRIGGER)
(sys/declare superglobal JOY_OCULUS_AX)
(sys/declare superglobal JOY_OCULUS_BY)
(sys/declare superglobal JOY_OCULUS_MENU)
(sys/declare superglobal JOY_OPENVR_MENU)
(sys/declare superglobal JOY_SELECT)
(sys/declare superglobal JOY_START)
(sys/declare superglobal JOY_DPAD_UP)
(sys/declare superglobal JOY_DPAD_DOWN)
(sys/declare superglobal JOY_DPAD_LEFT)
(sys/declare superglobal JOY_DPAD_RIGHT)
(sys/declare superglobal JOY_L)
(sys/declare superglobal JOY_L2)
(sys/declare superglobal JOY_L3)
(sys/declare superglobal JOY_R)
(sys/declare superglobal JOY_R2)
(sys/declare superglobal JOY_R3)
(sys/declare superglobal JOY_AXIS_0)
(sys/declare superglobal JOY_AXIS_1)
(sys/declare superglobal JOY_AXIS_2)
(sys/declare superglobal JOY_AXIS_3)
(sys/declare superglobal JOY_AXIS_4)
(sys/declare superglobal JOY_AXIS_5)
(sys/declare superglobal JOY_AXIS_6)
(sys/declare superglobal JOY_AXIS_7)
(sys/declare superglobal JOY_AXIS_8)
(sys/declare superglobal JOY_AXIS_9)
(sys/declare superglobal JOY_AXIS_MAX)
(sys/declare superglobal JOY_ANALOG_LX)
(sys/declare superglobal JOY_ANALOG_LY)
(sys/declare superglobal JOY_ANALOG_RX)
(sys/declare superglobal JOY_ANALOG_RY)
(sys/declare superglobal JOY_ANALOG_L2)
(sys/declare superglobal JOY_ANALOG_R2)
(sys/declare superglobal JOY_VR_ANALOG_TRIGGER)
(sys/declare superglobal JOY_VR_ANALOG_GRIP)
(sys/declare superglobal JOY_OPENVR_TOUCHPADX)
(sys/declare superglobal JOY_OPENVR_TOUCHPADY)
(sys/declare superglobal MARGIN_LEFT)
(sys/declare superglobal MARGIN_TOP)
(sys/declare superglobal MARGIN_RIGHT)
(sys/declare superglobal MARGIN_BOTTOM)
(sys/declare superglobal KEY_CODE_MASK)
(sys/declare superglobal KEY_MODIFIER_MASK)
(sys/declare superglobal KEY_MASK_SHIFT)
(sys/declare superglobal KEY_MASK_ALT)
(sys/declare superglobal KEY_MASK_META)
(sys/declare superglobal KEY_MASK_CTRL)
(sys/declare superglobal KEY_MASK_CMD)
(sys/declare superglobal KEY_MASK_KPAD)
(sys/declare superglobal KEY_MASK_GROUP_SWITCH)
(sys/declare superglobal VERTICAL)
(sys/declare superglobal HORIZONTAL)
(sys/declare superglobal VALIGN_TOP)
(sys/declare superglobal VALIGN_CENTER)
(sys/declare superglobal VALIGN_BOTTOM)
(sys/declare superglobal KEY_ESCAPE)
(sys/declare superglobal KEY_TAB)
(sys/declare superglobal KEY_BACKTAB)
(sys/declare superglobal KEY_BACKSPACE)
(sys/declare superglobal KEY_ENTER)
(sys/declare superglobal KEY_KP_ENTER)
(sys/declare superglobal KEY_INSERT)
(sys/declare superglobal KEY_DELETE)
(sys/declare superglobal KEY_PAUSE)
(sys/declare superglobal KEY_PRINT)
(sys/declare superglobal KEY_SYSREQ)
(sys/declare superglobal KEY_CLEAR)
(sys/declare superglobal KEY_HOME)
(sys/declare superglobal KEY_END)
(sys/declare superglobal KEY_LEFT)
(sys/declare superglobal KEY_UP)
(sys/declare superglobal KEY_RIGHT)
(sys/declare superglobal KEY_DOWN)
(sys/declare superglobal KEY_PAGEUP)
(sys/declare superglobal KEY_PAGEDOWN)
(sys/declare superglobal KEY_SHIFT)
(sys/declare superglobal KEY_CONTROL)
(sys/declare superglobal KEY_META)
(sys/declare superglobal KEY_ALT)
(sys/declare superglobal KEY_CAPSLOCK)
(sys/declare superglobal KEY_NUMLOCK)
(sys/declare superglobal KEY_SCROLLLOCK)
(sys/declare superglobal KEY_F1)
(sys/declare superglobal KEY_F2)
(sys/declare superglobal KEY_F3)
(sys/declare superglobal KEY_F4)
(sys/declare superglobal KEY_F5)
(sys/declare superglobal KEY_F6)
(sys/declare superglobal KEY_F7)
(sys/declare superglobal KEY_F8)
(sys/declare superglobal KEY_F9)
(sys/declare superglobal KEY_F10)
(sys/declare superglobal KEY_F11)
(sys/declare superglobal KEY_F12)
(sys/declare superglobal KEY_F13)
(sys/declare superglobal KEY_F14)
(sys/declare superglobal KEY_F15)
(sys/declare superglobal KEY_F16)
(sys/declare superglobal KEY_KP_MULTIPLY)
(sys/declare superglobal KEY_KP_DIVIDE)
(sys/declare superglobal KEY_KP_SUBTRACT)
(sys/declare superglobal KEY_KP_PERIOD)
(sys/declare superglobal KEY_KP_ADD)
(sys/declare superglobal KEY_KP_0)
(sys/declare superglobal KEY_KP_1)
(sys/declare superglobal KEY_KP_2)
(sys/declare superglobal KEY_KP_3)
(sys/declare superglobal KEY_KP_4)
(sys/declare superglobal KEY_KP_5)
(sys/declare superglobal KEY_KP_6)
(sys/declare superglobal KEY_KP_7)
(sys/declare superglobal KEY_KP_8)
(sys/declare superglobal KEY_KP_9)
(sys/declare superglobal KEY_SUPER_L)
(sys/declare superglobal KEY_SUPER_R)
(sys/declare superglobal KEY_MENU)
(sys/declare superglobal KEY_HYPER_L)
(sys/declare superglobal KEY_HYPER_R)
(sys/declare superglobal KEY_HELP)
(sys/declare superglobal KEY_DIRECTION_L)
(sys/declare superglobal KEY_DIRECTION_R)
(sys/declare superglobal KEY_BACK)
(sys/declare superglobal KEY_FORWARD)
(sys/declare superglobal KEY_STOP)
(sys/declare superglobal KEY_REFRESH)
(sys/declare superglobal KEY_VOLUMEDOWN)
(sys/declare superglobal KEY_VOLUMEMUTE)
(sys/declare superglobal KEY_VOLUMEUP)
(sys/declare superglobal KEY_BASSBOOST)
(sys/declare superglobal KEY_BASSUP)
(sys/declare superglobal KEY_BASSDOWN)
(sys/declare superglobal KEY_TREBLEUP)
(sys/declare superglobal KEY_TREBLEDOWN)
(sys/declare superglobal KEY_MEDIAPLAY)
(sys/declare superglobal KEY_MEDIASTOP)
(sys/declare superglobal KEY_MEDIAPREVIOUS)
(sys/declare superglobal KEY_MEDIANEXT)
(sys/declare superglobal KEY_MEDIARECORD)
(sys/declare superglobal KEY_HOMEPAGE)
(sys/declare superglobal KEY_FAVORITES)
(sys/declare superglobal KEY_SEARCH)
(sys/declare superglobal KEY_STANDBY)
(sys/declare superglobal KEY_OPENURL)
(sys/declare superglobal KEY_LAUNCHMAIL)
(sys/declare superglobal KEY_LAUNCHMEDIA)
(sys/declare superglobal KEY_LAUNCH0)
(sys/declare superglobal KEY_LAUNCH1)
(sys/declare superglobal KEY_LAUNCH2)
(sys/declare superglobal KEY_LAUNCH3)
(sys/declare superglobal KEY_LAUNCH4)
(sys/declare superglobal KEY_LAUNCH5)
(sys/declare superglobal KEY_LAUNCH6)
(sys/declare superglobal KEY_LAUNCH7)
(sys/declare superglobal KEY_LAUNCH8)
(sys/declare superglobal KEY_LAUNCH9)
(sys/declare superglobal KEY_LAUNCHA)
(sys/declare superglobal KEY_LAUNCHB)
(sys/declare superglobal KEY_LAUNCHC)
(sys/declare superglobal KEY_LAUNCHD)
(sys/declare superglobal KEY_LAUNCHE)
(sys/declare superglobal KEY_LAUNCHF)
(sys/declare superglobal KEY_UNKNOWN)
(sys/declare superglobal KEY_SPACE)
(sys/declare superglobal KEY_EXCLAM)
(sys/declare superglobal KEY_QUOTEDBL)
(sys/declare superglobal KEY_NUMBERSIGN)
(sys/declare superglobal KEY_DOLLAR)
(sys/declare superglobal KEY_PERCENT)
(sys/declare superglobal KEY_AMPERSAND)
(sys/declare superglobal KEY_APOSTROPHE)
(sys/declare superglobal KEY_PARENLEFT)
(sys/declare superglobal KEY_PARENRIGHT)
(sys/declare superglobal KEY_ASTERISK)
(sys/declare superglobal KEY_PLUS)
(sys/declare superglobal KEY_COMMA)
(sys/declare superglobal KEY_MINUS)
(sys/declare superglobal KEY_PERIOD)
(sys/declare superglobal KEY_SLASH)
(sys/declare superglobal KEY_0)
(sys/declare superglobal KEY_1)
(sys/declare superglobal KEY_2)
(sys/declare superglobal KEY_3)
(sys/declare superglobal KEY_4)
(sys/declare superglobal KEY_5)
(sys/declare superglobal KEY_6)
(sys/declare superglobal KEY_7)
(sys/declare superglobal KEY_8)
(sys/declare superglobal KEY_9)
(sys/declare superglobal KEY_COLON)
(sys/declare superglobal KEY_SEMICOLON)
(sys/declare superglobal KEY_LESS)
(sys/declare superglobal KEY_EQUAL)
(sys/declare superglobal KEY_GREATER)
(sys/declare superglobal KEY_QUESTION)
(sys/declare superglobal KEY_AT)
(sys/declare superglobal KEY_A)
(sys/declare superglobal KEY_B)
(sys/declare superglobal KEY_C)
(sys/declare superglobal KEY_D)
(sys/declare superglobal KEY_E)
(sys/declare superglobal KEY_F)
(sys/declare superglobal KEY_G)
(sys/declare superglobal KEY_H)
(sys/declare superglobal KEY_I)
(sys/declare superglobal KEY_J)
(sys/declare superglobal KEY_K)
(sys/declare superglobal KEY_L)
(sys/declare superglobal KEY_M)
(sys/declare superglobal KEY_N)
(sys/declare superglobal KEY_O)
(sys/declare superglobal KEY_P)
(sys/declare superglobal KEY_Q)
(sys/declare superglobal KEY_R)
(sys/declare superglobal KEY_S)
(sys/declare superglobal KEY_T)
(sys/declare superglobal KEY_U)
(sys/declare superglobal KEY_V)
(sys/declare superglobal KEY_W)
(sys/declare superglobal KEY_X)
(sys/declare superglobal KEY_Y)
(sys/declare superglobal KEY_Z)
(sys/declare superglobal KEY_BRACKETLEFT)
(sys/declare superglobal KEY_BACKSLASH)
(sys/declare superglobal KEY_BRACKETRIGHT)
(sys/declare superglobal KEY_ASCIICIRCUM)
(sys/declare superglobal KEY_UNDERSCORE)
(sys/declare superglobal KEY_QUOTELEFT)
(sys/declare superglobal KEY_BRACELEFT)
(sys/declare superglobal KEY_BAR)
(sys/declare superglobal KEY_BRACERIGHT)
(sys/declare superglobal KEY_ASCIITILDE)
(sys/declare superglobal KEY_NOBREAKSPACE)
(sys/declare superglobal KEY_EXCLAMDOWN)
(sys/declare superglobal KEY_CENT)
(sys/declare superglobal KEY_STERLING)
(sys/declare superglobal KEY_CURRENCY)
(sys/declare superglobal KEY_YEN)
(sys/declare superglobal KEY_BROKENBAR)
(sys/declare superglobal KEY_SECTION)
(sys/declare superglobal KEY_DIAERESIS)
(sys/declare superglobal KEY_COPYRIGHT)
(sys/declare superglobal KEY_ORDFEMININE)
(sys/declare superglobal KEY_GUILLEMOTLEFT)
(sys/declare superglobal KEY_NOTSIGN)
(sys/declare superglobal KEY_HYPHEN)
(sys/declare superglobal KEY_REGISTERED)
(sys/declare superglobal KEY_MACRON)
(sys/declare superglobal KEY_DEGREE)
(sys/declare superglobal KEY_PLUSMINUS)
(sys/declare superglobal KEY_TWOSUPERIOR)
(sys/declare superglobal KEY_THREESUPERIOR)
(sys/declare superglobal KEY_ACUTE)
(sys/declare superglobal KEY_MU)
(sys/declare superglobal KEY_PARAGRAPH)
(sys/declare superglobal KEY_PERIODCENTERED)
(sys/declare superglobal KEY_CEDILLA)
(sys/declare superglobal KEY_ONESUPERIOR)
(sys/declare superglobal KEY_MASCULINE)
(sys/declare superglobal KEY_GUILLEMOTRIGHT)
(sys/declare superglobal KEY_ONEQUARTER)
(sys/declare superglobal KEY_ONEHALF)
(sys/declare superglobal KEY_THREEQUARTERS)
(sys/declare superglobal KEY_QUESTIONDOWN)
(sys/declare superglobal KEY_AGRAVE)
(sys/declare superglobal KEY_AACUTE)
(sys/declare superglobal KEY_ACIRCUMFLEX)
(sys/declare superglobal KEY_ATILDE)
(sys/declare superglobal KEY_ADIAERESIS)
(sys/declare superglobal KEY_ARING)
(sys/declare superglobal KEY_AE)
(sys/declare superglobal KEY_CCEDILLA)
(sys/declare superglobal KEY_EGRAVE)
(sys/declare superglobal KEY_EACUTE)
(sys/declare superglobal KEY_ECIRCUMFLEX)
(sys/declare superglobal KEY_EDIAERESIS)
(sys/declare superglobal KEY_IGRAVE)
(sys/declare superglobal KEY_IACUTE)
(sys/declare superglobal KEY_ICIRCUMFLEX)
(sys/declare superglobal KEY_IDIAERESIS)
(sys/declare superglobal KEY_ETH)
(sys/declare superglobal KEY_NTILDE)
(sys/declare superglobal KEY_OGRAVE)
(sys/declare superglobal KEY_OACUTE)
(sys/declare superglobal KEY_OCIRCUMFLEX)
(sys/declare superglobal KEY_OTILDE)
(sys/declare superglobal KEY_ODIAERESIS)
(sys/declare superglobal KEY_MULTIPLY)
(sys/declare superglobal KEY_OOBLIQUE)
(sys/declare superglobal KEY_UGRAVE)
(sys/declare superglobal KEY_UACUTE)
(sys/declare superglobal KEY_UCIRCUMFLEX)
(sys/declare superglobal KEY_UDIAERESIS)
(sys/declare superglobal KEY_YACUTE)
(sys/declare superglobal KEY_THORN)
(sys/declare superglobal KEY_SSHARP)
(sys/declare superglobal KEY_DIVISION)
(sys/declare superglobal KEY_YDIAERESIS)
(sys/declare superglobal HALIGN_LEFT)
(sys/declare superglobal HALIGN_CENTER)
(sys/declare superglobal HALIGN_RIGHT)
(sys/declare superglobal MIDI_MESSAGE_NOTE_OFF)
(sys/declare superglobal MIDI_MESSAGE_NOTE_ON)
(sys/declare superglobal MIDI_MESSAGE_AFTERTOUCH)
(sys/declare superglobal MIDI_MESSAGE_CONTROL_CHANGE)
(sys/declare superglobal MIDI_MESSAGE_PROGRAM_CHANGE)
(sys/declare superglobal MIDI_MESSAGE_CHANNEL_PRESSURE)
(sys/declare superglobal MIDI_MESSAGE_PITCH_BEND)

(defenum Mouse
  (LEFT BUTTON_LEFT)
  (RIGHT BUTTON_RIGHT)
  (MIDDLE BUTTON_MIDDLE)
  (XBUTTON1 BUTTON_XBUTTON1)
  (XBUTTON2 BUTTON_XBUTTON2)
  (WHEEL_UP BUTTON_WHEEL_UP)
  (WHEEL_DOWN BUTTON_WHEEL_DOWN)
  (WHEEL_LEFT BUTTON_WHEEL_LEFT)
  (WHEEL_RIGHT BUTTON_WHEEL_RIGHT)
  (MASK_LEFT BUTTON_MASK_LEFT)
  (MASK_RIGHT BUTTON_MASK_RIGHT)
  (MASK_MIDDLE BUTTON_MASK_MIDDLE)
  (MASK_XBUTTON1 BUTTON_MASK_XBUTTON1)
  (MASK_XBUTTON2 BUTTON_MASK_XBUTTON2))

(defenum Corner
  (TOP_LEFT CORNER_TOP_LEFT)
  (TOP_RIGHT CORNER_TOP_RIGHT)
  (BOTTOM_RIGHT CORNER_BOTTOM_RIGHT)
  (BOTTOM_LEFT CORNER_BOTTOM_LEFT))

(defenum Joy
  (BUTTON_0 JOY_BUTTON_0)
  (BUTTON_1 JOY_BUTTON_1)
  (BUTTON_2 JOY_BUTTON_2)
  (BUTTON_3 JOY_BUTTON_3)
  (BUTTON_4 JOY_BUTTON_4)
  (BUTTON_5 JOY_BUTTON_5)
  (BUTTON_6 JOY_BUTTON_6)
  (BUTTON_7 JOY_BUTTON_7)
  (BUTTON_8 JOY_BUTTON_8)
  (BUTTON_9 JOY_BUTTON_9)
  (BUTTON_10 JOY_BUTTON_10)
  (BUTTON_11 JOY_BUTTON_11)
  (BUTTON_12 JOY_BUTTON_12)
  (BUTTON_13 JOY_BUTTON_13)
  (BUTTON_14 JOY_BUTTON_14)
  (BUTTON_15 JOY_BUTTON_15)
  (BUTTON_MAX JOY_BUTTON_MAX)
  (SONY_CIRCLE JOY_SONY_CIRCLE)
  (SONY_X JOY_SONY_X)
  (SONY_SQUARE JOY_SONY_SQUARE)
  (SONY_TRIANGLE JOY_SONY_TRIANGLE)
  (XBOX_B JOY_XBOX_B)
  (XBOX_A JOY_XBOX_A)
  (XBOX_X JOY_XBOX_X)
  (XBOX_Y JOY_XBOX_Y)
  (DS_A JOY_DS_A)
  (DS_B JOY_DS_B)
  (DS_X JOY_DS_X)
  (DS_Y JOY_DS_Y)
  (VR_GRIP JOY_VR_GRIP)
  (VR_PAD JOY_VR_PAD)
  (VR_TRIGGER JOY_VR_TRIGGER)
  (OCULUS_AX JOY_OCULUS_AX)
  (OCULUS_BY JOY_OCULUS_BY)
  (OCULUS_MENU JOY_OCULUS_MENU)
  (OPENVR_MENU JOY_OPENVR_MENU)
  (SELECT JOY_SELECT)
  (START JOY_START)
  (DPAD_UP JOY_DPAD_UP)
  (DPAD_DOWN JOY_DPAD_DOWN)
  (DPAD_LEFT JOY_DPAD_LEFT)
  (DPAD_RIGHT JOY_DPAD_RIGHT)
  (L JOY_L)
  (L2 JOY_L2)
  (L3 JOY_L3)
  (R JOY_R)
  (R2 JOY_R2)
  (R3 JOY_R3)
  (AXIS_0 JOY_AXIS_0)
  (AXIS_1 JOY_AXIS_1)
  (AXIS_2 JOY_AXIS_2)
  (AXIS_3 JOY_AXIS_3)
  (AXIS_4 JOY_AXIS_4)
  (AXIS_5 JOY_AXIS_5)
  (AXIS_6 JOY_AXIS_6)
  (AXIS_7 JOY_AXIS_7)
  (AXIS_8 JOY_AXIS_8)
  (AXIS_9 JOY_AXIS_9)
  (AXIS_MAX JOY_AXIS_MAX)
  (ANALOG_LX JOY_ANALOG_LX)
  (ANALOG_LY JOY_ANALOG_LY)
  (ANALOG_RX JOY_ANALOG_RX)
  (ANALOG_RY JOY_ANALOG_RY)
  (ANALOG_L2 JOY_ANALOG_L2)
  (ANALOG_R2 JOY_ANALOG_R2)
  (VR_ANALOG_TRIGGER JOY_VR_ANALOG_TRIGGER)
  (VR_ANALOG_GRIP JOY_VR_ANALOG_GRIP)
  (OPENVR_TOUCHPADX JOY_OPENVR_TOUCHPADX)
  (OPENVR_TOUCHPADY JOY_OPENVR_TOUCHPADY))

(defenum Margin
  (LEFT MARGIN_LEFT)
  (TOP MARGIN_TOP)
  (RIGHT MARGIN_RIGHT)
  (BOTTOM MARGIN_BOTTOM))

(defenum KeyMask
  (CODE_MASK KEY_CODE_MASK)
  (MODIFIER_MASK KEY_MODIFIER_MASK)
  (SHIFT KEY_MASK_SHIFT)
  (ALT KEY_MASK_ALT)
  (META KEY_MASK_META)
  (CTRL KEY_MASK_CTRL)
  (CMD KEY_MASK_CMD)
  (KPAD KEY_MASK_KPAD)
  (GROUP_SWITCH KEY_MASK_GROUP_SWITCH))

(defenum Orientation
  (VERTICAL VERTICAL)
  (HORIZONTAL HORIZONTAL))

(defenum VAlign
  (TOP VALIGN_TOP)
  (CENTER VALIGN_CENTER)
  (BOTTOM VALIGN_BOTTOM))

(defenum Key
  (ESCAPE KEY_ESCAPE)
  (TAB KEY_TAB)
  (BACKTAB KEY_BACKTAB)
  (BACKSPACE KEY_BACKSPACE)
  (ENTER KEY_ENTER)
  (KP_ENTER KEY_KP_ENTER)
  (INSERT KEY_INSERT)
  (DELETE KEY_DELETE)
  (PAUSE KEY_PAUSE)
  (PRINT KEY_PRINT)
  (SYSREQ KEY_SYSREQ)
  (CLEAR KEY_CLEAR)
  (HOME KEY_HOME)
  (END KEY_END)
  (LEFT KEY_LEFT)
  (UP KEY_UP)
  (RIGHT KEY_RIGHT)
  (DOWN KEY_DOWN)
  (PAGEUP KEY_PAGEUP)
  (PAGEDOWN KEY_PAGEDOWN)
  (SHIFT KEY_SHIFT)
  (CONTROL KEY_CONTROL)
  (META KEY_META)
  (ALT KEY_ALT)
  (CAPSLOCK KEY_CAPSLOCK)
  (NUMLOCK KEY_NUMLOCK)
  (SCROLLLOCK KEY_SCROLLLOCK)
  (F1 KEY_F1)
  (F2 KEY_F2)
  (F3 KEY_F3)
  (F4 KEY_F4)
  (F5 KEY_F5)
  (F6 KEY_F6)
  (F7 KEY_F7)
  (F8 KEY_F8)
  (F9 KEY_F9)
  (F10 KEY_F10)
  (F11 KEY_F11)
  (F12 KEY_F12)
  (F13 KEY_F13)
  (F14 KEY_F14)
  (F15 KEY_F15)
  (F16 KEY_F16)
  (KP_MULTIPLY KEY_KP_MULTIPLY)
  (KP_DIVIDE KEY_KP_DIVIDE)
  (KP_SUBTRACT KEY_KP_SUBTRACT)
  (KP_PERIOD KEY_KP_PERIOD)
  (KP_ADD KEY_KP_ADD)
  (KP_0 KEY_KP_0)
  (KP_1 KEY_KP_1)
  (KP_2 KEY_KP_2)
  (KP_3 KEY_KP_3)
  (KP_4 KEY_KP_4)
  (KP_5 KEY_KP_5)
  (KP_6 KEY_KP_6)
  (KP_7 KEY_KP_7)
  (KP_8 KEY_KP_8)
  (KP_9 KEY_KP_9)
  (SUPER_L KEY_SUPER_L)
  (SUPER_R KEY_SUPER_R)
  (MENU KEY_MENU)
  (HYPER_L KEY_HYPER_L)
  (HYPER_R KEY_HYPER_R)
  (HELP KEY_HELP)
  (DIRECTION_L KEY_DIRECTION_L)
  (DIRECTION_R KEY_DIRECTION_R)
  (BACK KEY_BACK)
  (FORWARD KEY_FORWARD)
  (STOP KEY_STOP)
  (REFRESH KEY_REFRESH)
  (VOLUMEDOWN KEY_VOLUMEDOWN)
  (VOLUMEMUTE KEY_VOLUMEMUTE)
  (VOLUMEUP KEY_VOLUMEUP)
  (BASSBOOST KEY_BASSBOOST)
  (BASSUP KEY_BASSUP)
  (BASSDOWN KEY_BASSDOWN)
  (TREBLEUP KEY_TREBLEUP)
  (TREBLEDOWN KEY_TREBLEDOWN)
  (MEDIAPLAY KEY_MEDIAPLAY)
  (MEDIASTOP KEY_MEDIASTOP)
  (MEDIAPREVIOUS KEY_MEDIAPREVIOUS)
  (MEDIANEXT KEY_MEDIANEXT)
  (MEDIARECORD KEY_MEDIARECORD)
  (HOMEPAGE KEY_HOMEPAGE)
  (FAVORITES KEY_FAVORITES)
  (SEARCH KEY_SEARCH)
  (STANDBY KEY_STANDBY)
  (OPENURL KEY_OPENURL)
  (LAUNCHMAIL KEY_LAUNCHMAIL)
  (LAUNCHMEDIA KEY_LAUNCHMEDIA)
  (LAUNCH0 KEY_LAUNCH0)
  (LAUNCH1 KEY_LAUNCH1)
  (LAUNCH2 KEY_LAUNCH2)
  (LAUNCH3 KEY_LAUNCH3)
  (LAUNCH4 KEY_LAUNCH4)
  (LAUNCH5 KEY_LAUNCH5)
  (LAUNCH6 KEY_LAUNCH6)
  (LAUNCH7 KEY_LAUNCH7)
  (LAUNCH8 KEY_LAUNCH8)
  (LAUNCH9 KEY_LAUNCH9)
  (LAUNCHA KEY_LAUNCHA)
  (LAUNCHB KEY_LAUNCHB)
  (LAUNCHC KEY_LAUNCHC)
  (LAUNCHD KEY_LAUNCHD)
  (LAUNCHE KEY_LAUNCHE)
  (LAUNCHF KEY_LAUNCHF)
  (UNKNOWN KEY_UNKNOWN)
  (SPACE KEY_SPACE)
  (EXCLAM KEY_EXCLAM)
  (QUOTEDBL KEY_QUOTEDBL)
  (NUMBERSIGN KEY_NUMBERSIGN)
  (DOLLAR KEY_DOLLAR)
  (PERCENT KEY_PERCENT)
  (AMPERSAND KEY_AMPERSAND)
  (APOSTROPHE KEY_APOSTROPHE)
  (PARENLEFT KEY_PARENLEFT)
  (PARENRIGHT KEY_PARENRIGHT)
  (ASTERISK KEY_ASTERISK)
  (PLUS KEY_PLUS)
  (COMMA KEY_COMMA)
  (MINUS KEY_MINUS)
  (PERIOD KEY_PERIOD)
  (SLASH KEY_SLASH)
  (K0 KEY_0)
  (K1 KEY_1)
  (K2 KEY_2)
  (K3 KEY_3)
  (K4 KEY_4)
  (K5 KEY_5)
  (K6 KEY_6)
  (K7 KEY_7)
  (K8 KEY_8)
  (K9 KEY_9)
  (COLON KEY_COLON)
  (SEMICOLON KEY_SEMICOLON)
  (LESS KEY_LESS)
  (EQUAL KEY_EQUAL)
  (GREATER KEY_GREATER)
  (QUESTION KEY_QUESTION)
  (AT KEY_AT)
  (A KEY_A)
  (B KEY_B)
  (C KEY_C)
  (D KEY_D)
  (E KEY_E)
  (F KEY_F)
  (G KEY_G)
  (H KEY_H)
  (I KEY_I)
  (J KEY_J)
  (K KEY_K)
  (L KEY_L)
  (M KEY_M)
  (N KEY_N)
  (O KEY_O)
  (P KEY_P)
  (Q KEY_Q)
  (R KEY_R)
  (S KEY_S)
  (T KEY_T)
  (U KEY_U)
  (V KEY_V)
  (W KEY_W)
  (X KEY_X)
  (Y KEY_Y)
  (Z KEY_Z)
  (BRACKETLEFT KEY_BRACKETLEFT)
  (BACKSLASH KEY_BACKSLASH)
  (BRACKETRIGHT KEY_BRACKETRIGHT)
  (ASCIICIRCUM KEY_ASCIICIRCUM)
  (UNDERSCORE KEY_UNDERSCORE)
  (QUOTELEFT KEY_QUOTELEFT)
  (BRACELEFT KEY_BRACELEFT)
  (BAR KEY_BAR)
  (BRACERIGHT KEY_BRACERIGHT)
  (ASCIITILDE KEY_ASCIITILDE)
  (NOBREAKSPACE KEY_NOBREAKSPACE)
  (EXCLAMDOWN KEY_EXCLAMDOWN)
  (CENT KEY_CENT)
  (STERLING KEY_STERLING)
  (CURRENCY KEY_CURRENCY)
  (YEN KEY_YEN)
  (BROKENBAR KEY_BROKENBAR)
  (SECTION KEY_SECTION)
  (DIAERESIS KEY_DIAERESIS)
  (COPYRIGHT KEY_COPYRIGHT)
  (ORDFEMININE KEY_ORDFEMININE)
  (GUILLEMOTLEFT KEY_GUILLEMOTLEFT)
  (NOTSIGN KEY_NOTSIGN)
  (HYPHEN KEY_HYPHEN)
  (REGISTERED KEY_REGISTERED)
  (MACRON KEY_MACRON)
  (DEGREE KEY_DEGREE)
  (PLUSMINUS KEY_PLUSMINUS)
  (TWOSUPERIOR KEY_TWOSUPERIOR)
  (THREESUPERIOR KEY_THREESUPERIOR)
  (ACUTE KEY_ACUTE)
  (MU KEY_MU)
  (PARAGRAPH KEY_PARAGRAPH)
  (PERIODCENTERED KEY_PERIODCENTERED)
  (CEDILLA KEY_CEDILLA)
  (ONESUPERIOR KEY_ONESUPERIOR)
  (MASCULINE KEY_MASCULINE)
  (GUILLEMOTRIGHT KEY_GUILLEMOTRIGHT)
  (ONEQUARTER KEY_ONEQUARTER)
  (ONEHALF KEY_ONEHALF)
  (THREEQUARTERS KEY_THREEQUARTERS)
  (QUESTIONDOWN KEY_QUESTIONDOWN)
  (AGRAVE KEY_AGRAVE)
  (AACUTE KEY_AACUTE)
  (ACIRCUMFLEX KEY_ACIRCUMFLEX)
  (ATILDE KEY_ATILDE)
  (ADIAERESIS KEY_ADIAERESIS)
  (ARING KEY_ARING)
  (AE KEY_AE)
  (CCEDILLA KEY_CCEDILLA)
  (EGRAVE KEY_EGRAVE)
  (EACUTE KEY_EACUTE)
  (ECIRCUMFLEX KEY_ECIRCUMFLEX)
  (EDIAERESIS KEY_EDIAERESIS)
  (IGRAVE KEY_IGRAVE)
  (IACUTE KEY_IACUTE)
  (ICIRCUMFLEX KEY_ICIRCUMFLEX)
  (IDIAERESIS KEY_IDIAERESIS)
  (ETH KEY_ETH)
  (NTILDE KEY_NTILDE)
  (OGRAVE KEY_OGRAVE)
  (OACUTE KEY_OACUTE)
  (OCIRCUMFLEX KEY_OCIRCUMFLEX)
  (OTILDE KEY_OTILDE)
  (ODIAERESIS KEY_ODIAERESIS)
  (MULTIPLY KEY_MULTIPLY)
  (OOBLIQUE KEY_OOBLIQUE)
  (UGRAVE KEY_UGRAVE)
  (UACUTE KEY_UACUTE)
  (UCIRCUMFLEX KEY_UCIRCUMFLEX)
  (UDIAERESIS KEY_UDIAERESIS)
  (YACUTE KEY_YACUTE)
  (THORN KEY_THORN)
  (SSHARP KEY_SSHARP)
  (DIVISION KEY_DIVISION)
  (YDIAERESIS KEY_YDIAERESIS))

(defenum HAlign
  (LEFT HALIGN_LEFT)
  (CENTER HALIGN_CENTER)
  (RIGHT HALIGN_RIGHT))

(defenum MIDIMessage
  (NOTE_OFF MIDI_MESSAGE_NOTE_OFF)
  (NOTE_ON MIDI_MESSAGE_NOTE_ON)
  (AFTERTOUCH MIDI_MESSAGE_AFTERTOUCH)
  (CONTROL_CHANGE MIDI_MESSAGE_CONTROL_CHANGE)
  (PROGRAM_CHANGE MIDI_MESSAGE_PROGRAM_CHANGE)
  (CHANNEL_PRESSURE MIDI_MESSAGE_CHANNEL_PRESSURE)
  (PITCH_BEND MIDI_MESSAGE_PITCH_BEND))

(defclass _GDLisp (Node) main
  (defvar global_name_generator)

  (defn _init ()
    (set self:global_name_generator (FreshNameGenerator:new [] 0))))

(defn cons (a b)
  (Cons:new a b))

(defn car (a)
  a:car)

(defn cdr (a)
  a:cdr)

(defn set-car (b a)
  (set a:car b))

(defn set-cdr (b a)
  (set a:cdr b))

(defn intern (a)
  (Symbol:new a))

(defn length (x)
  (let ((result 0))
    (while (sys/instance-direct? x Cons)
      (set result (+ result 1))
      (set x x:cdr))
    result))

(defn funcall (f &rest args)
  (cond
    ((sys/instance-direct? f Function) (f:call_funcv args))
    (#t (push-error "Attempt to call non-function"))))

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

(defn instance? (value type)
  (cond
    ((= (typeof type) Int) (= (typeof value) type))
    (#t (sys/instance-direct? value type))))

(defn sys/instance-direct? (value type)
  (sys/call-magic DIRECT-INSTANCE-CHECK)
  (sys/instance-direct? value type))

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
  (let ((t (typeof a)))
    (cond
      ((<= Array t PoolColorArray) (array->list a))
      (#t a))))

(defn sys/qq-smart-array (a)
  (let ((t (typeof a)))
    (cond
      ((<= Array t PoolColorArray) a)
      (#t (list->array a)))))

;; GDScript built-ins that we use unmodified

(sys/declare superfunction str (a) public)
(sys/declare superfunction int (a) public)
(sys/declare superfunction bool (a) public)
(sys/declare superfunction randomize () public)
(sys/declare superfunction randi () public) ; TODO Should we wrap this and other random functions and make a nice interface to them?
(sys/declare superfunction randf () public)
(sys/declare superfunction rand-range (a b) public)
(sys/declare superfunction clamp (a b c) public)
(sys/declare superfunction abs (a) public)
(sys/declare superfunction len (a) public) ; TODO Eventually, we'll want this to be a multimethod which works on lists as well as arrays. (And possibly elt as well?)
(sys/declare superfunction get-global-mouse-position () public) ; TODO Definitely want to wrap this (and all of the mouse functions) in a nice namespace or module or something
(sys/declare superfunction push-error (a) public)
(sys/declare superfunction push-warning (a) public)
(sys/declare superfunction typeof (a) public)

(sys/declare superglobal PI public)
(sys/declare superglobal SPKEY public)

;; TYPE_* Constants

;; TODO Remove these rename stubs somehow (I don't want the original names in-scope)

(sys/declare superglobal TYPE_NIL)
(defconst Null TYPE_NIL)
(sys/declare superglobal TYPE_BOOL)
(defconst Bool TYPE_BOOL)
(sys/declare superglobal TYPE_INT)
(defconst Int TYPE_INT)
(sys/declare superglobal TYPE_REAL)
(defconst Float TYPE_REAL)
(sys/declare superglobal TYPE_STRING)
(defconst String TYPE_STRING)
(sys/declare superglobal TYPE_VECTOR2)
(defconst Vector2 TYPE_VECTOR2)
(sys/declare superglobal TYPE_RECT2)
(defconst Rect2 TYPE_RECT2)
(sys/declare superglobal TYPE_VECTOR3)
(defconst Vector3 TYPE_VECTOR3)
(sys/declare superglobal TYPE_TRANSFORM2D)
(defconst Transform2D TYPE_TRANSFORM2D)
(sys/declare superglobal TYPE_PLANE)
(defconst Plane TYPE_PLANE)
(sys/declare superglobal TYPE_QUAT)
(defconst Quat TYPE_QUAT)
(sys/declare superglobal TYPE_AABB)
(defconst AABB TYPE_AABB)
(sys/declare superglobal TYPE_BASIS)
(defconst Basis TYPE_BASIS)
(sys/declare superglobal TYPE_TRANSFORM)
(defconst Transform TYPE_TRANSFORM)
(sys/declare superglobal TYPE_COLOR)
(defconst Color TYPE_COLOR)
(sys/declare superglobal TYPE_NODE_PATH)
(defconst NodePath TYPE_NODE_PATH)
(sys/declare superglobal TYPE_RID)
(defconst RID TYPE_RID)
(sys/declare superglobal TYPE_OBJECT)
(defconst Object TYPE_OBJECT)
(sys/declare superglobal TYPE_DICTIONARY)
(defconst Dictionary TYPE_DICTIONARY)
(sys/declare superglobal TYPE_ARRAY)
(defconst Array TYPE_ARRAY)
(sys/declare superglobal TYPE_RAW_ARRAY)
(defconst PoolByteArray TYPE_RAW_ARRAY)
(sys/declare superglobal TYPE_INT_ARRAY)
(defconst PoolIntArray TYPE_INT_ARRAY)
(sys/declare superglobal TYPE_REAL_ARRAY)
(defconst PoolRealArray TYPE_REAL_ARRAY)
(sys/declare superglobal TYPE_STRING_ARRAY)
(defconst PoolStringArray TYPE_STRING_ARRAY)
(sys/declare superglobal TYPE_VECTOR2_ARRAY)
(defconst PoolVector2Array TYPE_VECTOR2_ARRAY)
(sys/declare superglobal TYPE_VECTOR3_ARRAY)
(defconst PoolVector3Array TYPE_VECTOR3_ARRAY)
(sys/declare superglobal TYPE_COLOR_ARRAY)
(defconst PoolColorArray TYPE_COLOR_ARRAY)
(sys/declare superglobal TYPE_MAX public) ; (Note: We actually do want to expose this TYPE_* constant)

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
