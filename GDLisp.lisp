
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

(sys/declare superglobal (BUTTON_LEFT BUTTON_LEFT) public)
(sys/declare superglobal (BUTTON_RIGHT BUTTON_RIGHT) public)
(sys/declare superglobal (BUTTON_MIDDLE BUTTON_MIDDLE) public)
(sys/declare superglobal (BUTTON_XBUTTON1 BUTTON_XBUTTON1) public)
(sys/declare superglobal (BUTTON_XBUTTON2 BUTTON_XBUTTON2) public)
(sys/declare superglobal (BUTTON_WHEEL_UP BUTTON_WHEEL_UP) public)
(sys/declare superglobal (BUTTON_WHEEL_DOWN BUTTON_WHEEL_DOWN) public)
(sys/declare superglobal (BUTTON_WHEEL_LEFT BUTTON_WHEEL_LEFT) public)
(sys/declare superglobal (BUTTON_WHEEL_RIGHT BUTTON_WHEEL_RIGHT) public)
(sys/declare superglobal (BUTTON_MASK_LEFT BUTTON_MASK_LEFT) public)
(sys/declare superglobal (BUTTON_MASK_RIGHT BUTTON_MASK_RIGHT) public)
(sys/declare superglobal (BUTTON_MASK_MIDDLE BUTTON_MASK_MIDDLE) public)
(sys/declare superglobal (BUTTON_MASK_XBUTTON1 BUTTON_MASK_XBUTTON1) public)
(sys/declare superglobal (BUTTON_MASK_XBUTTON2 BUTTON_MASK_XBUTTON2) public)
(sys/declare superglobal (CORNER_TOP_LEFT CORNER_TOP_LEFT) public)
(sys/declare superglobal (CORNER_TOP_RIGHT CORNER_TOP_RIGHT) public)
(sys/declare superglobal (CORNER_BOTTOM_RIGHT CORNER_BOTTOM_RIGHT) public)
(sys/declare superglobal (CORNER_BOTTOM_LEFT CORNER_BOTTOM_LEFT) public)
(sys/declare superglobal (JOY_BUTTON_0 JOY_BUTTON_0) public)
(sys/declare superglobal (JOY_BUTTON_1 JOY_BUTTON_1) public)
(sys/declare superglobal (JOY_BUTTON_2 JOY_BUTTON_2) public)
(sys/declare superglobal (JOY_BUTTON_3 JOY_BUTTON_3) public)
(sys/declare superglobal (JOY_BUTTON_4 JOY_BUTTON_4) public)
(sys/declare superglobal (JOY_BUTTON_5 JOY_BUTTON_5) public)
(sys/declare superglobal (JOY_BUTTON_6 JOY_BUTTON_6) public)
(sys/declare superglobal (JOY_BUTTON_7 JOY_BUTTON_7) public)
(sys/declare superglobal (JOY_BUTTON_8 JOY_BUTTON_8) public)
(sys/declare superglobal (JOY_BUTTON_9 JOY_BUTTON_9) public)
(sys/declare superglobal (JOY_BUTTON_10 JOY_BUTTON_10) public)
(sys/declare superglobal (JOY_BUTTON_11 JOY_BUTTON_11) public)
(sys/declare superglobal (JOY_BUTTON_12 JOY_BUTTON_12) public)
(sys/declare superglobal (JOY_BUTTON_13 JOY_BUTTON_13) public)
(sys/declare superglobal (JOY_BUTTON_14 JOY_BUTTON_14) public)
(sys/declare superglobal (JOY_BUTTON_15 JOY_BUTTON_15) public)
(sys/declare superglobal (JOY_BUTTON_MAX JOY_BUTTON_MAX) public)
(sys/declare superglobal (JOY_SONY_CIRCLE JOY_SONY_CIRCLE) public)
(sys/declare superglobal (JOY_SONY_X JOY_SONY_X) public)
(sys/declare superglobal (JOY_SONY_SQUARE JOY_SONY_SQUARE) public)
(sys/declare superglobal (JOY_SONY_TRIANGLE JOY_SONY_TRIANGLE) public)
(sys/declare superglobal (JOY_XBOX_B JOY_XBOX_B) public)
(sys/declare superglobal (JOY_XBOX_A JOY_XBOX_A) public)
(sys/declare superglobal (JOY_XBOX_X JOY_XBOX_X) public)
(sys/declare superglobal (JOY_XBOX_Y JOY_XBOX_Y) public)
(sys/declare superglobal (JOY_DS_A JOY_DS_A) public)
(sys/declare superglobal (JOY_DS_B JOY_DS_B) public)
(sys/declare superglobal (JOY_DS_X JOY_DS_X) public)
(sys/declare superglobal (JOY_DS_Y JOY_DS_Y) public)
(sys/declare superglobal (JOY_VR_GRIP JOY_VR_GRIP) public)
(sys/declare superglobal (JOY_VR_PAD JOY_VR_PAD) public)
(sys/declare superglobal (JOY_VR_TRIGGER JOY_VR_TRIGGER) public)
(sys/declare superglobal (JOY_OCULUS_AX JOY_OCULUS_AX) public)
(sys/declare superglobal (JOY_OCULUS_BY JOY_OCULUS_BY) public)
(sys/declare superglobal (JOY_OCULUS_MENU JOY_OCULUS_MENU) public)
(sys/declare superglobal (JOY_OPENVR_MENU JOY_OPENVR_MENU) public)
(sys/declare superglobal (JOY_SELECT JOY_SELECT) public)
(sys/declare superglobal (JOY_START JOY_START) public)
(sys/declare superglobal (JOY_DPAD_UP JOY_DPAD_UP) public)
(sys/declare superglobal (JOY_DPAD_DOWN JOY_DPAD_DOWN) public)
(sys/declare superglobal (JOY_DPAD_LEFT JOY_DPAD_LEFT) public)
(sys/declare superglobal (JOY_DPAD_RIGHT JOY_DPAD_RIGHT) public)
(sys/declare superglobal (JOY_L JOY_L) public)
(sys/declare superglobal (JOY_L2 JOY_L2) public)
(sys/declare superglobal (JOY_L3 JOY_L3) public)
(sys/declare superglobal (JOY_R JOY_R) public)
(sys/declare superglobal (JOY_R2 JOY_R2) public)
(sys/declare superglobal (JOY_R3 JOY_R3) public)
(sys/declare superglobal (JOY_AXIS_0 JOY_AXIS_0) public)
(sys/declare superglobal (JOY_AXIS_1 JOY_AXIS_1) public)
(sys/declare superglobal (JOY_AXIS_2 JOY_AXIS_2) public)
(sys/declare superglobal (JOY_AXIS_3 JOY_AXIS_3) public)
(sys/declare superglobal (JOY_AXIS_4 JOY_AXIS_4) public)
(sys/declare superglobal (JOY_AXIS_5 JOY_AXIS_5) public)
(sys/declare superglobal (JOY_AXIS_6 JOY_AXIS_6) public)
(sys/declare superglobal (JOY_AXIS_7 JOY_AXIS_7) public)
(sys/declare superglobal (JOY_AXIS_8 JOY_AXIS_8) public)
(sys/declare superglobal (JOY_AXIS_9 JOY_AXIS_9) public)
(sys/declare superglobal (JOY_AXIS_MAX JOY_AXIS_MAX) public)
(sys/declare superglobal (JOY_ANALOG_LX JOY_ANALOG_LX) public)
(sys/declare superglobal (JOY_ANALOG_LY JOY_ANALOG_LY) public)
(sys/declare superglobal (JOY_ANALOG_RX JOY_ANALOG_RX) public)
(sys/declare superglobal (JOY_ANALOG_RY JOY_ANALOG_RY) public)
(sys/declare superglobal (JOY_ANALOG_L2 JOY_ANALOG_L2) public)
(sys/declare superglobal (JOY_ANALOG_R2 JOY_ANALOG_R2) public)
(sys/declare superglobal (JOY_VR_ANALOG_TRIGGER JOY_VR_ANALOG_TRIGGER) public)
(sys/declare superglobal (JOY_VR_ANALOG_GRIP JOY_VR_ANALOG_GRIP) public)
(sys/declare superglobal (JOY_OPENVR_TOUCHPADX JOY_OPENVR_TOUCHPADX) public)
(sys/declare superglobal (JOY_OPENVR_TOUCHPADY JOY_OPENVR_TOUCHPADY) public)
(sys/declare superglobal (MARGIN_LEFT MARGIN_LEFT) public)
(sys/declare superglobal (MARGIN_TOP MARGIN_TOP) public)
(sys/declare superglobal (MARGIN_RIGHT MARGIN_RIGHT) public)
(sys/declare superglobal (MARGIN_BOTTOM MARGIN_BOTTOM) public)
(sys/declare superglobal (KEY_CODE_MASK KEY_CODE_MASK) public)
(sys/declare superglobal (KEY_MODIFIER_MASK KEY_MODIFIER_MASK) public)
(sys/declare superglobal (KEY_MASK_SHIFT KEY_MASK_SHIFT) public)
(sys/declare superglobal (KEY_MASK_ALT KEY_MASK_ALT) public)
(sys/declare superglobal (KEY_MASK_META KEY_MASK_META) public)
(sys/declare superglobal (KEY_MASK_CTRL KEY_MASK_CTRL) public)
(sys/declare superglobal (KEY_MASK_CMD KEY_MASK_CMD) public)
(sys/declare superglobal (KEY_MASK_KPAD KEY_MASK_KPAD) public)
(sys/declare superglobal (KEY_MASK_GROUP_SWITCH KEY_MASK_GROUP_SWITCH) public)
(sys/declare superglobal (VERTICAL VERTICAL) public)
(sys/declare superglobal (HORIZONTAL HORIZONTAL) public)
(sys/declare superglobal (VALIGN_TOP VALIGN_TOP) public)
(sys/declare superglobal (VALIGN_CENTER VALIGN_CENTER) public)
(sys/declare superglobal (VALIGN_BOTTOM VALIGN_BOTTOM) public)
(sys/declare superglobal (KEY_ESCAPE KEY_ESCAPE) public)
(sys/declare superglobal (KEY_TAB KEY_TAB) public)
(sys/declare superglobal (KEY_BACKTAB KEY_BACKTAB) public)
(sys/declare superglobal (KEY_BACKSPACE KEY_BACKSPACE) public)
(sys/declare superglobal (KEY_ENTER KEY_ENTER) public)
(sys/declare superglobal (KEY_KP_ENTER KEY_KP_ENTER) public)
(sys/declare superglobal (KEY_INSERT KEY_INSERT) public)
(sys/declare superglobal (KEY_DELETE KEY_DELETE) public)
(sys/declare superglobal (KEY_PAUSE KEY_PAUSE) public)
(sys/declare superglobal (KEY_PRINT KEY_PRINT) public)
(sys/declare superglobal (KEY_SYSREQ KEY_SYSREQ) public)
(sys/declare superglobal (KEY_CLEAR KEY_CLEAR) public)
(sys/declare superglobal (KEY_HOME KEY_HOME) public)
(sys/declare superglobal (KEY_END KEY_END) public)
(sys/declare superglobal (KEY_LEFT KEY_LEFT) public)
(sys/declare superglobal (KEY_UP KEY_UP) public)
(sys/declare superglobal (KEY_RIGHT KEY_RIGHT) public)
(sys/declare superglobal (KEY_DOWN KEY_DOWN) public)
(sys/declare superglobal (KEY_PAGEUP KEY_PAGEUP) public)
(sys/declare superglobal (KEY_PAGEDOWN KEY_PAGEDOWN) public)
(sys/declare superglobal (KEY_SHIFT KEY_SHIFT) public)
(sys/declare superglobal (KEY_CONTROL KEY_CONTROL) public)
(sys/declare superglobal (KEY_META KEY_META) public)
(sys/declare superglobal (KEY_ALT KEY_ALT) public)
(sys/declare superglobal (KEY_CAPSLOCK KEY_CAPSLOCK) public)
(sys/declare superglobal (KEY_NUMLOCK KEY_NUMLOCK) public)
(sys/declare superglobal (KEY_SCROLLLOCK KEY_SCROLLLOCK) public)
(sys/declare superglobal (KEY_F1 KEY_F1) public)
(sys/declare superglobal (KEY_F2 KEY_F2) public)
(sys/declare superglobal (KEY_F3 KEY_F3) public)
(sys/declare superglobal (KEY_F4 KEY_F4) public)
(sys/declare superglobal (KEY_F5 KEY_F5) public)
(sys/declare superglobal (KEY_F6 KEY_F6) public)
(sys/declare superglobal (KEY_F7 KEY_F7) public)
(sys/declare superglobal (KEY_F8 KEY_F8) public)
(sys/declare superglobal (KEY_F9 KEY_F9) public)
(sys/declare superglobal (KEY_F10 KEY_F10) public)
(sys/declare superglobal (KEY_F11 KEY_F11) public)
(sys/declare superglobal (KEY_F12 KEY_F12) public)
(sys/declare superglobal (KEY_F13 KEY_F13) public)
(sys/declare superglobal (KEY_F14 KEY_F14) public)
(sys/declare superglobal (KEY_F15 KEY_F15) public)
(sys/declare superglobal (KEY_F16 KEY_F16) public)
(sys/declare superglobal (KEY_KP_MULTIPLY KEY_KP_MULTIPLY) public)
(sys/declare superglobal (KEY_KP_DIVIDE KEY_KP_DIVIDE) public)
(sys/declare superglobal (KEY_KP_SUBTRACT KEY_KP_SUBTRACT) public)
(sys/declare superglobal (KEY_KP_PERIOD KEY_KP_PERIOD) public)
(sys/declare superglobal (KEY_KP_ADD KEY_KP_ADD) public)
(sys/declare superglobal (KEY_KP_0 KEY_KP_0) public)
(sys/declare superglobal (KEY_KP_1 KEY_KP_1) public)
(sys/declare superglobal (KEY_KP_2 KEY_KP_2) public)
(sys/declare superglobal (KEY_KP_3 KEY_KP_3) public)
(sys/declare superglobal (KEY_KP_4 KEY_KP_4) public)
(sys/declare superglobal (KEY_KP_5 KEY_KP_5) public)
(sys/declare superglobal (KEY_KP_6 KEY_KP_6) public)
(sys/declare superglobal (KEY_KP_7 KEY_KP_7) public)
(sys/declare superglobal (KEY_KP_8 KEY_KP_8) public)
(sys/declare superglobal (KEY_KP_9 KEY_KP_9) public)
(sys/declare superglobal (KEY_SUPER_L KEY_SUPER_L) public)
(sys/declare superglobal (KEY_SUPER_R KEY_SUPER_R) public)
(sys/declare superglobal (KEY_MENU KEY_MENU) public)
(sys/declare superglobal (KEY_HYPER_L KEY_HYPER_L) public)
(sys/declare superglobal (KEY_HYPER_R KEY_HYPER_R) public)
(sys/declare superglobal (KEY_HELP KEY_HELP) public)
(sys/declare superglobal (KEY_DIRECTION_L KEY_DIRECTION_L) public)
(sys/declare superglobal (KEY_DIRECTION_R KEY_DIRECTION_R) public)
(sys/declare superglobal (KEY_BACK KEY_BACK) public)
(sys/declare superglobal (KEY_FORWARD KEY_FORWARD) public)
(sys/declare superglobal (KEY_STOP KEY_STOP) public)
(sys/declare superglobal (KEY_REFRESH KEY_REFRESH) public)
(sys/declare superglobal (KEY_VOLUMEDOWN KEY_VOLUMEDOWN) public)
(sys/declare superglobal (KEY_VOLUMEMUTE KEY_VOLUMEMUTE) public)
(sys/declare superglobal (KEY_VOLUMEUP KEY_VOLUMEUP) public)
(sys/declare superglobal (KEY_BASSBOOST KEY_BASSBOOST) public)
(sys/declare superglobal (KEY_BASSUP KEY_BASSUP) public)
(sys/declare superglobal (KEY_BASSDOWN KEY_BASSDOWN) public)
(sys/declare superglobal (KEY_TREBLEUP KEY_TREBLEUP) public)
(sys/declare superglobal (KEY_TREBLEDOWN KEY_TREBLEDOWN) public)
(sys/declare superglobal (KEY_MEDIAPLAY KEY_MEDIAPLAY) public)
(sys/declare superglobal (KEY_MEDIASTOP KEY_MEDIASTOP) public)
(sys/declare superglobal (KEY_MEDIAPREVIOUS KEY_MEDIAPREVIOUS) public)
(sys/declare superglobal (KEY_MEDIANEXT KEY_MEDIANEXT) public)
(sys/declare superglobal (KEY_MEDIARECORD KEY_MEDIARECORD) public)
(sys/declare superglobal (KEY_HOMEPAGE KEY_HOMEPAGE) public)
(sys/declare superglobal (KEY_FAVORITES KEY_FAVORITES) public)
(sys/declare superglobal (KEY_SEARCH KEY_SEARCH) public)
(sys/declare superglobal (KEY_STANDBY KEY_STANDBY) public)
(sys/declare superglobal (KEY_OPENURL KEY_OPENURL) public)
(sys/declare superglobal (KEY_LAUNCHMAIL KEY_LAUNCHMAIL) public)
(sys/declare superglobal (KEY_LAUNCHMEDIA KEY_LAUNCHMEDIA) public)
(sys/declare superglobal (KEY_LAUNCH0 KEY_LAUNCH0) public)
(sys/declare superglobal (KEY_LAUNCH1 KEY_LAUNCH1) public)
(sys/declare superglobal (KEY_LAUNCH2 KEY_LAUNCH2) public)
(sys/declare superglobal (KEY_LAUNCH3 KEY_LAUNCH3) public)
(sys/declare superglobal (KEY_LAUNCH4 KEY_LAUNCH4) public)
(sys/declare superglobal (KEY_LAUNCH5 KEY_LAUNCH5) public)
(sys/declare superglobal (KEY_LAUNCH6 KEY_LAUNCH6) public)
(sys/declare superglobal (KEY_LAUNCH7 KEY_LAUNCH7) public)
(sys/declare superglobal (KEY_LAUNCH8 KEY_LAUNCH8) public)
(sys/declare superglobal (KEY_LAUNCH9 KEY_LAUNCH9) public)
(sys/declare superglobal (KEY_LAUNCHA KEY_LAUNCHA) public)
(sys/declare superglobal (KEY_LAUNCHB KEY_LAUNCHB) public)
(sys/declare superglobal (KEY_LAUNCHC KEY_LAUNCHC) public)
(sys/declare superglobal (KEY_LAUNCHD KEY_LAUNCHD) public)
(sys/declare superglobal (KEY_LAUNCHE KEY_LAUNCHE) public)
(sys/declare superglobal (KEY_LAUNCHF KEY_LAUNCHF) public)
(sys/declare superglobal (KEY_UNKNOWN KEY_UNKNOWN) public)
(sys/declare superglobal (KEY_SPACE KEY_SPACE) public)
(sys/declare superglobal (KEY_EXCLAM KEY_EXCLAM) public)
(sys/declare superglobal (KEY_QUOTEDBL KEY_QUOTEDBL) public)
(sys/declare superglobal (KEY_NUMBERSIGN KEY_NUMBERSIGN) public)
(sys/declare superglobal (KEY_DOLLAR KEY_DOLLAR) public)
(sys/declare superglobal (KEY_PERCENT KEY_PERCENT) public)
(sys/declare superglobal (KEY_AMPERSAND KEY_AMPERSAND) public)
(sys/declare superglobal (KEY_APOSTROPHE KEY_APOSTROPHE) public)
(sys/declare superglobal (KEY_PARENLEFT KEY_PARENLEFT) public)
(sys/declare superglobal (KEY_PARENRIGHT KEY_PARENRIGHT) public)
(sys/declare superglobal (KEY_ASTERISK KEY_ASTERISK) public)
(sys/declare superglobal (KEY_PLUS KEY_PLUS) public)
(sys/declare superglobal (KEY_COMMA KEY_COMMA) public)
(sys/declare superglobal (KEY_MINUS KEY_MINUS) public)
(sys/declare superglobal (KEY_PERIOD KEY_PERIOD) public)
(sys/declare superglobal (KEY_SLASH KEY_SLASH) public)
(sys/declare superglobal (KEY_0 KEY_0) public)
(sys/declare superglobal (KEY_1 KEY_1) public)
(sys/declare superglobal (KEY_2 KEY_2) public)
(sys/declare superglobal (KEY_3 KEY_3) public)
(sys/declare superglobal (KEY_4 KEY_4) public)
(sys/declare superglobal (KEY_5 KEY_5) public)
(sys/declare superglobal (KEY_6 KEY_6) public)
(sys/declare superglobal (KEY_7 KEY_7) public)
(sys/declare superglobal (KEY_8 KEY_8) public)
(sys/declare superglobal (KEY_9 KEY_9) public)
(sys/declare superglobal (KEY_COLON KEY_COLON) public)
(sys/declare superglobal (KEY_SEMICOLON KEY_SEMICOLON) public)
(sys/declare superglobal (KEY_LESS KEY_LESS) public)
(sys/declare superglobal (KEY_EQUAL KEY_EQUAL) public)
(sys/declare superglobal (KEY_GREATER KEY_GREATER) public)
(sys/declare superglobal (KEY_QUESTION KEY_QUESTION) public)
(sys/declare superglobal (KEY_AT KEY_AT) public)
(sys/declare superglobal (KEY_A KEY_A) public)
(sys/declare superglobal (KEY_B KEY_B) public)
(sys/declare superglobal (KEY_C KEY_C) public)
(sys/declare superglobal (KEY_D KEY_D) public)
(sys/declare superglobal (KEY_E KEY_E) public)
(sys/declare superglobal (KEY_F KEY_F) public)
(sys/declare superglobal (KEY_G KEY_G) public)
(sys/declare superglobal (KEY_H KEY_H) public)
(sys/declare superglobal (KEY_I KEY_I) public)
(sys/declare superglobal (KEY_J KEY_J) public)
(sys/declare superglobal (KEY_K KEY_K) public)
(sys/declare superglobal (KEY_L KEY_L) public)
(sys/declare superglobal (KEY_M KEY_M) public)
(sys/declare superglobal (KEY_N KEY_N) public)
(sys/declare superglobal (KEY_O KEY_O) public)
(sys/declare superglobal (KEY_P KEY_P) public)
(sys/declare superglobal (KEY_Q KEY_Q) public)
(sys/declare superglobal (KEY_R KEY_R) public)
(sys/declare superglobal (KEY_S KEY_S) public)
(sys/declare superglobal (KEY_T KEY_T) public)
(sys/declare superglobal (KEY_U KEY_U) public)
(sys/declare superglobal (KEY_V KEY_V) public)
(sys/declare superglobal (KEY_W KEY_W) public)
(sys/declare superglobal (KEY_X KEY_X) public)
(sys/declare superglobal (KEY_Y KEY_Y) public)
(sys/declare superglobal (KEY_Z KEY_Z) public)
(sys/declare superglobal (KEY_BRACKETLEFT KEY_BRACKETLEFT) public)
(sys/declare superglobal (KEY_BACKSLASH KEY_BACKSLASH) public)
(sys/declare superglobal (KEY_BRACKETRIGHT KEY_BRACKETRIGHT) public)
(sys/declare superglobal (KEY_ASCIICIRCUM KEY_ASCIICIRCUM) public)
(sys/declare superglobal (KEY_UNDERSCORE KEY_UNDERSCORE) public)
(sys/declare superglobal (KEY_QUOTELEFT KEY_QUOTELEFT) public)
(sys/declare superglobal (KEY_BRACELEFT KEY_BRACELEFT) public)
(sys/declare superglobal (KEY_BAR KEY_BAR) public)
(sys/declare superglobal (KEY_BRACERIGHT KEY_BRACERIGHT) public)
(sys/declare superglobal (KEY_ASCIITILDE KEY_ASCIITILDE) public)
(sys/declare superglobal (KEY_NOBREAKSPACE KEY_NOBREAKSPACE) public)
(sys/declare superglobal (KEY_EXCLAMDOWN KEY_EXCLAMDOWN) public)
(sys/declare superglobal (KEY_CENT KEY_CENT) public)
(sys/declare superglobal (KEY_STERLING KEY_STERLING) public)
(sys/declare superglobal (KEY_CURRENCY KEY_CURRENCY) public)
(sys/declare superglobal (KEY_YEN KEY_YEN) public)
(sys/declare superglobal (KEY_BROKENBAR KEY_BROKENBAR) public)
(sys/declare superglobal (KEY_SECTION KEY_SECTION) public)
(sys/declare superglobal (KEY_DIAERESIS KEY_DIAERESIS) public)
(sys/declare superglobal (KEY_COPYRIGHT KEY_COPYRIGHT) public)
(sys/declare superglobal (KEY_ORDFEMININE KEY_ORDFEMININE) public)
(sys/declare superglobal (KEY_GUILLEMOTLEFT KEY_GUILLEMOTLEFT) public)
(sys/declare superglobal (KEY_NOTSIGN KEY_NOTSIGN) public)
(sys/declare superglobal (KEY_HYPHEN KEY_HYPHEN) public)
(sys/declare superglobal (KEY_REGISTERED KEY_REGISTERED) public)
(sys/declare superglobal (KEY_MACRON KEY_MACRON) public)
(sys/declare superglobal (KEY_DEGREE KEY_DEGREE) public)
(sys/declare superglobal (KEY_PLUSMINUS KEY_PLUSMINUS) public)
(sys/declare superglobal (KEY_TWOSUPERIOR KEY_TWOSUPERIOR) public)
(sys/declare superglobal (KEY_THREESUPERIOR KEY_THREESUPERIOR) public)
(sys/declare superglobal (KEY_ACUTE KEY_ACUTE) public)
(sys/declare superglobal (KEY_MU KEY_MU) public)
(sys/declare superglobal (KEY_PARAGRAPH KEY_PARAGRAPH) public)
(sys/declare superglobal (KEY_PERIODCENTERED KEY_PERIODCENTERED) public)
(sys/declare superglobal (KEY_CEDILLA KEY_CEDILLA) public)
(sys/declare superglobal (KEY_ONESUPERIOR KEY_ONESUPERIOR) public)
(sys/declare superglobal (KEY_MASCULINE KEY_MASCULINE) public)
(sys/declare superglobal (KEY_GUILLEMOTRIGHT KEY_GUILLEMOTRIGHT) public)
(sys/declare superglobal (KEY_ONEQUARTER KEY_ONEQUARTER) public)
(sys/declare superglobal (KEY_ONEHALF KEY_ONEHALF) public)
(sys/declare superglobal (KEY_THREEQUARTERS KEY_THREEQUARTERS) public)
(sys/declare superglobal (KEY_QUESTIONDOWN KEY_QUESTIONDOWN) public)
(sys/declare superglobal (KEY_AGRAVE KEY_AGRAVE) public)
(sys/declare superglobal (KEY_AACUTE KEY_AACUTE) public)
(sys/declare superglobal (KEY_ACIRCUMFLEX KEY_ACIRCUMFLEX) public)
(sys/declare superglobal (KEY_ATILDE KEY_ATILDE) public)
(sys/declare superglobal (KEY_ADIAERESIS KEY_ADIAERESIS) public)
(sys/declare superglobal (KEY_ARING KEY_ARING) public)
(sys/declare superglobal (KEY_AE KEY_AE) public)
(sys/declare superglobal (KEY_CCEDILLA KEY_CCEDILLA) public)
(sys/declare superglobal (KEY_EGRAVE KEY_EGRAVE) public)
(sys/declare superglobal (KEY_EACUTE KEY_EACUTE) public)
(sys/declare superglobal (KEY_ECIRCUMFLEX KEY_ECIRCUMFLEX) public)
(sys/declare superglobal (KEY_EDIAERESIS KEY_EDIAERESIS) public)
(sys/declare superglobal (KEY_IGRAVE KEY_IGRAVE) public)
(sys/declare superglobal (KEY_IACUTE KEY_IACUTE) public)
(sys/declare superglobal (KEY_ICIRCUMFLEX KEY_ICIRCUMFLEX) public)
(sys/declare superglobal (KEY_IDIAERESIS KEY_IDIAERESIS) public)
(sys/declare superglobal (KEY_ETH KEY_ETH) public)
(sys/declare superglobal (KEY_NTILDE KEY_NTILDE) public)
(sys/declare superglobal (KEY_OGRAVE KEY_OGRAVE) public)
(sys/declare superglobal (KEY_OACUTE KEY_OACUTE) public)
(sys/declare superglobal (KEY_OCIRCUMFLEX KEY_OCIRCUMFLEX) public)
(sys/declare superglobal (KEY_OTILDE KEY_OTILDE) public)
(sys/declare superglobal (KEY_ODIAERESIS KEY_ODIAERESIS) public)
(sys/declare superglobal (KEY_MULTIPLY KEY_MULTIPLY) public)
(sys/declare superglobal (KEY_OOBLIQUE KEY_OOBLIQUE) public)
(sys/declare superglobal (KEY_UGRAVE KEY_UGRAVE) public)
(sys/declare superglobal (KEY_UACUTE KEY_UACUTE) public)
(sys/declare superglobal (KEY_UCIRCUMFLEX KEY_UCIRCUMFLEX) public)
(sys/declare superglobal (KEY_UDIAERESIS KEY_UDIAERESIS) public)
(sys/declare superglobal (KEY_YACUTE KEY_YACUTE) public)
(sys/declare superglobal (KEY_THORN KEY_THORN) public)
(sys/declare superglobal (KEY_SSHARP KEY_SSHARP) public)
(sys/declare superglobal (KEY_DIVISION KEY_DIVISION) public)
(sys/declare superglobal (KEY_YDIAERESIS KEY_YDIAERESIS) public)
(sys/declare superglobal (HALIGN_LEFT HALIGN_LEFT) public)
(sys/declare superglobal (HALIGN_CENTER HALIGN_CENTER) public)
(sys/declare superglobal (HALIGN_RIGHT HALIGN_RIGHT) public)
(sys/declare superglobal (MIDI_MESSAGE_NOTE_OFF MIDI_MESSAGE_NOTE_OFF) public)
(sys/declare superglobal (MIDI_MESSAGE_NOTE_ON MIDI_MESSAGE_NOTE_ON) public)
(sys/declare superglobal (MIDI_MESSAGE_AFTERTOUCH MIDI_MESSAGE_AFTERTOUCH) public)
(sys/declare superglobal (MIDI_MESSAGE_CONTROL_CHANGE MIDI_MESSAGE_CONTROL_CHANGE) public)
(sys/declare superglobal (MIDI_MESSAGE_PROGRAM_CHANGE MIDI_MESSAGE_PROGRAM_CHANGE) public)
(sys/declare superglobal (MIDI_MESSAGE_CHANNEL_PRESSURE MIDI_MESSAGE_CHANNEL_PRESSURE) public)
(sys/declare superglobal (MIDI_MESSAGE_PITCH_BEND MIDI_MESSAGE_PITCH_BEND) public)

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
    ((= (instance? a GDLisp:Number) (instance? b GDLisp:Number))
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

(sys/declare superfunction (int int) (a) public gdscript-prim)
(sys/declare superfunction (bool bool) (a) public gdscript-prim)
(sys/declare superfunction (randomize randomize) () public gdscript-prim)
(sys/declare superfunction (randi randi) () public gdscript-prim)
(sys/declare superfunction (randf randf) () public gdscript-prim)
(sys/declare superfunction (rand-range rand-range) (a b) public gdscript-prim)
(sys/declare superfunction (clamp clamp) (a b c) public gdscript-prim)
(sys/declare superfunction (abs abs) (a) public gdscript-prim)
(sys/declare superfunction (len len) (a) public gdscript-prim) ; TODO Eventually, we'll want this to be a multimethod which works on lists as well as arrays. (And possibly elt as well?)
(sys/declare superfunction (get-global-mouse-position get-global-mouse-position) () public gdscript-prim) ; TODO Definitely want to wrap this (and all of the mouse functions) in a nice namespace or module or something
(sys/declare superfunction (push-error push-error) (a) public gdscript-prim)
(sys/declare superfunction (push-warning push-warning) (a) public gdscript-prim)
(sys/declare superfunction (load load) (a) public gdscript-prim)
(sys/declare superfunction (acos acos) (a) public gdscript-prim)
(sys/declare superfunction (asin asin) (a) public gdscript-prim)
(sys/declare superfunction (atan atan) (a) public gdscript-prim)
(sys/declare superfunction (atan2 atan2) (a b) public gdscript-prim)
(sys/declare superfunction (cos cos) (a) public gdscript-prim)
(sys/declare superfunction (cosh cosh) (a) public gdscript-prim)
(sys/declare superfunction (sin sin) (a) public gdscript-prim)
(sys/declare superfunction (sinh sinh) (a) public gdscript-prim)
(sys/declare superfunction (tan tan) (a) public gdscript-prim)
(sys/declare superfunction (tanh tanh) (a) public gdscript-prim)
(sys/declare superfunction (ceil ceil) (a) public gdscript-prim)
(sys/declare superfunction (char char) (a) public gdscript-prim)
(sys/declare superfunction (exp exp) (a) public gdscript-prim)
(sys/declare superfunction (floor floor) (a) public gdscript-prim)
(sys/declare superfunction (sqrt sqrt) (a) public gdscript-prim)
(sys/declare superfunction (fmod fmod) (a b) public gdscript-prim)
(sys/declare superfunction (fposmod fposmod) (a b) public gdscript-prim)
(sys/declare superfunction (posmod posmod) (a b) public gdscript-prim)
(sys/declare superfunction (sign sign) (a) public gdscript-prim)
(sys/declare superfunction (ord ord) (a) public gdscript-prim)
(sys/declare superfunction (hash hash) (a) public gdscript-prim)
(sys/declare superfunction (get-stack get-stack) () public gdscript-prim)
(sys/declare superfunction (is-nan is-nan) (a) public gdscript-prim)
(sys/declare superfunction (is-inf is-inf) (a) public gdscript-prim)
(sys/declare superfunction (is-equal-approx is-equal-approx) (a b) public gdscript-prim)
(sys/declare superfunction (is-zero-approx is-zero-approx) (a) public gdscript-prim)
(sys/declare superfunction (inverse-lerp inverse-lerp) (a b c) public gdscript-prim)
(sys/declare superfunction (lerp lerp) (a b c) public gdscript-prim)
(sys/declare superfunction (lerp-angle lerp-angle) (a b c) public gdscript-prim)
(sys/declare superfunction (pow pow) (a b) public gdscript-prim)
(sys/declare superfunction (stepify stepify) (a b) public gdscript-prim)
(sys/declare superfunction (step-decimals step-decimals) (a) public gdscript-prim)
(sys/declare superfunction (seed seed) (a) public gdscript-prim)
(sys/declare superfunction (rand-seed rand-seed) (a) public gdscript-prim)
(sys/declare superfunction (deg2rad deg2rad) (a) public gdscript-prim)
(sys/declare superfunction (rad2deg rad2deg) (a) public gdscript-prim)
(sys/declare superfunction (db2linear db2linear) (a) public gdscript-prim)
(sys/declare superfunction (linear2db linear2db) (a) public gdscript-prim)
(sys/declare superfunction (is-instance-valid is-instance-valid) (a) public gdscript-prim)
(sys/declare superfunction (log log) (a) public gdscript-prim)
(sys/declare superfunction (wrapf wrapf) (a b c) public gdscript-prim)
(sys/declare superfunction (wrapi wrapi) (a b c) public gdscript-prim)
(sys/declare superfunction (print-stack print-stack) () public gdscript-prim)
(sys/declare superfunction (round round) (a) public gdscript-prim)
(sys/declare superfunction (cartesian2polar cartesian2polar) (a b) public gdscript-prim)
(sys/declare superfunction (polar2cartesian polar2cartesian) (a b) public gdscript-prim)
(sys/declare superfunction (range-lerp range-lerp) (a b c d e) public gdscript-prim)
(sys/declare superfunction (move-toward move-toward) (a b c) public gdscript-prim)
(sys/declare superfunction (nearest-po2 nearest-po2) (a) public gdscript-prim)
(sys/declare superfunction (instance-from-id instance-from-id) (a) public gdscript-prim)
(sys/declare superfunction (parse-json parse-json) (a) public gdscript-prim)
(sys/declare superfunction (to-json to-json) (a) public gdscript-prim)
(sys/declare superfunction (validate-json validate-json) (a) public gdscript-prim)
(sys/declare superfunction (dict2inst dict2inst) (a) public gdscript-prim)
(sys/declare superfunction (inst2dict inst2dict) (a) public gdscript-prim)
(sys/declare superfunction (str2var str2var) (a) public gdscript-prim)
(sys/declare superfunction (var2str var2str) (a) public gdscript-prim)
(sys/declare superfunction (weakref weakref) (a) public gdscript-prim)
(sys/declare superfunction (ease ease) (a b) public gdscript-prim)
(sys/declare superfunction (funcref funcref) (a b) public gdscript-prim)
(sys/declare superfunction (type-exists type-exists) (a) public gdscript-prim)
(sys/declare superfunction (smoothstep smoothstep) (a b c) public gdscript-prim)
(sys/declare superfunction (range range) (a &opt b c) public gdscript-prim)

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

;; Global constants

(sys/declare superglobal (PI PI) public)
(sys/declare superglobal (TAU TAU) public)
(sys/declare superglobal (SPKEY SPKEY) public)
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

(sys/declare superglobal (TYPE_NIL TYPE_NIL) public)
(sys/declare value Null public)
(sys/declare superglobal (TYPE_BOOL TYPE_BOOL) public)
(sys/declare value Bool public)
(sys/declare superglobal (TYPE_INT TYPE_INT) public)
(sys/declare value Int public)
(sys/declare superglobal (TYPE_REAL TYPE_REAL) public)
(sys/declare value Float public)
(sys/declare superglobal (TYPE_STRING TYPE_STRING) public)
(sys/declare value String public)
(sys/declare superglobal (TYPE_VECTOR2 TYPE_VECTOR2) public)
(sys/declare value Vector2 public)
(sys/declare superglobal (TYPE_RECT2 TYPE_RECT2) public)
(sys/declare value Rect2 public)
(sys/declare superglobal (TYPE_VECTOR3 TYPE_VECTOR3) public)
(sys/declare value Vector3 public)
(sys/declare superglobal (TYPE_TRANSFORM2D TYPE_TRANSFORM2D) public)
(sys/declare value Transform2D public)
(sys/declare superglobal (TYPE_PLANE TYPE_PLANE) public)
(sys/declare value Plane public)
(sys/declare superglobal (TYPE_QUAT TYPE_QUAT) public)
(sys/declare value Quat public)
(sys/declare superglobal (TYPE_AABB TYPE_AABB) public)
(sys/declare value AABB public)
(sys/declare superglobal (TYPE_BASIS TYPE_BASIS) public)
(sys/declare value Basis public)
(sys/declare superglobal (TYPE_TRANSFORM TYPE_TRANSFORM) public)
(sys/declare value Transform public)
(sys/declare superglobal (TYPE_COLOR TYPE_COLOR) public)
(sys/declare value Color public)
(sys/declare superglobal (TYPE_NODE_PATH TYPE_NODE_PATH) public)
(sys/declare value NodePath public)
(sys/declare superglobal (TYPE_RID TYPE_RID) public)
(sys/declare value RID public)
(sys/declare superglobal (TYPE_OBJECT TYPE_OBJECT) public)
(sys/declare value Object public)
(sys/declare superglobal (TYPE_DICTIONARY TYPE_DICTIONARY) public)
(sys/declare value Dictionary public)
(sys/declare superglobal (TYPE_ARRAY TYPE_ARRAY) public)
(sys/declare value Array public)
(sys/declare superglobal (TYPE_RAW_ARRAY TYPE_RAW_ARRAY) public)
(sys/declare value PoolByteArray public)
(sys/declare superglobal (TYPE_INT_ARRAY TYPE_INT_ARRAY) public)
(sys/declare value PoolIntArray public)
(sys/declare superglobal (TYPE_REAL_ARRAY TYPE_REAL_ARRAY) public)
(sys/declare value PoolRealArray public)
(sys/declare superglobal (TYPE_STRING_ARRAY TYPE_STRING_ARRAY) public)
(sys/declare value PoolStringArray public)
(sys/declare superglobal (TYPE_VECTOR2_ARRAY TYPE_VECTOR2_ARRAY) public)
(sys/declare value PoolVector2Array public)
(sys/declare superglobal (TYPE_VECTOR3_ARRAY TYPE_VECTOR3_ARRAY) public)
(sys/declare value PoolVector3Array public)
(sys/declare superglobal (TYPE_COLOR_ARRAY TYPE_COLOR_ARRAY) public)
(sys/declare value PoolColorArray public)
(sys/declare superglobal (TYPE_MAX TYPE_MAX) public)

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
