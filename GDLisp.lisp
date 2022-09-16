
;;;; Library file for GDLisp. This file must be included as a singleton
;;;; in any project which uses GDLisp compiled files.

(sys/nostdlib)

;;; Global GDLisp constants and enums

(defconst nil ())
(defconst GODOT-VERSION (sys/special-ref godot-version))
(sys/declare superglobal GDLisp public) ; This file :)
(sys/bootstrap constants)
(sys/bootstrap constant-enums)

(sys/declare superglobal (PI PI) public)
(sys/declare superglobal (TAU TAU) public)
(sys/declare superglobal (INF INF) public)

(defenum ConnectFlags
  (DEFERRED 1)
  (PERSIST 2)
  (ONESHOT 4)
  (REFERENCE_COUNTED 8))

(defenum Notification
  (POSTINITIALIZE 0)
  (PREDELETE 1))

;; Note: Godot doesn't consider the below names constant for some
;; reason, since they're apparently defined *on* `Object` in some
;; weird way. So we have to hardcode the numerical values in the
;; enums. We have test cases to make sure these numbers are consistent
;; with the Godot values for the same.

(sys/declare value CONNECT_DEFERRED public)
(sys/declare value CONNECT_PERSIST public)
(sys/declare value CONNECT_ONESHOT public)
(sys/declare value CONNECT_REFERENCE_COUNTED public)
(sys/declare value NOTIFICATION_POSTINITIALIZE public)
(sys/declare value NOTIFICATION_PREDELETE public)

;;; GDScript global types

(sys/bootstrap non-singleton-types)
(sys/bootstrap singleton-types)

;;; GDLisp main class initialization

(defclass _GDLisp (Node) main
  (defvar global_name_generator (FreshNameGenerator:new [] 0))

  (defvar symbol_table {})

  ;; Primitive types
  (defvar Null (PrimitiveType:new TYPE_NIL))
  (defvar Bool (PrimitiveType:new TYPE_BOOL))
  (defvar Int (PrimitiveType:new TYPE_INT))
  (defvar Float (PrimitiveType:new TYPE_REAL))
  (defvar String (PrimitiveType:new TYPE_STRING))
  (defvar Vector2 (Vector2PrimitiveType:new))
  (defvar Rect2 (PrimitiveType:new TYPE_RECT2))
  (defvar Vector3 (Vector3PrimitiveType:new))
  (defvar Transform2D (Transform2DPrimitiveType:new))
  (defvar Plane (PlanePrimitiveType:new))
  (defvar Quat (QuatPrimitiveType:new))
  (defvar AABB (PrimitiveType:new TYPE_AABB))
  (defvar Basis (BasisPrimitiveType:new))
  (defvar Transform (TransformPrimitiveType:new))
  (defvar Color (ColorPrimitiveType:new))
  (defvar NodePath (PrimitiveType:new TYPE_NODE_PATH))
  (defvar RID (PrimitiveType:new TYPE_RID))
  (defvar Object (ObjectPrimitiveType:new))
  (defvar Dictionary (PrimitiveType:new TYPE_DICTIONARY))
  (defvar Array (PrimitiveType:new TYPE_ARRAY))
  (defvar PoolByteArray (PrimitiveType:new TYPE_RAW_ARRAY))
  (defvar PoolIntArray (PrimitiveType:new TYPE_INT_ARRAY))
  (defvar PoolRealArray (PrimitiveType:new TYPE_REAL_ARRAY))
  (defvar PoolStringArray (PrimitiveType:new TYPE_STRING_ARRAY))
  (defvar PoolVector2Array (PrimitiveType:new TYPE_VECTOR2_ARRAY))
  (defvar PoolVector3Array (PrimitiveType:new TYPE_VECTOR3_ARRAY))
  (defvar PoolColorArray (PrimitiveType:new TYPE_COLOR_ARRAY))

  ;; Synthetic types
  (defvar Any (AnyType:new))
  (defvar AnyRef (AnyRefType:new))
  (defvar AnyVal (AnyValType:new))
  (defvar Number (NumberType:new))
  (defvar BaseArray (BaseArrayType:new))
  (defvar Nothing (NothingType:new))

  ;; GDScript native types lookup table
  (defvar native_types_lookup)
  (defvar primitive_types_lookup)

  (sys/bootstrap singleton-backing-types)

  (defn _init ()
    (set self:primitive_types_lookup
         {TYPE_NIL @Null TYPE_BOOL @Bool TYPE_INT @Int TYPE_REAL @Float TYPE_STRING @String TYPE_VECTOR2 @Vector2
          TYPE_RECT2 @Rect2 TYPE_VECTOR3 @Vector3 TYPE_TRANSFORM2D @Transform2D TYPE_PLANE @Plane TYPE_QUAT @Quat
          TYPE_AABB @AABB TYPE_BASIS @Basis TYPE_TRANSFORM @Transform TYPE_COLOR @Color TYPE_NODE_PATH @NodePath
          TYPE_RID @RID TYPE_OBJECT @Object TYPE_DICTIONARY @Dictionary TYPE_ARRAY @Array
          TYPE_RAW_ARRAY @PoolByteArray TYPE_INT_ARRAY @PoolIntArray TYPE_REAL_ARRAY @PoolRealArray
          TYPE_STRING_ARRAY @PoolStringArray TYPE_VECTOR2_ARRAY @PoolVector2Array
          TYPE_VECTOR3_ARRAY @PoolVector3Array TYPE_COLOR_ARRAY @PoolColorArray})

    (sys/bootstrap native-types-table)
    (set (elt self:native_types_lookup "Object") @Object))

  (defn typeof (value)
    (let ((t ((literally typeof) value)))
      (cond
        ((/= t TYPE_OBJECT) (elt self:primitive_types_lookup t))
        ((value:get_script))
        (#t (self:native_types_lookup:get (value:get_class) self:Any))))))

;;; Public support classes

(defclass Cons (Reference)
  (defvar car)
  (defvar cdr)

  (defn _init (@car @cdr)
    (self:set-meta "__gdlisp_Primitive_Cons" 1))

  (defn (get caar) ()
    self:car:car)

  (defn (get cadr) ()
    self:car:cdr)

  (defn (get cdar) ()
    self:cdr:car)

  (defn (get cddr) ()
    self:cdr:cdr)

  (defn (get caaar) ()
    self:car:car:car)

  (defn (get caadr) ()
    self:car:car:cdr)

  (defn (get cadar) ()
    self:car:cdr:car)

  (defn (get caddr) ()
    self:car:cdr:cdr)

  (defn (get cdaar) ()
    self:cdr:car:car)

  (defn (get cdadr) ()
    self:cdr:car:cdr)

  (defn (get cddar) ()
    self:cdr:cdr:car)

  (defn (get cdddr) ()
    self:cdr:cdr:cdr))

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
  (defn _init (@contents)))

(defclass Symbol (Reference)
  ;; Note: This will be obsolete once we have StringName in GDScript,
  ;; which seems to be coming in Godot 4. For now, this manual wrapper
  ;; stores symbols in the least efficient way possible.
  (defvar contents)
  (defn _init (@contents)
    (self:set-meta "__gdlisp_Primitive_Symbol" 1)))

(defclass FreshNameGenerator (Reference)
  ;; This is meant to be identical to FreshNameGenerator in the Rust
  ;; source. We want to be able to consistently generate names in the
  ;; same way on both Rust and Godot and to be able to communicate
  ;; FreshNameGenerator state between the two (via to_json /
  ;; from_json)

  (defconst DEFAULT_PREFIX "_G")

  (defvar reserved)
  (defvar index)

  (defn _init (@reserved @index))

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

;;; GDLisp type constants and support types

(defclass GDLispSpecialType (Reference) private)

(defclass PrimitiveType (GDLispSpecialType) private
  (defvar primitive-value)

  (defn _init (@primitive-value))

  (defn satisfies? (value)
    (= ((literally typeof) value) self:primitive-value)))

(defclass ObjectPrimitiveType (PrimitiveType) private

  (defn _init ()
    (super TYPE_OBJECT)))

;; NOTE: Due to current Godot bugs, we can't actually define this,
;; since GDScript fails to parse constructor calls on Object directly.
;; See https://github.com/godotengine/godot/issues/41462. Has been
;; fixed in 4.0 and it doesn't look like they're planning to backport.
;;
;;  (defn new ()
;;    ((literally Object):new)))

(defclass Vector2PrimitiveType (PrimitiveType) private
  (defconst AXIS_X 0)
  (defconst AXIS_Y 1)
  (defconst ZERO V{0 0})
  (defconst ONE V{1 1})
  (defconst INF V{INF INF})
  (defconst LEFT V{-1 0})
  (defconst RIGHT V{1 0})
  (defconst UP V{0 -1})
  (defconst DOWN V{0 1})

  (defn _init ()
    (super TYPE_VECTOR2)))

(defclass Vector3PrimitiveType (PrimitiveType) private
  (defconst AXIS_X 0)
  (defconst AXIS_Y 1)
  (defconst AXIS_Z 2)
  (defconst ZERO V{0 0 0})
  (defconst ONE V{1 1 1})
  (defconst INF V{INF INF INF})
  (defconst LEFT V{-1 0 0})
  (defconst RIGHT V{1 0 0})
  (defconst UP V{0 1 0})
  (defconst DOWN V{0 -1 0})
  (defconst FORWARD V{0 0 -1})
  (defconst BACK V{0 0 1})

  (defn _init ()
    (super TYPE_VECTOR3)))

(defclass Transform2DPrimitiveType (PrimitiveType) private
  (defconst IDENTITY (Transform2D V{1 0} V{0 1} V{0 0}))
  (defconst FLIP_X (Transform2D V{-1 0} V{0 1} V{0 0}))
  (defconst FLIP_Y (Transform2D V{1 0} V{0 -1} V{0 0}))

  (defn _init ()
    (super TYPE_TRANSFORM2D)))

(defclass PlanePrimitiveType (PrimitiveType) private
  (defconst PLANE_YZ (Plane 1 0 0 0))
  (defconst PLANE_XZ (Plane 0 1 0 0))
  (defconst PLANE_XY (Plane 0 0 1 0))

  (defn _init ()
    (super TYPE_PLANE)))

(defclass QuatPrimitiveType (PrimitiveType) private
  (defconst IDENTITY (Quat 0 0 0 1))

  (defn _init ()
    (super TYPE_QUAT)))

(defclass BasisPrimitiveType (PrimitiveType) private
  (defconst IDENTITY (Basis V{1 0 0} V{0 1 0} V{0 0 1}))
  (defconst FLIP_X (Basis V{-1 0 0} V{0 1 0} V{0 0 1}))
  (defconst FLIP_Y (Basis V{1 0 0} V{0 -1 0} V{0 0 -1}))
  (defconst FLIP_Z (Basis V{1 0 0} V{0 -1 0} V{0 0 -1}))

  (defn _init ()
    (super TYPE_BASIS)))

(defclass TransformPrimitiveType (PrimitiveType) private
  (defconst IDENTITY (Transform V{1 0 0} V{0 1 0} V{0 0 1} V{0 0 0}))
  (defconst FLIP_X (Transform V{-1 0 0} V{0 1 0} V{0 0 1} V{0 0 0}))
  (defconst FLIP_Y (Transform V{1 0 0} V{0 -1 0} V{0 0 -1} V{0 0 0}))
  (defconst FLIP_Z (Transform V{1 0 0} V{0 -1 0} V{0 0 -1} V{0 0 0}))

  (defn _init ()
    (super TYPE_TRANSFORM)))

(defclass ColorPrimitiveType (PrimitiveType) private
    (defconst aliceblue (Color 0.941176 0.972549 1 1))
    (defconst antiquewhite (Color 0.980392 0.921569 0.843137 1))
    (defconst aqua (Color 0 1 1 1))
    (defconst aquamarine (Color 0.498039 1 0.831373 1))
    (defconst azure (Color 0.941176 1 1 1))
    (defconst beige (Color 0.960784 0.960784 0.862745 1))
    (defconst bisque (Color 1 0.894118 0.768627 1))
    (defconst black (Color 0 0 0 1))
    (defconst blanchedalmond (Color 1 0.921569 0.803922 1))
    (defconst blue (Color 0 0 1 1))
    (defconst blueviolet (Color 0.541176 0.168627 0.886275 1))
    (defconst brown (Color 0.647059 0.164706 0.164706 1))
    (defconst burlywood (Color 0.870588 0.721569 0.529412 1))
    (defconst cadetblue (Color 0.372549 0.619608 0.627451 1))
    (defconst chartreuse (Color 0.498039 1 0 1))
    (defconst chocolate (Color 0.823529 0.411765 0.117647 1))
    (defconst coral (Color 1 0.498039 0.313726 1))
    (defconst cornflower (Color 0.392157 0.584314 0.929412 1))
    (defconst cornsilk (Color 1 0.972549 0.862745 1))
    (defconst crimson (Color 0.862745 0.0784314 0.235294 1))
    (defconst cyan (Color 0 1 1 1))
    (defconst darkblue (Color 0 0 0.545098 1))
    (defconst darkcyan (Color 0 0.545098 0.545098 1))
    (defconst darkgoldenrod (Color 0.721569 0.52549 0.0431373 1))
    (defconst darkgray (Color 0.662745 0.662745 0.662745 1))
    (defconst darkgreen (Color 0 0.392157 0 1))
    (defconst darkkhaki (Color 0.741176 0.717647 0.419608 1))
    (defconst darkmagenta (Color 0.545098 0 0.545098 1))
    (defconst darkolivegreen (Color 0.333333 0.419608 0.184314 1))
    (defconst darkorange (Color 1 0.54902 0 1))
    (defconst darkorchid (Color 0.6 0.196078 0.8 1))
    (defconst darkred (Color 0.545098 0 0 1))
    (defconst darksalmon (Color 0.913725 0.588235 0.478431 1))
    (defconst darkseagreen (Color 0.560784 0.737255 0.560784 1))
    (defconst darkslateblue (Color 0.282353 0.239216 0.545098 1))
    (defconst darkslategray (Color 0.184314 0.309804 0.309804 1))
    (defconst darkturquoise (Color 0 0.807843 0.819608 1))
    (defconst darkviolet (Color 0.580392 0 0.827451 1))
    (defconst deeppink (Color 1 0.0784314 0.576471 1))
    (defconst deepskyblue (Color 0 0.74902 1 1))
    (defconst dimgray (Color 0.411765 0.411765 0.411765 1))
    (defconst dodgerblue (Color 0.117647 0.564706 1 1))
    (defconst firebrick (Color 0.698039 0.133333 0.133333 1))
    (defconst floralwhite (Color 1 0.980392 0.941176 1))
    (defconst forestgreen (Color 0.133333 0.545098 0.133333 1))
    (defconst fuchsia (Color 1 0 1 1))
    (defconst gainsboro (Color 0.862745 0.862745 0.862745 1))
    (defconst ghostwhite (Color 0.972549 0.972549 1 1))
    (defconst gold (Color 1 0.843137 0 1))
    (defconst goldenrod (Color 0.854902 0.647059 0.12549 1))
    (defconst gray (Color 0.745098 0.745098 0.745098 1))
    (defconst green (Color 0 1 0 1))
    (defconst greenyellow (Color 0.678431 1 0.184314 1))
    (defconst honeydew (Color 0.941176 1 0.941176 1))
    (defconst hotpink (Color 1 0.411765 0.705882 1))
    (defconst indianred (Color 0.803922 0.360784 0.360784 1))
    (defconst indigo (Color 0.294118 0 0.509804 1))
    (defconst ivory (Color 1 1 0.941176 1))
    (defconst khaki (Color 0.941176 0.901961 0.54902 1))
    (defconst lavender (Color 0.901961 0.901961 0.980392 1))
    (defconst lavenderblush (Color 1 0.941176 0.960784 1))
    (defconst lawngreen (Color 0.486275 0.988235 0 1))
    (defconst lemonchiffon (Color 1 0.980392 0.803922 1))
    (defconst lightblue (Color 0.678431 0.847059 0.901961 1))
    (defconst lightcoral (Color 0.941176 0.501961 0.501961 1))
    (defconst lightcyan (Color 0.878431 1 1 1))
    (defconst lightgoldenrod (Color 0.980392 0.980392 0.823529 1))
    (defconst lightgray (Color 0.827451 0.827451 0.827451 1))
    (defconst lightgreen (Color 0.564706 0.933333 0.564706 1))
    (defconst lightpink (Color 1 0.713726 0.756863 1))
    (defconst lightsalmon (Color 1 0.627451 0.478431 1))
    (defconst lightseagreen (Color 0.12549 0.698039 0.666667 1))
    (defconst lightskyblue (Color 0.529412 0.807843 0.980392 1))
    (defconst lightslategray (Color 0.466667 0.533333 0.6 1))
    (defconst lightsteelblue (Color 0.690196 0.768627 0.870588 1))
    (defconst lightyellow (Color 1 1 0.878431 1))
    (defconst lime (Color 0 1 0 1))
    (defconst limegreen (Color 0.196078 0.803922 0.196078 1))
    (defconst linen (Color 0.980392 0.941176 0.901961 1))
    (defconst magenta (Color 1 0 1 1))
    (defconst maroon (Color 0.690196 0.188235 0.376471 1))
    (defconst mediumaquamarine (Color 0.4 0.803922 0.666667 1))
    (defconst mediumblue (Color 0 0 0.803922 1))
    (defconst mediumorchid (Color 0.729412 0.333333 0.827451 1))
    (defconst mediumpurple (Color 0.576471 0.439216 0.858824 1))
    (defconst mediumseagreen (Color 0.235294 0.701961 0.443137 1))
    (defconst mediumslateblue (Color 0.482353 0.407843 0.933333 1))
    (defconst mediumspringgreen (Color 0 0.980392 0.603922 1))
    (defconst mediumturquoise (Color 0.282353 0.819608 0.8 1))
    (defconst mediumvioletred (Color 0.780392 0.0823529 0.521569 1))
    (defconst midnightblue (Color 0.0980392 0.0980392 0.439216 1))
    (defconst mintcream (Color 0.960784 1 0.980392 1))
    (defconst mistyrose (Color 1 0.894118 0.882353 1))
    (defconst moccasin (Color 1 0.894118 0.709804 1))
    (defconst navajowhite (Color 1 0.870588 0.678431 1))
    (defconst navyblue (Color 0 0 0.501961 1))
    (defconst oldlace (Color 0.992157 0.960784 0.901961 1))
    (defconst olive (Color 0.501961 0.501961 0 1))
    (defconst olivedrab (Color 0.419608 0.556863 0.137255 1))
    (defconst orange (Color 1 0.647059 0 1))
    (defconst orangered (Color 1 0.270588 0 1))
    (defconst orchid (Color 0.854902 0.439216 0.839216 1))
    (defconst palegoldenrod (Color 0.933333 0.909804 0.666667 1))
    (defconst palegreen (Color 0.596078 0.984314 0.596078 1))
    (defconst paleturquoise (Color 0.686275 0.933333 0.933333 1))
    (defconst palevioletred (Color 0.858824 0.439216 0.576471 1))
    (defconst papayawhip (Color 1 0.937255 0.835294 1))
    (defconst peachpuff (Color 1 0.854902 0.72549 1))
    (defconst peru (Color 0.803922 0.521569 0.247059 1))
    (defconst pink (Color 1 0.752941 0.796078 1))
    (defconst plum (Color 0.866667 0.627451 0.866667 1))
    (defconst powderblue (Color 0.690196 0.878431 0.901961 1))
    (defconst purple (Color 0.627451 0.12549 0.941176 1))
    (defconst rebeccapurple (Color 0.4 0.2 0.6 1))
    (defconst red (Color 1 0 0 1))
    (defconst rosybrown (Color 0.737255 0.560784 0.560784 1))
    (defconst royalblue (Color 0.254902 0.411765 0.882353 1))
    (defconst saddlebrown (Color 0.545098 0.270588 0.0745098 1))
    (defconst salmon (Color 0.980392 0.501961 0.447059 1))
    (defconst sandybrown (Color 0.956863 0.643137 0.376471 1))
    (defconst seagreen (Color 0.180392 0.545098 0.341176 1))
    (defconst seashell (Color 1 0.960784 0.933333 1))
    (defconst sienna (Color 0.627451 0.321569 0.176471 1))
    (defconst silver (Color 0.752941 0.752941 0.752941 1))
    (defconst skyblue (Color 0.529412 0.807843 0.921569 1))
    (defconst slateblue (Color 0.415686 0.352941 0.803922 1))
    (defconst slategray (Color 0.439216 0.501961 0.564706 1))
    (defconst snow (Color 1 0.980392 0.980392 1))
    (defconst springgreen (Color 0 1 0.498039 1))
    (defconst steelblue (Color 0.27451 0.509804 0.705882 1))
    (defconst tan (Color 0.823529 0.705882 0.54902 1))
    (defconst teal (Color 0 0.501961 0.501961 1))
    (defconst thistle (Color 0.847059 0.74902 0.847059 1))
    (defconst tomato (Color 1 0.388235 0.278431 1))
    (defconst transparent (Color 1 1 1 0))
    (defconst turquoise (Color 0.25098 0.878431 0.815686 1))
    (defconst violet (Color 0.933333 0.509804 0.933333 1))
    (defconst webgray (Color 0.501961 0.501961 0.501961 1))
    (defconst webgreen (Color 0 0.501961 0 1))
    (defconst webmaroon (Color 0.501961 0 0 1))
    (defconst webpurple (Color 0.501961 0 0.501961 1))
    (defconst wheat (Color 0.960784 0.870588 0.701961 1))
    (defconst white (Color 1 1 1 1))
    (defconst whitesmoke (Color 0.960784 0.960784 0.960784 1))
    (defconst yellow (Color 1 1 0 1))
    (defconst yellowgreen (Color 0.603922 0.803922 0.196078 1))

  (defn _init ()
    (super TYPE_COLOR)))

;; Named types like _Engine whose name can be returned from get_class
;; but which do not exist in the runtime namespace exposed to
;; GDScript.
(defclass NamedSyntheticType (GDLispSpecialType) private
  (defvar name)

  (defn _init (@name))

  (defn satisfies? (value)
    (= (value:get-class) self:name)))

;; Note: All of these synthetic types would theoretically be defobject
;; if we weren't writing them in the standard library. But for
;; complicated reasons, we can't use macro expansion at all in stdlib,
;; and defobject is behind three layers of macro (defobject and
;; deflazy, and the expansion includes define-symbol-macro), so it's
;; off-limits.

(defclass AnyType (GDLispSpecialType) private
  (defn satisfies? (_value)
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
  (defn satisfies? (_value)
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

;;; Polymorphic functions

(defn len (x)
  (cond
    ((= x nil) 0)
    ((sys/instance-direct? x Cons)
     (let ((result 0))
       (while (sys/instance-direct? x Cons)
         (set result (+ result 1))
         (set x x:cdr))
       result))
    (#t ((literally len) x))))

;;; List functions

(defn list (&rest args)
  (sys/call-magic LIST)
  args)

(defn cons (a b)
  (Cons:new a b))

(defn init (a)
  (cond
    ((sys/instance-direct? a:cdr Cons) (cons a:car (init a:cdr)))
    (#t nil)))

(defn last (a)
  (cond
    ((sys/instance-direct? a:cdr Cons) (last a:cdr))
    (#t a:car)))

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

(defn list/elt (list n)
  (list/tail list n):car)

(defn list/fold (f xs &opt x)
  (cond
    ((= x nil)
     (set x xs:car)
     (set xs xs:cdr)))
  (while (/= xs nil)
    (set x (funcall f x xs:car))
    (set xs xs:cdr))
  x)

(defn list/map (f xs)
  (let ((outer (cons nil nil)))
    (let ((curr outer))
      (while (/= xs nil)
        (set curr:cdr (cons (funcall f xs:car) nil))
        (set curr curr:cdr)
        (set xs xs:cdr))
      outer:cdr)))

(defn list/filter (p xs)
  (let ((outer (cons nil nil)))
    (let ((curr outer))
      (while (/= xs nil)
        (cond
          ((funcall p xs:car)
           (set curr:cdr (cons xs:car nil))
           (set curr curr:cdr)))
        (set xs xs:cdr))
      outer:cdr)))

(defn list/reverse (arg)
  (let ((rev nil))
    (while (/= arg nil)
      (set rev `(,arg:car . ,rev))
      (set arg arg:cdr))
    rev))

(defn append (&rest args)
  (cond
    ((= args nil) nil)
    (#t
     (let ((outer (cons nil nil)))
       (let ((curr outer))
         (while (/= args:cdr nil)
           (let ((inner-value args:car))
             (while (/= inner-value nil)
               (set curr:cdr (cons inner-value:car nil))
               (set curr curr:cdr)
               (set inner-value inner-value:cdr)))
           (set args args:cdr))
         (set curr:cdr args:car)
         outer:cdr)))))

(defn list/tail (list k)
  (for _i (range k)
    (set list list:cdr))
  list)

(defn sys/qq-smart-list (a)
  (cond
    ((instance? a GDLisp:BaseArray) (array->list a))
    (#t a)))

;;; Array functions

(defn elt (arr n)
  (sys/call-magic ARRAY-SUBSCRIPT)
  (elt arr n))

(defn set-elt (x arr n)
  (sys/call-magic ARRAY-SUBSCRIPT-ASSIGNMENT)
  (set-elt x arr n))

(defn member? (value arr)
  (sys/call-magic ARRAY-MEMBER-CHECK)
  (member? value arr))

(defn sys/qq-smart-array (a)
  (cond
    ((instance? a GDLisp:BaseArray) a)
    (#t (list->array a))))

(defn array/fold (f xs &opt x)
  (let ((starting-index 0))
    (cond
      ((= x nil)
       (set x (elt xs 0))
       (set starting-index 1)))
    (for i (range starting-index (len xs))
      (set x (funcall f x (elt xs i))))
    x))

(defn array/map (f xs)
  (let ((result []))
    (for i (len xs)
      (result:push_back (funcall f (elt xs i))))
    result))

(defn array/filter (p xs)
  (let ((result []))
    (for i (len xs)
      (cond
        ((funcall p (elt xs i))
         (result:push_back (elt xs i)))))
    result))

(defn array/reverse (arr)
  (let ((len (len arr))
        (result (arr:duplicate)))
    (for i (range len)
      (set (elt result i) (elt arr (- len i 1))))
    result))

;;; Higher-order functions

(defn funcall (f &rest args)
  (apply f args))

;; funcall alias used in IIFEs. The user can shadow funcall (though
;; it's a bad idea), but shadowing names in sys/ is undefined behavior.
(defn sys/funcall (f &rest args)
  (apply f args))

(defn apply (f &rest args)
  (let ((args1 (init args))
        (args2 (last args)))
    (cond
      ((sys/instance-direct? f Function) (f:call_funcv (append args1 args2)))
      (#t (push-error "Attempt to call non-function")))))

;;; Miscellaneous simple functions

(defn vector (x y &opt z)
  (sys/call-magic VECTOR)
  (cond
    ((= z ()) V{x y})
    (#t V{x y z})))

(defn NodePath (s)
  (sys/call-magic NODEPATH-SYNTAX)
  ((literally NodePath) s))

(defn not (x)
  (sys/call-magic BOOLEAN-NOT)
  (not x))

(defn intern (a)
  (cond
    ((member? a GDLisp:symbol_table) (elt GDLisp:symbol_table a))
    (#t (set (elt GDLisp:symbol_table a) (Symbol:new a)))))

(defn gensym (&opt prefix)
  (cond
    ((= prefix ()) (Symbol:new (GDLisp:global_name_generator:generate)))
    (#t (Symbol:new (GDLisp:global_name_generator:generate_with prefix)))))

(defn sys/get-node (obj path)
  (sys/call-magic GET-NODE-SYNTAX)
  (obj:get-node path))

(defn sys/native-class-private () private ; TODO(!!) Unused?
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

;;; Math operators

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

(defn gcd (&rest args)
  (list/fold #'binary-gcd args 0))

(defn lcm (&rest args)
  (list/fold #'binary-lcm args 1))

(defn binary-gcd (a b) private
  (while (/= b 0)
    (let ((tmp a))
      (set a b)
      (set b (mod tmp b))))
  a)

(defn binary-lcm (a b) private
  (/ (* a b) (binary-gcd a b)))

;;; Comparison operators

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
    ((bin-equal a:car b:car) (bin-equal a:cdr b:cdr))
    (#t #f)))

;;; GDScript built-ins that we use unmodified

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

;; Note that, in the spirit of internal consistency with the type
;; names, we do name some of these functions differently. For
;; instance, the GDScript function `bool` is mapped to the GDLisp
;; function `Bool`, for consistency with the `Bool` type.

(sys/declare superfunction (Bool bool) (a) public)
(sys/declare superfunction (Int int) (a) public)
(sys/declare superfunction (Float float) (a) public)
(sys/declare superfunction (String String) (a) public)
(sys/declare superfunction (AABB AABB) (a b) public)
(sys/declare superfunction (RID RID) (a) public)
(sys/declare superfunction (Dictionary Dictionary) (a) public)
(sys/declare superfunction (Array Array) (a) public)
(sys/declare superfunction (PoolColorArray PoolColorArray) (a) public)
(sys/declare superfunction (PoolByteArray PoolByteArray) (a) public)
(sys/declare superfunction (PoolIntArray PoolIntArray) (a) public)
(sys/declare superfunction (PoolRealArray PoolRealArray) (a) public)
(sys/declare superfunction (PoolVector2Array PoolVector2Array) (a) public)
(sys/declare superfunction (PoolVector3Array PoolVector3Array) (a) public)

(sys/declare superfunction (Vector2 Vector2) (a b) public)
(sys/declare superfunction (Vector3 Vector3) (a b c) public)

(sys/min-godot-version 3050000
  (sys/declare superfunction (deep-equal deep-equal) (a b) public))

;;; Varargs functions

;; (See https://github.com/Mercerenies/gdlisp/issues/79 for details on
;; why we have to wrap these ourselves)

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

(defn Rect2 (a b &opt c d) ; TODO Not a perfect translation of the pair of overloads provided
  (sys/call-magic VARARG-RECT2)
  (cond
    ((= c nil) (Rect2 a b))
    (#t (Rect2 a b c d))))

(defn Transform2D (a &opt b c)
  (sys/call-magic VARARG-TRANSFORM2D)
  (cond
    ((= b nil) (Transform2D a))
    ((= c nil) (Transform2D a b))
    (#t (Transform2D a b c))))

(defn Plane (a b &opt c d)
  (sys/call-magic VARARG-PLANE)
  (cond
    ((= c nil) (Plane a b))
    ((= d nil) (Plane a b c))
    (#t (Plane a b c d))))

(defn Quat (a &opt b c d) ; TODO Not a perfect translation of the overloads provided
  (sys/call-magic VARARG-QUAT)
  (cond
    ((= b nil) (Quat a))
    ((= c nil) (Quat a b))
    (#t (Quat a b c d))))

(defn Basis (a &opt b c)
  (sys/call-magic VARARG-BASIS)
  (cond
    ((= b nil) (Basis a))
    ((= c nil) (Basis a b))
    (#t (Basis a b c))))

(defn Transform (a &opt b c d) ; TODO Not a perfect translation of the overloads provided
  (sys/call-magic VARARG-TRANSFORM)
  (cond
    ((= b nil) (Transform a))
    ((= c nil) (Transform a b))
    (#t (Transform a b c d))))

(defn Color (a &opt b c d) ; TODO Not a perfect translation of the overloads provided
  (sys/call-magic VARARG-COLOR)
  (cond
    ((= b nil) (Color a))
    ((= d nil) (Color a b c))
    (#t (Color a b c d))))

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

;; TYPE_* Constants

;;; Built-In Macros

(defmacro or (&rest args)
  (let ((args (list/reverse args)))
    (cond
      (args
       (let ((result `((#t ,args:car))))
         (set args args:cdr)
         (while (/= args nil)
           (set result `((,args:car) . ,result))
           (set args args:cdr))
         `(cond . ,result)))
      (#t #f))))

(defmacro and (&rest args)
  (let ((args (list/reverse args)))
    (cond
      (args
       (let ((result `((#t ,args:car))))
         (set args args:cdr)
         (while (/= args nil)
           (set result `(((not ,args:car) #f) . ,result))
           (set args args:cdr))
         `(cond . ,result)))
       (#t #t))))

(defmacro let* (vars &rest body)
  (cond
    ((= vars nil) `(progn ,.body))
    (#t `(let (,vars:car)
           (let* ,vars:cdr ,.body)))))

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

(defmacro update (field updater)
  (cond
    ((not (instance? updater Cons)) (set updater (list updater))))
  (set updater:cdr (cons field updater:cdr))
  `(set ,field ,updater))

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

(defmacro list/for (var list &rest body)
  (let ((iter (gensym)))
    `(let ((,iter ,list)
           (,var ()))
       (while (/= ,iter ())
         (set ,var (access-slot ,iter car))
         (set ,iter (access-slot ,iter cdr))
         ,.body))))
