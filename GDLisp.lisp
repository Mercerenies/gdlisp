
;; Library file for GDLisp. This file must be included as a singleton
;; in any project which uses GDLisp compiled files.

(sys/nostdlib)

(defconst nil ())

(defconst GODOT-VERSION (sys/special-ref godot-version))

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

(sys/declare superglobal GDLisp public) ; This file :)

;;; GLOBAL ENUMS ;;;

(sys/bootstrap constants)
(sys/bootstrap constant-enums)

;;; GDScript Types ;;;

(sys/bootstrap non-singleton-types)
(sys/bootstrap singleton-types)

(defclass _GDLisp (Node) main
  (defvar global_name_generator (FreshNameGenerator:new [] 0))

  (defvar symbol_table {})

  ;; Primitive types
  (defvar Null (PrimitiveType:new TYPE_NIL))
  (defvar Bool (PrimitiveType:new TYPE_BOOL))
  (defvar Int (PrimitiveType:new TYPE_INT))
  (defvar Float (PrimitiveType:new TYPE_REAL))
  (defvar String (PrimitiveType:new TYPE_STRING))
  (defvar Vector2 (PrimitiveType:new TYPE_VECTOR2))
  (defvar Rect2 (PrimitiveType:new TYPE_RECT2))
  (defvar Vector3 (PrimitiveType:new TYPE_VECTOR3))
  (defvar Transform2D (PrimitiveType:new TYPE_TRANSFORM2D))
  (defvar Plane (PrimitiveType:new TYPE_PLANE))
  (defvar Quat (PrimitiveType:new TYPE_QUAT))
  (defvar AABB (PrimitiveType:new TYPE_AABB))
  (defvar Basis (PrimitiveType:new TYPE_BASIS))
  (defvar Transform (PrimitiveType:new TYPE_TRANSFORM))
  (defvar Color (PrimitiveType:new TYPE_COLOR))
  (defvar NodePath (PrimitiveType:new TYPE_NODE_PATH))
  (defvar RID (PrimitiveType:new TYPE_RID))
  (defvar Object (PrimitiveType:new TYPE_OBJECT))
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

(defn cons (a b)
  (Cons:new a b))

(defn init (a)
  (cond
    ((sys/instance-direct? a:cdr Cons) (cons a:car (init a:cdr)))
    (#t nil)))

(defn tail (a)
  (cond
    ((sys/instance-direct? a:cdr Cons) (tail a:cdr))
    (#t a:car)))

(defn intern (a)
  (cond
    ((member? a GDLisp:symbol_table) (elt GDLisp:symbol_table a))
    (#t (set (elt GDLisp:symbol_table a) (Symbol:new a)))))

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
    ((bin-equal a:car b:car) (bin-equal a:cdr b:cdr))
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
      (set rev `(,arg:car . ,rev))
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

(sys/min-godot-version 3050000
  (sys/declare superfunction (deep-equal deep-equal) (a b) public))

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

  (defn _init (@primitive-value))

  (defn satisfies? (value)
    (= ((literally typeof) value) self:primitive-value)))

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

;; BUILT-IN MACROS

(defmacro or (&rest args)
  (let ((args (reverse args)))
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
  (let ((args (reverse args)))
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
