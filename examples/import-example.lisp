
(use "res://simple-functions.lisp" as simple)
(use "res://simple-functions.lisp" open)
(use "res://simple-functions.lisp")
(use "res://simple-functions.lisp" ((double as simple-double)))

(simple/double 10)
(double 10)
(simple-functions/double 10)
(simple-double 10)
