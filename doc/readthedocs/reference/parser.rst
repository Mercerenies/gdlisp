
The GDLisp Parser
=================

The GDLisp language is a Lisp dialect, with a few extensions to make
object-oriented programming using Godot types more straightforward.

Listed below is a full description of the GDLisp grammar using
`Extended Backup-Naur form
<https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form>`_.

GDLisp source code is always written in UTF-8. GDLisp supports the
full breadth of Unicode code points, so unless otherwise stated,
"character" refers to a valid Unicode character.

.. code-block:: ebnf

   prefixed-expr = [ prefix ], expr ;
   prefix = "'" | "#'" | "`" | "," | ",." ;
   expr = literal | list-expr | array-expr |
          dict-expr | vector-expr |
          nested-name | nested-node-path |
          self-name | self-node-path ;
   literal = "#t" | "#f" | integer | float | string | symbol ;
   list-expr = "(", ")" | "(", prefixed-expr, {prefixed-expr}, ["." prefixed-expr], ")" ;
   array-expr = "[", {prefixed-expr}, "]" ;
   dict-expr = "{", {prefixed-expr, prefixed-expr}, "}" ;
   vector-expr = "V{", prefixed-expr, prefixed-expr, [prefixed-expr], "}" ;
   nested-name = expr, ":", symbol ;
   nested-node-path = expr, ":", node-path ;
   self-name = "@", symbol ;
   self-node-path = node-path ;
   integer = ? integer literal ? ;
   float = ? floating point literal ? ;
   string = ? string literal ? ;
   symbol = ? symbol literal ? ;
   node-path = ? node path literal ? ;

GDLisp code, like any other Lisp dialect, is built up of zero or more
`S-expressions <https://en.wikipedia.org/wiki/S-expression>`_, where
an S-expression is defined to be an (optionally prefixed) form of any
of the following.

* A literal expression
* A list
* An array
* A dictionary
* A (2D or 3D) vector
* A nested name or node path
* A self name or node path

Unless otherwise stated, an arbitrary amount of whitespace is allowed
between adjacent tokens in the above EBNF grammar. "Whitespace" is,
here, defined as any Unicode character with the ``White_Space=yes``
property.

Literals
--------

.. code-block:: ebnf

   literal = "#t" | "#f" | integer | float | string | symbol ;
   integer = ? integer literal ? ;
   float = ? floating point literal ? ;
   string = ? string literal ? ;
   symbol = ? symbol literal ? ;
   node-path = ? node path literal ? ;

Literals in GDLisp are integers, floating point values, strings,
symbols, or node path literals.

Integer Literals
^^^^^^^^^^^^^^^^

An integer literal consists of an optional sign (``+`` or ``-``)
followed by one or more ASCII digits (``0`` to ``9``). The following
are valid integer literals: ``0``, ``+56``, ``-9``, ``10000``,
``00900``. Leading zeroes in an integer literal are ignored. Unlike in
C, the presence of a leading zero does *not* cause the subsequent
number to be interpreted as ASCII.

Floating Point Literals
^^^^^^^^^^^^^^^^^^^^^^^

A floating point literal is an expression which matches the following
regular expression

.. code-block:: text

   [+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?

and is *not* a valid integer literal.

String Literals
^^^^^^^^^^^^^^^

A string literal is a sequence of zero or more characters enclosed in
quotation marks ``"``. Inside the quotation marks is a sequence of
individual string characters, where each individual character is one
of

* Any Unicode character other than a backslash ``\`` or a quotation
  mark ``"``.
* A valid escape sequence beginning in a backslash ``\``.

Escape Sequences
""""""""""""""""

An escape sequence in GDLisp begins with a backslash and consists of
one or more characters indicating what character the sequence should
be translated to in the resulting code. GDLisp supports most of the
commonly-used escape sequences found in C-style languages.

* ``\n`` translates to a newline (``0x0A``)
* ``\t`` translates to a horizontal tab (``0x09``)
* ``\r`` translates to a carriage return (``0x0D``)
* ``\a`` translates to the 'alert' character (``0x07``)
* ``\b`` translates to the backspace character (``0x08``)
* ``\f`` translates to a form feed (``0x0C``)
* ``\v`` translates to a vertical tab (``0x0B``)
* ``\"`` translates to a literal quotation mark (``0x22``)
* ``\'`` translates to a literal apostrophe (``0x27``)
* ``\\`` translates to a literal backslash (``0x5C``)

Finally, a backslash followed by a ``u`` is a Unicode literal. Unicode
literals can be represented in two forms: basic and extended.

A basic Unicode literal consists of ``\u`` followed by exactly four
valid hexadecimal characters. The four characters are interpreted as a
number in base 16 and must point to a valid Unicode code point. Note
that there are characters outside of the basic multilingual plane
(such as emoji) that cannot be represented in this way. For such
characters, the extended form is provided.

An extended Unicode literal consists of ``\u`` followed by a
curly-brace-enclosed list of at least one hexadecimal character. The
characters in the list are interpreted as a number in base 16 and must
point to a valid Unicode code point.

A backslash followed by any other character in a string literal is an
error.

Symbol Literals
^^^^^^^^^^^^^^^

Symbols are the cornerstone of a Lisp program and are used as variable
and function names. These are, generally speaking, the valid
identifiers in a Lisp program.

.. code-block:: ebnf

   symbol = starting-char, { following-char }, { qualifier } ;
   qualifier = ".", following-char, { following-char } ;

A symbol consists of a starting character, followed by zero or more
following characters, then subsequently followed by zero or more
qualifiers. A qualifier consists of a dot followed by one or more
following characters.

The starting character of a symbol literal can be any of the following.

* An ASCII letter
* Any of the following: ``_~+=-\/!%^&*<>?``
* Any non-ASCII character which falls into the Unicode categories L,
  Mn, Nl, No, S, Pc, Pd, or Po.

A "following" character can be any starting character, a valid ASCII
number, or any non-ASCII character in the Unicode category N.

The following are examples of valid identifiers in GDLisp: ``foo``,
``bar``, ``satisfies?``, ``set-element``, ``list/map``,
``com.mercerenies.gdlisp``.

Node Path Literals
^^^^^^^^^^^^^^^^^^

A node path literal is the primary means of accessing nodes in the
scene tree whose names are known at compile-time. A node path literal
consists of a dollar sign ``$`` followed by either a quoted string
literal or a sequence of one or more of the following:
* An ASCII letter or number
* Any of the following: ``_~+=-\/!$%^&*<>?``

Note thta only ASCII characters are allowed in the non-quoted node
path form. To include Unicode characters in a node path, it is
necessary to quote the path.

Lists
-----

.. code-block:: ebnf

   list-expr = "(", ")" | "(", prefixed-expr, {prefixed-expr}, ["." prefixed-expr], ")" ;

In GDLisp, the fundamental unit of composition is a *cons cell*,
sometimes called a *pair*. A cons cell consists of two elements,
conventionally referred to as the *car* and the *cdr*, separated by a
dot and enclosed in parentheses.

.. code-block:: text

  (a . b)

By convention, lists are built up as a singly-linked list, using cons
cells as the links. The car of each is the first element, or "head",
of the list, and the cdr of each is the rest of the list, or "tail".
The end of the list is denoted with the special "null" atom,
indicating by a pair of parentheses with nothing in between.

This convention is so widely used in Lisp programs that the syntax
supports it directly. That is, a sequence of of one or more
S-expressions, followed by the dotted end of the list, is interpreted
as a list whose final cdr is the rightmost term. Concretely, the
following are equivalent.

.. code-block:: text

  (a b c . d)
  (a . (b . (c . d)))

Similarly, if the dotted terminator is left off, it is assumed to be
the special ``()`` null object, so the following are equivalent.

.. code-block:: text

  (a b c d)
  (a . (b . (c . d . ())))

An S-expression can be viewed as a *dotted list*, consisting of a
leading list of values (in the ``car`` portion of cons cells)
terminated by an arbitrary non-cons value as the final ``cdr``. For
instance, the S-expression ``(a b c . d)`` (or, equivalently, ``(a .
(b . (c . d)))`` would be viewed as a dotted list consisting of the
elements ``a``, ``b``, and ``c``, terminated by ``d``.

We call a dotted list which is terminated by the ``()`` null object a
*proper list*. All lists appearing unquoted in GDLisp source code must
be proper lists.

Arrays
------

.. code-block:: ebnf

   array-expr = "[", {prefixed-expr}, "]" ;

Like in GDScript, a GDLisp array is a general-purpose random-access
data structure. An array literal consists of square brackets
containing zero or more expressions. Note that, unlike in GDScript,
elements in an array literal are *not* separated by commas.

Dictionaries
------------

.. code-block:: ebnf

   dict-expr = "{", {prefixed-expr, prefixed-expr}, "}" ;

A dictionary expression is a collection of an even number of
expressions, enclosed in curly braces. The first element of each pair
of expressions is a key and the second is a value in the resulting
dictionary. It is an error to have a brace-enclosed collection of an
*odd* number of expressions.


Vectors
-------

.. code-block:: ebnf

   vector-expr = "V{", prefixed-expr, prefixed-expr, [prefixed-expr], "}" ;

Vectors are used in Godot to represent objects in 2D or 3D space. A
vector literal in GDLisp consists of two or three (depending on
dimension) expressions delimited by ``V{`` and ``}``.

Vectors can also be constructed (non-literally) by the ``vector``
built-in function. This can be useful in macro expansion, where
returning a literal vector object can introduce complexity.

Nested Names
------------

.. code-block:: ebnf

   nested-name = expr, ":", symbol ;
   nested-node-path = expr, ":", node-path ;
   self-name = "@", symbol ;
   self-node-path = node-path ;

Godot is built on an single-inheritance, messaging-passing
object-oriented paradigm. This means that it's very common to call a
function *on* an object, not just an independent function that exists
in the abstract.

In GDLisp, the equivalent to the GDScript "dot" operator is the
``access-slot`` built-in special form. That is, ``foo.bar`` in
GDScript would be translated to the GDLisp ``(access-slot foo bar)``.
This is relatively awkward and verbose to write all the time, so
several shortcuts are provided. These are purely syntax sugar and are
*not* new data structures in the abstract syntax tree of the
language.

The following translations are made.

1. ``foo:bar``, with a literal colon in the middle, is translated to
   ``(access-slot foo bar)``. The left-hand side can be any
   (non-prefixed) expression, and the right-hand side must be a symbol
   literal. Note that a prefix in front of the left-hand side will be
   parsed at a lower precedence than the ``:``, so ``'a:b`` will be
   parsed as ``(quote (access-slot a b))``, not ``(access-slot
   (quote a) b)``. If the precedence is not to your liking, then you
   can always write out the S-expressions by hand rather than using
   the syntax sugar, and sometimes this is necessary in complex macro
   expansions.

2. ``foo:$bar`` translates to a function call equivalent to
   ``(foo:get-node "bar")``. Written in full generality, this is
   ``((access-slot foo get-node) "bar")``. Internally, the *actual*
   expression calls a GDLisp built-in function that allows for better
   optimization potential, but the effect is the same as long as your
   custom nodes do not override ``get-node``. The right-hand side must
   be a node path literal, either quoted or unquoted.

3. ``@bar`` translates to ``(access-slot self bar)``, or equivalently
   ``self:bar``.

4. A node path literal on its own ``$bar`` translates to a function
   call equivalent to ``((access-slot self get-node) "bar")``. As with
   the nested form, ``@$bar`` does not *literally* translate to the
   latter, instead factoring through a GDLisp middleman for
   optimization purposes.

Prefixes
--------

.. code-block:: ebnf

   prefixed-expr = [ prefix ], expr ;
   prefix = "'" | "#'" | "`" | "," | ",." ;

Expressions, in general, can have a single prefix applied to them.
Each prefix is mere syntax sugar for some slightly more complicated
S-expression form. The semantics of these forms are explained in
future sections, but the translations are defined here.

* ``'foo`` translates to ``(quote foo)``

* ``#'foo`` translates to ``(function foo)``

* ```foo`` translates to ``(quasiquote foo)``

* ``,foo`` translates to ``(unquote foo)``

* ``,.foo`` translates to ``(unquote-spliced foo)``

Comments
--------

There are two types of comments in GDLisp. Both are ignored entirely
by the implementation and will *not* be present in the compiled
GDScript code.

Line comments begin with a semicolon ``;`` and continue until the next
carriage return or linefeed character, or until the end of the file.
Conventionally, line comments which occupy the *entire* line will be
written with two semicolons, so it's common to see a pair of
semicolons denote a line comment. But this is merely a convention and
is, as far as the GDLisp parser is concerned, an irrelevant
distinction.

Block comments begin with ``#|`` and continue until the next ``|#``.
Note that block comments cannot be nested, so additional leading
sequences of ``#|`` inside a block comment will be ignored, not paired
off against matching delimiters.
