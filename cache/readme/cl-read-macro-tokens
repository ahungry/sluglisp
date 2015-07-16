cl-read-macro-tokens
====================

Why should things, that affect Lisp-reader, be limited to special characters (called macro-characters)?
I want arbitrary symbols to be able to alter reader behavior!

This package adds possibility to define 'macro-tokens'.

Macro-tokens
------------

Macro-token is any symbol, which happens to be in CAR-position of a list.

Example:
```lisp
CL-USER> (ql:quickload 'cl-read-macro-tokens)
CL-USER> (cl-read-macro-tokens:enable-read-macro-tokens)
CL-USER> (setf (gethash 'foo cl-read-macro-tokens:*read-macro-tokens*)
               ;; Very simple custom reader, which ignores token altogether
               (lambda (stream token)
                 (cl-read-macro-tokens:read-list-old stream token)))
CL-USER> '(foo 1 2 3)
(1 2 3)
CL-USER> (cl-read-macro-tokens:disable-read-macro-tokens)
CL-USER> '(foo 1 2 3)
(FOO 1 2 3)
;; *READ-MACRO-TOKENS* is obviously preserved across enables/disables
CL-USER> (cl-read-macro-tokens:enable-read-macro-tokens)
CL-USER> '(foo 1 2 3)
(1 2 3)
```
The package works by redefining a function, which is associated to #\( macrocharacter in the readtable.
Both ENABLE-READ-MACRO-TOKENS and DISABLE-READ-MACRO-TOKENS can be given optional READTABLE argument,
which defaults to \*READTABLE\*.

When read-macro-tokens are enabled, one can temporarily disable them, using
WITH-NO-READ-MACRO-TOKENS or WITH-NO-READ-MACRO-TOKENS1 read-macro token.

```lisp
CL-USER> (setf (gethash 'foo cl-read-macro-tokens:*read-macro-tokens*)
               ;; Very simple custom reader, which ignores token altogether
               (lambda (stream token)
                 (cl-read-macro-tokens:read-list-old stream token)))
CL-USER> '(foo 1 (cl-read-macro-tokens:with-no-read-macro-tokens (foo 2) 3))
(1 (progn (foo 2)) 3)
CL-USER> '(foo 1 (cl-read-macro-tokens:with-no-read-macro-tokens1 (foo 2) 3))
(1 (foo 2) 3)
```

The former one expands into progn-form, while the other into ordinary list-form.
Hence, former is used when defining code, and latter when defining data.

Read-macros revisited
---------------------

What if you had a macro, that at the same time would be able to modify the lisp-reader in some way?
I.e., what if you had a macro, that is at the same time read-macro token?

DEFMACRO!! macro lets you define such objects.

```lisp
CL-USER> (defmacro!! nihilling-vectors (&body body)
           (let ((*readtable* (copy-readtable))
                 (stash-function (get-dispatch-macro-character #\# #\()))
             (set-dispatch-macro-character #\# #\(
                                           (lambda (stream char subchar)
                                             (funcall stash-function stream char subchar)
                                             nil))
             (call-next-method))
           "Vectors inside the body of this macro are read in as NILs"
           `(progn ,@body))
CL-USER> (nihilling-vectors '(#(1 2 3)))
(NIL)
```

There is an important subtlety with read-macro-tokens. Namely, if since they affect
lisp-reader, which knows nothing about macroexpansions, if I naively define a macro,
whose expansions contains read-macro-tokens, things will not work as expected.

```lisp
CL-USER> (defmacro mask-nihilling-vectors (&body body)
           `(nihilling-vectors ,@body))
CL-USER> (mask-nihilling-vectors '(#(1 2 3)))
'(#(1 2 3))
```

The reader modification behaviour is lost.

However, DEFMACRO!! takes special care to scan its body and inherit all reader-macro
features of tokens, found there. So

```lisp
CL-USER> (defmacro!! nomask-nihilling-vectors (&body body)
           `(nihilling-vectors ,@body))
CL-USER> (nomask-nihilling-vectors '(#(1 2 3)))
'(NIL)
```

If there are more than one distinct token in the macroexpansion, the new macro inherits
from them all. In fact, it uses CLOS under the hood, do to all this stuff.

READ-MACROLET, WITH-MACRO-CHARACTER and WITH-DISPATCH-MACRO-CHARACTER
----------------------------------------------------------------------

OK, since we now may modify reader's behaviour in more ways, let's add few more conveniencies.

READ-MACROLET lets you define read-macro-tokens, that are valid only inside body of
some other read-macro-token

```lisp
CL-USER> (defmacro!! my-reader-context (&body body)
           ;; Define the behaviour with respect to the reader
           (read-macrolet ((reader-context #'reader-context-handler)
                           (reader-context-2 #'reader-context-handler-2))
             (call-next-method))
           ;; Define, how read-in macro would be expanded
           `(progn ,@body)
```

Usually these macro-token contexts serve to temporarily change meaning of macro-characters.
For this WITH-MACRO-CHARACTER and WITH-DISPATCH-MACRO-CHARACTER become handy.

```lisp
;; Somewhere inside READ-MACROLET
(reader-context (lambda (stream token)
                  (with-macro-character (#\[ #'some-clever-function)
                    (with-macro-character (#\# #\. #'some-even-more-clever-function)
                      ;; So, inside READER-CONTEXTs body meaning of '[' and '#.' would be different from
                      ;; the outside meaning.
                      `(progn ,@(read-list-old stream token))))))
```

See NIHILLING-CHARS-AND-STRINGS in macro-tests.lisp for a full example using all these macro.


SET-MACRO-TOKEN-READER
----------------------

If you want something more subtle than establishing new reader rules for all subforms of a given
form, then DEFMACRO!! won't do.
Or, if you want to define something with custom reader properties, which is not a macro from
codewalker point of view (e.g., a function)

For this you may use SET-MACRO-TOKEN-READER (example is taken from ESRAP-LIQUID)
```lisp
(flet ((the-handler (stream token)
  (let ((expression (with-esrap-reader-context
                      (read stream t nil t))))
    `(,cl-read-macro-tokens::the-token ,expression ,@(read-list-old stream token)))))
  (set-macro-token-reader parse #'the-handler))

CL-USER> (parse "a" "a")
(PARSE (descend-with-rule 'string "a") "a")
```

As you can see, first form of the subforms is read using special reader rules
(established by ESRAP-READER-CONTEXT macro), while all the others are read as usual.

The other thing to note is THE-TOKEN symbol. It is a SYMBOL-MACRO, which expands to the
name of the token the handler is used for.

Beware! Gotchas!
----------------

  - now only very simple mechanism of analyzing the body of a macro is implemented.
    Namely, the body is FLATTENed and all the symbols are tested on READ-MACRO-TOKEN properties,
    not only those in CAR-positions.
    So, if you have read-macro-token FOO defined, and in macroexpansion of
    MACRO!! BAR there appears, say, variable FOO, then BAR mysteriously inherits
    read-macro-token properties of FOO.
    I know this is a bug, but a correct version requires codewalking without macroexpansion of
    the body, with collecting symbols in car-positions.
    That is to say, patches that cure this are very welcome.
  - For some mysterious reason things do not work right, if you use
    in DEFMACRO!!'s body macro, which were DEFMACRO!!-ed in the same file.
    This is why in tests to this system macro are contained in two separate files - macro-tests.lisp
    and macro2-tests.lisp
    The macro in the second file expand into macro, defined in the first.
    

TODO:
-----

  - Make DEFINE-READ-MACRO actually work.
  - (done) Make a convenience wrapper around DEFMACRO, which allows to define macros,
    which are at the same time read-macro-tokens.
    They should scan their body, and if they contain other such macro-read-macro-tokens,
    they should inherit read-macro-token function from them.
    - Rewrite things, so that you can add read-macro functionality to any DEFMACRO-like macro.
    - When DEFMACRO!! scans for parent read-macro, it should only consider symbols in CAR-position, not
      all of them
  - Support all major implementations
    - (done) SBCL
    - (done) CMUCL
    - (done) Clozure
    - ECL - reader written in C, hard to rehack
    - CLISP - reader written in C, hard to rehack

All in all, in order to do ECL and CLISP, it is easier to write a separately packaged lisp-reader,
e.g. based on SBCL's one and then use e.g. READER-INTERCEPT system to substitute ECL's and CLISP's
C-ish readers with lispy one.