defmacro-enhance
================

Enhancement of `defmacro` in spirit of [let-over-lambda](http://www.letoverlambda.com/index.cl/toc).

Following features are supported:

1.  `g!-symbols` in body of a macro become gensyms:

    ```lisp
    CL-USER> (defmacro! foo (bar)
              `(let ((,g!-bar ,bar))
                 asdf))
    FOO
    CL-USER> (macroexpand-1 '(foo (+ 1 2)))
    (LET ()
      (LET ((#:BAR1115 (+ 1 2)))
        ASDF))
    T
    CL-USER>
    ```

2.  `o!-symbols` in the lambda-list of a macro become once-only arguments

    ```lisp
    CL-USER> (defmacro! square (o!-x)
               `(* ,o!-x ,o!-x))
    SQUARE
    CL-USER> (defparameter a 1)
    A
    CL-USER> (square (incf a))
    4
    CL-USER> (square (incf a))
    9
    CL-USER>
    ```

3.  `e!-symbols` in the body of the macro are interned in the package,
where macro is *expanded*, not where it is defined

    ```lisp
    CL-USER> (in-package defmacro-enhance)
    #<PACKAGE "DEFMACRO-ENHANCE">
    DEFMACRO-ENHANCE> (defmacro! aif (test then &optional else)
                        `(let ((,e!-it ,test))
                           (if ,e!-it
                               ,then
                               ,@(if else `(,else)))))
    AIF
    DEFMACRO-ENHANCE> (in-package cl-user)
    #<PACKAGE "COMMON-LISP-USER">
    CL-USER> (macroexpand-1 '(defmacro-enhance::aif (+ 1 2) it 6))
    (LET ()
      (LET ((IT (+ 1 2)))
        (IF IT
            IT
            6)))
    T
    CL-USER> (defmacro-enhance::aif (+ 1 2) it 6)
    3
    CL-USER>
    ```

As is noted in the header of let-over-lambda code, all the differences from that
version should be clearly noted.
Here they are:

1.  letter-bang-hyphen symbols are used instead of letter-bang symbols to denote symbols,
    for which something special should be done (E.g `g!-foo` instead of` g!foo`)

2.  In once-only functionality name of a symbol in body of a macro is the same as in the
    lambda-list of arguments. I.e., instead of let-over-lambda's

    ```lisp
    (defmacro square (o!x)
        `(* ,g!x ,g!x)) ; why in the world did g! appeared here? And is o! still accessible?
    ```

    we have

    ```lisp
    (defmacro square (o!-x)
        `(* ,o!-x ,o!-x))
    ```

3.  New, `e!-symbols` are added, that allow to express anaphoric macros more conveniently.

4.  `p!-symbols` can be used to quickly define PROGN-flattening fields.

Few code-duplication was necessary, to make code portable (and not SBCL specific) and to be
able not to depend on RUTILS, since the plan is to rewrite anaphoric utilities there through e!- construction.

`DEFUN!` and `DEFMACRO-DRIVER!`
---------------------------

Not only `DEFMACRO` deserves to be enhanced. Sometimes, use just want to use gensyms and
variable injections in other constructs. Hence, `DEFUN!` and `DEFMACRO-DRIVER!` provide
deformations of original constructs, that understand `G!-` and `E!-` symbols
(There is not much use of O!-symbols for defun - all args are already once-only,
and for defmacro-driver syntax of arguments is very different from generic one, so
I didn't implemented it yet.)

Rolling your own
----------------

Finally, all the abovementioned deformations are constructed with help of the following
underlying building blocks: `DEFINE-/G!` `DEFINE-/O!` and `DEFINE-/E!`.
Basically, these allow you to add understanding of `G!-` `O!`- and `E!-` symbols, respectively,
to any construct you like.
In particular, in the source of this package, defmacro, which understands `O!-` and `G!-`
is implemented on top of `defmacro`, which understands only `G!-` like this:
    
```lisp
(define-/o! defmacro/g!/o! defmacro/g! (name args &body body)
        `(,name ,args ,@body))
```

These building blocks are for now (2013/03/08) not perfect. In particular, they do not
scan lambda-list for ARGS and BODY arguments and simply assume they are there.

SPLICING-MACRO
--------------

`DEFMACRO!` now also supports features from `CL-SPLICING-MACRO` (better see
README there https://github.com/mabragor/cl-splicing-macro)

Simply write your macros to expand into SPROGN-form (and don't forget to specify
sample-macroexpansion parameters with help of &sample lambda-keyword).
The only peculiarity is that this will not work together with once-only (o!-symbols),
since their usage results in `CAR` of the expansion being `LET`, not `SPROGN`.

```lisp
(defmacro! fail-sprogn (o!-x &sample ())
  `(sprogn ,o!-x ,o!-x))

(macroexpand-1 '(fail-sprogn (+ 1 1)))
(LET ((#:G123 (+ 1 1))) ;; not what you would want.
  (SPROGN #:123 #:123))
```

PROGN-aware fields
------------------

Sometimes you want a certain field of your macro to automatically 'flatten'
all the subforms to the 'top-level', if they are inside PROGNs.

```lisp
(your-macros (progn a b (progn c d))) ;; these two are desired
(your-macros a b c d)                 ;; to have identical expansion
```

This you can easily achieve by defining YOUR-MACROS like this (note the P!-)
```lisp
(defmacro! your-macros (&rest p!-params)
  (whatever-here))
```

P!-symbols will also flatten results of expansion of macros, that
happen to expand to `PROGN`.
```lisp
(defmacro foo ()
  '(progn bar baz))

;; these two also have the same expansion
(your-macros foo (foo))
(your-macros foo bar baz)
```

Of course, this feature is kind of 'dual' to `SPLICING-MACRO`,
described earlier.
