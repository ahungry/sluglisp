cl-indeterminism
================

Do some cool stuff with undefined functions and variables.

By now, 'cool stuff' includes:
  - FIND-UNDEFS - described in section "Finding"
  - MACROEXPAND-ALL-TRANSFORMING-UNDEFS - described in section "Transforming"
  - MACROEXPAND-CC-ALL-TRANSFORMING-UNDEFS - described in section "Transforming at compile-time"

Finding
-------

Codewalk the form and find, which variables and functions are undefined.

```lisp
CL-USER> (ql:quickload 'cl-indeterminism)
CL-USER> (cl-indeterminism:find-undefs '(foo bar baz))
((:FUNCTIONS FOO) (:VARIABLES BAZ BAR))
```

FIND-UNDEFS is now a macro which expands in the surrounding context, making it possible to
catch undefined variables relative to the current lexenv.

```lisp
CL-USER> (let ((a 1)) (cl-indeterminism:find-undefs 'a))
((:FUNCTIONS) (:VARIABLES))
```

Still, one can explicitly specify to find undefs with respect to top-level environment

```lisp
CL-USER> (let ((a 1)) (cl-indeterminism:find-undefs 'a :env :null))
((:FUNCTIONS) (:VARIABLES A))
```

Can be also used to find list of undefined variables at compile time

```lisp
(defmacro list-the-undefs (&body body &environment env)
  `(list ,@(mapcar (lambda (x) `(quote ,x))
		   (find-undefs `(progn ,@body) :env env))))
(let ((bar 1)) (declare (ignore bar)) (find-undefs '(foo bar baz)))
((:FUNCTIONS FOO) (:VARIABLES BAZ))
```


Uses profound HU.DWIM.WALKER system to do the heavy lifting of code walking
and is, in fact, just a convenience wrapper around it.

Note: if your implementation is not supported by HU.DWIM.WALKER or CL-CURLEX,
then branch "without-curlex" is for you - there initial unsophisticated behaviour
 of the system is fixed forever.


Transforming
------------

Codewalk the form and transform undefined variables and functions in it on the fly to something else.
Has a side effect of expanding all the macros in the form.

```lisp
CL-USER> (ql:quickload 'cl-indeterminism)
CL-USER> (let ((cl-indeterminism:*variable-transformer* (lambda (x) `(quote ,x))))
           (cl-indeterminism:macroexpand-all-transforming-undefs '(a b c)))
(A 'B 'C)
CL-USER> (let ((cl-indeterminism:*function-transformer*
                (lambda (form)
                  (if (keywordp (car form))
                      (cl-indeterminism:fail-transform)
                      `(,(intern (string (car form)) "KEYWORD")
                        ,@(cdr form))))))
           (cl-indeterminism:macroexpand-all-transforming-undefs '(a b c)))
(:A B C)
```

The API consists of the following ingredients:

  - MACROEXPAND-ALL-TRANSFORMING-UNDEFS - macro, which actually does the transformation.
    Accepts optional keyword :ENV, which can be :NULL or :CURRENT (default) and acts the same as in FIND-UNDEFS.
  - \*VARIABLE-TRANSFORMER\* - NIL, or a reference to a function, which accepts variable name, and
    returns arbitrary lisp-form.
    This function is used to transform all undefined variables. If NIL, variables are left as-is, nothing happens.
    If non-NIL, the returned form is recursively walked, so all undefined variables and
    function in it are also expanded. Beware of infinite recursions, use FAIL-TRANSFORM to prevent it!
  - \*FUNCTION-TRANSFORMER\* - analogous to \*VARIABLE-TRANSFORMER\*, only handles transformation of
    undefined functions. When NIL, undefined functions are left as-is.
    When non-NIL, expected to accept entire form on undefined function call and return arbitrary lisp-form.
    The returned form is recursively walked, so all undefined variables and
    function in it are also expanded. Beware of infinite recursions, use FAIL-TRANSFORM to prevent it!
  - FAIL-TRANSFORM - a macro, which can be used inside functions, to which \*VARIABLE-TRANSFORMER\* and
    \*FUNCTION-TRANSFORMER\* are bound.
    When invoked, causes transformation not to be performed, that is undefined variable/function call is left
    as-is. Useful to prevent infinite recursions, as in example above, when transformation on
    unknown function is not performed if its name is already a keyword.

I personally plan to use all this machinery in my fork of ESRAP packrat parser (https://github.com/mabragor/esrap).
There, the idea is to transform all undefined variable names inside DEFRULE macro into calls to functions,
which try to continue parsing with help of a rule, corresponding to the name of undefined variable.
See branch 'liquid' there for examples of usage of MACROEXPAND-ALL-TRANSFORMING-UNDEFS.

Transforming at compile-time
----------------------------

Works similar to **transforming** at runtime, but is far more interesting, isn't it?

```lisp
CL-USER> (ql:quickload 'cl-indeterminism)
CL-USER> (defmacro autoquote (form &environment env)
           (let ((cl-indeterminism:*variable-transformer* (lambda (x) `(quote ,x))))
             (cl-indeterminism:macroexpand-cc-all-transforming-undefs form :env env)))
;; So, our AUTOQUOTE macro automatically quotes
;; all the undefined variables inside its form
CL-USER> (defun foo ()
           (autoquote (list b c)))
CL-USER> (foo)
(B C)
;; And it takes into account lexical environment
;; it is expanded in, so here B is not quoted
CL-USER> (let ((b 1))
           (defun foo ()
             (autoquote (list b c))))
CL-USER> (foo)
(1 C)
```

Note, that lexical environment has to be explicitly fetched with help of &ENVIRONMENT keyword
of the enclosing macro and passed to MACROEXPAND-CC-ALL-TRANSFORMING-UNDEFS.
This is due to my failure to fetch this compile environment in some implementations
with some other tricks.

TODO:
-----

  - (done) find undefined variables with respect to current lexenv, not null lexenv
    - (done) allow to specify null lexenv
  - (done) macro to manipulate undefined functions and variables conveniently

BUGS & GOTCHAS:
-----

  - MACROLETs in the enclosing scope are not handled correctly

        CL-USER> (macrolet ((bar (b) nil)) (cl-indeterminism:find-undefs '(bar a)))
        ((:FUNCTIONS) (:VARIABLES A))

    although it should see that variable A is not really there
    (hence, it does not go into the body of BAR). This is due to the limitation of
    CL-CURLEX, where only names of functions, variabes and macros are captured, not their
    definitions.
  - HU.DWIM.WALKER, which is used in implementation of MACROEXPAND-ALL-TRANSFORMING-UNDEFS
    and MACROEXPAND-CC-ALL-TRANSFORMING-UNDEFS does not know about some declarations,
    specific to compilers, such as TRULY-DYNAMIC-EXTENT. This results in tons of style
    warnings on standard output. So, for now I simply kludged to muffle those.
    However, if you suspect that macroexpansion is done wrong, you maybe should
    enable those warnings again, look for clues, and then maybe send a patch :)

