cl-splicing-macro
=================

Features of this package are defmacro-enhance useable! (I.e. in DEFMACRO! from DEFMACRO-ENHANCE
system both sampling expansion and splicing expansion can be used)

This package adds a possibility for macros to inject several forms
(not just one) into surrounding macros's body.

```lisp
CL-SPLICING-MACRO> (defmacro bar (&rest things)
                     "Just a toy macros, that expands into a PLUS"
                     `(+ ,@things))
CL-SPLICING-MACRO> (define-maybe-splicing-macro foo (a b &sample (1 2))
                     "Just a toy macro that will splice in the body of surrounding macro."
                     `(sprogn ,a ,b))
CL-SPLICING-MACRO> (macroexpand-1 '(bar 1 2 (foo 3 4) 5 6))
(+ 1 2 3 4 5 6) ; enjoy!
```

Sampling macroexpansion
-----------------------

Whether a given macros is splicing or not is decided on basis of whether its
expansion contains SPROGN in CAR-position, or not.
For this to work, we need a "characteristic expansion" of this macros, or
a "testing expansion".

New magic word &SAMPLE in macro-lambda-list is used
to provide parameters of a macro, that result in generic-enough expansion
(against which checks on the structure of the expansion can be made).

Sampling expansion for a given macros, if present, can be obtained with help
of TESTING-EXPANSION macros.

```lisp
CL-SPLICING-MACRO> (define-maybe-sampling-macro my-macro (a &sample (qwerty))
                     ...) ; define-my-clever-macro
CL-SPLICING-MACRO> (if (indeed-clever-expansion-p (testing-expansion my-macro))
                       do-something-useful)
```

By default, DEFINE-MAYBE-SAMPLING-MACRO is a wrapper around plain DEFMACRO, but
DEFINE-/SAMPLING! macro-construction macros can be used to add sampling
feature to your favorite DEFMACRO-like operator (e.g. DEFMACRO! from Let-Over-Lambda).
```lisp
CL-SPLICING-MACRO> (define-/sampling! def-my-fav-macro-sampling def-my-fav-macro)
```

