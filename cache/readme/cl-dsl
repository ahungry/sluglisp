cl-dsl
======

Easily define domain specific languages.

        ;; define auxillary macros and functions
        MY-PACKAGE> (defmacro foo () ...)
        MY-PACKAGE> (defun bar () ...)
        ;; define a master-macro of your DSL
        MY-PACKAGE> (defmacro dsl (with-macrolets (foo) (with-flets (bar) ....)))
        ;; now it can be used like this, no need to explicitly import FOO
        CL-USER>    (my-package:dsl (foo 1 2 3))
        ;; Outside MY-PACKAGE:DSL FOO may well mean something else.
        CL-USER>    (foo "blah blah blah")

Domain specific language usually consists of a set of certain macros and functions.
Usually almost all of these macros and functions make sense only when used inside
a "master-macro" form (Characteristic example of this is ITERATE - domain specific language for iteration).

If one wants to deploy his newly written DSL as a package, one faces a problem: how to
make all these symbols accessible in other packages.

There are several obvious choices:
  - put all relevant symbols into :EXPORT array of DEFPACKAGE, and clobber the namespace of target package.
    Also, using your DSL will require :USE-ing your package.
  - implement all the relevant macros and functions as MACROLETs and FLETs in the master macro.
    There is still some trouble with import of symbols in this approach, which can be cured
    by using MACROLET!'s and FLET!s from DEFMACRO-ENHANCE package.
    However, macros implemented as macrolets are much more harder to debug, since MACROEXPAND-1 does not
    work on them

This package provides third, I believe, smarter, choice.
It exports two utility functions - WITH-MACROLETS and WITH-FLETS.
Then you define your DSL as follows:
  - implement all your auxillary macros and functions with DEFMACROs and DEFUNs in your package
  - export only symbol for your main macro from your package
  - in main macro use WITH-MACROLETS and WITH-FLETS to define macrolets and flets in the
    macro user's package which will expand into macros and functions in your package




