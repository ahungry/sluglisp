cl-curlex
===========

Leak \*LEXENV\* variable which describes current lexical environment into body of a call.
Update: do other exotic stuff, which requires access to the current lexical environment.

Basic example:

```lisp
CL-USER> (ql:quickload 'cl-curlex)
CL-USER> (cl-curlex:with-current-lexenv *lexenv*)
#S(SB-KERNEL:LEXENV
:FUNS NIL
:VARS NIL
:BLOCKS NIL
:TAGS NIL
:TYPE-RESTRICTIONS NIL
:LAMBDA #<SB-C::CLAMBDA
        :%SOURCE-NAME SB-C::.ANONYMOUS.
        :%DEBUG-NAME (LAMBDA ())
        :KIND NIL
        :TYPE #<SB-KERNEL:FUN-TYPE (FUNCTION NIL (VALUES T &OPTIONAL))>
        :WHERE-FROM :DEFINED
        :VARS NIL {AE26BA9}>
:CLEANUP NIL
:HANDLED-CONDITIONS NIL
:DISABLED-PACKAGE-LOCKS NIL
:%POLICY ((COMPILATION-SPEED . 1) (DEBUG . 1) (INHIBIT-WARNINGS . 1)
          (SAFETY . 1) (SPACE . 1) (SPEED . 1))
:USER-DATA NIL)
```

Slightly more sophisticated:

```lisp
CL-USER> (let ((a 1)) (cl-curlex:with-current-lexenv (let ((b 1)) *lexenv*)))
#S(SB-KERNEL:LEXENV
  :FUNS NIL
  :VARS ((A . #<SB-C::LAMBDA-VAR :%SOURCE-NAME A {AEA1371}>))
  :BLOCKS NIL
  :TAGS NIL
  :TYPE-RESTRICTIONS NIL
  :LAMBDA #<SB-C::CLAMBDA
          :%SOURCE-NAME SB-C::.ANONYMOUS.
          :%DEBUG-NAME (LET ((A 1))
                              )
          :KIND :ZOMBIE
          :TYPE #<SB-KERNEL:BUILT-IN-CLASSOID FUNCTION (read-only)>
          :WHERE-FROM :DEFINED
          :VARS (A) {AEA14B1}>
  :CLEANUP NIL
  :HANDLED-CONDITIONS NIL
  :DISABLED-PACKAGE-LOCKS NIL
  :%POLICY ((COMPILATION-SPEED . 1) (DEBUG . 1) (INHIBIT-WARNINGS . 1)
            (SAFETY . 1) (SPACE . 1) (SPEED . 1))
  :USER-DATA NIL)
```

Example of use of ABBROLET:

```lisp
CL-USER> (macrolet ((bar () 123))
           (cl-curlex:abbrolet ((foo bar))
             (foo)))
123
```

Package exports several macros that do interesting stuff with current lexical environment:
  - FART-CURRENT-LEXENV which simply prints current lexenv, but not leaks it into its body,
  - WITH-CURRENT-LEXENV, which leaks \*LEXENV\* variable into its body
  - WITH-CURRENT-CC-LEXENV - like WITH-CURRENT-LEXENV, only useful when defining macros.
    I.e., instead of leaking \*LEXENV\* into the run-time from compile-time, it makes
    lexical environment accesible to macros via \*LEXENV\* at compile-time.
  - ABBROLET, which allows to locally function or macro with another name

N.B.: leaking is for reading purposes only, and \*LEXENV\* captures state of lexical environment as it were on enter
to WITH-CURRENT-LEXENV, not as it is when \*LEXENV\* var is used - this is why in the "sophisticated" example
B variable is not seen in \*LEXENV\* - it was not there, when we entered WITH-CURRENT-LEXENV, it was binded somewhere
inside.

N.B.: Although the ultimate goal is to leak lexenv in all major implementations, the form of a \*LEXENV\*
will be (intentionally) implementation specific.

N.B.: Is WITH-CURRENT-CC-LEXENV really necessary? After all, ANSI standard specifies, that one of the
arguments to \*MACROEXPAND-HOOK\* function is lexical environment, in which macro is expanded.
However, what it does not specify, is a way for a macro to actually access that variable, when macro
is defined using DEFMACRO. And in fact, SBCL safely gensyms this variable away.
So, the one, who wishes to access \*LEXENV\* in macros has 2 choices:
  - Roll his own implementation of DEFMACRO, which leaks \*LEXENV\* to all macros.
    This is implementation-dependent.
  - Write something like WITH-CURRENT-CC-LEXENV, which in principle allows to access \*LEXENV\*
    inside macro-definition, but this is not the default for all macro. This is also implementation-dependent.
I decided to go second way, since it requires far less code and is, in my opinion, cleaner.
    

Main use-case for ABBROLET is to locallly INTERN some function or macro from one package to another,
see e.g. my CL-LARVAL project to see, how it's used to define DSL *locally*.

        CL-USER> (abbrolet ((name1 other-package::name2))
                   (name1 a b c))

TODO:
  - support major CL implementations for \*-current-lexenv macros
    - SBCL
      - lexenv capture is not full, only names of functions, variables and so on are captured,
        advanced features like package locks and policies are not captured
    - (wont, don't have an access to sources or other spec of non-standard features) Allegro
    - CLISP
      - there is no centralized concept of lexical environment - dont know, what to do
      - even if there was one - WITHOUT-PACKAGE-LOCKS does not seem to work right - 
        it converts every symbol to T, thus rendering patching of compiler impossible.
    - CMUCL
      - same as SBCL, only names of variables, functions and so on are captured.
    - ECL
      - actual values of variables, functions etc are also stripped
      - I also removed :DECLARE's and 'LBs, since I don't know how to handle them smartly
      - WITH-CURRENT-LEXENV and FART-CURRENT-LEXENV work only when compiled, not from the Slime's REPL.
        Although compiled function can, of course, be invoked from the REPL, with the desired results.
    - LispWorks - probably won't have the sources either
    - OpenMCL (Clozure CL)
      - only variables and functions are captured
  - support major CL implementations for ABBROLET
    - SBCL done
    - CMUCL done
    - ECL - everything, except abbroletting global functions, done
    - CCL - everything, except abbroletting global functions, done
