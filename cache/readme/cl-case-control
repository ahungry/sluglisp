cl-case-control
====

This package supports case-controlling in both "starndard" case-insensitive environment and "extended" case-sensitive environment.

Note that the ANSI Common Lisp is a case-sensitive language about symbol names.

    (intern "AbC") ; => |AbC|
    (symbol-name '|AbC|) ; => "AbC"

The term "*case-insensitive*" here is not for those "*barrier*" symbols but for "*non-barrier*" symbols on some extended CL implementations s.t. GNU CLISP.

    (in-package :cs-common-lisp-user)
    (intern "Abc") ; => Abc
    (symbol-name 'Abc) ; => "Abc"
    (in-package :common-lisp-user)
    (symbol-name 'Abc) ; => "ABC"
    (symbol-name 'ABC) ; => "ABC"

Those differences between standard "*case-insensitive*" mode and extended "*case-sensitive*" modes cause some troubles at coding.

This package is a helper for solving such problems.

## APIs:

### general-purpose predicates:

* *[Function]* `string-designator-p`

  Returns *true* if given object is a string designator, otherwise, returns *false*.

* *[Function]* `lowercase-char-p` and `uppercase-char-p`

  Returns *true* if given character is a lowercase (uppercase), otherwise, returns *false*.

  If the given object is not a character, these predicates signal an type error. 

### general-purpose converters:
* *[Function]* `char-invertcase`

  Returns case inverted character.

  If the given object is not a character, `char-invertcase` signals an type error.

* *[Function]* `string-invertcase`

  Returns case inverted string. 

  If the given object is not a string designator, `string-invertcase` signals an type error.

  Note: `string-invertcase` accepts not only string but also string designator,
  but applying `string-invertcase` to symbol is depricated because an intuitive result is not necessarily obtained. 

  e.g.

        (string-invertcase "aBc") ; => "AbC"
        (string-invertcase #\a) ; => "A"
        (string-invertcase :|aBc|) ; => "AbC"
        (string-invertcase NIL) ; => "nil"
        (string-invertcase Nil) ; => "nil"
        (string-invertcase nil) ; => "nil"

  Especially on GNU CLISP environment;

        (in-package :common-lisp-user)
        (symbol-name 'Abc) ; => "ABC"
        (casectl:string-invertcase 'Abc) ; => "abc"
        (in-package :cs-common-lisp-user)
        (symbol-name 'Abc) ; => "Abc"
        (casectl:string-invertcase 'Abc) ; => "Abc"
        (symbol-name :Abc) ; => "abc"
        (casectl:string-invertcase :Abc) ; => "abc"

  (`casectl:char-invertcase` and `casectl:string-invertcase` are just synonyms of exported functions of EXT package of CLISP, respectively.

  For CLISP's case sensitiveness, see CLISP Implementation Notes [11.5. Package Case-Sensitivity](http://clisp.org/impnotes/package-case.html))

### predicates for system/package case sensitiveness:

* *[Function]* `case-sensitive-p`

  Returns *true* if the system (compiler and/or interpreter) supports case-sensitiveness, otherwise, returns *false*.

* *[Function]* `case-sensitive-package-p`

  Returns *true* if the given package supports case-sensitiveness, otherwise, returns *false*.

  Current `*package*` is used when an package parameter is ommited.

### utilities to unify codes in both case-sensitive environment and case-insensitive environment:

* *[Function]* `adj-case`

  Adjust string case. This function is a helper for making intern parameter.

  e.g.

        (in-package :a-case-insensitive-package)
        (setf (symbol-value (intern (adj-case "Foo"))) 1)
        (in-package :a-case-sensitive-package)
        (setf (symbol-value (intern (adj-case "Foo"))) 2)

        (in-package :another-package)
        ;; Is the :another-package case-sensitive or not?
        ;; No problem. Everything's fine.
        (list a-case-insensitive-package::Foo
              a-case-sensitive-package::Foo )
        ; => (1 2)

  `adj-case` can also spcify a package explicitly.

        (adj-case "Foo" :a-case-insensitive-package) ; => "FOO"
        (adj-case "Foo" :a-case-sensitive-package) ; => "Foo"

   WARNING: on GNU CLISP, specifying package explicitly
   in both `adj-case` and `intern` may cause error because of incomaptibility between cl:intern and cs-cl:intern.

        (in-package :a-case-insensitive-package)
        ;; no problem
        (intern (adj-case "Foo" :another-case-insensitive-package)
                :another-case-insensitive-package )
        ; => ANOTHER-CASE-INSENSITIVE-PACKAGE::FOO

        ;; maybe a bug
        (in-package :a-case-sensitive-package)
        (intern (adj-case "Bar" :a-case-insensitive-package)
                :a-case-insensitive-package )
        ; => a-case-insensitive-package::|bar|

* *[Function]* `case-selective-intern`

  A `intern` wrapper.

  e.g. `(case-selective-intern "Foo")`

  And also, you can specify intern-package optionally.


* *[Macro]* `case-selective-defpackage`

  A `defpackage` wrapper.

  e.g.

        (case-selective-defpackage :package-name
          (and (case-sensitive-p)
               (would-you-want-to-make-case-sensitive-package-p) )
          (:nicknames ...)
          ... )

## History
* removed `string-designator` definition; use `trivial-types` package, instead.

## License:
Under MIT license.
