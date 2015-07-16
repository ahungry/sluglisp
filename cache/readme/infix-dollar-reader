infix-dollar-reader
===================

Infix `$` operator for reducing redundant paranthesis.

### usage:

1. load package;

        (asdf:load-system :infix-dollar-reader)

2. enable to use infix `$` notation;

        (syntax:use-syntax :infix-dollar)

3. write your forms with infix `$` notation s.t. `(the fixnum $ + x 10)`

### example:

        (fun x y $ fun2 z)

This form above would be expanded to below form.

        (fun x y (fun2 z))

And also,

        (= (+ 1 2 $ * 3 4 $ + 5 6) (+ 1 2 (* 3 4 (+ 5 6)))) ; => T

### limitation:

A symbol name which has any `$` characters would cause unexpected behavior.

You should not use any `$` characters in a symbol name without barrier or intern from a string.

e.g.

        (syntax:use-syntax :infix-dollar)

        (defconstant foo$- 1) ; => FOO
        ;; would be parsed as (defconstant foo(- 1))

        (defconstant |FOO$-| 1) ; => FOO$- or |FOO$-|
        ;; not be parsed a #\$ as a #\( because of barrier

        (intern "FOO$-") ; => FOO$-
        ;; #\$, $-in-a-string and $-in-a-comment are not affected

### license:

infix-dollar-reader is under MIT license.
