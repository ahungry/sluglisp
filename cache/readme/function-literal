Function-Literal
================

Clojure inspired Common Lisp function literal read macro

This file contains a read macro allowing for function
literals in Common Lisp similar to those in Clojure,
for example:

    (mapcar #f(- % %2) '(1 2 3 4) '(5 6 7 8))

is equivalent to:

    (mapcar (lambda (a b) (- a b)) '(1 2 3 4) '(5 6 7 8))

Note however that unlike function literals seen in Clojure,
these can be nested freely, as in:

    ; function that doubles the squared value
    #f(* 2 (#f(* % %) %))

Note also that *Let over Lambda's* `Sharp Backquote` is a natural
consequence of this read macro as can be seen below. The only difference is the `f` after the `#` to prevent a conflict with Common Lisp's built in vector read-macro.

    (mapcar #f`(,% ,%) '(A B C))

yielding:

    ((A A) (B B) (C C))

Constant functions are another natural use of this macro.

    (mapcar #f10 (loop for x from 1 upto 10 collect x))

yielding:

    (10 10 10 10 10 10 10 10 10 10)

#License

This software falls under the WTFPL.
