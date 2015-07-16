
# Trivialib.Type-Unify

[![Build Status](https://travis-ci.org/guicho271828/trivialib.type-unify.svg)](https://travis-ci.org/guicho271828/trivialib.type-unify)

* Unifies a parametrized type specifier against an actual type specifier. Importantly, it handles complicated array-subtypes and number-related types correctly.
* Dependency to the implementation-dependent feature is MINIMAL, except `typexpand` in introspect-environment.


## Usage

```cl
;;; ------------- type-unify1 --------------
;;; single query variant

(type-unify1 '(a)                ; type variable
             '(array a *)        ; template
             '(array char (3)))  ; actual type specifier
;; '((A . CHAR)), T              ; unification


;;; It is able to infer that the STRING type is actually an abbreviation of (ARRAY CHARACTER *).
;;; note that our code does not depend on an implimentation-dependent TYPEXPAND in this regard.
;;; (== we do not require TYPEXPAND will expand STRING into (ARRAY CHARACTER *).
;;;     For the detailed mechanism, see the source code of TYPE-R.)
(type-unify1 '(a b)
             '(array a b)
             'string)
;; ((A . CHARACTER) (B . *)), T

;;; Nested types 
(type-unify1 '(a b)
             '(cons (cons a b) (cons b a))
             '(cons (cons fixnum string) (cons string fixnum)))
;; ((A . FIXNUM) (B . STRING)), T

;;; deftyped types are expanded by typexpand --- implementation dependent.
;;; Works on major implementation: ccl, sbcl.
;;; Using introspect-environment as a compatibility layer.
(deftype cons2 (x) `(cons ,x ,x))
;; CONS2
(type-unify1 '(a b)
             '(cons2 (cons2 a))
             '(cons (cons fixnum fixnum) (cons fixnum fixnum)))
;; ((A . FIXNUM)), T
(type-unify1 '(a b)
             '(cons (cons a a) (cons a a))
             '(cons2 (cons2 fixnum)))
;; ((A . FIXNUM)), T

;;; number-related types
(type-unify1 '(a) '(integer * a) 'fixnum)
;; ((A . 4611686018427387903)), T

(type-unify1 '(a) '(integer * a) '(unsigned-byte 5))
;; ((A . 31)), T

(type-unify1 '() `(integer ,most-negative-fixnum ,most-positive-fixnum) 'FIXNUM)
;; NIL, T


;;; ------------- type-unify --------------
;;; multi-query variant

(type-unify '(a)
            '(a a)
            '((array * 6) (array char *)))

;; --> '((a . (and (array * 6) (array char *)))), t
;;; We do not try to merge the resulting assignment of unification.

;;; However, we DO detect the disjointness of types.
(type-unify '(a) '(a a) '((array * 6) (array * 7)))        ; -> nil, nil
(type-unify '(a) '(a a) '((array * 6) (array * (* *))))    ; -> nil, nil
(type-unify '(a) '(a a) '((array * (* * *)) (array * (* *))))    ; -> nil, nil
(type-unify '(a) '(a a) '((array * (* *)) (array * (* * *))))    ; -> nil, nil
(type-unify '(a) '(a a) '((array base-char) (array extended-char))) ; -> nil, nil
(type-unify '(a) '(a a) '(complex real))   ; -> nil, nil
(type-unify '(a) '(a a) '(rational float)) ; -> nil, nil
(type-unify '(a) '(a a) '(integer ratio))  ; -> nil, nil


;; When no assignment is required, it means a success, and the secondary value is T.
(type-unify '(a) '() '()) ;; -> nil, t  (a is unassigned)
(type-unify '()  '() '()) ;; -> nil, t  (a is unassigned)
(type-unify '()  '(fixnum) '(fixnum)) ;; -> nil, t  (a is unassigned)
(type-unify '()  '(number) '(fixnum)) ;; -> nil, t  (a is unassigned)
```


## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.2.8 on X86 Linux 3.13.0-57-generic (author's environment)

Also, it depends on the following libraries:

+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility
+ alexandria by ** :
    Alexandria is a collection of portable public domain utilities.
+ trivia by *Masataro Asai* :
    NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


