Defmemo
=======

Defmemo is a generic defun form, performing memoization over calls
to such defined function.  It supports arbitrary lambda lists and
multiple return values.  (optimize speed)-friendly, otherwise it might
have been implemented as in Peter Norvig's PAIP.  Preserves arguments
and documentation.  Memoizing hash table is stored under property
`:memo` of defmemo'ed function.

## Usage

Three functions are exported: `defmemo` (like defun), `get-memo`
and `clear-memo` (defmemo'ed symbol).  Clear-memo is not needed on
implementations supporting weak hash tables (via trivial-garbage).

```lisp
(defmemo fib (n)
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 100)
; => 573147844013817084101

(get-memo 'fib)
; => #<hash-table :TEST equal :COUNT 101 :WEAKNESS :key>

(hash-table-count (clear-memo 'fib))
; => 0
```
