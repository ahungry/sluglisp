# Generators

Generators is a library that provides python style generators in
common lisp, by using cl-cont delimited continuations

This library is more of an interesting toy, though as far as I know it
does work.  I dont think I have ever used this in application code,
though I think that with care, it could be.

## Requirements

 * [CL-CONT](http://common-lisp.net/project/cl-cont/) provides the
   continuations that make this work
 * Alexandria
 * Iterate

## Gotchas

 * Continuations don't play nicely with exception/signal handling,
   unwind-protect, throw/catch, or dynamic variables.  Consider using
   cl-cont:without-call/cc, if you need this functionality in a
   generator. (see-also %mv-gen for an example)

 * because cl-cont uses a code walker, it might get confused when
   interacting other code walkers (eg: iterate).  Inside of
   generators, you might have better luck with less code walking

## API

### make-generator

Creates a new generator, the body of which can yield values to the calling scope

### yield
yield - returns the next value from the generator to the caller

### yielding
yielding - yields every value in the passed in generators


## Iterate clauses

### (for v in-generator gen)

iterates through the values produced by a generator

### (FOR node a-node-of-lisp-tree tree)

iterates through all the nodes of a lisp tree
    eg: '(1 (2 3) 4) generates (1 (2 3) 2 3 4)

### (FOR node a-leaf-of-lisp-tree tree)

iterates through all the leaves of a lisp tree
    eg: '(1 (2 3) 4) generates (1 2 3 4)

## Example:

```
(defun generate-lisp-tree-nodes (trees &optional leaves-only?)
  "Do a depth first traversal of some set of trees yielding every node/leaf "
  (make-generator ()
    (iter (for n in (alexandria:ensure-list trees))
      (etypecase n
        (atom (yield n))
        (list
         (unless leaves-only? (yield n))
         (yielding (generate-lisp-tree-nodes n)))))))
```


## Authors

 * [Acceleration.net](http://www.acceleration.net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

```
;; Copyright (c) 2013 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
