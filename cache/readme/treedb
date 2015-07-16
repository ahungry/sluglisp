# treedb

treedb is a simple in-memory database for hierarchical keys.
This means, your keys are structured like a path in a file system or a system regstry.

## Features

* storing values in memory in a tree structure
* tree inspection
* alist and json export
* implemented in alists

### Planned features

* persistence
* restarts
* other internal implementations
* serialization
* tree queries/manipulation

## Installation

treedb comes with an ASDF system, so it can be loaded via ASDF
or as a local project via quicklisp.

## Usage

treedb lives in the package `treedb`.

Here are some examples:

```common-lisp
;;create a new instance
(defparameter *db* (treedb:make-alist-treedb))

;;set values:
(setf (treedb:node *db* :a :1) "Hello" ; /:a/:1 <- "Hello"
      (treedb:node *db* :a :2) :world  ; /:a/:2 <- :world
      (treedb:node *db* :b) 1          ; /:b <- 1
      (treedb:node *db* :c) '((1 . 2) (3 . 4))) ; /:c <- '((1 . 2) (3 . 4))

;;get values
(treedb:node *db* :a :1) ; => "Hello"

;;delete nodes
(treedb:del-node *db* :b)

;;list keys
(treedb:children *db*) ; => (:A :C)
(treedb:children *db* :a) ; => (:|1| :|2|)

;;take a subtree
(treedb:subtree *db* :a) ; => subtree from :a

;;convert to alists or json (via cl-json)
(treedb:to-alist (treedb:subtree *db* :a))
  ; => ((:1 . "Hello") (:2 . :world))
(treedb:to-json (treedb:subtree *db* :a))
  ; => "{\"2\":\"Hello\",\"2\":\"world\"}"
```