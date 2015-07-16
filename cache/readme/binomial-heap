    ___  __ __   ____ _    __ ___  __        _  _ ____ ___  ____
    | .\ |_\| \|\|   ||\/\ |_\|  \ | |   ___ ||_|\| __\|  \ | . \
    | .<_| /|  \|| . ||   \| /| . \| |__|___\| _ ||  ]_| . \| __/
    |___/|/ |/\_/|___/|/v\/|/ |/\_/|___/     |/ |/|___/|/\_/|/


# Abstract

Binomial-heap is a compact and  succint implementation of the [binomial heap data structure](http://en.wikipedia.org/wiki/Binomial_heap) in Common Lisp programming language. Insertion,  extremum access, extremum extraction, and union operations are performed in O(logn) time.


# Demo

    (defvar *list* (loop repeat 20 collect (random 100)))
    ; => (25 50 12 53 53 55 41 71 71 41 33 8 71 57 28 4 89 96 58 25)

    (defvar *heap* (make-instance 'bh:binomial-heap :test #'<))
    ; => #<BINOMIAL-HEAP {1002C4EC31}>

    (dolist (item *list*)
      (insert-key *heap* item))
    ; => NIL

    (bh::print-tree (bh::head-of *heap*))
    ; => -> ( 2) 25
    ;      -> ( 1) 89
    ;        -> ( 0) 96
    ;      -> ( 0) 58
    ;    -> ( 4) 4
    ;      -> ( 3) 12
    ;        -> ( 2) 41
    ;          -> ( 1) 53
    ;            -> ( 0) 55
    ;          -> ( 0) 71
    ;        -> ( 1) 25
    ;          -> ( 0) 50
    ;        -> ( 0) 53
    ;      -> ( 2) 8
    ;        -> ( 1) 41
    ;          -> ( 0) 71
    ;        -> ( 0) 33
    ;      -> ( 1) 57
    ;        -> ( 0) 71
    ;      -> ( 0) 28
    ;    NIL

    (bh:get-extremum-key *heap*)
    ; => 4

    (loop for x in (sort (copy-list *list*) (test-of *heap*))
          for y = (extract-extremum-key *heap*)
          unless (= x y)
          collect (cons x y))
    ; => NIL

    (let ((h1 (make-instance 'bh:binomial-heap :test #'string<))
          (h2 (make-instance 'bh:binomial-heap :test #'string<))
          (l1 '("foo" "bar" "baz" "mov" "mov"))
          (l2 '("i" "see" "dead" "binomial" "trees")))
      (dolist (l l1) (bh:insert-key h1 l))
      (bh::print-tree (bh::head-of h1))
    ; => -> ( 0) "mov"
    ;    -> ( 2) "bar"
    ;      -> ( 1) "baz"
    ;        -> ( 0) "mov"
    ;      -> ( 0) "foo"
    ; NIL
      (dolist (l l2) (bh:insert-key h2 l))
      (bh::print-tree (bh::head-of h2))
    ; => -> ( 0) "trees"
    ;    -> ( 2) "binomial"
    ;      -> ( 1) "i"
    ;        -> ( 0) "see"
    ;      -> ( 0) "dead"
    ; NIL
      (let ((h3 (bh:unite-heaps h1 h2)))
        (bh::print-tree (bh::head-of h3))))
    ; => -> ( 1) "mov"
    ;      -> ( 0) "trees"
    ;    -> ( 3) "bar"
    ;      -> ( 2) "binomial"
    ;        -> ( 1) "i"
    ;          -> ( 0) "see"
    ;        -> ( 0) "dead"
    ;      -> ( 1) "baz"
    ;        -> ( 0) "mov"
    ;      -> ( 0) "foo"
    ; NIL

# Caveats

Despite binomial  heaps are  known to perform  decrease/increase key  and delete operations in O(logn) time, this is practically not that easy to implement. (For the rest  of this talk, I'll  skip the deletion  operation because of it  can be achieved through  setting the key  field of a  node to the absolute  extremum -- i.e.  negative infinity -- and extracting the extremum.) Consider below example.

    --> [ Z ] -->
          ^
          |
          |
    --> [ X ] -->
         ^^^
         |||
         ||+----------------------------+
         |+----------+        ...       |
         |           |                  |
    --> [ W0 ] --> [ W1 ] --> ... --> [ WN ]

Suppose you decreased the key field of X and you need to bubble up X by swapping nodes  in upwards  direction  appropriately.  Because of  random access is  not possible  in heap  data  structures, you  need to  figure  out your  own way  of accessing to nodes -- in this  example consider you have the pointers in advance to the  every `BINOMIAL-TREE` in the `BINOMIAL-HEAP`. There are two  ways to swap nodes:

## Swapping Key Fields

If you just swap the key fields of the nodes

    (rotatef (key-of x) (key-of z))

everything will be fine, except that the pointers to the nodes that lost their original  key fields  will  get  invalidated.  Now  you  cannot guarantee the validity of  your node pointers and  hence cannot issue any  more decrease key operations.

## Swapping Node Instances

If you  swap the two node  instances, your pointers won't  get invalidated but this time you'll need to update the sibling and parent pointers as well,

    (setf (parent-of w0) z
          (parent-of w1) z
    ...
    (parent-of wn) z)

which will make  your O(logn) complexity dreams fade  away.  (Moreover, you'll need to traverse sibling  lists at levels of nodes `X` and `Y` to be able to find previous siblings to `X` and `Y` if you are not using doubly-linked-lists. But even this scheme doesn't save us from the traversal of `W1`, ..., `WN` nodes.)

## Solution

So how can we manage to perform decrease key operation in O(logn) time without invalidating any node pointers? The solution I come up with to this problem is as follows.

We can keep a separate hash table for the pointers to the nodes. When a node's key  field  gets modified,  related  hash table  entry  will  get modified  as well. And instead of returning to the user the actual `BINOMIAL-TREE` instances, we'll return  to the user the key  of the related hash  table entry. (Consider this hash table  as a mapping between  the hash table keys and  the pointer to the actual node instance.)

Sounds too hairy? I think so. I'd be appreciated for any sort of enlightenment of a better solution.
