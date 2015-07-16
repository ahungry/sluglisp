# CL-SOPHIA
Common Lisp high-level API for [Sophia key-value storage](http://sphia.org/)

### Installation
##### libsophia
```
$ cd install
$ make
$ sudo make install
```
##### cl-sophia
```lisp
(ql:quickload "cl-sophia")
```

### Examples
##### Set and get
```lisp
(with-database ("test")
  (setf ($ "x") "a"
        ($ "y") "b")
  (values ($ "x")
          ($ "y")
          ($ "z")))
;; => "a"
;; => "b"
;; => NIL
```
##### Delete
```lisp
(with-database ("test")
  (setf ($ "x") "a")
  (let ((x ($ "x")))
    (setf ($ "x") nil)
    (values x ($ "x"))))
;; => "a"
;; => NIL
```
##### Transaction
```lisp
(with-database ("test" :comparator :u32)
  (with-transaction ()
    (setf ($ 0) "a"
          ($ 1) "b"
          ($ 2) "c"))
  (ignore-errors
    (with-transaction ()
      (setf ($ 1) nil)
      (error "Bam!")))
  (values ($ 0)
          ($ 1)
          ($ 2)))
;; => "a"
;; => "b"
;; => "c"
```
##### Nested transactions
```lisp
(with-database ("test")
  (with-transaction ()
    (setf ($ "x") "foo"
          ($ "y") "bar")
    (with-transaction ()
      (setf ($ "z") "baz")))
  (values ($ "x")
          ($ "y")
          ($ "z")))
;; => "foo"
;; => "bar"
;; => "baz"
```
##### Iterators
```lisp
(with-database ("test" :comparator :u32)
  (dotimes (i 3)
    (setf ($ i) (format nil "~r" i)))
  (let (result)
    (let ((*order* :>=))                ; by default
      (map-object (lambda (key value)
                    (push (cons key value) result))))
    (values result)))
;; => ((2 . "two") (1 . "one") (0 . "zero"))
```
