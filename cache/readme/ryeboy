Ryeboy
======

[![Build Status](https://travis-ci.org/AeroNotix/ryeboy.svg?branch=master)](https://travis-ci.org/AeroNotix/ryeboy)

![monolith](http://i.imgur.com/ljUZbEF.jpg)

A Common Lisp client to [Riemann](https://github.com/aphyr/riemann)

Usage:

```lisp
(ql:quickload :ryeboy)
(use-package :ryeboy)

(let ((conn (make-connection))
      (ht (make-hash-table :test #'equal)))
  (setf (gethash "foo" ht) "bar")
  (send-event conn (make-event)) ;; Sends with time/host defaults
  (send-events conn
               (make-event :tags (list "foo" "bar" "baz"))
               (make-event :attrs ht)
               (make-event :service "somefunkyservice")
               (make-event :service "somethingisbroken" :state "offline"))
  (print (query conn "service = \"somefunkyservice\"")))
```
