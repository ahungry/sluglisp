# Multival-Plist - Property List stores multiple values per one key.

## Usage

    (defparameter *plist* '(:foo 1 :bar 2 :foo 3))
    
    (getf *plist* :foo)
    ;=> 1
    (getf-all *plist* :foo)
    ;=> (1 3)
    
    (remf-all *plist* :foo)
    ;=> T
    (getf-all *plist* :foo :undef)
    ;=> :UNDEF

## Description

Multival-Plist provides utilities to handle Property Lists that may contain multiple values per key.

## API Reference

### [Function] getf-all (plist key &optional default)

    *plist*
    ;=> (:FOO 1 :BAR 2 :FOO 3)
    (getf *plist* :foo)
    ;=> 1
    (getf-all *plist* :foo)
    ;=> (1 3)

This is a version of `getf` enabled to manage multiple keys. If the `plist` has two or more pairs that they have given `key` as a key, returns the values of each pairs as one list.

### [Function] \(setf getf-all\) (val plist key &optional default)

    (setf (getf-all *plist* :foo) '("Hello" "World"))
    ;; *plist* = '(:foo "Hello" :bar 2 :foo "World")
    
    ;; same as (setf (getf-all *plist* :foo) '(3000))
    (setf (getf-all *plist* :foo) 3000)
    ;; *plist* = '(:foo 3000 :bar 2)

Changes the stored value(s) of the given `key`. This removes or adds pairs as necessary to store the new list.

### [Macro] remf-all (plist key)

    (remf-all *plist* :foo)

Removes a key and associated values for the given `key`.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.
