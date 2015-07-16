define-json-expander
=====================

Small 'library' for writing simple interfaces between JSON files and CLOS.
The `define-json-expander' macro creates both a CLOS-class and a decoder function named DECODE-* where * = name (this can be changed with *accessor-prefix*) for simple use.
define-json-expander is supposed to be used in tandem with cl-json as its decode-functions expects cl-json list-decoded JSON output as input.

This library is written by Johan Sjölén and is licensed under the MIT license.

Usage
-----

define-json-expander has the same syntax as defclass with two new slot-options: :json-prop :json-decoder.
:json-prop expects a keyword that represents a property name.
:json-decoder expects a function of one argument whose output will be used as the value for the slot.
The input to the :json-decoder function is the value of the json property.

All accessors (if not already defined) are on the form *-OF

If no :json-prop is defined then the slot-name as a keyword will be taken automatically.

All unknown/unusued data will be put in the REST slot of the resulting object.


Example usage
-------------

The following JSON is taken from json.org/example.html


**JSON**

```
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
```

**Code**

```
(define-json-expander menu ()
 ((menu :json-decoder #'decode-menu-slots)))

(define-json-expander menu-slots ()
         ((id)
          (value)
          (popup :json-decoder #'decode-menuitem)))

(define-json-expander menuitem ()
  ((menuitem :json-decoder (lambda (x) (loop for obj in x collect (decode-button obj))))))

(define-json-expander button ()
  ((value) (onclick)))
```