# Metap

Metap provides metaclass propagation along class inheritance structure.
Metap uses closer-mop and changes
- *c2mop:ensure-class-using-class :around class name &rest args*

so it could conflicts with other library modifing same method.

## Motivation

You got some idea which use metaclass like

```
(defclass some-meta-class (standard-class) ())
(defmethod something1 ((class some-meta-class) ...) ...)
(defmethod something2 ((class some-meta-class) ...) ...)
```

and use this for some classes like

```
(defclass some-class1 () () (:metaclass 'some-meta-class))
(defclass some-class2 (some-class1) () (:metaclass 'some-meta-class))
(defclass some-class3 (some-class2) () (:metaclass 'some-meta-class))
(defclass some-class4 (some-class1) () (:metaclass 'some-meta-class))
... :metaclass :metaclass :metaclass :metaclass
```

This is boring.
Using metap, it can simply be written like

```
(defclass some-mixin () ())
(metap:register-m1-m2-pair 'some-mixin 'some-meta-class)

(defclass some-class1 (some-mixin) ())
(defclass some-class2 (some-class1) ())
(defclass some-class3 (some-class2) ())
(defclass some-class4 (some-class1) ())
```

Also see cl-singleton-mixin (https://github.com/hipeta/cl-singleton-mixin) which is written by using metap.

## APIs

### Variables

- \*metap-m1-m2-pairs\*
 * All pairs registered in metap.

### Functions

- register-m1-m2-pair (m1class m2class)
 * Register m1class and m2class pair which you want to enable metaclass propagation.

### Macros

- validate-superclass* (&body validations)
 * Syntax sugar for c2mop:validate-superclass.

## Installation

```
(ql:quickload :metap)
```

## License

Metap is released under the MIT License, see LICENSE file.
