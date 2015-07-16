# Cl-singleton-mixin

Cl-singleton-mixin simply provides singleton-mixin class by using metap (https://github.com/hipeta/metap).

## Example

```
(ql:quickload :cl-singleton-mixin)

(defclass some-singleton (singleton-mixin) ())

(eq (make-instance 'some-singleton)
    (make-instance 'some-singleton))  ; => T

(defclass some-child (some-singleton) ())

(eq (make-instance 'some-child)
    (make-instance 'some-child))  ; => T
```

## Installation

```
(ql:quickload :cl-singleton-mixin)
```

## License

Cl-singleton-mixin is released under the MIT License, see LICENSE file.
