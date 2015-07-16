# clos-fixtures

[![Build Status](https://travis-ci.org/eudoxia0/clos-fixtures.svg?branch=master)](https://travis-ci.org/eudoxia0/clos-fixtures)

ASDF-loadable fixtures for CLOS classes.

# Usage

## Fixture Syntax

```lisp
(package:class-name
  (:slot-name1 value
   :slot-name2 value)
  (:slot-name1 value
   :slot-name2 value))
(package:another-class
  ...)
```

## Example

```lisp
(app:user
  (:name "eudoxia"
   :groups (:admin :staff))
  (:name "joe"
   :groups (:admin)))
(app:company
  (:name "Initech"
   :city "Denver"))
```

## `register-fixture`

This method has to be defined for every class you want to load.

An example using the `user` model from the last example:

```lisp
(defmethod clos-fixtures:register-fixture ((user myapp:user))
  (cl-mongo:db.insert "users" (cl-mongo:kv (name user) (groups user))))
```

## Loading from Lisp

```lisp
(clos-fixtures:load-fixtures
  #P"/path/to/fixture-file")
```

## Loading from ASDF

Simply put this in your components tree (Check `clos-fixtures-test.asd` for an example):

```lisp
(:fixture "filename")
```

Optionally, you can use the `:package` argument so you don't have to specify the
package in the fixture itself. An example from the tests:

```lisp
(:fixture "molecules" :package :clos-fixtures-test)
```

```lisp
(molecule
 (:name "Methane"
  :formula "CH4")
 (:name "Cubane"
  :formula "C8H8"))
```

# License

Copyright (c) 2014-2015 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
