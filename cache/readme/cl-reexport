# Cl-Reexport

Cl-reexport makes a package reexport symbols which are external symbols in other packages. This fanctionality is intended to be used with (virtual) hierarchical packages. For detail, see Usage section.

## Usage

Think about a (virutal) hierarchical package structure. Since Common Lisp standard has just a flat package system, the three packages are naturally at the same level, but you can regard the packages constituting a hierarchy virtually.

    FOO-package
    - FOO-PACKAGE.BAR
    - FOO-PACKAGE.BAZ

Here, their definitions are:

    (in-package :cl-user)

    (defpackage foo-package.bar
      (:use :cl)
      (:export :x))

    (defpackage foo-package.baz
      (:use :cl)
      (:export :y))

    (defpackage foo-package
      (:use :cl))

If you want reexport external symbols in FOO-PACKAGE.BAR and FOO-PACKAGE.BAZ from FOO-PACKAGE, you may just write as below:

    (in-package :foo-package)
    (cl-reexport:reexport-from :foo-package.bar)
    (cl-reexport:reexport-from :foo-package.baz)

    (in-package :cl-user)

    (describe 'x)
    >> FOO-PACKAGE.BAR:X
    >>   [symbol]

    (describe 'y)
    >> FOO-PACKAGE.BAZ:Y
    >>   [symbol]

### :INCLUDE and :EXCLUDE options

You can also reexport the only symbols you specify or the symbols other than you specify using :INCLUDE option or :EXCLUDE option. Since they are exclusive, you can not use :INCLUDE option and :EXCLUDE option at the same time.

    (in-package :cl-user)

    (defpackage bar-package.a
      (:use :cl)
      (:export :x :y :z))

    (defpackage bar-package.b
      (:use :cl)
      (:export :p :q :r))

    (defpackage bar-package
      (:use :cl))

    (in-package :bar-package)

    (cl-reexport:reexport-from :bar-package.a
                               :include '(:x))

    (cl-reexport:reexport-from :bar-package.b
                               :include '(:p))

    (in-package :cl-user)

    (do-external-symbols (x :bar-package)
      (print x))
    >> BAR-PACKAGE.A:X
    >> BAR-PACKAGE.B:Q
    >> BAR-PACKAGE.B:R

## Installation

You can install `cl-reexport` via Quicklisp:

    (ql:quicklisp :cl-reexport)


## Difference from ASDF 3's one-package-per-file fanctionality

ASDF 3 has one-package-per-file fanctionality and its runtime support. The structural difference between (virtual) hierarchical packages and ASDF 3's one-package-per-file fanctionality is:

* (Virtual) hierarchical packages have one system definition and several packages constitute a hierarchical structure.
* ASDF 3's one-package-per-file style has hierarchical system definitions and hierarchical packages, whcih are corresponding each other.

When you use former style, you can use cl-reexport.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the LLGPL License.
