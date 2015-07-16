CL-Inflector
========

A common lisp library to easily pluralize and singularize English and Portuguese words.

This is a port of the ruby ActiveSupport Inflector module.

Installation
------------------
Just use [Quicklisp](www.quicklisp.org):

    > (ql:quickload 'cl-inflector)

Usage
-----------
Example Usage:

    > (use-package :cl-inflector)
    > (let ((dollars 1.7)
            (users 34)
            (purchases 1))
           (format nil "The site has ~D ~A, with a total of ~D ~A and $~D ~A"  
                   users (pluralize users "user") 
                   purchases (pluralize purchases "purchase") 
                   dollars (pluralize dollars "dollar")))
    "The site has 34 users, with a total of 1 purchase and $1.7 dollars"

Basic Usage, `plural-of` and `singular-of`:

    > (plural-of "octopus") 
    "octopi"
    > (plural-of "datum")
    "data"
    > (singular-of "children")
    "child"
    > (singular-of "cats")
    "cat"
    > (singular-of "data")
    "datum"

Basic Usage, `pluralize`:

    > (pluralize 2 "octopus")
    "octopi"
    > (pluralize 1 "octopus")
    "octopus"

You can pass in the default plural to be used. If not, the inflector is used to determine the plural.

    > (pluralize 2 "tooth" "teeth")
    "teeth"
    > (pluralize 2 "tooth")
    "tooths"

Use `irregular` to add an irregular:

    > (singular-of "feet")
    "feet"
    > (irregular "foot" "feet")
    > (singular-of "feet")
    "foot"
    > (plural-of "foot")
    "feet"

Use `uncountable` to add an uncountable:

    > (plural-of "advice")
    "advices"
    > (singular-of "advice")
    "advice"
    > (singular-of "advices")
    "advice"
    > (uncountable "advice")
    > (plural-of "advice")
    "advice"

You can also pluralize/singularize symbols (very useful for writing macros):

    > (symbol-plural-of 'book)
    BOOKS
    > (symbol-singular-of 'pages)
    PAGE
    
Internationalization support
---------------
Currently, cl-inflector has built-in support for english(`en_US`) and brazilian portuguese(`pt_BR`), and it also offers a simple API to change between languages:

    > (available-languages)
    (:EN_US :PT_BR)
    > (current-language-name)
    :EN_US

For changing between languages, use `set-language!`:

    > (plural-of "país")
    "país"
    > (set-language! :pt_BR)
    :PT_BR
    > (plural-of "país")
    "países"

You can add irregular words and uncountable in the same way as presented above, but you still does not support adding a whole new language.

For more examples, check the [tests source](https://github.com/AccelerationNet/cl-inflector/blob/master/tests/inflector.lisp).
License
---------------

Released under the MIT license, please see `LICENSE` for more details

Thanks
-------------
  - This was originally a support package for the [Vana web framework][1]
  - [Xach][2] - For [quicklisp][3], really made getting back into CL much easier.
  - Siebel - For [PCL][4], which has been a great reference.


  [1]: https://github.com/sgrove/vana
  [2]: http://xach.livejournal.com/
  [3]: http://www.quicklisp.org/
  [4]: http://gigamonkeys.com/book/

## Authors
 * [SGrove](https://github.com/sgrove)
 * [Acceleration.net](http://www.acceleration.net/) - [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)
  * [André Miranda](http://github.com/EuAndreh)
