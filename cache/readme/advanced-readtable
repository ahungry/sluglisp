advanced-readtable
==================

Features
- per-package aliases for packages
- per-package shortcuts for package hierarchies
- extendable find-package and find-symbol
- local use package in form package:(here form where package used)
- local intern package like in SBCL: package::(symbol1 symbol2) will intern
                                     package::symbol1 and package::symbol2

To start
--------

Either use named-readtables and write

    (in-readtable :advanced)
    
or simply add to advanced-readtable to current readtable

    (advanced-readtable:!)

If you want also to substitute cl:defpackage, cl:in-package,
cl:find-package and cl:find-symbol by there adbanced versions,
use (advanced-readtable:!!) or (in-readtable :advanced) and
(advanced-readtable:activate-cl-substitutes).

SLIME integration
-----------------

Append contents of init.el to the end of your ~/.emacs file.

Then, when you start slime, advanced-readtable will be loaded and used by slime.
So you will have all completions for hierarchy packages, local-nicknames, 
aliases, ...

Note: in REPL you will have advanced-readtable instead of the standard 
readtable.

Hierarchy packages
------------------

Advanced-readtable has fully functional built-in support of hierarchy-packages.
.name means "subpackage name in current package", ..name -- "subpackage name in above package",
...name -- "subpackage in two-level-up package" and so on.
In in-package you may use .. for above package, ... for two level up, and so on.
Verbose documentation one may see at [allegro CL](http://www.franz.com/support/documentation/9.0/doc/packages.htm#relative-2).

    CL-USER> (defpackage .test (:use cl)))
    #<PACKAGE "COMMON-LISP-USER.TEST">
    CL-USER> (in-package .test)
    TEST> (in-package ..)
    CL-USER> (defpackage .test.a (:use cl))
    #<PACKAGE "COMMON-LISP-USER.TEST.A">
    CL-USER> (in-package .test.a)
    A> '...::car
    CAR
    A> (eq '...::car 'cl:car)
    T
    A> (in-package ...test)
    TEST> (in-package ..)
    CL-USER>

You may use local-nicknames in defpackage (syntax taken from [SBCL](https://github.com/nikodemus/SBCL/commit/3c11847d1e12db89b24a7887b18a137c45ed4661))

    CL-USER> (defpackage :foo (:use :cl) (:local-nicknames (:it :cl)))
    CL-USER> (in-package :foo)
    FOO> (it:car '(1 2 3))
    1


API
===

_push-import-prefix_ -- enables import prefix on package name
--------------------------------------------

For example, you have packages com.clearly-useful.iterator-protocol, com.clearly-useful.reducers, ...
You may use them as

    (push-import-prefix :com.clearly-useful)
    (iterator-protocol:do-iterator ...)
    (reducers:r/map #'1+ data)

and so on.
Package prefix is enabled per package so it is safe to use it in your package.

If there is package, which name coincides with shortcut, package name has priority.

So, if you make

    (defpackage :reducers ...)

after that reducers:... will refer to new package, not com.clearly-useful.reducers.

_push-local-nickname_ -- enables nickname for package in current package
-------------------------------------------

Enables package nickname in CURRENT-PACKAGE.
For example, you found COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST package and want to use
it. But don't want to USE-PACKAGE them, because some exported symbols from it are clashing 
with yours.

You may do it right:

    (push-local-nickname :com.informatimago.common-lisp.cesarum.list :ilist)
    (ilist:circular-length l)

Local-nicknames are local, so you may use it freely.

If package A wants package LIB version 1, and package B wants package LIB version 2, one can simply 
rename LIB version 1 to LIB1 and LIB version 2 to LIB2 and make

    (push-local-nickname :lib1 :lib :a)
    (push-local-nickname :lib2 :lib :b)

This command also adds local subpackage alias. In the previous example a.lib 
and b.lib will be aliases to lib1 and lib2. If there is a real package with 
such name, alias will be shadowed, so don't worry too much about it.

_push-local-package_ -- sets local-package for a symbol
----------------------------------------------

Many macroses use there own clauses. 
For example, ITERATE uses FOR, COLLECT and so on. 
If you don't want to USE-PACKAGE iterate, this function will help.

    (push-local-package 'iter:iter :iterate)
    (iter:iter (for i from 1 to 10) (collect i))

Caution: this function enables package substitution in all cases, 
where SYMBOL is the car of a list.
For example, this will be error:

    (let (iter:iter for) (list iter:iter for))
    
, because first for is in ITERATE package, but second -- is not.

Be careful: this change is not local to your package.

_set-macro-symbol_ symbol func -- sets FUNC to process the SYMBOL.
--------------------------
FUNC will get stream of reader and the symbol (see set-macro-character).

To prevent symbol from processing (for example in set-macro-symbol construction) you should enclose it in bars.

This construction will set 'foo as an alias to 'long-package-name:long-name:

    (set-macro-symbol '|FOO|
      (lambda (stream symbol)
         (declare (ignore stream symbol))
            'long-package-name:long-name))
 
Another way to prevent symbol processing is setting `advanced-readtable:*enable-symbol-readmacro*` to nil. 
Remember, that symbol processing is done during reading the file, so, if you need to temporarily disable
`*enable-symbol-readmacro*`, then enclose it in #.

Now you may make something like 

    html:[body [table (as-html sql:[select * from t1])]]

html:[ and sql:[ will have different handlers and you may mix them in
one expression.

Moreover, you may alias variables from other packages and set them through 
alias. But be careful: this change is not local to your package. If you write qualified name
of the symbol, you should enclose package-name in bars:

    (set-macro-symbol '|OTHER-PACKAGE|:foo
      (lambda (stream symbol)
         (declare (ignore stream symbol))
            'long-package-name:long-name))
                                   

_get-macro-symbol_ - syntax is like get-macro-character, 
------------------

Returns function, assigned by set-macro-symbol

Low-level API
-------------

There are five lists:
-  `*package-finders*` -- global for find-package
-  `*symbol-finders*` -- global for find-symbol
-  (package-finders package) -- per-package for find-package
-  (symbol-finders package) -- per-package for find-symbol
-  (extra-finders symbol) -- per-symbol for (symbol ....) package substitution

They are all alists. Key denotes handler and should be uniq for the list.
Value should have form (lambda (name package) ...) and return symbol for
symbol-finders and extra-finders and return pacakge for package-finders.

You may freely change them to develop your own symbol or package schemes
(for example, hierarchy-packages, conduits and so on).

Middle-level API
----------------

To simplify adding new handlers with keys there is macro _set-handler_

    (set-handler (package-finders pack) '(:my handler1) #'handler-func)

will set handler for package pack, if there are no hanler with key 
(:my handler1). So you may set it in your file and not be afraid, that it
will duplicate on reloading.

Restrictions
------------

You must only ASCII characters for first letter of every part of package name 
and for first letter of symbols, that you want to use in set-macro-symbol

If you really need other characters you may set them by calling

    (set-macro-character c #'advanced-readtable:read-token-with-colons t)
    
for every your character.

If you need to temporary disable macro-characted substitution, you may set 
`advanced-readtable:*enable-symbol-readmacro*` to nil. It could be useful, if you
describe a lot of symbols and don't want to enclose every of them in || (and upcase, of course).
