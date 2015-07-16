transparent-wrap
================

[![Build Status](https://travis-ci.org/DalekBaldwin/transparent-wrap.svg?branch=master)](https://travis-ci.org/DalekBaldwin/transparent-wrap)

This is a small utility for generating wrapper functions that have the same signature as the functions they wrap so that you can still interactively see the same function signature in the SLIME minibuffer. (It also offers the same feature for macros, which is not quite as difficult but is included for completeness' sake.)

## Table of Contents
  * [Example](#example)
  * [Basic Usage](#basic-usage)
  * [Performance](#performance)
  * [Limitations](#limitations)

Example
-------

When using `lispbuilder-sdl` on SBCL, you may encounter all kinds of floating point errors, but you don't want to disable those errors globally - you want to do your own math before calling out to foreign functions. In that case you can do something like this:

```lisp
(defpackage :sdl-wrap
  (:use :cl)
  #.`(:export
      ,@(loop for symbol being the external-symbols of :sdl
           when (and (fboundp symbol)
                     (eql (symbol-package symbol) (find-package :sdl)))
           collect symbol)))

#.`(progn
     ,@(loop for symbol being the external-symbols of :sdl
          when (and (fboundp symbol)
                    (eql (symbol-package symbol) (find-package :sdl)))
          collect
            (transparent-wrap:create-transparent-defun
             symbol
             (lambda (real-function-call)
               `(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
                  ,real-function-call))
             :sdl-wrap)))
```

and fix it without having to massively edit the package's source code or your client code. Just import the wrapper package instead. Now you can get the functionality you need and see that `sdl-wrap:draw-string-solid-*` has the signature `(string x y &key (justify :left) (surface lispbuilder-sdl:*default-surface*) (font lispbuilder-sdl:*default-font*) (color lispbuilder-sdl:*default-color*))` without manually searching for it!

Basic Usage
-----------

```lisp
;; function
(create-transparent-defun 'package:function
                          (lambda (code)
                            `(wrap ,code))
                          :wrapping-package)
```

```lisp
(defmacro wrapper (code)
  `(wrap ,code))

;; macro
(transparent-defun package:function wrapper :wrapping-package)
```

```lisp
;; function
(create-transparent-defmacro package:macro
                             (lambda (code)
                               ``(wrap ,,code))
                             :wrapping-package)
```

```lisp
(defmacro wrapper (code)
  `(wrap ,code))

;; macro
(transparent-defmacro package:macro wrapper :wrapping-package)
```

Performance
-----------

For some argument lists, the wrapping layer imposes a considerable overhead, since we have to manually ensure that we only pass exactly the same optional and keyword arguments that appeared in the outer call in case the wrapped function explicitly checks whether any of those arguments were supplied. There are two ways to mitigate this overhead:

1. Set the keyword argument `:force-rest` to `t` in `create-transparent-defun`. This adds a `&rest` parameter when wrapping a function that has `&key` arguments but no `&rest` argument. This way, the keyword arguments can be passed through with `apply` without checking which ones are present, with minimal clutter added to the function signature.

2. When turning a development build into a production build, you can swap out `create-transparent-defun` for `create-opaque-defun` to include the same wrapping logic but strip out all the infrastructure for imitating the function signature.

Limitations
-----------

This library uses `trivial-arguments` to retrieve function and macro signatures. On some implementations, this will not retrieve default arguments for some parameters. When no signature can be found, `transparent-defun` falls back to basic `opaque-defun` functionality and creates a wrapper with a `&rest` parameter only.

Init-forms can have side effects, and they are normally evaluated in left-to-right order. This library will only hoist init-forms into wrappers until it reaches the first parameter, either optional or keyword, that has a supplied-p check. If you're confident that the ordering of your init-forms won't matter, you can set the keyword argument `:allow-reordered-init-forms` to `t` and see later parameters' init-forms in the wrapper function signature. You will still not see init-forms for any parameters that have supplied-p checks, since this can still affect the behavior of the wrapped function.

Wrappers for generic functions with `&rest` and/or `&key` parameters will have those parameters subsumed by a single `&rest` parameter to allow for variations in congruent method lambda lists.
