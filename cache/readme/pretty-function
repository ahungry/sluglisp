
# pretty-function

http://github.com/nallen05/pretty-function

## Introduction

_pretty-function_ provides an API for making individual functions pprint differently when written to an output stream

## Download

Download the latest gzipped tarball:

* [Version 0.1.1](http://cloud.github.com/downloads/nallen05/pretty-function/pretty-function_0.1.1.tar.gz) 3.28.2009 -- Updated documentation for Github, converted to Markdown
* [Version 0.1](http://cloud.github.com/downloads/nallen05/pretty-function/pretty-cuntion_0.1.tar.gz) 21.09.2007

or get the latest version from the [Git](http://git-scm.com/) repository:

    git clone git://github.com/nallen05/pretty-function.git

## License

[BSD](http://en.wikipedia.org/wiki/BSD_License). Thanks to Streamtech for letting me open source this tool.

## Supported Implementations

`ENABLE-PRETTY-FUNCTION-PRINTING` seems to work and the tests in pretty-function-test.lisp pass on the following implimentations:

* ACL
* Clisp
* CMUCL
* Lispworks
* OpenMCL/MCL
* SBCL

the rest should fail gracefully by not pprinting pretty functions any differently from normal functions

## When to use pretty functions

`pretty-function` is intended to make code with a lot of first class functions and closures saner to debug.

There is a little extra work involved when creating or garbage-collecting pretty functions, so they probubly shouldn't be used in situations where arbitrarily large amounts of closures being created and garbage-collected at runtime.

A good example use-case is a [Hunchentoot](http://weitz.de/hunchentoot/)-powered web application (_note: Hunchentoot underwent major API changes to become Hunchentoot 1.x, does it still behave like this?_):

Hunchentoot deals with HTTP requests by means of a global [dispatch table](http://weitz.de/hunchentoot/#handlers). The dispatch table is a list of function indicators ("dispatchers") that often contains a buch of closures (such as the one created by [`CREATE-STATIC-FILE-DISPATCHER-AND-HANDLER`](http://weitz.de/hunchentoot/#create-static-file-dispatcher-and-handler)). When hunchentoot gets an HTTP request, each dispatcher is called with the request object as an argument. when a dispatcher decides to handle this request, it signals its intent by returning another function (a "handler") that, when called, does all the HTML/HTTP stuff associated with handling the request.

Dspatchers are perfect canidates for pretty-functions since, as pretty functions, they can pprint something informative like.

    #<static-file-dispatcher "/foo/bar.html">

instead of the usual

    #<what-the-hell-am-i>

in a stack trace or when a programmer is visually inspecting the dispatch table via the REPL.

Hunchentoot's dynamically generated handlers, on the other hand, are not good canidates for pretty function forms because there could be [zillions](http://en.wikipedia.org/wiki/Indefinite_and_fictitious_large_numbers) of them being created and garbage collected at runtime.

The existence of pretty functions in a lisp image does not create any more work when creating or garbage collecting "normal" functions or closures (those not created with pretty function forms).

## API

* `ENABLE-PRETTY-FUNCTION-PRINTING (&optional (priority 0) (table *print-pprint-dispatch*))`

- - -

   _Function_ that modifies the pprint dispatch table `TABLE` to pprint functions using their pretty function printer (see `GET-FUNCTION-PRINTER`).

   this means that you can make all the pretty functions you want, but until you run

    (enable-pretty-function-printing)

   the won't pprint differently in the REPL or in stack traces!

   for info in its arguments, see Common Lisp's `SET-PPRINT-DISPATCH`.

* `NAMED-LAMBDA (name lambda-list &body body)`

- - -

   _Macro_ like `LAMBDA` except the resultant function is prittern as

    #<named-lambda NAME>

   when pprinted to a stream and pretty printing functions has been enabled (see `ENABLE-PRETTy-FUNCTION-PRINTING`).

   `NAME` is not evaluated.

   _Caveat: unlike `LAMBDA`, `NAMED-LAMBDA` cannot be used as the first element of a list_

   so

    ((lambda (a b) (+ a b)) 5 6) => 11

   but

    ((named-lambda mistake (a b) (+ a b)) 5 6) ==> THROWS AN ERROR

* `NAMED-LAMBDA (name-form lambda-list &body body)`

- - -

   _Macro_ like `NAMED-LAMBDA` except `NAME-FORM` is evaluated

* `WITH-FUNCTION-PRINTER (printer fn-form)`

- - -

   _Macro_ returns the result of evaluating `FN-FORM`, which should return a function.

   The resultant function will be writtern by `PRINTER` when pprinted to a stream and pretty printing functions has been enabled (see `ENABLE-PRETTY-FUNCTION-PRINTING`).

   `PRINTER` should be a lambda expression or name of a function that takes `STREAM` as it's only argument and prints a pretty representation of `FUNCTION` to that `STREAM`.

    CL-USER> (enable-pretty-function-printing)
    
    CL-USER> (let ((n 0))
               (setf x (with-function-printer (lambda (s) (format s "#<counter ~A>" n))
                          (lambda () (incf n)))))
    
    #<counter 0>
    
    CL-USER> (funcall x)
    1
    
    CL-USER> x
    #<counter 1>

* `*PRETTY-FUNCTION-PRINTING-SUPPORTED-P*`

- - -

  _Variable_. Is `T` on implementations that support pretty function printing, `NIL, on the rest.

* `PRINT-PRETTY-FUNCTION-TABLE (&optional (stream *standard-output*))`

- - -

  _Function_. prints all known pretty functions to `STREAM`

* `CLEAR-PRETTY-FUNCTION-TABLE ()`

- - -

  _Function_ that turns all known pretty functions into normal, non-pretty functions.

  Individual pretty functions can also be turned back into normal functions by `SETF`-ing their `GET-FUNCTION-PRINTER` to `NIL`

* `GET-FUNCTION-PRINTER (function)`

- - -

  `SETF`-able _Function_ for accessing the pretty function printer of `FUNCTION` or `NIL` if `FUNCTION` is not a pretty function.

   You can turn a non-pretty function into a pretty function by `SETF`-ing `GET-FUNCTION-PRINTER` to a an acceptible printer (see `WITH-FUNCTION-PRINTER`). You can also turn a pretty function back into a normal function by `SETF`-ing its `GET-FUNCTION-PRINTER` to `NIL`




