# Thread.Comm.Rendezvous

## Usage

    (let ((rdv (make-rendezvous)))
      (bordeaux-threads:make-thread (lambda () (accept-rendezvous rdv)))
      (call-rendezvous rdv 2))

## Installation

Use Quicklisp.

## API

Thread.Comm.Rendezvous provides [CSP(Communicating sequential processes)](http://en.wikipedia.org/wiki/Communicating_sequential_processes) style inter thread communication channel.

All API are MT-Safe.

Currently run only on SBCL and Clozure-CL.

### [Function] make-rendezvous (&optional name)

Make and return new rendezvous object.

### [Generic Function] call-rendezvous (rendezvous value)

Send _value_ to and wait until _rendezvous_ acceptor thread.

Return _value_ or acceptor thread's reply value if acceptor has :reply keyword arg.

### [Generic Function] accept-rendezvous (rendezvous &key reply)

Return _value_ from passed by _rendezvous_ caller thread.

_:reply_ should be one argument funcallable object.
It called after accept call-rendezvous _value_ and before restart call-rendezvous thread.
It's evaluated value becomes call-rendezvous thread's return value.

### [Generic Function] rendezvous-name (rendezvous)

Return _rendezvous_'s name.

### [Function] nickname-package (&optional (name :rdv))

Add nickname for this package (:THREAD.COMM.RENDEZVOUS).

## Author

* Kazuo Koga

## Copyright

Copyright (c) 2011 Kazuo Koga

# License

Licensed under the MIT License.
(http://www.opensource.org/licenses/MIT)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
