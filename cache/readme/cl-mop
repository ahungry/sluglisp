# cl-mop
###### Basic tools to make dealing with CLOS easier and portable

The goal of this library is to provide portable Common Lisp facilities for working with CLOS instances. This first release is minimal because I'm following my usual MO of only implementing things as I need them. There is therefore no call for me to go beyond `slot-names`, `map-slots`, `shallow-copy` and `deep-copy` yet (and I've used `deep-copy` in exactly one place so far, so be wary of that one; the others have been at least slightly battle-tested).

### Supports

In theory, it supports CMUCL, Lispworks, SBCL, Allegro CL, GNU Clisp and OpenMCL with native threads. Really, I've only deployed it with SBCL and did some cursory testing with GNU Clisp. If someone tries it out extensively elsewhere, let me know. As always, patches welcome for CLs not on that list.

### Exports

All of the exported symbols designate **methods** and **not functions**. While I've found the defaults published here to be useful and adequate, there are a *lot* of crazy things you can do with CLOS that I don't even try to account for. If you have special cases on hand, you can easily define your own methods for those and carry on as normal where `cl-mop` is sufficient.

In addition to these symbols, `cl-mop` also re-exports `class-slots` and `slot-definition-name`. These functions are imported in a portable fashion which accounts for the different providing packages accross different CL implementations.

##### slot-names

Designates two methods; one acting on classes and one acting on instances. In both cases, it returns a list of symbols designating the slot-names of the argument.

##### map-slots

Takes a function and an instance, and returns the sequence resulting from running the function on (slot-name slot-value) over each **bound** slot in an instance. It silently ignores unbound slots, you won't even get a `NIL` in the resulting list.

##### to-alist

Takes an instance and returns the alist of `((slot-name . value) ...)` for all bound slots. The trivial application of `map-slots`, included because I found myself using it quite often.

##### shallow-copy

Takes a CLOS instance and returns a shallow copy of it. I've found this useful in a bunch of places where I really want a copy of an existing instance with one or two slots changed (but don't want to destructively modify the underlying instance).

##### deep-copy

Takes a CLOS instance and returns a deep copy of it. That is, it allocates a new instance, then deep-copies each slot. **USE CAUTION** Like I said, I use this in exactly one place, and the only reason `shallow-copy` won't do there is that one of the slots is a vector I need to change (doing this to a shallow copy would change both the copy and the original). It works in that one use case, but I wouldn't count on it if I was doing something much crazier. 

### License

This library is hereby released for public consumption under the Expat license, reproduced below and in a separate LICENSE file.

Copyright (c) 2012 Inaimathi

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
