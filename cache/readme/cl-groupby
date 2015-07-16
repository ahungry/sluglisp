cl-groupby
==========
A short implementation of Scalas groupBy functionality 
for Common Lisp. The function comes in its own package to avoid naming conflicts. 
Actually it should be part of a utility library.

License
-------
MIT License. See included LICENSE file.

Motivation
----------
Let's think of the task "find the first unique character" in a given string.

     (defparameter *str* "lisp rulez")

The naive approach in Lisp is as simple as elegant:
   
    (char (remove-if #'(lambda (x) (> (count x *str*) 1)) *str*) 0)

For simplicity let's ignore the point that it will throw an error when there is no uniqe char in str. 
_Unfortunately the performance of the function is quite bad, quadratic runtime._

A quicker solution would look like the following lines. While this _is_ fast, it is not as intuitive as the first solution.

    (defun first-uniq-char (str)
      (let* ((size (length str))
             (cnt (make-hash-table :size size)))
        (do
            ((i 0 (incf i)) )
            ((<= size i))
          (incf (gethash (char str i) cnt 0)))
        (do 
            ((i 0 (incf i))
             (c nil))
            ((<= size i) 
             c))))

Compare both funnctions with a long string:

    (defparameter *strlong* 
      (coerce (loop for i to 10000
                collect (code-char (+ (char-code #\a) (random 24)))) 
        'string))
    (setf (char *strlong* (- (length *strlong*) 2)) #\z)

    (time (char (remove-if #'(lambda (x) (> (count x *strlong*) 1)) *strlong*) 0))
    (time (first-uniq-char *strlong*))

A factor of something around than 600 on my machine.

Now we make use of our new groubby function:

    (time (remove-if #'(lambda (x) (> (second x) 1)) 
             (mapcar #'(lambda (x) (list (first x) (length (second x)))) 
                     (gb:groupby #'(lambda (x) x) *strlong*))))

This seems to be only a little bit slower than the second, quick function above, 
while being an intuitive and quick solution again. First group the data by 
its identity, then count each of the groups and remove the groups with more 
than one elements.

As you can see, with the groupby higher order function it is possible to 
write very comprehensive code, that takes the standard lisp solutions to the next 
level. Its drawback seems to be, that it is quite not simple to identify the good 
use of this feature to proper solve problems.


Installation
------------

Maybe we will get this into Quicklisp? Until then:
<!-- Waiting for Quicklisp incude ;-)

The most simple and recommended way to install cl-grouby is by using
[Quicklisp](http://www.quicklisp.org). If you installed Quicklisp a simple

    (ql:quickload :groubpy)

will download the package and acually load it. You only need to do
this once per machine. Later a

    (require :groupby)

will be enough.
-->


You may get the code with:

    git clone git://github.com/wlbr/cl-groubpy.git

Either you add this to your asdf repository, then you will only need
to do a `(require :groupby)` in your source.

Or, you may put the source in a subdirectory of your project and add
the file `groupby.asd` with its full path to your own asdf definition.

Or, you may put the source in a subdirectory of your project and load
the file "groupby.asd" directly. After that a `(asdf:load-system "groupby")`
should be sufficient.

Or, as a kind of worst case, but definitely the most simple way, as this 
project consists of one function only: simply do a 
direct:

    (load "package.lisp")
    (load "groupby.lisp")

Well, the whole thing is really just one function. Probably it is best to simply copy 
the few lines into your own code.

Dependencies
---------------
None except for asdf.

xlunit for the unit tests only (the tests are not included in the asdf system).

Testing
----------
Tested with SBCL and CCL. No rocket science required, should run in
any environment.

A set of unit tests is included in tests.lisp.


Reporting problems
------------------
If you run into any trouble or find bugs, please report them 
via [the Github issue tracker](http://github.com/wlbr/cl-groupby/issues).



Contributors
------------
Written by Michael Wolber.
