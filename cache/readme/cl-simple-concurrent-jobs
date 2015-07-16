cl-simple-concurrent-jobs
=========================

A simple API for running concurrent jobs in Common Lisp. `cl-scj` for short.


Dependencies
============

`bordeaux-threads` and `chanl` - for pain-free installation use [quicklisp](http://quicklisp.org) and something like `(ql:quickload '(bordeaux-threads chanl))`


Documentation
=============

This is it. There's currently no other documentation. The focus here is 'simple' so a README should be enough to describe it. By example, no less.

Create a job executor

    > (setf *je* (cl-scj:create-job-executor :num-threads 4))
    #<CL-SIMPLE-CONCURENT-JOBS::JOBEXECUTOR {25842F19}>

At this point the threads have been created and are blocking, waiting for jobs. First let's create a way to create jobs easily

    > (defun make-job (id)
        (let ((out *standard-output*))
          (lambda ()
            (format out "Starting very important job number ~d~%" id)
            (sleep (random 5))
            (format out "Finished very important job number ~d~%" id)
            (format out "Job ~d returning result ~d~%" id (/ id 100))
            (/ id 100))))
    MAKE-JOB

We can test it

    > (funcall (make-job 1000))
    Starting very important job number 1000
    Finished very important job number 1000
    Job 1000 returning result 10
    10

Add one to the executor

    > (cl-scj:add-job *je* (make-job 0))
    Starting very important job number 0
    1
    Finished very important job number 0
    Job 0 returning result 0

That returned value of 1 is the number of total jobs the executor think it's supposed to have run. Now that it's completed its job, we can get the results:

    > (cl-scj:join-results *je*)
    (0)

Excellent. What about these 4 threads though?

    > (setf *je* (cl-scj:create-job-executor :num-threads 4))
    > (dotimes (x 20) (cl-scj:add-job *je* (make-job (+ 1 x))))
    Starting very important job number 1
    Starting very important job number 2
    Starting very important job number 3
    Starting very important job number 4
    NIL
    Finished very important job number Finished very important job number 21

    Job Job 21 returning result  returning result 11//15000

    Starting very important job number 5
    Starting very important job number 6
    Finished very important job number 4
    Job 4Finished very important job number 3
     returning result Job 31 returning result /25
    3/100
    ...
    <snip>

That might take a while to finish. You should notice that only four jobs began concurrently (because we specified `:threads 4`). Afterwards, the ~~random sleep~~ important job time variation jumbled them up.

If you're quick, you can call `join-results` and see it block

    > (cl-scj:join-results *je*)
    (1/5 19/100 17/100 9/50 3/20 4/25 13/100 7/50 11/100 3/25 9/100 1/10 7/100 2/25 3/50 1/20 3/100 1/25 1/50 1/100 0)

The last thing to consider is how to stop the jobs (if you want to). First kick off an irritating number of jobs

    > (dotimes (x 1000) (cl-scj:add-job *je* (make-job x)))
    Starting very important job number Starting very important job number 01

    Starting very important job number 2
    Starting very important job number 3
    NIL
    ...
    <snip>

And after you have had enough of looking at the mangled `*standard-output*`

    > (cl-scj:stop)
    Finished very important job number 43
    Job 43 returning result 43/100
    Finished very important job number 40
    Job 40 returning result 2/5
    ...
    <snip>

The currently executing jobs will complete and push their results onto the return value of `(cl-scj:join-results *je*)` but that call will return immediately with whichever results it already has.

Finally, if you want to check if the results are available in a non-blocking-way you can try `(cl-scj:has-all-results *je*)` but note that if you call `stop` this will probably never be `T` unless you stopped it while all remaining jobs were executing.

Some miscellaneous notes:
* An error is a valid form of completion as far as this is concerned, so don't believe that there were no errors in your callables just because `join-results` returns immediately
* An error puts a `nil` in results
* You can't call `join-results` more than once on a `JobExecutor` - it calls `stop`
* I don't know if the API is thread-safe
* There is more README than code

If you're still confused please let me know why and I'll try to improve the documentation. Fixes, suggestions, requests, criticisms, enhancements and other contributions are all very welcome. See the contact section below if you're keen.


Why should I use it?
====================

I'm not sure. I wrote it because I couldn't find something with such a simple API to do exactly what I wanted (dynamically execute jobs concurrently then give me all the results). If you know of a pre-existing or better system that does this with a simple API please let me know.


Is it safe? Will it work for me?
================================

Maybe. It's working for me so far.


The Latest Version
==================

To the extent that I'm using versioning, this is version `1.0.0`. If I ever need to create another version, I promise to follow [Semantic Versioning 2.0.0](http://semver.org/spec/v2.0.0.html).


Installation
============

I just use `(asdf:load-system :cl-simple-concurrent-jobs)` - YMMV.


Licensing
=========

Please see the LICENSE file


Contact
=======

Github issues and pull-requests are the preferred method. If you live in Brighton and want to buy me a beer go ahead, you'll get a special mention here (remind me that it's a contribution to cl-simple-concurrent-jobs):

Beer contributors: `'()`