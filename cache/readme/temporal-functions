temporal-functions
==================

Adds tlambda & tdefun along with a small collection of related funcs and macros


tlambda is a lambda with an internal concept of time

    (tlambda () (before (seconds 10) (print "hi")))

Gives a function that will work for the next 10 seconds. After that with will return `nil` and release an 'expired' signal.

    (expiredp *) ;; will return true if function has expired

Can be fairly complicated and will expand into a fast state-machine, by only using the signal mechanism if all stages have expired.

    (tlambda ()
      (then
       (before (seconds 3)
         (print "hi"))
       (before (seconds 3)
         (print "still here"))
       (before (seconds 3)
         (print "Bored now"))))

Also has brevity macros

    (before (millseconds 1300)
      (print "ping!"))

    ;; expands to

    (tlambda ()
      (before (millseconds 1300)
        (print "ping!")))

    ;; which, in turn, expands to a form equivilent to

    (let* ((start (get-internal-real-time))
           (deadline (+ start (millseconds 1300))))
      (let ((func
             (lambda ()
               (let ((time (get-internal-real-time)))
                 (labels ((expired () (when (>= time deadline)
                                        deadline)))
                   (if (expired)
                       (signal-expired)
                       (print "ping!")))))))
        func))
