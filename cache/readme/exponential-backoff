# Exponential backoff
![](https://travis-ci.org/death/exponential-backoff.svg)

An implementation of the [exponential backoff](http://en.wikipedia.org/wiki/Exponential_backoff) algorithm in Common Lisp.

Inspired by the implementation found in [Chromium](https://github.com/adobe/chromium/blob/master/net/base/backoff_entry.cc).
Read the [header file](https://github.com/adobe/chromium/blob/master/net/base/backoff_entry.h) to learn about each of the parameters.

# Example

If you like to SLEEP or otherwise block, and don't care to signal
errors until giving up:

```lisp
(defun noisy-sleep (seconds)
  (format t "Sleeping for ~A seconds.~%" seconds)
  (sleep seconds))

(exponential-backoff:with-exponential-backoff ((:initial-delay-ms 1000 :jitter-factor 0.1)
                                               :max-retries 3 :sleep-function #'noisy-sleep)
  (format t "Trying...~%")
  (error "Something bad happened."))

;; Output:
;;
;; Trying...
;; Sleeping for 0.9575 seconds.
;; Trying...
;; Sleeping for 1.8265 seconds.
;; Trying...
;; Sleeping for 3.855 seconds.
;; Trying...
;; ; Evaluation aborted on #<SIMPLE-ERROR "Something bad happened." {1009EB9E83}>.
```

A more involved example that uses the backoff information by itself:

```lisp
(loop with b = (exponential-backoff:exponential-backoff
                :num-failures-to-ignore 1
                :initial-delay-ms (* 5 1000)
                :jitter-factor 0.1
                :max-backoff-ms (* 5 60 1000))
      for time-to-wait = (exponential-backoff:time-until-release b)
      for will-succeed in '(t nil nil nil t nil nil t t nil nil)
      do (format t "Waiting for ~F ms before ~:[failing~;succeeding~]...~%" time-to-wait will-succeed)
      do (sleep (/ time-to-wait 1000))
      do (exponential-backoff:inform b will-succeed))

;; Output:
;;
;; Waiting for 0.0 ms before succeeding...
;; Waiting for 0.0 ms before failing...
;; Waiting for 0.0 ms before failing...
;; Waiting for 4647.0 ms before failing...
;; Waiting for 9893.5 ms before succeeding...
;; Waiting for 0.0 ms before failing...
;; Waiting for 9357.5 ms before failing...
;; Waiting for 18710.0 ms before succeeding...
;; Waiting for 0.0 ms before succeeding...
;; Waiting for 0.0 ms before failing...
;; Waiting for 9969.0 ms before failing...
```

# License

MIT
