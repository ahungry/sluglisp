
#LINUX DEPENDENCIES

  Requires either fam or gamin, and corresponding development package
  (for /usr/include/fam.h). It seem that gamin is a better choice,
  because it uses inotify facility in modern kernels. 

#SYNOPSIS

```common-lisp
(ql:quickload :cl-fam)

(cl-fam:fam-open)

(cl-fam:fam-monitor-directory "/tmp")

;; #<CL-FAM::FAM-DIRECTORY-REQUEST {10201EC243}>

(loop while (cl-fam:fam-pending-p)
      collect (multiple-value-list (cl-fam:fam-next-event t)))

;; ((:FAM-EXISTS "wfsnp" #<CL-FAM::FAM-DIRECTORY-REQUEST {10201EC243}>)
;; (:FAM-EXISTS ".tmp18421" #<CL-FAM::FAM-DIRECTORY-REQUEST {10201EC243}>)
;; (:FAM-EXISTS "ztjvq" #<CL-FAM::FAM-DIRECTORY-REQUEST {10201EC243}>)

;; touch some files in /tmp

;; FAM-NEXT-EVENT returns FAM-EVENT object by default

(loop while (cl-fam:fam-pending-p)
      as event = (cl-fam:fam-next-event)
      collect (cons (cl-fam:fam-code event)
                    (cl-fam:fam-filename event)))

;; ((:FAM-EXISTS "wghpa"))

;; Multiple monitoring requests can run at the same time, they can be
;; distinguished by the request object returned by the
;; FAM-MONITOR-FILE/DIRECTORY functions. Each connection has a file
;; descriptor which can be used in select/poll, to integrate event
;; monitoring into multiple wait scenarios

(cl-fam:fam-fd)
;; 6
```

For more full description of the API see http://www.docunext.com/wiki/Gamin

See docstrings for exported functions for more info.

If you want to try it out with multiple threads, bind `CL-FAM:*FAM*`
in the thread, this way the FAM connection would be open per-thread.
  

