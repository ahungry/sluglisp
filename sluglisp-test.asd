(in-package :cl-user)
(defpackage sluglisp-test-asd
  (:use :cl :asdf))
(in-package :sluglisp-test-asd)

(defsystem sluglisp-test
  :author "Matthew Carter"
  :license "AGPLv3"
  :depends-on (:sluglisp
               :prove)
  :components ((:module "t"
                :components
                ((:file "sluglisp"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
