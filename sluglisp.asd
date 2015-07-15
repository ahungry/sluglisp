(in-package :cl-user)
(defpackage sluglisp-asd
  (:use :cl :asdf))
(in-package :sluglisp-asd)

(defsystem sluglisp
  :version "0.1"
  :author "Matthew Carter"
  :license "AGPLv3"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "Web based index of Quicklisp projects"
  :in-order-to ((test-op (load-op sluglisp-test))))
