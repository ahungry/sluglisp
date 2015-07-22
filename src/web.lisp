(in-package :cl-user)
(defpackage sluglisp.web
  (:use :cl
        :caveman2
        :sluglisp.config
        :sluglisp.view
        :sluglisp.model
        :sluglisp.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :sluglisp.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (let ((packages (package-names)))
    (render #P"index.html"
            (list
             :packages packages
             :projectc (length packages)
             ))))

(defroute "/search/*" (&key splat)
  (let ((packages (package-search (car splat))))
    (render #P"index.html"
            (list
             :packages packages
             :search (car splat)
             :projectc (length packages)
             ))))

(defroute "/package/*" (&key splat)
  (let ((package-source (package-source (car splat))))
    (render #P"package.html"
            (list
             :package (list :name (car splat)
                            :type (car package-source)
                            :href (if package-source (build-github-href package-source) "")
                            :remote (cadr package-source)
                            :stars (package-stars (car splat))
                            :readme (package-readme (car splat))
                            )))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
