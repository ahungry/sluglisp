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
  (render #P"index.html"
          (list
           :packages (package-names)
           )))

(defroute "/package/*" (&key splat)
  (render #P"package.html"
          (list
           :package (list :name (car splat)
                          :type (car (package-source (car splat)))
                          :href (build-github-href (cadr (package-source (car splat))))
                          :remote (cadr (package-source (car splat)))
                          :stars (package-stars (car splat))
                          :readme (package-readme (car splat))
                          ))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
