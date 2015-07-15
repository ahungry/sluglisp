(in-package :cl-user)
(defpackage sluglisp.model
  (:use :cl :split-sequence :glyphs :drakma)
  (:import-from :sluglisp.config
                :config)
  (:export
   :list-packages
   :package-source
   :package-readme
   :package-names))
(in-package :sluglisp.model)

(in-readtable glyphs:syntax)

(defparameter *project-dir* "~/src/lisp/quicklisp-projects")

(defun list-packages (&optional (project-dir *project-dir*))
  "Generate a list of all the packages."
  (cdr (directory
        (format nil "~a/*.*" (string-right-trim "/" project-dir)))))

(defun package-names ()
  "Pull out just the package names"
  (ψ (list-packages)
     α → (list :name (car (last (butlast (split-sequence #\/ (namestring α))))))))

(defun package-source (name)
  "Get the readme data from the remote package"
  (split-sequence
   #\Space
   (car
    (with-open-file (s (format nil "~a/~a/source.txt" *project-dir* name))
      (loop for line = (read-line s nil 'eof)
         until (eq line 'eof)
         collect line)))))

;; https://raw.githubusercontent.com/ahungry/glyphs/master/README.md
;; https://github.com/ahungry/glyphs.git
(ƒ build-github-readme-url
   ~"https://github.com(.*).git"~ → |"https://raw.githubusercontent.com\\1/master/README.md"|)

(defun md-to-html (string)
  (with-output-to-string (stream)
    (let ((3bmd-code-blocks:*code-blocks* t)
          (3bmd:*smart-quotes* t))
      (3bmd:parse-string-and-print-to-stream string stream))))

(defun package-readme (name)
  (let ((source (package-source name)))
    (when (equal "git" (car source))
      (let ((readme-html
             (drakma:http-request
              (build-github-readme-url (cadr source)))))
        (when readme-html (md-to-html readme-html))))))
