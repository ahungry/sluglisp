(in-package :cl-user)
(defpackage sluglisp.model
  (:use :cl :split-sequence :glyphs :drakma :cl-json :cl-ppcre)
  (:import-from :sluglisp.config
                :config)
  (:export
   :list-packages
   :build-github-href
   :package-source
   :package-readme
   :package-stars
   :package-search
   :package-names))
(in-package :sluglisp.model)

(in-readtable glyphs:syntax)

(defparameter *projects* (make-hash-table :test #'equal))
(defparameter *project-readme* (make-hash-table :test #'equal))
(defparameter *project-stars* (make-hash-table :test #'equal))
(defparameter *loaded-projects-p* nil)

(defun reset-cache ()
  (defparameter *project-readme* (make-hash-table :test #'equal))
  (defparameter *project-stars* (make-hash-table :test #'equal)))

(defun read-and-cache-projects ()
  "get the list of packages"
  (setf *loaded-projects-p* t)
  (with-open-file (s "~/src/lisp/sluglisp/projects.txt")
    (loop for line = (read-line s nil 'eof)
       until (eq 'eof line)
       do (let ((split (split-sequence #\Space line)))
            (setf (gethash (car split) *projects*) split)))))

(defun package-names ()
  "Pull out just the package names"
  (unless *loaded-projects-p* (read-and-cache-projects))
  (loop for k being the hash-keys of *projects*
     for v being the hash-values of *projects*
     collect (let ((readme-p (cache-load (car v) "readme")))
               (list :name (car v)
                     :readme (if (and readme-p (not (equal "NIL" readme-p))) "1" "0")
                     :stars (or  (cache-load (car v) "stars")
                                 (gethash (car v) *project-stars* 0))
                     :type (cadr v)))))

(defun package-source (name)
  "Get the readme data from the remote package"
  (cdr (gethash name *projects*)))

(ƒ build-github-href ~"^(http|https|git):"~ → |"https:"|)

;; https://github.com/ahungry/glyphs.git
;; https://raw.githubusercontent.com/ahungry/glyphs/master/README.md
(ƒ build-github-readme-url
   ~"(https|git|http)://github.com(.*).git"~ → |"https://raw.githubusercontent.com\\2/master/README.md"|)

;; https://github.com/ahungry/glyphs.git
;; https://raw.githubusercontent.com/ahungry/glyphs/master/README.markdown
(ƒ build-github-readme-url-long
   ~"(https|git|http)://github.com(.*).git"~ → |"https://raw.githubusercontent.com\\2/master/README.markdown"|)

;; https://github.com/ahungry/glyphs.git
;; https://raw.githubusercontent.com/ahungry/glyphs/master/README.markdown
(ƒ build-github-readme-url-tiny
   ~"(https|git|http)://github.com(.*).git"~ → |"https://raw.githubusercontent.com\\2/master/README"|)

;; https://github.com/ahungry/glyphs.git
;; https://raw.githubusercontent.com/ahungry/glyphs/master/README.org
(ƒ build-github-readme-url-org
   ~"(https|git|http)://github.com(.*).git"~ → |"https://raw.githubusercontent.com\\2/master/README.org"|)

;; https://github.com/ahungry/glyphs.git
;; https://api.github.com/repos/ahungry/glyphs/stargazers
(ƒ build-github-stargazers-url
   ~"(https|git|http)://github.com(.*).git"~ → |"https://api.github.com/repos\\2"|)

(defun char-vector-to-string (v)
  (format nil "~{~a~}" (mapcar #'code-char (coerce v 'list))))

(defun remote-json-request (uri)
  "Pull in remote JSON.  Drakma returns it as a large vector of
character codes, so we have to parse it out to string form for
cl-json."
  (let* ((json-response-raw (http-request uri))
         (json-response-string (char-vector-to-string json-response-raw))
         (json (decode-json-from-string json-response-string)))
    json))

(defun md-to-html (string)
  "Turn the markdown to html"
  (with-output-to-string (stream)
    (let ((3bmd-code-blocks:*code-blocks* t)
          (3bmd:*smart-quotes* t))
      (3bmd:parse-string-and-print-to-stream string stream))))

(defmacro jkey (k &rest rest)
  `(cdr (assoc ,k ,@rest)))

(defun cache-load (name type)
  "Load up a previously cached file"
  (let ((file-name (format nil "~~/src/lisp/sluglisp/cache/~a/~a" type name)))
    (when (probe-file file-name)
      (format nil "~{~a~^~%~}"
              (with-open-file (s file-name)
                (loop for line = (read-line s nil 'eof)
                   until (eq line 'eof)
                   collect line))))))

(defun cache-save (name type content)
  "Save the readme contents into a file in the repository"
  (let ((file-name (format nil "~~/src/lisp/sluglisp/cache/~a/~a" type name)))
    (with-open-file (s file-name :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format s "~a" content)))
  content)

(defun package-stars-remote (name)
  "Get the stargazers from github"
  (let ((source (package-source name)))
    (if source
      (or (let ((stars-url (build-github-stargazers-url (cadr source))))
            (when (and (equal "git" (car source))
                       stars-url)
              (jkey :stargazers--count (remote-json-request stars-url)))) 0)
      0)))

(defun package-stars (name)
  "Pull out the package stars or get the remote endpoint"
  (unless (gethash name *project-stars*)
    (setf (gethash name *project-stars*)
          (or (cache-load name "stars")
              (cache-save name "stars" (package-stars-remote name)))))
  (when (equal (gethash name *project-stars*) "0")
    (setf (gethash name *project-stars*)
          (cache-save name "stars" (package-stars-remote name))))
  (gethash name *project-stars*))


(defun package-readme-remote (name)
  "Get the readme data remotely"
  (let* ((source (package-source name))
         (readme-url (build-github-readme-url (cadr source))))
    (when (and (equal "git" (car source))
               readme-url)
      (let ((readme-html
             (drakma:http-request readme-url)))
        (unless (stringp readme-html)
          (setf readme-html (drakma:http-request (build-github-readme-url-long (cadr source)))))
        (unless (stringp readme-html)
          (setf readme-html (drakma:http-request (build-github-readme-url-tiny (cadr source)))))
        (unless (stringp readme-html)
          (setf readme-html (drakma:http-request (build-github-readme-url-org (cadr source)))))
        readme-html))))

(defun package-readme (name)
  "Get the readme for the package, cache it after retrieval."
  (if (gethash name *project-readme*) (gethash name *project-readme*)
      (setf (gethash name *project-readme*)
            (or (cache-load name "readme") ;; Load from file if we have
                (cache-save name "readme" (package-readme-remote name)))))
  (when (stringp (gethash name *project-readme*))
    (md-to-html (gethash name *project-readme*))))

(defun search-match-p (name term)
  "Scan a file for a match"
  (let ((markdown (cache-load name "readme")))
    (scan (format nil "(?m)(?i)~a" term) markdown)))

(defun package-search (term)
  "Scan the packages for a term"
  (remove-if-not
   (lambda (n) (search-match-p (getf n :name) term))
   (package-names)))
