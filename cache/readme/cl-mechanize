# About
CL-MECHANIZE is a [WWW::Mechanize] work-alike for Common Lisp
implemented as a thin wrapper on top of [Drakma].

This package is still in the early stages of development.
Please read the source before using; do not expect this to do anything useful.

[WWW::Mechanize]: http://search.cpan.org/dist/WWW-Mechanize/lib/WWW/Mechanize.pm
[Drakma]: http://weitz.de/drakma

# Getting
`git clone git://github.com/joachifm/cl-mechanize.git cl-mechanize`

Use [QuickLisp] to easily install the dependencies.

[QuickLisp]: http://quicklisp.org/

# Usage

    $ lisp

    ;; Add system definition to the ASDF search path
    (pushnew (merge-pathnames "relative/path/to/cl-mechanize"
                              (user-homedir-pathname))
             asdf:*central-registry*)

    ;; Load system
    (asdf:operate 'asdf:load-op :cl-mechanize)
    (in-package :cl-mechanize-user)

    ;; Create browser object
    (defvar *browser* (make-instance 'browser)

    ;; Do a google search
    (fetch "http://www.google.com" *browser*)

    (let* ((page (browser-page *browser*))
           (search-form (car (page-forms page))))
      (setf (form-inputs search-form)
            '(("q" . "google")))
      (submit search-form *browser*)

      (let ((results (browser-page *browser*)))
        (format t "~A~%" (ppcre:all-matches-as-strings "<title>[a-z].*</title>"
                                                     (page-content results)))
        (dolist (link (page-links results))
          (format t "~A~%" (link-text link)))

        ;; Traverse the DOM
        (stp:do-recursively (n (page-dom results))
            ...)))
