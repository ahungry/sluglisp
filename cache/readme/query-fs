query-fs
========

It is a mirror of QueryFS monotone repository. QueryFS is stable enough that difference is minimal, but the true repository is in Monotone: http://mtn-host.prjek.net/viewmtn/cl-fuse/branch/changes/com.ignorelist.401a0bf1.raskin.cl-fuse.query-fs


To run a minimal test, create /tmp/test-query-fs/ and copy example-queries as
queries there, and example-plugins as plugins. Then load query-fs and run

(query-fs:run-fs :target "/tmp/test-query-fs/")

in your Common Lisp REPL. Example command for SBCL and Quicklisp:

sbcl --load setup.lisp --eval '(quicklisp:quickload :query-fs)' --eval '(query-fs:run-fs :target "/tmp/test-query-fs/")' --eval '(quit)'

