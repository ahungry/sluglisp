These are example applications for Weblocks.
Originally these examples were in Weblocks repository and then moved out.

# Weblocks Demo

A simple Weblocks demo, contains two editable related tables.

* `(ql:quickload :weblocks-demo)`
* `(weblocks-demo:start-weblocks-demo)`
* Go to http://localhost:8080/weblocks-demo url

# Weblocks CLSQL Demo

Similar to "Weblocks Demo", but uses CLSQL as database layer.

* `(ql:quickload :weblocks-clsql-demo)`
* Put correct auth data for mysql into weblocks-clsql-demo/conf/stores.lisp 
  (for quicklisp usually .quicklisp/dists/quicklisp/software/weblocks-examples-.../weblocks-clsql-demo/conf/stores.lisp)
  There should be just access to empty database.
* Again `(ql:quickload :weblocks-clsql-demo)`
* `(weblocks-clsql-demo:start-weblocks-clsql-demo)`
* Go to http://localhost:8080/ url

# Weblocks Elephant Demo

Similar to "Weblocks Demo", but uses Elephant as database layer.

* `(ql:quickload :weblocks-elephant-demo)`
* Install Berkeley DB. For me it worked with libdb4.6 and libdb4.6-dev 
* Update elephant config. It is usually situated in .quicklisp/dists/quicklisp/software/elephant-.../my-config.sexp 
  I've changed first 4 strings of my-config.sexp to

  ```lisp
  (:COMPILER . :GCC) (:BERKELEY-DB-VERSION . "4.6")
  (:BERKELEY-DB-INCLUDE-DIR . "/usr/local/BerkeleyDB.4.6/include/")
  (:BERKELEY-DB-LIB-DIR . "/usr/local/BerkeleyDB.4.6/lib/")
  (:BERKELEY-DB-LIB . "/usr/local/BerkeleyDB.4.6/lib/libdb-4.6.so")
  ```

* Create weblocks-elephant-demo/data (for quicklisp usually .quicklisp/dists/quicklisp/software/weblocks-examples-.../weblocks-elephant-demo/data)
* `(weblocks-elephant-demo:start-weblocks-elephant-demo)`
* Go to http://localhost:8080/weblocks-demo url

# Simple Blog

A simple blog, contains two editable related tables with users, posts (admin interface), frontend for posts and Slava Akhmechet photo.

* `(ql:quickload :simple-blog)`
* `(simple-blog:start-simple-blog)`
* Go to http://localhost:8080/ url
