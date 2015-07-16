cl-fbclient
===========
Common Lisp library for working with firebird databases
-----------
Library is a set of classes and methods for working with firebird databases.
Basic classes:
- fb-database
- fb-transaction
- fb-statement
- fb-error

Supported SQL-vars types:
- float
- double
- integer
- decimal(numeric)
- varchar
- timestamp
- BLOB

-----------
**Examples:**
-----------

**First**
<pre>
(require 'cl-fbclient)

;; create an instance of the database and automatically connect to the database
(defparameter *db* (make-instance 'cl-fbclient:fb-database
  				   :path "/path-to-db/db-file.fdb"))
             
;; query that returns no value
;; (transaction will be created, started and commited automatically)
(cl-fbclient:fb-noresult-query *db* "INSERT INTO T1(A1,A2) VALUES(121, 42)")

;; to query and write results to the list
;; (transaction will be created, started and commited automatically)
(cl-fbclient:fb-query-fetch-all *db* "SELECT * FROM t1")

;; disconnecting from DB
(cl-fbclient:fb-disconnect *db*)
</pre>
**Second(easier)**
<pre>
(require 'cl-fbclient)

(cl-fbclient:fb-with-database (DB :path "/path-to-db/db-file.fdb")
    (cl-fbclient:fb-with-transaction (DB TR)
     	(cl-fbclient:fb-query "SELECT * FROM T1" :tr TR)
        (cl-fbclient:fb-query "INSERT INTO T1(A1,A2) VALUES(121, 42)" :tr TR)
        (cl-fbclient:fb-query "SELECT * FROM T1" :tr TR)))
</pre>
**Third(even easier)**
<pre>
(require 'cl-fbclient)

(cl-fbclient:fb-with-database (DB :path "/path-to-db/db-file.fdb")
     (cl-fbclient:fb-query "SELECT * FROM T1" :db DB)
     (cl-fbclient:fb-query "INSERT INTO T1(A1,A2) VALUES(121, 42)" :db DB)
     (cl-fbclient:fb-query "SELECT * FROM T1" :db DB))
</pre>


**Tested on:**
  - SBCL(ubuntu)
  - CLISP(ubuntu)
  - SBCL-win32-threads (by akovalenko). (WindowsXP)

Documentation: <a href="http://github.com/klimenko-serj/cl-fbclient/wiki">**cl-fbclient/wiki**</a>
---------------     
<b>Please report me about BUGs and ask your questions by e-mail: *klimenko.serj@gmail.com* </b>

TODO:
---------------
- Add 'get metainfo' functions
- Create support types: date, time, array
- Add docstrings
- support WIKI.
- ...

