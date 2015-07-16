CQLCL
=====

A Common Lisp client to Cassandra using the native protocol (V2)

Installation
============

[QuickLisp](http://quicklisp.org) is probably the most sane way to
install CL libraries. Ensure you have this set up and working.


```shell
cd $QUICKLISP_HOME/local-projects
git clone https://github.com/AeroNotix/cqlcl.git
sbcl --eval "(ql:quickload :cqlcl)"
```

This will install all required dependencies.

API
---

```lisp
(defparameter *cxn* (cqlcl:make-connection))
(let ((create-keyspace "CREATE KEYSPACE cqlcl
                        WITH replication = {
                            'class': 'SimpleStrategy', 'replication_factor': '1'
                        }"))
  (query *cxn* create-keyspace))
;; T
;; "CREATED: cqlcl."
(let ((create-table "CREATE TABLE cqlcl.readme (
                         id uuid PRIMARY KEY,
                         name text,
                         value int
                     )"))
    (query *cxn* create-table))
;; T
;; "CREATED: cqlcl.readme

(query *cxn* "SELECT * FROM cqlcl.readme")
;; NIL

(prepare *cxn* "INSERT INTO cqlcl.readme (id, name, value) VALUES(?, ?, ?)")
;; No value

(execute *cxn* "INSERT INTO cqlcl.readme (id, name, value) VALUES(?, ?, ?)"
    (uuid:make-v4-uuid) "HELLO" 123)
;; No value
(execute *cxn* "INSERT INTO cqlcl.readme (id, name, value) VALUES(?, ?, ?)"
    (uuid:make-v4-uuid) "HELLO" 123)
;; No value
(execute *cxn* "INSERT INTO cqlcl.readme (id, name, value) VALUES(?, ?, ?)"
    (uuid:make-v4-uuid) "HELLO" 123)
;; No value

(query *cxn* "SELECT * FROM cqlcl.readme")

((B16805B2-FDA8-4DF5-811B-77F46FFE2BBB "HELLO" 123)
 (DF0922E4-BB82-4C59-A32C-CBFD859CC3AD "HELLO" 123)
 (43B0683F-8372-4BFC-B2CB-93706360A2D7 "HELLO" 123))
```

Type mappings
-------------

| Cassandra type | CL type |
|:--------------------- |:------- |
| ascii | string |
| bigint | integer |
| blob | byte-vector |
| boolean | boolean |
| counter | not implemented |
| decimal | float |
| double | float |
| float | float |
| inet | cqlcl:ip |
| int | integer |
| timestamp | integer |
| timeuuid | uuid:uuid |
| uuid | uuid:uuid |
| varchar/text | string |
| varint | not implemented |
| list | list |
| set | list |
| map | hashtbale |
| custom | not implemented |
