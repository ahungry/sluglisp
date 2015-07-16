# CLSQL-Helper 

A library providing a clutch of utilities to make working with clsql easier

 * single interface functions that make a best effort to read/write a date in (m)any formats
  * convert-to-clsql-date(time) and 
 * simplified sql-expression interface
  * clsql-or(s), clsql-and(s) and clsql-exp which make building where expressions easier
 * clsql-mop help, find the primary keys of an object and query / test equality with these
 * A basic sql pretty printer, so that the code printed from
   log-database-query , which uses the clsql printer and allows easily
   recording queries executing on a given connection
 * coersion to/from clsql data types an value

## db-object - class

### Web Thread Safety - through dynamic connection binding

If you def-view-classes inherit from clsql-helper:db-object then they
will never use stored database connections (from view-database slot)
and instead will prefer the dynamic *default-database*.

Using with-database/with-a-database then become useful ways to
interact with clsql in a multithreaded web environment.

### Automatic Primary Key filling

While there is code in clsql to handle filling primary keys, I have
never quite wrapped my head around it.  Instead if you inherit from
mssql-db-object or pg-db-object the db-objects will autofill single
primary keys (from IDENTITY / SERIAL) Primary Key columns.

This is somewhat specific to how we design databases (most all tables
have a single IDENTITY / SERIAL PK) and might not be applicable to 
all situations
 
## Future

 * My company [Acceleration.net](http://www.acceleration.net) is a
   significant contributer to clsql so I hope that some of these
   changes can be moved upstream eventually


## Authors
 * [Acceleration.net](http://www.acceleration.net/) - [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

```
;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```