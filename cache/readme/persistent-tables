Persistent Hash Tables
----------------------

This is a dead simple implementation of persistent hash tables on top
of a port of David Va Horn's persistent random-access lists, SRFI-101.

These are like hash tables in most respects, with similar, but not
identical performance profiles, with the major exception that
modification is functional, `ptbl-set` returns a _new_ hash table with
the new association in it.  The previous table is left unmodified. 

Example Usage:
--------------

    (setq tbl ({} the-empty-ptbl :x 10))
    (ptbl-get tbl :x) -> 10
    (ptbl-get the-empty-ptbl :x) -> nil

The function `{}` allows you to quickly create a new ptbl or to update
the associations in a `ptbl`, when the first argument is itself a
`ptbl`.  Eg:

    ({} :x 10 :y 11)

Creates a new `ptbl` with `:x` and `:y` associations.  


    ({} a-table :x 10 :y 11)

Returns a new `ptbl` with the same associations as `a-table` except
where `:x` and `:y` are concerned.

