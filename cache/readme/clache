CLACHE
========

CLACHE is a general caching library for Common Lisp.

Overview
--------

CLACHE provides a general caching facility for Common Lisp. The API
is similar with standard hash-table interface. Let me show you an
overview of API.

* `getcache` - Get cache from storage
* `setcache` - Store cache into storage
* `remcache` - Remove cache from storage
* `clrcache` - Clear all cache in storage

As you can see, it is easy to use. Here is an example:

    ;; Store cache
    (setcache 1 "foo")
    ;;=> 1
    
    ;; Get cache
    (getcache 1)
    ;;=> 1, T
    
    ;; Get non-exited cache
    (getcache 42)
    ;;=> NIL, NIL
    
    ;; Remove cache
    (remcache 1)
    ;;=> T
    
    ;; Clear all cache
    (clrcache)

API
---

### Caches

A cache is a triple of a key, a value, and an expiration time.

### Cache Keys

Any object can be used as a cache key if the object can be converted
into a string properly by using `cache-key-to-string`.

### Cache Values

Same as cache keys, any object can be used as a cache value. However,
a type of cache values can be limited by storages. So you have to be
careful what storage are you using.

### Expiration Time

An expiration time describes how long caches live in seconds. If an
expiration time is `nil`, such caches will never be expired:
persistent cache.

### Cache Existence

If a cache is stored in a storage and has not yet been expired or a
persitent cache, we express the cache exists in the storage.

### Storages

Storage is an abstract layer of maintaining caches. You can access
storages via API.

### Default Storage

### Function: `getcache`

    getcache key &optional storage

Retrieve a cache value from `storage` indicated by `key` and return
values of the cache value and a boolean whether the cache exists in
`storage`. The cache value will be `nil` if such the cache doesn't
exist. For example, `(getcache "not-existed-cache")` will return `nil`,
`nil`.

### Function: `setcache`

    setcache key value &optional expire storage

Store a cache `value` into `storage` with `key` and `expire`. `expire`
is an expiration time in seconds. If `expire` is `nil`, the cache will
never be expired. The return value is `value` that has been stored.

### Function: `(setf getcache)`

    (setf getcache) value key &optional expire storage

Same as `setcache`.

### Function: `remcache`

    remcache key &optional storage

Remove a cache from `storage` indicated by `key`. If the cache has
been successfully removed, this function returns `t`, otherwise
returns `nil`.

### Function: `clrcache`

    clrcache &optional storage

Remove all caches from `storage`. The return value is undefined.

### Macro: `with-cache`

### Annotation: `cache`

Protocol
--------

Supported Implementations
-------------------------

* Allegro CL v8.2
* SBCL v1.0.47
* CMU CL v20b
* Clozure CL v1.6
* ECL v11.1.1
* GNU CLISP v2.48

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
