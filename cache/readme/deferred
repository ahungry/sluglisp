About Deferred
--------------
Sometimes I would like to include code in my project that is only available if certain libraries are loaded. Since I don't want to require these libraries as dependencies, but can't refer to their symbols either without them being loaded, things get a bit messy. This library attempts to provide a much more convenient way.

How To
------
Deferred uses two components: a dispatch reader macro on `#^` and two macros, `WITH-DEFERRED-LIBRARY` or `WHEN-PACKAGES`. `#^` is used to circumvent the reader problem of being unable to refer to symbols in packages that don't exist at read-time. The macro is used to then acquire access to the symbol's environment to figure out the proper transformation for the symbol into proper values.

`WITH-DEFERRED-LIBRARY` is used to specify deferred libraries that are needed at execution time. Meaning the required library can be loaded only just when the code block is executed.

```
(with-deferred-library (:drakma)
  (#^http-request "http://google.com"))
```

Deferred symbols can be used in function calls, with `FUNCTION` and `QUOTE` and as values (f.e. special variables). Currently unsupported are deferred `SETF` functions as it's impossible to properly defer a `SETF`-expander until execution-time.

If instead you are looking for code that is only compiled when certain libraries are already loaded before yours, there is `WHEN-PACKAGES`. This macro only outputs code when the specified list of packages can be found, otherwise it expands to NIL. This is useful if you don't want to encounter the edge-cases and performance penalties of `WITH-DEFERRED-LIBRARY` and can depend on the fact that if the deferred feature is needed, its dependencies are loaded before your system.

```
(defun frillneckedlizard ()
  (error "NOT IMPLEMENTED!"))

(when-packages (:drakma)
  (defun frillneckedlizard ()
    (#^drakma:http-request "http://frillneckedlizard.moe")))
```

If the default dispatch reader macro has been overwritten in your readtable for one reason or another, you can use `(named-readtables:in-readtable :deferred)`.
