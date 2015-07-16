# cl-gendoc

This is a simple but flexible modular document generator for Common
Lisp, because I couldn't find something similar:

```lisp
(gendoc (:output-filename "docs.html"
         :css "simple.css")
  (:mdf "intro.md")
  (:mdf "details.md")
  (:apiref :some-package :another-package)
  (:mdf "closing.md"))
```

Of some interest is probably the API reference generator:

```lisp
(defun some-function (X)
  "=> output-forms

This takes `X` and produces *output-forms*"
  ...)
```

The docstring is processed as markdown with `3bmd`, optionally
specifying a return-spec if the first line starts with `"=>"`.

[See the generated documentation for cl-gendoc](http://rpav.github.com/cl-gendoc).
