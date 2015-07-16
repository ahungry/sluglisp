About Form-Fiddle
-----------------
Often times I need to destructure a form definition in a macro. This is a set of simple utilities to help with that.

How To
------
There's individual functions to extract each part of a lambda-definition-form: function, name, qualifiers, lambda-list, body, declarations, docstring and the forms. You can get all in one with `split-lambda-form`, or directly as a binding macro with `with-destructured-lambda-form`.

    (split-lambda-form '(defun lambda-body (lambda-form)
                          (cddr lambda-form)))

    (with-destructured-lambda-form (:forms forms)
        '(defmacro foo (bar)
           (declare (ignore bar))
           "Testing macro!"
           (print "test!"))
      forms)

See Also
--------
* [Lambda-Fiddle](https://shinmera.github.io/lambda-fiddle) To destructure lambda-lists.
