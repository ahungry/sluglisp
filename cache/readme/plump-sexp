plump-sexp
==========

Yet another SEXP to HTML thing. This time with Plump back-end.

    (plump:serialize (plump-sexp:parse '((div :id "foo") (p "bar") "baz")))
    => <div id="bar"><p>foo</p>baz</div>

Of course, you can now also transform any kind of Plump document into such an SEXP tree:

    (plump-sexp:serialize (plump:parse "<div id="bar"><p>foo</p>baz</div>"))
    => (:!ROOT ((:DIV :ID "bar") (:P "foo") "baz"))

