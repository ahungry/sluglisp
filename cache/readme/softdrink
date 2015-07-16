About Softdrink
---------------
Softdrink is a small toolkit to extract CSS from HTML or inline it. Softdrink relies on [lQuery](http://shinmera.github.io/lquery/) and [LASS](http://shinmera.github.io/LASS/).

How To
------
Extracting style information happens with `slurp`.

    (softdrink:slurp "<div><foo/><foo id=\"bla\" style=\"bar:baz;bluh: bbab\" /></div>")

Injecting CSS back in happens with `mix`. You can then prettify the output with `pour`.

    (softdrink:pour (softdrink:mix "<foo><p/></foo>" '(p :text-decoration underline)))

Softdrink includes support for the LASS item-types `:BLOCK` and `:PROPERTY`. Any other type needs to be added with `DEFINE-INLINER` or `DEFINE-MANIPULATOR`.

See Also
--------
* [lQuery](http://shinmera.github.io/lquery/), used to easily traverse and manipulate the DOM
* [LASS](http://shinmera.github.io/LASS/), to process and markup CSS in a more Lispy manner.
* [Plump](http://shinmera.github.io/plump/), to parse and serialize X(HT)ML.
