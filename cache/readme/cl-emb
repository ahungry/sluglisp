# cl-emb: Embedded Common Lisp

A mixture of features from eRuby and HTML::Template. You could name it "Yet
Another LSP" (LispServer Pages) but it's a bit more than that and not limited to
a certain server or text format.

This is a mirror of http://mtn-host.prjek.net/projects/cl-emb

The primary development repository is in Monotone, this repository will receive
just the automated snapshots.

# License

[LLGPL](http://opensource.franz.com/preamble.html)

# Installing

```lisp
(ql:quickload :cl-emb)
```

CL-EMB can also be installed manually with [ASDF-INSTALL](http://weitz.de/asdf-install/).

# Usage

## [generic function] `EXECUTE-EMB name &key env generator-maker => string`

`NAME` can be a registered (with `REGISTER-EMB`) emb code or a pathname (type
`PATHNAME`) of a file containing the code. Returns a string.  Keyword parameter
ENV to pass objects to the code. `ENV` must be a plist. `ENV` can be accessed
within your emb code.  The `GENERATOR-MAKER` is a function which gets called
with a key and value from the given `ENV` and should return a generator function
like described
[here](http://www.cs.northwestern.edu/academics/courses/325/readings/graham/generators.html).


## [generic function] `REGISTER-EMB name code => emb-function`

Internally registeres given `CODE` with `NAME` to be called with
`EXECUTE-EMB`. `CODE` can be a string or a pathname (type `PATHNAME`) of a file
containing the code.

## [function] `PPRINT-EMB-FUNCTION name`

`DEBUG` function. Pretty prints function form, if `*DEBUG*` was `T` when the
function was registered.

## [function] `CLEAR-EMB name`

Remove named emb code.

## [function] `CLEAR-EMB-ALL`

Remove all registered emb code.

## [function] `CLEAR-EMB-ALL-FILES`

Remove all registered file emb code (registered/executed by a pathname).

## [special variable] `*EMB-START-MARKER*` (default `"<%"`)

Start of scriptlet or expression. Remember that a following `#\=` indicates an
expression.

## [special variable] `*EMB-END-MARKER*` (default `"%>"`)

End of scriptlet or expression.

## [special variable] `*ESCAPE-TYPE*`

Default value for escaping `@var` output is `:RAW` Can be changed to `:XML`,
`:HTML`, `:URI`, `:URL`, `:URL-ENCODE`, `:LATEX`.

## [special variable] `*FUNCTION-PACKAGE*`

Package the emb function body gets interned to.

Default: `(find-package :cl-emb-intern)`.

## [special variable] `*DEBUG*`

Debugging mode if `T`. Default: `NIL`.

## [special variable] `*LOCKING-FUNCTION*`

Function to call to lock access to an internal hash table.  Must accept a
function designator which must be called with the lock hold.

**IMPORTANT:** The locking function must return the value of the function it
calls!

Example:

```lisp
(defvar *emb-lock* (kmrcl::make-lock "emb-lock")
  "Lock for CL-EMB.")

(defun emb-lock-function (func)
  "Lock function for CL-EMB."
  (kmrcl::with-lock-held (*emb-lock*)
  (funcall func)))

(setf emb:*locking-function* 'emb-lock-function)
```

Files get cached and reread when they change.

The emb code consists of normal text (HTML, XML, or any other text format) and
special tags you know from eRuby or JSP (JavaServer Pages) which can hold Common
Lisp or CL-EMB's template tags, perhaps comparable to JSP's taglib.

- `<% ... %>` is a scriptlet tag, and wraps Common Lisp code.
- `<%= ... %>` is an expression tag. Its content gets evaluated and fed as a
            parameter to `(FORMAT T "~A" ...)`.
- `<%# ... #%>` is a comment. Everything within will be removed/ignored.  Can't
            be nested!

## Examples

```lisp
CL-USER> (asdf:oos 'asdf:load-op :cl-emb)
CL-USER> (cl-emb:register-emb "test1"
                              "10 stars: <% (dotimes (i 10) %>*<% ) %>")
#<CL-EMB::EMB-FUNCTION {9B74259}>
CL-USER> (cl-emb:execute-emb "test1")
"10 stars: **********"

CL-USER> (cl-emb:register-emb "test2" "2 + 2 = <%= (+ 2 2) %>")
#<CL-EMB::EMB-FUNCTION {9BCACE1}>
CL-USER> (cl-emb:execute-emb "test2")
"2 + 2 = 4"

CL-USER> (let ((emb:*emb-start-marker* "<?emb")
               (emb:*emb-end-marker* "?>"))
           (emb:register-emb "marker-test"
                             "42 + 42 = <?emb= (+ 42 42) ?>"))
#<CL-EMB::EMB-FUNCTION {97BEFD9}>
CL-USER> (emb:execute-emb "marker-test")
"42 + 42 = 84"
```

# Template Tags

You can use special template tags instead of Common Lisp code between `<%` and
`%>`. This will be translated to Common Lisp and serves as a simple shortcut for
you.

And more important: It's easier to use for non-programmers. A designer can work
on HTML code and insert these simple template tags.

Template tags start with `@`.

Currently supported: `@if`, `@else`, `@endif`, `@ifnotempty`, `@unless`, 
`@endunless`, `@var`, `@repeat`, `@endrepeat`, `@loop`, `@endloop`, `@include`,
`@includevar`, `@call`, `@with`, `@endwith`, `@set`, `@genloop`, `@endgenloop`,
`@insert`.

`@if` and `@unless` check if the given parameter is set in the supplied
environment (parameter ENV of `EXECUTE-EMB`). The environment is a plist with
keyword + value pairs. Must be terminated with `@endif` or `@endunless`.

`@ifnotempty` works like `@if` but considers the empty string false.

`@ifequal` accepts two parameters interpreted as variable names. It works like
`@if` but checks whether the values of two variables are equal. Variable names
are intepreted as in `@var`.

Note that `@ifnotempty` and `@ifequal` are supposed to be used together with
`@else` and `@endif`.

`@var` emits the corresponding value from the environment. Uses the escape type
defined in `*ESCAPE-TYPE*` (Default `:raw`, no escaping) or with -escape
modifier. E.g. `<% @var foo -escape xml %>` or without modifier `<% @var foo
%>` Supported escaping: `raw`, `xml` (aka `html`), `uri` (aka `url` or
`url-encode`), `latex`.

`@insert` inserts a given (text) file. Parameter from the environment. E.g. `<%
@insert textfile %>`.

`@repeat` repeats everything between it and `@endrepeat` the given
times. Parameter can be a number or a name. The name will be used to lookup the
corresponding value from the environment.

`@loop` loops over a named list in the environment. Environment gets set to
current plist inside this list. Must be terminated with `@endloop`.

`@include` includes a given file. Relative to current template. `@includevar`
does the same, but the parameter is treated like a variable name containing the
path to the file. Variable name is treated like in `@var`.

`@call` calls a given emb-function, which was registered with `REGISTER-EMB`.

`@with` is similar to `@loop` as it sets the current environment to the named
plist. `@loop` needs a list of plists and `@with` just a plist associated to the
given name. Block ends in `@endwith`.

`@set` is used to set special variables like `*ESCAPE-TYPE*` from within a emb
code. This way a default for a file can be specified in the file itself. The
variables are changed for the current and called/included code.  Changes to the
variables in called/included code don't effect the caller/ includer. E.g. `<%
@set escape=uri %>`. Currently supported: `escape` (`raw`, `xml`, `html`, `url`,
`uri`, `url-encode`, `latex`).

`@genloop` starts a special kind of loop: a generator loop. It must be
terminated by `@endgenloop` and operates on a generator returned by the given
`GENERATOR-MAKER` (see `EXECUTE-EMB`). The `GENERATOR-MAKER` gets called with
two parameters: the key (which is the argument to `@genloop`) and the
corresponding value in the plist. Each time in the loop the generator is called
first with the parameter `:TEST` to see if there's data left.  The generator
must return a plist on `:NEXT`, which will be the current `ENV` (like `@with` or
within a normal `@loop`).

The parameters which access the environment can just be the name of a keyword
symbol in the plist. `foo` -> :FOO in `(:FOO "bar")` Or you can provide a path
within a nested plist structure by dividing the parts of the path with a
slash. `foo/bar` -> Value of `:BAR` inside the plist at `:FOO`. `(:FOO (:BAR
"yeah"))` -> `"yeah"` Starting the parameter with a slash lets it traverse the
nested plists from the top. That way you can access top values inside loops.

Writing `<% @var foo/bar/quux %>` can be translated to `(GETF (GETF (GETF ENV
:FOO) :BAR) :QUUX)`.

## Examples

```lisp
CL-USER> (cl-emb:register-emb "test1"
                              "Foo: <% @if foo %>Yes!<% @else %>No!<% @endif %>")
#<CL-EMB::EMB-FUNCTION {9C0F2D1}>
CL-USER> (cl-emb:execute-emb "test1" :env '(:foo t))
"Foo: Yes!"
CL-USER> (cl-emb:execute-emb "test1")
"Foo: No!"
CL-USER> (cl-emb:execute-emb "test1" :env '(:foo nil))
"Foo: No!"

CL-USER> (cl-emb:register-emb "test2"
                              "What is set? -> <% @call test1 %>")
#<CL-EMB::EMB-FUNCTION {9C526E9}>
CL-USER> (cl-emb:execute-emb "test2" :env '(:foo t))
"What is set? -> Foo: Yes!"

CL-USER> (cl-emb:register-emb "test3"
                              "10 stars: <% @repeat 10 %>*<% @endrepeat %>")
#<CL-EMB::EMB-FUNCTION {9C9F1D1}>
CL-USER> (cl-emb:execute-emb "test3")
"10 stars: **********"

CL-USER> (cl-emb:register-emb "test4"
                              "<% @loop numbers %>[<% @var de %>,<% @var en %>]<% @endloop %>")
#<CL-EMB::EMB-FUNCTION {9174DF1}>
CL-USER> (cl-emb:execute-emb "test4"
                             :env '(:numbers ((:de "EINS" :en "ONE")
                                              (:de "ZWEI" :en "TWO"))))
"[EINS,ONE][ZWEI,TWO]"

CL-USER> (emb:register-emb "test5"
                           "<a href=\"http://somewhere.test/test.cgi?<% @var foo -escape uri %>\"><% @var foo %></a>")
#<CL-EMB::EMB-FUNCTION {9FBF5F1}>
CL-USER> (let ((emb:*escape-type* :html))
           (emb:execute-emb "test5" :env '(:foo "10 > 7")))
"<a href=\"http://somewhere.test/test.cgi?10+%3E+7\">10 &gt; 7</a>"

CL-USER> (emb:register-emb "test6" "1. <% @with one %>BAZ: <% @var baz %><% @endwith%>
2. <% @with two %>BAZ: <% @var baz %><% @endwith%>")
#<CL-EMB::EMB-FUNCTION {9916EB1}>
CL-USER> (emb:execute-emb "test6" :env '(:one (:baz "first")
                                         :two (:baz "second")))
"1. BAZ: first
2. BAZ: second"

CL-USER> (emb:register-emb "test7" " - <% @var foo -escape uri %> - ")
#<CL-EMB::EMB-FUNCTION {96F1239}>
CL-USER> (emb:pprint-emb-function "test7")

(LAMBDA (&OPTIONAL CL-EMB-INTERN::ENV)
  (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
    (PROGN
     (WRITE-STRING " - ")
     (FORMAT T "~A" (CL-EMB::ECHO (GETF CL-EMB-INTERN::ENV :FOO) :ESCAPE :URI))
     (WRITE-STRING " - "))))
; No value

CL-USER> (emb:register-emb "test8" "<% @set escape=xml %>--<% @var hey %>--")
#<CL-EMB::EMB-FUNCTION {962B839}>
CL-USER> (emb:register-emb "test9" "--<% @var hey %>--<% @call test8 %>--<% @var hey %>--")
#<CL-EMB::EMB-FUNCTION {96931A9}>
CL-USER> (emb:execute-emb "test9" :env '(:hey "5>2"))
"--5>2----5&gt;2----5>2--"

CL-USER> (emb:register-emb "test10" "Square root from 1 to <% @var numbers %>: <% @genloop numbers %>sqrt(<% @var number %>) = <% @var sqrt %> <% @endgenloop %>")
#<CL-EMB::EMB-FUNCTION {581EC765}>
CL-USER> (defun make-sqrt-1-to-n-gen (key n)
           (declare (ignore key))
           (let ((i 1))
             #'(lambda (cmd)
                 (ecase cmd
                   (:test (> i n))
                   (:get `(:number ,i :sqrt ,(sqrt i)))
                   (:next (prog1 `(:number ,i :sqrt ,(sqrt i))
                            (unless (> i n)
                              (incf i))))))))
MAKE-SQRT-1-TO-N-GEN
CL-USER> (emb:execute-emb "test10" :env '(:numbers 10) :generator-maker 'make-sqrt-1-to-n-gen)
"Square root from 1 to 10: sqrt(1) = 1.0 sqrt(2) = 1.4142135 sqrt(3) = 1.7320508 sqrt(4) = 2.0 sqrt(5) = 2.236068 sqrt(6) = 2.4494898 sqrt(7) = 2.6457512 sqrt(8) = 2.828427 sqrt(9) = 3.0 sqrt(10) = 3.1622777 "

CL-USER> (emb:register-emb "test11" "<% @loop bands %>Band: <% @var band %> (Genre: <% @var /genre %>)<br><% @endloop %>")
#<CL-EMB::EMB-FUNCTION {58ADB12D}>
CL-USER> (emb:execute-emb "test11" :env '(:genre "Rock" :bands ((:band "Queen") (:band "The Rolling Stones") (:band "ZZ Top"))))
"Band: Queen (Genre: Rock)<br>Band: The Rolling Stones (Genre: Rock)<br>Band: ZZ Top (Genre: Rock)<br>"

CL-USER> (emb:register-emb "test12" "<% @repeat /foo/bar/count %>*<% @endrepeat %>")
#<CL-EMB::EMB-FUNCTION {58B7583D}>
CL-USER> (emb:execute-emb "test12" :env '(:foo (:bar (:count 42))))
"******************************************"

CL-USER> (emb:register-emb "test13" "The file:<pre><% @insert textfile %></pre>")
#<CL-EMB::EMB-FUNCTION {5894326D}>
CL-USER> (emb:execute-emb "test13" :env '(:textfile "/etc/gentoo-release"))
"The file:<pre>Gentoo Base System version 1.6.14
</pre>"
```

# Credits

Uses code from John Wiseman. See http://lemonodor.com/archives/000128.html and
lsp-LICENSE.txt Thanks to Edi Weitz for letting me use his code for
`ESCAPE-FOR-XML`.

Thanks to Eitarow Fukamachi for the whitespace-trimming patch.

Thanks to Christoph Finkensiep for making `getf*` a generic function.

# Author

Stefan Scholl <stesch@no-spoon.de>

# Current Maintainer

Michael Raskin <38a938c2@rambler.ru>
