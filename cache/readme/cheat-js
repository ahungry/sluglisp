
# Cheat-JS - macros for JavaScript. Kinda.

This document assumes you already know Common Lisp and JavaScript.

## About Cheat-JS

Lisp macros are powerful and easy to implement because Lisp programs
are made of s-expressions.

Lisp-style macros are difficult to add to other languages because most
languages have very non-uniform syntax compared to Lisp. Source
transformations (and most importantly macros) would be easier in, say,
JavaScript, if it were possible to convert the JavaScript code into
s-expressions, transform it, and convert it back into JavaScript code.

Turns out that we can transform JavaScript code into an AST made of
s-expressions using
[`parse-js`](http://marijnhaverbeke.nl/parse-js/). Converting back
into JavaScript code can be done with
[`cl-uglify-js`](https://github.com/mishoo/cl-uglify-js) (ironically,
`cl-uglify-js:ast-gen-code` is a capable pretty printer). All that
remains to be done to have macros (well, defined in another language)
is define transformations to be applied on the output of
`parse-js`. This is what Cheat-JS does: get the `parse-js` AST, apply
the transformations, convert back to JavaScript code.

The idea is rather obvious - the main reason Cheat-JS exists is that I
could not find something similar on the net. There are probably many
people who privately do similar things with tools like `parse-js` and
the pretty-printer part of `cl-uglify-js` - that, or my Google skills
failed me :)

### Important Note

Cheat-JS includes a modified version of `parse-js`, written by Marijn
Haverbeke. This is necessary because I (Miron Brezuleanu) needed to
modify `parse-js` a little. The license of `parse-js` is in the
`LICENSE-parse-js.txt` file. The modified files from `parse-js`
included in Cheat-JS are `parse.lisp`, `tokenize.lisp` and
`util.lisp`. The modifications were permitted by the `parse-js`
license. This is not an official copy of `parse-js` and is not
supported by Marijn Haverbeke. If the modified parsing code in
Cheat-JS breaks, it's exclusively my fault - I messed up the code.

Cheat-JS also uses `cl-uglify-js` unmodified, via
[Quicklisp](http://www.quicklisp.org/). These two libraries do most of
the work, Cheat-JS is mostly 'glue code'.

### BIG WARNING

I haven't used Cheat-JS on any large projects. I don't have enough
imagination to compensate for this lack of experience, so it may have
a lot of problems I haven't thought about. Right now it's just a proof
of concept.

## Some simple examples

Instead of writing:

    var Person = function(name, shoeSize) {
        this.name = name;
        this.shoeSize = shoeSize;
    };

Cheat-JS makes it possible to write:

    var Person = @defclass(name, shoeSize);

(Some irregularities in JavaScript syntax make it much harder to
expand something like `@defclass(Person, name, shoeSize);` into
`function Person(name, shoeSize) { this.name = name; this.shoeSize =
shoeSize; };`).

This assumes that we have defined a `@defclass` macro which does the
above expansion - we'll define two such macros in this document.

One of the `parse-js` modifications necessary for this to work is
allow `@` as a character in identifiers (the Cheat-JS recommended
convention for naming macros is `@` followed by the macro
name). Currently macro names can't be nested
(i.e. `some.namespace.@iife` is not valid syntax).

Instead of writing:

    var greeter = (function () {
        return {
            'hello': function(name) {
                console.log('hello, '+ name);
            };
        };
    }());

Cheat-JS makes it possible to write:

    var greeter = @iife(
        return {
            'hello': function(name) {
                console.log('hello, '+ name);
            }
        };
    );

This assumes that we have defined a `@iife` macro that wraps its
arguments with `(function () { ... }())`
([meaning of the IIFE acronym](http://www.benalman.com/news/2010/11/immediately-invoked-function-expression/)).

I also had to modify `parse-js` to convince it to parse macro
invocations that look like function calls, but have a list of
statements instead of a list of parameters (see the invocation of
`@iife` above).

Instead of writing:

    (function () {
        var testResult = someTest();
        if (testResult) {
            console.log("Yes!");
            console.log("We passed the test with result ", testResult, ".");
        }
    })();

Cheat-JS makes it possible to write:

    @whenLet(testResult, someTest();
        console.log("Yes!");
        console.log("We passed the test with result ", testResult, ".");
    );

This macro is similar to
[`alexandria:when-let`](http://common-lisp.net/project/alexandria/draft/alexandria.html#Data-and-Control-Flow). `@whenLet`
has the most complicated interface possible for a Cheat-JS macro: its
argument list has both expressions (`testResult`, `someTest()`) and
statements (the `console.log` call). When invoking such macros,
separate the expressions and the statements with a semicolon, as in
the example above.

It is of course possible to define the anaphoric version of
`@whenLet`, `@awhen` (from
[On Lisp](http://paulgraham.com/onlisp.html), page 190).

The guide on how to write Cheat-JS macros (below in this document) is
based on defining `@defclass` (and even a safer version of
`@defclass`), `@iife`, `@whenLet` and `@awhen`.

## Getting started

You can get Cheat-JS at `http://github.com/mbrezu/cheat-js`. It's
probably best to `git clone` it inside the `local-projects` directory
of your Quicklisp install, so you can load it with `(ql:quickload
:cheat-js)` in your REPL.

Note: I've only tested it with SBCL, so I recommend you use SBCL
too. It should work with other CL implementations, though (all the
code required is standard CL).

Running the tests with:

    (cheat-js:run-tests)

gives some confidence that things are not obviously broken (they are
most likely broken, but in ways that are subtle enough to fool the
tests).

The next section will provide you with some pointers on how to define
your Cheat-JS macros. If you run into problems, look at the
`tests.lisp` file for example code - the code there matches the text
in the next section.

## Your first Cheat-JS macros

Before running the macroexpansion function `cheat-js:explode` on your
JavaScript source code, you need to install your macros.

First reset the list of installed macros:

    > (cheat-js:clear-macros)

To define a macro, you need to know three things:

 * how the macro invocation call looks like (what you want to write in
   the JavaScript source code); this is the macro's "API"; it is
   JavaScript code (or almost);
 * how the macro expansion looks like in JavaScript; this is the
   macro's "result";
 * how to transform the AST of the invocation into the AST of the
   expansion; this is the macro's "implementation", written in Common
   Lisp.

Let's define `@defclass`.

### Defining `@defclass`

We need to see how the macro invocation looks like in JavaScript:

    var Person = @defclass(name, shoeSize)

We can tell that the macro is an 'args only' macro (i.e. the
invocation looks like a normal JavaScript function invocation, it does
not contains statements). We can inform Cheat-JS about this:

    > (cheat-js:register-args-macro "@defclass")

We also want to see the AST for the invocation (we use Cheat-JS's
parsing function because the above snippet is not parsable by
`parse-js` without tweaks; in particular, the parsing won't work as
expected if we don't call `register-args-macro` as above, so don't
skip that step):

    > (cheat-js:parse-js "var Person = @defclass(name, shoeSize);")
    (:TOPLEVEL
     ((:VAR
       (("Person" :MACRO-CALL (:NAME "@defclass")
         (:ARGS (:NAME "name") (:NAME "shoeSize")))))))

The part that starts with `:MACRO-CALL` is the interesting part; this
is the AST representation of our macro invocation; this is what we
need to transform into the expansion (don't worry about the apparently
missing `(` in front of `:MACRO-CALL` above, it's because the list
following `:VAR` is made of conses, not lists).

What does the expansion look like? Let's see:

    > (cheat-js:parse-js "var Person = function(name, shoeSize)
                          {
                              this.name = name;
                              this.shoeSize = shoeSize;
                          };")

    (:TOPLEVEL
     ((:VAR
       (("Person" :FUNCTION NIL ("name" "shoeSize")
         ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
          (:STAT
           (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))))))

Comparing the two ASTs reveals that we need to transform the
`(:MACRO-CALL` s-expression into the `(:FUNCTION` s-expression.

To make it clearer, we need to write a Common Lisp function to transform this:

    (:MACRO-CALL (:NAME "@defclass")
     (:ARGS (:NAME "name") (:NAME "shoeSize")))

into this:

    (:FUNCTION NIL ("name" "shoeSize")
     ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
      (:STAT (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))

We only need the macro arguments to perform the expansion. Since we
told Cheat-JS this is an 'args only' macro, it knows we're only
interested in the arguments (not the entire `:MACRO-CALL` tree), so
that's what it will pass to our expander function:

    > (defun defclass-expander (args)
        (let* ((names (mapcar #'second args)))
          `(:function nil ,names
                      ,(mapcar (lambda (name)
                                 `(:stat
                                   (:assign t
                                            (:dot (:name "this") ,name)
                                            (:name ,name))))
                               names))))

The parameter `args` contains the list of arguments extracted from
`(:ARGS...` above (in our case `((:NAME "name") (:NAME
"shoeSize"))`). The value returned by the function should be the
expansion shown above (s-expression starting with `(:FUNCTION...`).

Let's test it:

    > (let ((args '((:NAME "name") (:NAME "shoeSize"))))
        (defclass-expander args))
    (:FUNCTION NIL ("name" "shoeSize")
               ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
                (:STAT (:ASSIGN T
                                (:DOT (:NAME "this") "shoeSize")
                                (:NAME "shoeSize")))))

Looks OK. Let's tell Cheat-JS about our function:

    > (cheat-js:register-macro-expander "@defclass" #'defclass-expander)

Now we can ask Cheat-JS to macroexpand our code:

    > (cheat-js:explode "var Person = @defclass(name, shoeSize);")
    "var Person = function(name, shoeSize)
    {
        this.name = name;
        this.shoeSize = shoeSize;
    };"

`cl-uglify-js` is a really good pretty printer, isn't it?

On to `@iife`.

### Defining `@iife`

Again, we'll see the invocation (both JavaScript and `parse-js` AST),
expansion and implementation for the macro.

Let's recall the JavaScript for the invocation from the examples
above:

    var greeter = @iife(
        return {
            'hello': function(name) {
                console.log('hello, '+ name);
            }
        };
    );

This is a 'body' macro - its only argument is a list of JavaScript
statements. Let's tell Cheat-JS:

    > (cheat-js:register-body-macro "@iife")

We can now ask the parser for the invocation AST. We'll work with a
simplified invocation, though - the example above will generate a
large AST, and we can just as well manage with a smaller one. It's
also better if our invocation has more than one statement, so let's
try this:

    > (cheat-js:parse-js "var a = @iife(alert(1); return 1;);")
    (:TOPLEVEL
     ((:VAR
       (("a" :MACRO-CALL (:NAME "@iife")
         (:BODY (:STAT (:CALL (:NAME "alert") ((:NUM 1))))
                (:RETURN (:NUM 1))))))))

The expansion we desire for this invocation:

    > (cheat-js:parse-js "var a = (function() { alert(1); return 1; })();")
    (:TOPLEVEL
     ((:VAR
       (("a" :CALL
         (:FUNCTION NIL NIL
          ((:STAT (:CALL (:NAME "alert") ((:NUM 1)))) (:RETURN (:NUM 1))))
         NIL)))))

OK. We need to expand:

    (:MACRO-CALL (:NAME "@iife")
      (:BODY (:STAT (:CALL (:NAME "alert") ((:NUM 1))))
             (:RETURN (:NUM 1))))

into:

    (:CALL (:FUNCTION NIL NIL
             ((:STAT (:CALL (:NAME "alert") ((:NUM 1))))
              (:RETURN (:NUM 1))))
           NIL)

The function to do this is:

    (defun iife-expander (body)
      `(:CALL (:FUNCTION NIL NIL ,body)
              NIL))

Since we told Cheat-JS this is a 'body only' macro, it extracts the
body from the `:MACRO-CALL` AST and passes it to our expander.

A quick test:

    > (let ((body '((:STAT (:CALL (:NAME "alert") ((:NUM 1))))
                    (:RETURN (:NUM 1)))))
        (iife-expander body))
    (:CALL
     (:FUNCTION NIL NIL
                ((:STAT (:CALL (:NAME "alert") ((:NUM 1))))
                 (:RETURN (:NUM 1))))
     NIL)

Looks OK. Let's install it:

    > (cheat-js:register-macro-expander "@iife" #'iife-expander)

Let's use it:

    > (cheat-js:explode "var a = @iife(alert(1); return 1;);")
    "var a = function() {
        alert(1);
        return 1;
    }();"

Oops. Some parens got dropped. This is not really an issue, the
resulting JavaScript is still valid. Let's try another example:

    > (cheat-js:explode "@iife(alert(1); return 1;);")
    "(function() {
        alert(1);
        return 1;
    })();"

Good! The parens were required this time, and they are there.

Now that we have `@iife` we can write a safer `@defclass` (and also
see an example of combining Cheat-JS macros).

### Defining a safer `@defclass`

We used the following Javascript 'class definition' for the
`@defclass` example:

    var Person = function(name, shoeSize)
    {
        this.name = name;
        this.shoeSize = shoeSize;
    };

This code has a well-known problem. To create a `Person` object, one
should use a call like `new Person('John', 42);`. If we forget the
`new` keyword, we are in trouble, because `this` inside the function
no longer refers to a newly created object, but to `window`, the
global object.

Isn't there a way around this? Let's try to define `Person` like this:

    var Person = (function () {
        function Person(name, shoeSize) {
            this.name = name;
            this.shoeSize = shoeSize;
        }
        return function (name, shoeSize) {
            return new Person(name, shoeSize);
        };
    }());

Now it doesn't matter if we use `new` or we leave it out. A new
`Person` object is always returned. The new definition is a bit
verbose, though. Maybe we can use... a macro?

Let's examine the three pieces of data we need for a new macro. The
invocation in JavaScript:

    var Person = @safeDefclass(Person, name, shoeSize);

This is certainly more like it, conciseness-wise.

`@safeDefclass`, like `@defclass`, is an 'args only` macro:

    > (cheat-js:register-args-macro "@safeDefclass")

The AST of the invocation:

    > (cheat-js:parse-js "var Person = @safeDefclass(Person, name, shoeSize);")
    (:TOPLEVEL
     ((:VAR
       (("Person" :MACRO-CALL (:NAME "@safeDefclass")
         (:ARGS (:NAME "Person") (:NAME "name") (:NAME "shoeSize")))))))

What about the expansion? Do we want the expansion shown above, or is
it possible to expand to something shorter? This is a macro-combining
opportunity, let's not waste it. The expansion is:

    var Person = @iife(
        function Person(name, shoeSize) {
            this.name = name;
            this.shoeSize = shoeSize;
        };
        return function (name, shoeSize) {
            return new Person(name, shoeSize);
        };
    );

If your REPL was restarted after defining `@iife`, please reinstall
`@iife` in the new REPL by using the instructions in the last section
(or the code in `tests.lisp`).

Now we can ask `cheat-js` about the AST of the expansion:

    > (cheat-js:parse-js "var Person = @iife(
                              function Person(name, shoeSize) {
                                  this.name = name;
                                  this.shoeSize = shoeSize;
                              };
                              return function (name, shoeSize) {
                                  return new Person(name, shoeSize);
                              };
                          );")
    (:TOPLEVEL
     ((:VAR
       (("Person" :MACRO-CALL (:NAME "@iife")
         (:BODY
          (:DEFUN "Person" ("name" "shoeSize")
           ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
            (:STAT
             (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))
          (:BLOCK NIL)
          (:RETURN
           (:FUNCTION NIL ("name" "shoeSize")
            ((:RETURN
              (:NEW (:NAME "Person") ((:NAME "name") (:NAME "shoeSize")))))))))))))

So our `:MACRO-CALL` expands into a new `:MACRO-CALL`. Cheat-JS should
be able to handle this, like a good little macroexpander.

To make writing the expansion function easier, let's isolate the
source and target ASTs. The invocation:

    (:MACRO-CALL (:NAME "@safeDefclass")
     (:ARGS (:NAME "Person") (:NAME "name") (:NAME "shoeSize")))

and our desired expansion:

    (:MACRO-CALL (:NAME "@iife")
     (:BODY
      (:DEFUN "Person" ("name" "shoeSize")
       ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name") (:NAME "name")))
        (:STAT (:ASSIGN T (:DOT (:NAME "this") "shoeSize") (:NAME "shoeSize")))))
      (:BLOCK NIL)
      (:RETURN
       (:FUNCTION NIL ("name" "shoeSize")
        ((:RETURN (:NEW (:NAME "Person") ((:NAME "name")
                                          (:NAME "shoeSize")))))))))

The expander function:

    (defun safe-defclass-expander (args)
      (let* ((names (mapcar #'second args))
             (class-name (first names))
             (field-names (rest names)))
        `(:MACRO-CALL
          (:NAME "@iife")
          (:BODY
           (:DEFUN ,class-name ,field-names
             ,(mapcar (lambda (field)
                        `(:STAT (:ASSIGN T
                                         (:DOT (:NAME "this") ,field)
                                         (:NAME ,field))))
                      field-names))
           (:BLOCK NIL)
           (:RETURN
             (:FUNCTION NIL ,field-names
                        ((:RETURN (:NEW (:NAME ,class-name)
                                        ,(mapcar (lambda (field)
                                                   (list :name field))
                                                 field-names))))))))))

Test the expander function:

    > (let* ((args '((:NAME "Person")
                     (:NAME "name")
                     (:NAME "shoeSize"))))
        (safe-defclass-expander args))
    (:MACRO-CALL (:NAME "@iife")
     (:BODY
      (:DEFUN "Person" ("name" "shoeSize")
       ((:STAT (:ASSIGN T (:DOT (:NAME "this") "name")
                        (:NAME "name")))
        (:STAT (:ASSIGN T (:DOT (:NAME "this") "shoeSize")
                        (:NAME "shoeSize")))))
      (:BLOCK NIL)
      (:RETURN
       (:FUNCTION NIL ("name" "shoeSize")
        ((:RETURN (:NEW (:NAME "Person") ((:NAME "name")
                                          (:NAME "shoeSize")))))))))

It seems to do what we want. Let's install and test it:

    > (cheat-js:register-macro-expander "@safeDefclass"
                                        #'safe-defclass-expander)
    > (cheat-js:explode "var Person = @safeDefclass(Person, name, shoeSize);")
    "var Person = function() {
        function Person(name, shoeSize) {
            this.name = name;
            this.shoeSize = shoeSize;
        }
        return function(name, shoeSize) {
            return new Person(name, shoeSize);
        };
    }();"

So composing macros (`@safeDefclass` expands into a call to `@iife`)
works.

### Defining `@whenLet`

The `@whenLet` invocation in the examples isn't very suitable for
defining `@whenLet`. We can do with a shorter body and more than one
variable in the 'let' part. Let's use this invocation:

    @whenLet(t1, 1, t2, 2, t3, 3; f(t1, t2, t3););

This is an 'args and body' macro (the `test1, 1, test2, 2, test3, 3`
is the 'args' part, `f(t1, t2, t3);` is the body). They are separated
by a semicolon. Let's tell Cheat-JS about this macro:

    > (cheat-js:register-args-and-body-macro "@whenLet")

The AST of the invocation:

    > (cheat-js:parse-js "@whenLet(t1, 1, t2, 2, t3, 3; f(t1, t2, t3););")
    (:TOPLEVEL
     ((:STAT
       (:MACRO-CALL (:NAME "@whenLet")
        ((:ARGS (:NAME "t1") (:NUM 1) (:NAME "t2") (:NUM 2) (:NAME "t3") (:NUM 3))
         (:BODY
          (:STAT (:CALL (:NAME "f") ((:NAME "t1") (:NAME "t2") (:NAME "t3"))))))))))

The `:MACRO-CALL` node has subnodes for both `:ARGS` and `:BODY`, they
will be both passed to our expander function.

What about the expansion? As the
[`when-let` documentation](http://common-lisp.net/project/alexandria/draft/alexandria.html#Data-and-Control-Flow)
says, `@whenLet` should run the statements in its body if all the
bound variables are true. And it would be nice to have a new scope for
our variables, so this is a suitable expansion:

    (function(t1, t2, t3) {
        if (t1 && t2 && t3) {
            f(t1, t2, t3);
        }
    })(1, 2, 3);

The AST of the expansion:

    >(cheat-js:parse-js "(function(t1, t2, t3) {
                             if (t1 && t2 && t3) {
                                 f(t1, t2, t3);
                             }
                         })(1, 2, 3);")
    (:TOPLEVEL
     ((:STAT
       (:CALL
        (:FUNCTION NIL ("t1" "t2" "t3")
         ((:IF (:BINARY :&& (:BINARY :&& (:NAME "t1") (:NAME "t2")) (:NAME "t3"))
           (:BLOCK
            ((:STAT (:CALL (:NAME "f") ((:NAME "t1") (:NAME "t2") (:NAME "t3"))))))
           NIL)))
        ((:NUM 1) (:NUM 2) (:NUM 3))))))

So we need to transform the `:MACRO-CALL` node of the invocation into
the topmost `:CALL` node of the expansion:

    (defun when-let-expander (args body)
      (let* ((grouped-args (group args 2))
             (arg-vars (mapcar #'first grouped-args))
             (arg-values (mapcar #'second grouped-args))
             (arg-var-names (mapcar #'second arg-vars)))
        `(:CALL
          (:FUNCTION NIL ,arg-var-names
                     ((:IF ,(make-binary-and-ast arg-vars)
                           (:BLOCK
                            ,body)
                           NIL)))
          ,arg-values)))

Notice that `when-let-expander` has two arguments, `args` and `body`,
which contain the contents of the `:ARGS` and `:BODY` nodes of the
`:MACRO-CALL` tree. This function uses two helper functions shown
below:

    (defun group (list n)
      (if (< (length list) n)
          (if list
              (list list))
          (cons (subseq list 0 n)
                (group (subseq list n) n))))

    (defun make-binary-and-ast (operands)
      (cond ((= 1 (length operands))
             (first operands))
            ((= 2 (length operands))
             (list* :binary :&& operands))
            ((> (length operands) 2)
             (let ((first-two (subseq operands 0 2))
                   (rest (subseq operands 2)))
               (make-binary-and-ast (cons (make-binary-and-ast first-two)
                                          rest))))
            (t (error "Incorrect number of operands for @whenLet."))))

We need `group` to help with destructuring the variables and values,
and `make-binary-and-ast` to build valid AST trees for 'and'-ing more
than two operands.

Let's test our function:

    > (let ((args '((:NAME "t1") (:NUM 1)
                    (:NAME "t2") (:NUM 2)
                    (:NAME "t3") (:NUM 3)))
            (body '((:STAT
                     (:CALL (:NAME "f")
                            ((:NAME "t1") (:NAME "t2") (:NAME "t3")))))))
        (when-let-expander args body))
    (:CALL
     (:FUNCTION NIL ("t1" "t2" "t3")
      ((:IF (:BINARY :&& (:BINARY :&& (:NAME "t1") (:NAME "t2")) (:NAME "t3"))
        (:BLOCK
         ((:STAT (:CALL (:NAME "f") ((:NAME "t1") (:NAME "t2") (:NAME "t3"))))))
        NIL)))
     ((:NUM 1) (:NUM 2) (:NUM 3)))
     
The result of the function is identical to our desired expansion, so
we can install our expander and test our new macro:

    > (cheat-js:register-macro-expander "@whenLet" #'when-let-expander)
    > (cheat-js:explode "@whenLet(t1, 1, t2, 2, t3, 3; f(t1, t2, t3););")
    "(function(t1, t2, t3) {
        if (t1 && t2 && t3) {
            f(t1, t2, t3);
        }
    })(1, 2, 3);"
    
Exercises: 

 1. Can you define `@let` by simplifyind `@whenLet` a little?
 2. Can you define `@iife` so `@iife(console.log(1);)` expands into
 `@let(;console.log(1);)`? Why is the first semicolon (just after the
 opening paren) necessary in the last `@let` invocation?

### Defining `@awhen`

... should be very easy. `awhen`, defined in *On Lisp*, page 190, is
just `when-let` with one anaphoric variable, `it`. So the invocation:

    @awhen(expr;f(it);)
    
should expand into:

    @whenLet(it, expr;f(it);)
    
Let's declare `@awhen` to be an 'args and body' macro:

    > (cheat-js:register-args-and-body-macro "@awhen")
    
If you have restarted your REPL after reading the last section, please
make sure to redefine `@whenLet` in your new REPL.

The AST of the invocation:

    > (cheat-js:parse-js "@awhen(expr;f(it);)")
    (:TOPLEVEL
     ((:STAT
       (:MACRO-CALL (:NAME "@awhen")
        ((:ARGS (:NAME "expr"))
         (:BODY (:STAT (:CALL (:NAME "f") ((:NAME "it"))))))))))
         
The AST of the expansion:

    > (cheat-js:parse-js "@whenLet(it, expr;f(it);)")
    (:TOPLEVEL
     ((:STAT
       (:MACRO-CALL (:NAME "@whenLet")
        ((:ARGS (:NAME "it") (:NAME "expr"))
         (:BODY (:STAT (:CALL (:NAME "f") ((:NAME "it"))))))))))         
         
The expander:

    (defun awhen-expander (args body)
      `(:MACRO-CALL (:NAME "@whenLet")
                    ((:ARGS (:NAME "it") ,(first args))
                     (:BODY ,@body))))
                     
Let's install the expander and test `@awhen`:

    > (cheat-js:register-macro-expander "@awhen" #'awhen-expander)
    > (cheat-js:explode "@awhen(expr;f(expr););")
    "(function(it) {
        if (it) {
            f(expr);
        }
    })(expr);"
    
It works!

### More macros

See the
[reference](https://github.com/mbrezu/Cheat-JS/blob/master/REFERENCE.md)
for macros already packaged with Cheat-JS.

### Conclusion

To define a Cheat-JS macro, you need to know the three things required
for any macro: the invocation, the expansion, the transformation. 

You also need to call one of the `cheat-js:register-*-macro` functions
to declare the type of your macro and
`cheat-js:register-macro-expander` to install the expander
function. It's best to call `cheat-js:clear-macros` before your macro
definitions to start clean.

Use `cheat-js:explode` to macroexpand JavaScript code after you
defined the macros.
     
## Troubleshooting

Right now Cheat-JS doesn't give very nice error messages when parsing
a macro invocation fails; I'll try to improve error handling there.

If things break, right now the best course is to isolate the problem
(it's a problem in the expander? is the expected AST for the expansion
incorrect? etc.). The examples above should provide some information
about how Cheat-JS works. Reading `cheat-js.lisp` (rather small right
now, less than a hundred lines) and the `tests.lisp` files may provide
more clues about what Cheat-JS expects from a macro definition.

## Closing thoughts

Still reading? Wow!

One thing is obvious: Cheat-JS makes it possible to write macro-like
transformations on JavaScript code, but it's not nearly as easy as
writing Common Lisp macros. Maybe this isn't a bad thing - we should
be writing macros only when there's no other way to avoid code
duplication.

There are plenty of quirks. There's only so many transformations you
can do (function calls are not as frequent in JavaScript as they are
in Common Lisp, and macro invocations are 'hooked' to function
calls). Maybe `parse-js` could be tweaked harder to make it possible
to insert macros at other points. For now, the transformations
possible with 'function call' macros are enough for me.

You need to be able to 'pattern match' ASTs and figure out how to
transform macro invocations ASTs into macro expansions ASTs. This is a
basic macro writing skill, but with an indirection (in Common Lisp the
source code is the AST, not so with JavaScript).

You also need to know Common Lisp. In theory, a Cheat-JS based
preprocessor could be distributed and used by people who are only
'consuming' macros produced by someone else (a 'macro
producer'). Hmmm, people will certainly be amused if a 'macro
producer' starts distributing a 40MB executable (this is about the
minimum size for SBCL standalone executables) that explodes constructs
in the source code into larger code :-)

With a JavaScript parser written in JavaScript it would be possible to
do what Cheat-JS does without Common Lisp (though without backquotes
the generation of macro expansions is probably a pain, and the AST
would have to be uniform - nested arrays, no classes, maybe? - to make
it easier to 'pattern match' and analyze).

Right now, the audience of Cheat-JS is probably the audience of
ParenScript (mostly because you need to be a lisper to fully use
Cheat-JS). I (Miron Brezuleanu) wrote a few thousands of lines of
ParenScript code and found that there is some 'impedance mismatch'
between ParenScript and JavaScript (especially around the '.' operator
in JavaScript and modules in JavaScript). This was most likely my
fault: instead of writing Lisp to be compiled to JavaScript, I was
trying to write JavaScript with s-expressions. I found it harder to
write code in ParenScript than in JavaScript, and the presence of
macros didn't compensate for this extra effort. I tried to find a way
to have macros while writing something closer to JavaScript. Cheat-JS
is what I came up with.

Thanks for taking the time to read about Cheat-JS; I hope you'll find
it useful (or at least amusing - or both)!

Please use the Github
[issues page](https://github.com/mbrezu/cheat-js/issues) to report any
bugs or to post feature requests.

