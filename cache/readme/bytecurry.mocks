<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. bytecurry.mocks</a>
<ul>
<li><a href="#sec-1-1">1.1. WITH-MOCKED-FUNCTIONS</a></li>
<li><a href="#sec-1-2">1.2. WITH-ADDED-METHODS</a></li>
<li><a href="#sec-1-3">1.3. More Info</a></li>
</ul>
</li>
</ul>
</div>
</div>

# bytecurry.mocks<a id="sec-1" name="sec-1"></a>

**bytecurry.mocks** is a small library for using mock functions and methods in unit tests.
It is framework agnostic. That is, it doesn't depend on any specific testing framework, nor is it
designed with any specific framework in mind. In fact **bytecurry.mocks** has no external dependencies.

**bytecurry.mocks** contains two macros: `with-mocked-functions` and `with-added-methods`.
These macros allow you to create mock functions and methods respectively in a limited scope
so that you can control the outputs of functions while running a unit test.

## WITH-MOCKED-FUNCTIONS<a id="sec-1-1" name="sec-1-1"></a>

`with-mocked-functions` is a macro which acts very similarly to `flet`, but binds the
functions with dynamic scope rather than lexical scope. It does this by redefining the function
before executing the body, then restores the original definition afterwards.

There are a couple of things to note when using this macro:
1.  You can't mock functions in a locked package (such as CL on sbcl)
2.  The compiler may inline function calls, in which case changing the function
    definition will have no effect on the inlined calls.

## WITH-ADDED-METHODS<a id="sec-1-2" name="sec-1-2"></a>

`with-added-methods` is somewhat similar to `with-mocked-functions`, but allows you to
define methods for a generic function in a limited scope (again with dynamic scope).
It defines new methods before executing the body, and then removes the method after the body.

Since it removes the method afterwards, and `defmethod` replaces any previous method with
the same specifiers, replacing an existing method will effectively remove the original
method outside the scope of `with-added-methods`.

## More Info<a id="sec-1-3" name="sec-1-3"></a>

For more information see the documentation strings.

The API manual is available [here](http://bytecurry.github.io/bytecurry.mocks/docs/).
