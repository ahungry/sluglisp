## lisp-unit2

*lisp-unit2* is a Common Lisp library that supports unit testing.  It
is a new version of a library of the [lisp-unit library written by
Chris Riesbeck][orig]. There is a long history of testing packages in
Lisp, usually called "regression" testers. More recent packages in
Lisp and other languages have been inspired by [JUnit for
Java][JUnit].

Recently longtime users at Acceleration.net felt motivated to refactor
significantly, attempting to make some broad improvements to the library
while maintaining its benefits and workflow

### Features

* Written in portable Common Lisp
* Loadable with [ASDF] or [Quicklisp]
* Simple to define and run tests
* Redefine functions and macros without reloading tests - tests are
  recompiled before each run and at definition time
* Tests have source-location for going to definition
* Supports testing: return values, printed output, macro expansions,
  and conditions.  Special support for testing floating and rational
  point numbers
* Running a single test returns a test-result object. Running many
  tests returns a test-result-db
* Tests grouped and accessed by name, tag, and package of test name
* Signals for starting and completing test runs (both individually and
  as a group)

#### Differences from lisp-unit version 1

* Simplified test retrieval / categorization.
 * Tests are stored by their name's symbol-package (easing the
   complexity of this package vs the package argument)
 * package arguments now just retrieve all tests in a given package
 * dynamic variable consolidation (eg: one test database with three hashtables
   instead of three dynamic variables containing hashtables)
* All output goes to its own stream (and can therefore be manipulated
  at will). *test-stream* defaults to *standard-output* as it always
  has.
* Debugging is controlled by *debug-hook*, continue restart activated
  around test
* Tests are named functions that show up on the stack (so that 
  goto-definition in a stack trace brings you to the test definition
* Contexts can be applied on a test or a run-test(s) to provide common
  setup/tear down locations (eg: provide a database to tests that need
  it).  Contexts are functions that accept a thunk and apply it inside
  of a dynamic context eg: 
  `(lambda (body-thunk) (let ((*var* 1))(funcall body-thunk)))`
* Signals used throughout (including to drive current output summaries)
 * assertions passing and failing (with abort restart, to cancel
   assertions)
 * tests starting and completing (with continue restart for resuming
   test run after an errors)
 * test batches starting and completing
 * Dynamic variables available in signal handlers and all tests
   *unit-test* and *results*
* Logging used throughout (to ease debugging of lisp-unit2) - compile
  time
* TAP output displays better messages about the error and is more
  consistent with the summary output
* Better job of reporting compiler warnings and errors: when defining
  the test (and while running it)
 * Summary output prints test starting messages. Compiler / run-time
   errors printed into the output is obviously tied to the test in
   question. (Previously the results printed after the error messages
   and so it was textually ambiguous to which test the messages
   applied)
* lisp-unit2 is no longer loadable as a single file
* :lisp-unit2 feature is available for conditional compilation
* Better optimized for running both in a build-environment (jenkins
  etc) and from the REPL (previously there were many non-standard
  boolean flags for controlling output and debugging).
* test bodies compiled with (debug 3) - TODO: revisit this in light of
  slow tests if that becomes an issue
* Default ordering for tests and results is the definition order
  (instead of random or reversed)
* assert-result -> assert-passes?
* (define-test name &body) became (define-test name (&key) &body)

### How to use lisp-unit2

1. Load using [Quicklisp][] : `(ql:quickload :lisp-unit2)` or [ASDF][]
   : `(asdf:load-system :lisp-unit2)`.
2. Define some tests (for best luck define tests in their own package
   by making their name be in a specific package).  By having tests in
   their own package, the test and the fn being tested can share the
   same name. (Tests are compiled to a function named after the test
   that runs it and an object in the test database)

```
(lisp-unit2:define-test my-tests::test-subtraction 
    (:tags '(my-tests::bar))
  (assert-eql 1 (- 2 1)))
```

3. Run all tests in your package

```
;; with-summary provides results while the tests are running
;;;; using the context function
(run-tests :package :my-tests
           :run-contexts #'with-summary-context)

;;;; using the context macro (does the same thing as
;;;; :run-contexts, See Contexts below for an explanation).
(with-summary () (run-tests :package :my-tests))

;; to print the results after the run is complete
(print-summary (run-tests :tags 'my-tests::bar))

;; The difference is in whether or not the output occurs while the
;; tests are running or after all the tests have run

;; to disable the debugger:
(let (*debugger-hook*)
  (run-tests :tests 'my-tests::test-subtraction))

;; to debug failed assertions with the context function
(run-tests :tests 'my-tests::test-subtraction
           :run-contexts #'with-failure-debugging-context)

;; or use the context macro
(with-failure-debugging ()
  (run-tests :tests 'my-tests::test-subtraction))
```

See the internal test suite for more and better examples (internal-test/*)

#### Defining Tests

The `define-test` macro is the primary way to install a
test. `define-test` creates a function with the same name as the test,
which can be called to execute the test.  This is nice because there
is a direct way to call every test, but also because the test has
implicit source destination and thus "go-to-definition" on any
printing of the test name will take you directly to the test in
question.  `define-test` also creates a unit-test object and inserts
it into the test-database.

The test body is compiled at the time of definition (so that any
compile warnings or errors are immediately noticeable) and also before
every run of the test (so that macro expansions are never out of
date).

* `package`: if package is provided the test name and all tags are
  reinterned into that package before definition.  This is mostly for
  porting lisp-unit systems where test-names were not functions and so
  tests could be named the same as the function they tested.
* `contexts` (see Contexts below): a (single or tree of) context
  fn that will be applied around the test body
* `tags`: a list of symbols used to organize tests into groups (not
  hierarchical)

#### Undefining Tests

Because lisp-unit2 keeps a database of tests and functions, removing a
tests is not as simple as deleteing the test from from your
file. Instead a function `uninstall-test` and a macro `undefine-test`
allow you to remove all the runtime components of a test from
lisp-unit2.  `Uninstall-test` accepts the test's symbolic name.
`Undefine-test` is a macro whose form mimics define-test, so that you
can simply add the two characters and compile the form to remove the
test.  This also gives you the ability to easily leave tests in your
test file that are inactive currently.

#### Running Tests

The primary entry point for running tests is
`lisp-unit2:run-tests`. `run-tests` and `get-tests` both accept (&key
tests tags package reintern-package) as discussed below in "Test
Organization". Each test can also be run individually by calling the
function named for the test, and by calling `lisp-unit2:run-test`
which each return a single `test-result`.

`run-tests` returns a test-results-db, and aside from the keys above 
accepts,

* `run-contexts`: a (single, or list of) context that will be
  applied around the entire body of running the tests
* `test-contexts`: a (single, or list of) context that will be
  applied around each tests body.  This will be around any contexts
  defined on the unit test
* `name`: a name used for this test run. Useful for identifying what
  defaults to all packages being tested. We attempt to make a good
  default if nothing is provided

Once you have a test-result-db (or list there of) you can call
`rerun-tests` or `rerun-failures` to rerun and produce new results.

#### Test Organization: Names, Tags, and Packages

Tests are organized by their name and by tags.  Both of these are
symbols in some package.  Tests can be retrieved by their name, the
package that their name is in, and any of the tags that reference the
test.

The most common way to retrieve and run unit tests is run-tests which
calls get-tests.

`(lisp-unit2:run-tests &key tests tags package reintern-package)`
`(lisp-unit2:get-tests &key tests tags package reintern-package)`

Both of these functions accept:

* tests: a single or list of symbols or unit-test objects
* tags: a single or list of symbols.  All tests in these tags will be
  returned
* package:  a single or list of packages (names or objects)

If no arguments are provided lisp-unit2 will run all tests in *package*

In some cases, particularly when converting from lisp-unit(1) we need
our tests to be in a different package (because tests are functions in
their name's package). In lisp-unit these tests would not conflict
with a function named the same (because tests were not functions). To
ease conversion, the reintern-package argument will reintern all test
names and tags provided into a different package.  Define test accepts 
a `package` argument that mirrors this functionality.  Suggested usage
is to either have tests be named differently from the functions they test
or to have tests and tags be in an explicitly referenced package, eg:
`(define-test my-tests::test1 (:tags '(my-tests::tag1)) ...)`

Tests are organized into the `*test-db*` which is an instance
`test-database`.  These can be rebound if you needed to write tests
about your test framework (see the internal example-tests).

##### Suites 

While lisp-unit does not have any specific notion of a suite, it is
believed that the tests are composable enough that explicit test
suites are not needed.

One suggestion would be to have named functions that run you specific
set of tests:

```
(defun run-{suite} ()
  (lisp-unit2:run-tests
    :name :{suite}
    {stuff to test} ))

(defun run-symbol-munger-and-lisp-unit2-tests () 
  (lisp-unit2:run-tests
    :name :symbol-munger-and-lisp-unit2-tests
    :package '(:symbol-munger-tests :lisp-unit2-tests)))

(defun run-error-and-basic-tests () 
  (lisp-unit2:run-tests
    :name :error-and-basic-tests
    :tests '(symbol-munger-test::test-basic)
    :tags '(:errors lisp-unit2-tests::errors)))
```

Another suggestion would be to define tests that call other tests:
```
(define-test suite-1 (:tags '(suites))
  (test-fn-1) ;; calls test-fn-1 unit-test
  (lisp-unit2::run-tests ...) ; runs an entire other set of unit tests)
```

#### Assertions

All `assert-*` macros signal `assertion-pass` and `assertion-fail` by
comparing their expected results to the actual results during
execution.  All other values in the assert forms are assumed to be
extra data to aid debugging.

`assert-{equality-test}` macros compare the actual to the expected
value of each of the values returned from expected (see:
multiple-values) (eg: `(assert-eql exp act)` => `(eql act exp)`) This
ordering was used so that functions like typep, member, etc could be
used as test.

`assert-false` and `assert-true` simply check the generic-boolean
truth of their first arg (eg: null and (not null)

`assert-{signal}` and `assert-{no-signal}` macros are used for testing
condition protocols.  Signals that are expected/not-expected handled.

##### Multiple-values and assertions

Actual values are compared to all expected values, that is:

```
LISP-UNIT2-TESTS> (lisp-unit2:with-assertion-summary ()
                    (assert-eql (values 1 2) (values 1 2 3)))


<No Test>:Passed (ASSERT-EQL (VALUES 1 2) (VALUES 1 2 3))
T
LISP-UNIT2-TESTS> (lisp-unit2:with-assertion-summary ()
                    (assert-eql (values 1 2 3) (values 1 2)))

Failed Form: (ASSERT-EQL (VALUES 1 2 3) (VALUES 1 2))
Expected 1; 2; 3
but saw 1; 2
```

#### Debugging

Debugging is controlled by `*debugger-hook*` (as is usual in common-lisp).
You can make lisp-unit simply record the error and move on by binding
`*debugger-hook*` to nil around your `run-tests` call.

If you would like to debug failed assertions you can wrap your call in
`with-failure-debugging` or apply the `with-failure-debugging-context`
to the unit-test run.

#### Output and Results

All output is printed to `*test-stream*` (which by default is
`*standard-output*`).  Most forms do not output results by default,
instead returning a result object.  All results objects can be printed
(to `*test-stream*`) by calling `print-summary` on the object in
question.  

`print-summary` prints information about passing as well as failing
tests. `print-failure-summary` can be called to print only messages
about failures, warnings, errors and empty tests (empty tests had no
assertions).  Care is taken to print tests with their short, but still
fully packaged symbol-name (so that go-to-definition) works.

When running interactively `with-summary(-context)` can provide
real-time output, printing start messages and result messages for each
test.  `with-assertion-summary(-context)` provides even more detailed
output printing a message for each assertion passed or failed.

Test results (from one or many runs) can be captured using
`with-test-results`.  The arg: `collection-place` will copy all the
results as they arrive into a location of your choosing (eg: variable,
object slot). The arg: `summarize?` will print a failure summary of
each test-run after all of the tests are finished running.  This is
useful for collecting separate results for many packages or systems
(see test-asdf-system-recursive).  If no args are provided summarize?
is defaulted to true.

##### TAP Output

Lisp-unit2 provides TAP (test anything protocol) test results (printed
in such a way that jenkins tap plugin can parse them).

`with-tap-summary` prints tap results as the tests run `write-tap`,
`write-tap-to-file` accept a test-results database and write the TAP
results either to *test-stream* or a file

#### ASDF

`asdf:test-system` is assumed to be the canonical way of testing a
every system, and so lisp-unit2 makes effort to work well with
test-system

Here is an example asdf:test-sytem definition, which will print the
verbose summary of the tests as they run.

```
(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :symbol-munger))))
  (asdf:oos 'asdf:load-op :symbol-munger-test)
  (let ((*package* (find-package :symbol-munger-test)))
    (eval (read-from-string "
            (lisp-unit2:with-summary ()
             (lisp-unit2:run-tests
              :package :symbol-munger-test
              :name :symbol-munger
              :run-context))
      "))))
```

Additionally, lisp-unit2 provides test-asdf-system-recursive which
accepts a (list or single) system name, looks up all its dependencies
and calls asdf:tests-system on each listed system.  Any lisp-unit or
lisp-unit2 test-results-dbs are collected and returned at the end.  A
failure summary is also printed for each result db (so that after
running many tests you are presented with a short synopsis of what ran
and what failed.

There is an interop layer for converting test results from other
systems to lisp-unit2 test results, so that we can gather and
summarize more information.  Currently this is only implemented for
lisp-unit1, but patches would be welcome to allow collecting test
results from any other lisp test systems. (This is currently a bit
tedious, simplification patches welcome as well, see interop.lisp).

#### Signals

Signals are used throughout lisp-unit2 to communicate progress and
results throughout lisp unit

* `missing-test`: when asked to run a test that does not exist
* `test-start`: when a unit test starts, contains a ref to the
  unit-test
* `test-complete`: when a test completes contains a ref to results for
  that test (test-result)
* `all-tests-start`, `all-tests-complete`: signaled at the beginning
  and end of run-tests, contains a ref to the test-results-db
* `collect-test-results`: used in it interop to send results to 
  `with-test-results`
* `assertion-pass`: signaled when an assertion passes
* `assertion-fail`: signaled when an assertion fails

All signals have an abort interrupt which simply cancels the signal.
This is mostly used for meta-testing (ie: testing lisp-unit2), but
there are conceivably other uses.

#### Contexts / Fixtures

Contexts allow you to manipulate the dynamic state of a given
unit-test or test-run.  These are functions of a single required
argument (a thunk), that they execute inside of a new/changed dynamic
environment.

A good example is the with-failure-debugging-context, which simply
invokes the debugger whenever we signal a failed assertion

```
(defun with-failure-debugging-context (body-fn)
  "A context that invokes the debugger on failed assertions"
  (handler-bind ((assertion-fail #'invoke-debugger))
    (funcall body-fn)))
```

Both `define-test` and `run-tests` accept a tree of contexts that is
flattened and turned into a single context function (see
`combine-contexts`).  This function then executes the body-fn for us
(see: `do-contexts`).  

This should allow any type of manipulation of the current lisp-unit2
environment through access to handling signals and setting up and
tearing down dynamic environments.

Example Contexts: 

* rolled-back-database-context: open a connection to the dev database and
  a transaction that we will abort at the end
* http-context: establish a dummy http context for use in testing web
  connected things
* html-output-context: establish global html building objects needed for 
  html output functions to work
* dev-mode-context: bind any dynamic variables related to DEV/LIVE mode
* failure-debugging-context: (as above) handling signals and debugging
  them instead of only recording them

Example test-with-contexts defintion:

```
(defmacro db-render-test (name (&rest args) &body body)
  `(lisp-unit2:define-test ,name
    (:tags ',args
     :contexts
     (list #'test-context #'dom-context #'database-context )
     :package :test-objects)
    ,@body))
```

#### Data Model

* `test-database`: a list and some indexes of all installed tests
* `unit-test`: an object containing the name, code, contexts, and
  documentation for a single unit-test
* `test-results-db`: a collection of results from a single `run-tests` call
* `test-result`: the result of running a single unit test
* `failure-result`: information useful for debugging failures (eg: unit-test,
  the form that failed, the expected and actual results)


##  Remaining Tasks
*  Expanded internal testing.

### Future Features
* Benchmarking tools
* More interop with other test systems
* Database layer to store test results to a database (so that we can
  track bugs / test durations) over time (goes with benchmarking)

[orig]: <http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html>
  "Original Lisp Unit"
[wiki]: <https://github.com/OdonataResearchLLC/lisp-unit/wiki>
  "Lisp Unit Wiki"
[JUnit]: <http://www.junit.org> "JUnit"
[Quicklisp]: <http://www.quicklisp.org> "Quicklisp"
[ASDF]: <http://common-lisp.net/project/asdf/> "ASDF"
[TAP]: <http://testanything.org/> "Test Anything Protocol"

## 0.2.0 Acknowledgments
* Russ Tyndall - Acceleration.net 

## 0.9.5 Acknowledgments

* [Jesse Alama][jessealama] for usability feedback. 
[jessealama]: <https://github.com/jessealama> "Jesse Alama"
