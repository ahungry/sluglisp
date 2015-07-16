# cl-junit-xml

Small library for writing [junit][]-compatible XML files.

[junit]: http://www.junit.org

## Example

    CL-JUNIT-XML> (let* ((junit (make-junit))
           (suite (add-child junit (make-testsuite "suite" :timestamp "now"))))
      (add-child suite (make-testcase "test" "class" 1.0
                                      :failure "invalid assertion"))
      (add-child suite (make-testcase "test 2" "class" 2.0
                                      :error "problem running the test"))
      (add-child suite (make-testcase "test 3" "class" 5.0))
      (write-xml junit T :pretty-p T))
    <?xml version="1.0" encoding="UTF-8"?>
    <testsuite name="suite" timestamp="now" id="0" tests="3" errors="1" failures="1" time="8.0">
      <testcase name="test 3" classname="class" time="5.0"/>
      <testcase name="test 2" classname="class" time="2.0">
        <error>
          <![CDATA[problem running the test]]></error>
      </testcase>
      <testcase name="test" classname="class" time="1.0">
        <failure>
          <![CDATA[invalid assertion]]></failure>
      </testcase>
    </testsuite>



## API

### `make-junit`

creates a new junit root XML object you can add suites to

### `make-testsuite`

creates a new junit testsuite add testcases to

### `make-testcase`

creates a new junit testcase

### `(add-child parent child)`

add cases to suites, and suites to junit. returns the child

### `(write-xml junit sink &key pretty-p &allow-other-keys)`

writes the junit XML to the given sink. Supports sinks of:

* `nil` - returs the XML as a string
* `T` - writes the XML to `*standard-output*`
* any string - writes the XML to that file, returns the pathname
* any pathname - writes the XML to that file, returns the pathname

if `pretty-p` is non-nil, then the XML produced is indented.

Other keys are allowed to support integration with other testing
libraries.

## Integration with other testing libraries

### [lisp-unit2][]

[lisp-unit2][] support is available via the `cl-junit-xml.lisp-unit2`
ASDF system, and adds some additional `write-xml` specializations for
[lisp-unit2][] objects.

Any dots (`.`) in test names will be read as part of the `classname`
for junit, which affects rendering in systems like Jenkins.

For example, assuming you start with `(lisp-unit2:run-tests :name :my-tests)`:

* the test `frobs` will appear in junit as package `MY-TESTS`, class
  `MY-TESTS.root`, name `frobs`
* the test `foo.frobs` will appear in junit as package `MY-TESTS`,
  class `MY-TESTS.FOO`, name `foo.frobs`

#### writing XML directly

    (write-xml (lisp-unit2:run-tests :name :my-tests) T :pretty-p T)

#### writing XML via the [lisp-unit2][]'s signals:

    (handler-bind
        ((lisp-unit2:all-tests-complete
           #'(lambda (c)
               (write-xml c T :pretty-p T))))
      (lisp-unit2:run-tests :name :my-tests))

[lisp-unit2]: https://github.com/AccelerationNet/lisp-unit2

### [lisp-unit][]

[lisp-unit][] support is available via the `cl-junit-xml.lisp-unit`
ASDF system, and adds some additional `write-xml` specializations for
[lisp-unit][] objects.

Adds a `:name` keyword arg to `write-xml` that will be used as the
default XML `classname`.

Classname handling is similar to lisp-unit2. For example, assuming you
start with `(write-xml test-results T :name :my-tests)`:

* the test `frobs` will appear in junit as package `MY-TESTS`, class
  `MY-TESTS.root`, name `frobs`
* the test `foo.frobs` will appear in junit as package `MY-TESTS`,
  class `MY-TESTS.FOO`, name `foo.frobs`

#### writing XML via the [lisp-unit][]'s signals:

    (handler-bind
        ((lisp-unit:tests-run-complete
           #'(lambda (c)
           (write-xml c T :pretty-p T :name "my-tests"))))
      (lisp-unit:signal-results)
      (lisp-unit:run-tests))

[lisp-unit]: http://www.cliki.net/lisp-unit
