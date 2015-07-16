About Modularize-Interfaces
---------------------------
This is an extension to [Modularize](https://github.com/Shinmera/modularize) that allows your application to define interfaces in-code that serve both as a primary documentation and as compliance control. Other modules can then implement interfaces to provide the actual functionality as outlined in the definition. A good example for a scenario in which this would be useful is databases. If other parts of the application can rely on a standardised interface it won't matter which database is really connected underneath and the type of database connection can be switched without having to change any of the other modules that rely on this functionality.

How To
------
Interfaces are defined through `DEFINE-INTERFACE`. These calls consist of a name (or a list of names) for the interface and a body of component definitions. You can define your own components, but more on that later. By default there are components for functions, macros, generic functions, methods and classes. A simple interface might look like so:

```
(define-interface radio
  (defun listen (frequency)
    "Listens on a given frequency for radio signals and outputs them.")
  (defun scan ()
    "Attempts to search the radio frequency range for recognisable signals.")
  (defun broadcast (station)
    "Starts up the given radio station to broadcast.")
  (defmacro define-station ((name frequency) &body body)
    "Defines a new radio station on the given frequency."))
```

This will generate you an interface with the specified stub forms. Calling any of these functions before an implementation actually implements the interface will result in an error. In the back, `DEFINE-INTERFACE` creates a module through `DEFINE-MODULE` and then expands the specified components.

If a module wants to implement an interface, it can define this in the `:IMPLEMENTS` module-option. It will then be automatically set up as the current implementation. Otherwise you can also directly do `(SETF (IMPLEMENTATION :interface) :my-module)`. If an interface is already implemented by another module, a condition is raised with restarts to either abort, delete the old module or override it. After an implementation is set, `TEST-INTERFACE` is called to check for compliance. Compliance checks must be implemented by the respective components and may or may not be sufficient to test for true interface compliance. If an implementing module gets deleted, `RESET-INTERFACE` is called, which returns it to its old state with error-ing stub definitions.

Defining the actual components of the interface in your module works just like writing any other function or whatever part it is in effect. You may want to use the provided `I-*` macros for the standard components though to make the intent more visible.

Adding new components to use in interface definition happens through `DEFINE-COMPONENT-EXPANDER`. This should evaluate to whatever stub forms you need, optimally resulting in an error if a user tries to access it without a conforming implementation in place. Additionally you will most likely want to add a `DEFINE-COMPONENT-TESTER` for each of your components. This is a simple function definition that is called whenever `TEST-INTERFACE` is invoked and should return two values, a boolean whether the test failed or not and a string that explains the reason for failure or passing. `TEST-INTERFACE` will convert this into warnings. It is discouraged to signal errors as this would prevent other components from being tested too and may interfere while developing an implementation.

For a super small sample use of an interface, have a look at [interfaces-test-implementation.asd](https://github.com/Shinmera/modularize-interfaces/blob/master/interfaces-test-implementation.asd), [interfaces-test-interface.lisp](https://github.com/Shinmera/modularize-interfaces/blob/master/interfaces-test-interface.lisp) and [interfaces-test-implementation.lisp](https://github.com/Shinmera/modularize-interfaces/blob/master/interfaces-test-implementation.lisp).
