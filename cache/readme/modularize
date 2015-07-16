About Modularize
----------------
Modularize is an attempt at providing a common interface to segregate major application components. This is achieved by adding special treatment to packages. Each module is a package that is specially registered, which allows it to interact and co-exist with other modules in better ways. For instance, by adding module definition options you can introduce mechanisms to tie modules together in functionality, hook into each other and so on.

How To
------
Each module should consist of a `DEFINE-MODULE` form and an ASDF system with the superclass `MODULARIZE:MODULE`. `DEFINE-MODULE` acts as a wrapper around `DEFPACKAGE`, so it will replace your usual `DEFPACKAGE` form. Any option you would use in `DEFPACKAGE` is also usable in `DEFINE-MODULE`, but the latter also allows for custom-defined options that perform specific actions upon module creation.

```
(define-module test-module
  (:use #:cl #:modularize)
  (:export #:greet))
```

If for some reason you absolutely do not want to use `DEFINE-MODULE`, you may define your package manually and call `MODULARIZE` on it afterwards. Note though that you also need to manually perform all changes that additional module options may otherwise perform for you. If your module-name somehow differs from the asdf system, you will need to specify this in your asdf system definition:

```
(asdf:defsystem some-test-module
  :class "modularize:module"
  :defsystem-depends-on (:modularize)
  :module-name "test-module"
  ...)
```

When loading a module-system, `*PACKAGE*` is automatically bound to `MODULARIZE-USER` so that you may use `DEFINE-MODULE` without having to switch to a specific package first. It might still be a good idea to precede your `DEFINE-MODULE` form with an `(in-package #:modularize-user)` though. The main difference over packages is that each module has a central storage table, which you can access with `MODULE-STORAGE`. This allows you to save metadata on modules to keep track of the different parts each module might play in your application.

Another function that might prove useful is `DELETE-MODULE`, which attempts to offer a mechanism to completely remove a module so as to revert its changes. Without any further additions, this will simply result in all symbols in the module package being `MAKUNBOUND` and `FMAKUNBOUND` and the package getting deleted. Since a module might also cause changes outside of its own package, it is therefore advised to add deletion hooks through `DEFINE-DELETE-HOOK` as to make sure other kinds of changes can be cleaned up as well.

Similarly if you want to tuck on functionality once a module is defined to initialise it, you can hook into that with `DEFINE-MODULARIZE-HOOK` and `DEFINE-OPTION-EXPANDER`. The former works like `DEFINE-DELETE-HOOK` and is called once `MODULARIZE` is called on a package, after the package's storage has been set up. The latter allows you to declare custom forms that a `DEFINE-MODULE` call should expand to. Note that module options are only expanded _after_ `MODULARIZE` is called, so you may use the storage in your expansions.

Lastly, if a component should extend the functionality of another, this can be more intuitively done through `DEFINE-MODULE-EXTENSION`. In looks and form it is the same as `DEFINE-MODULE`, with the difference that it won't create a new package, but use the one it is extending. Through this you can export new functions and other additions without running into a multiple-package mess.

For a super small sample use of a module, have a look at [modularize-test-module.asd](https://github.com/Shinmera/modularize/blob/master/modularize-test-module.asd) and [modularize-test-module.lisp](https://github.com/Shinmera/modularize/blob/master/modularize-test-module.lisp). For extensions to the module system, have a gander at [modularize-interfaces](https://github.com/Shinmera/modularize-interfaces) and [modularize-hooks](https://github.com/Shinmera/modularize-hooks).
